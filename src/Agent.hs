{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Agent where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.Async (Async)
import Control.Concurrent.Async qualified as Async
import Control.Concurrent.STM
import Control.Concurrent.STM.Delay (newDelay, waitDelay)
import Control.Exception
import Control.Monad (forever, void, when)
import Data.Foldable (for_, traverse_)
import Data.Maybe
import Data.Vector.Hashtables qualified as VHT
import Foreign.C.Types (CInt (..))
import Foreign.StablePtr
import GHC.Generics
import Internal.Callbacks
import Internal.Easy
import Internal.Multi
import Internal.Raw
import Language.C.Inline qualified as C
import Poller
import PyF
import Request
import System.Posix.Types
import Types

C.context (C.baseCtx <> C.funCtx <> C.fptrCtx <> C.bsCtx <> curlCtx)

C.include "<string.h>"
C.include "<stdlib.h>"

C.include "<curl/curl.h>"
C.include "HsFFI.h"

C.include "curl_hs_c.h"

data AgentMessage where
    Close :: AgentMessage
    Execute :: !RequestHandler -> AgentMessage
    UnpauseRead :: !RequestId -> AgentMessage
    UnpauseWrite :: !RequestId -> AgentMessage
    CancelRequest :: !RequestId -> AgentMessage

data AgentContext = AgentContext
    { multi :: !CurlMulti
    , eventManager :: !EventManager
    , msgQueue :: !(TQueue AgentMessage)
    , requests :: !(HashTable RequestId RequestHandler)
    , timerWaker :: !(TMVar ())
    , msgQueueAlive :: !(StablePtr (TQueue AgentMessage))
    }
    deriving (Generic)

data AgentHandle = AgentHandle
    { msgQueue :: !(TQueue AgentMessage)
    , agentThreadId :: !(Async ())
    , agent :: !AgentContext
    }
    deriving (Generic)

spawnAgent :: IO AgentHandle
spawnAgent = do
    eventManager <- newEventManager
    msgQueue <- newTQueueIO

    socketFunEnv <- curlCreateSocketCallbackCtx eventManager
    timerCtx <- curlCreateTimerCtx
    multi <- initCurlMulti socketFunEnv timerCtx

    agent <- Agent.new eventManager multi timerCtx msgQueue

    agentThreadId <- Async.async $ do
        Agent.run agent `catch` \(ex :: SomeException) -> print [fmt|agent died with exception {show ex}|]
    pure AgentHandle{msgQueue, agentThreadId, agent}

new :: EventManager -> CurlMulti -> CurlTimerFunEnv -> TQueue AgentMessage -> IO AgentContext
new eventManager multi timerCtx msgQueue = do
    requests <- VHT.initialize 100
    msgQueueAlive <- newStablePtr msgQueue
    pure $ AgentContext{timerWaker = timerCtx.timerWaiter, ..}

run :: AgentContext -> IO ()
run ctx = forever $ do
    -- sz <- VHT.size ctx.requests
    -- print [fmt|iterating {sz} requests|]

    -- print "polling messages"
    pollMessages ctx
    -- print "polling sockets"
    anyActions <- pollSockets ctx
    -- print "processing messages"
    when anyActions $ processMessages ctx

pollMessages :: AgentContext -> IO ()
pollMessages ctx@AgentContext{..} = go
  where
    go = do
        noActiveRequests <- VHT.null requests
        if noActiveRequests
            then do
                atomically (readTQueue msgQueue) >>= handleMessage ctx
                go
            else do
                msgs <- atomically (flushTQueue msgQueue)
                traverse_ (handleMessage ctx) msgs

handleMessage :: AgentContext -> AgentMessage -> IO ()
handleMessage ctx = \case
    Close -> error "Not implemented"
    Execute request -> doRequest ctx request
    CancelRequest requestId -> do
        request <- VHT.lookup ctx.requests requestId
        for_ request (cancelRequest ctx)
    UnpauseRead requestId -> do
        request <- VHT.lookup ctx.requests requestId
        case request of
            Just handler -> withCurlEasy handler.easy unpauseWrite
            Nothing -> pure ()
    UnpauseWrite _ -> error "Not implemented"

doRequest :: AgentContext -> RequestHandler -> IO ()
doRequest AgentContext{..} ci@(RequestHandler{..}) = withCurlMulti multi \multiPtr -> withCurlEasy easy \easyPtr -> do
    [C.block|void { curl_multi_add_handle($(CURLM* multiPtr), $(CURL* easyPtr)); }|]
    VHT.insert requests (handlerRequestId ci) ci

-- print "add new request"

pollSockets :: AgentContext -> IO Bool
pollSockets ctx@AgentContext{..} = do
    limitDelay <- newDelay 1000000
    anyActions <- awaitSocketActions limitDelay
    isExpired <- isJust <$> atomically (tryTakeTMVar timerWaker)
    when isExpired $ onTimer ctx
    pure $ anyActions || isExpired
  where
    triggerTimerLimited limitDelay = waitDelay limitDelay <|> readTMVar timerWaker
    -- Poll actions until we detect at least one or timeout rings
    awaitSocketActions limitDelay = do
        socketActivity <- atomically $ (True <$ pollForEvents eventManager) <|> (False <$ triggerTimerLimited limitDelay)
        if socketActivity
            then do
                actions <- flushEvents eventManager
                traverse_ (processSocketActionRequest ctx) actions
                pure True
            else pure False

processMultiMsg :: AgentContext -> CurlMultiMsg -> IO ()
processMultiMsg ac@AgentContext{..} msg = do
    let requestId = easyPtrRequestId msg.easy
    request <- VHT.lookup requests requestId
    case request of
        Just requestHandler -> completeRequestMulti ac requestHandler msg
        Nothing -> return ()

onTimer :: AgentContext -> IO ()
onTimer AgentContext{..} = withCurlMulti multi \multiPtr -> do
    [C.block|void {
        int running_handles;
        curl_multi_socket_action($(CURLM* multiPtr), CURL_SOCKET_TIMEOUT, 0, &running_handles);
    }|]

-- TODO: split processing messages
processSocketActionRequest :: AgentContext -> SocketActionRequest -> IO ()
processSocketActionRequest AgentContext{..} socketActionReq = withCurlMulti multi \multiPtr -> do
    let Fd fd' = fd socketActionReq
        CurlEventsOnSocket flags' = flags socketActionReq
    [C.block|void {
        int running_handles;
        curl_multi_socket_action($(CURLM* multiPtr), $(int fd'), $(int flags'), &running_handles);
    }|]

processMessages :: AgentContext -> IO ()
processMessages ac@AgentContext{..} = withCurlMulti multi $ \multiPtr -> do
    -- print "processing done request messages"
    [C.block|void {
        CURLMsg *message;
        int pending;
        
        while ((message = curl_multi_info_read($(CURLM* multiPtr), &pending))) {
            switch (message->msg) {
            case CURLMSG_DONE:
                $fun:(void (*completeRequestMulti')(CURL*, int))(message->easy_handle, message->data.result);
                break;
            default:
                break;
            }
        }
    }|]
  where
    completeRequestMulti' easyPtr resultCode = do
        -- print "in processing msg"
        processMultiMsg ac $ CurlMultiMsg{result = toEnum . fromIntegral @CInt $ resultCode, easy = easyPtr}

cancelRequest :: AgentContext -> RequestHandler -> IO ()
cancelRequest ctx reqHandler = withCurlMulti ctx.multi \multiPtr -> withCurlEasy reqHandler.easy \easyPtr -> do
    VHT.delete ctx.requests (handlerRequestId reqHandler)
    [C.block|void { 
        curl_multi_remove_handle($(CURLM* multiPtr), $(CURL* easyPtr));
        free($fptr-ptr:(simple_string* bufferSimpleString)->ptr);
    }|]
  where
    bufferSimpleString = reqHandler.responseSimpleString

-- print "request cancelled"

completeRequestMulti :: AgentContext -> RequestHandler -> CurlMultiMsg -> IO ()
completeRequestMulti ctx reqHandler msg = withCurlMulti ctx.multi \multiPtr -> withCurlEasy reqHandler.easy \easyPtr -> do
    let requestId = handlerRequestId reqHandler
    VHT.delete ctx.requests requestId
    [C.block|void { curl_multi_remove_handle($(CURLM* multiPtr), $(CURL* easyPtr));}|]
    void $ putMVar reqHandler.doneRequest msg.result
