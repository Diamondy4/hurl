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
import Control.Concurrent.STM.TBMQueue
import Control.Exception
import Control.Monad (forever, void, when)
import Data.Containers.ListUtils
import Data.Foldable (for_, traverse_)
import Data.Maybe
import Data.Vector.Hashtables qualified as VHT
import Foreign (freeHaskellFunPtr)
import Foreign.C.Types (CInt (..))
import Foreign.StablePtr
import GHC.Event
import GHC.Generics
import Internal.Callbacks
import Internal.Easy
import Internal.Multi
import Internal.Raw
import Language.C.Inline qualified as C
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
    , socketActionQueue :: !(TQueue SocketActionRequest)
    , requests :: !(HashTable RequestId RequestHandler)
    , fdMap :: !(HashTable Fd FdState)
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
    Just eventManager <- getSystemEventManager
    msgQueue <- newTQueueIO

    socketFunEnv <- curlCreateSocketCallbackCtx eventManager
    timerCtx <- curlCreateTimerCtx
    multi <- initCurlMulti socketFunEnv timerCtx

    agent <- Agent.new eventManager multi timerCtx socketFunEnv msgQueue

    agentThreadId <- Async.async $ do
        Agent.run agent `catch` \(ex :: SomeException) -> print [fmt|agent died with exception {show ex}|]
    pure AgentHandle{msgQueue, agentThreadId, agent}

new :: EventManager -> CurlMulti -> CurlTimerFunEnv -> CurlSocketFunEnv -> TQueue AgentMessage -> IO AgentContext
new eventManager multi timerCtx socketCtx msgQueue = do
    requests <- VHT.initialize 100
    msgQueueAlive <- newStablePtr msgQueue
    pure $ AgentContext{fdMap = socketCtx.fdMap, timerWaker = timerCtx.timerWaiter, socketActionQueue = socketCtx.socketActionQueue, ..}

run :: AgentContext -> IO ()
run ctx = forever $ do
    -- print "iterating"
    -- print "polling messages"
    pollMessages ctx
    -- print "polling sockets"
    poll ctx
    -- print "processing messages"
    processMessages ctx

pollMessages :: AgentContext -> IO ()
pollMessages ctx@AgentContext{..} = go Continue
  where
    go Stop = return ()
    go Continue = do
        noActiveRequests <- VHT.null requests
        if noActiveRequests
            then do
                atomically (readTQueue msgQueue) >>= handleMessage ctx
                go Continue
            else
                atomically (tryReadTQueue msgQueue) >>= \case
                    Just msg -> do
                        handleMessage ctx msg
                        go Continue
                    Nothing -> go Stop

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

poll :: AgentContext -> IO ()
poll ctx@AgentContext{..} = do
    limitDelay <- newDelay 1000000
    -- print "await socket actions"
    awaitSocketActions limitDelay
    isExpired <- isJust <$> atomically (tryTakeTMVar timerWaker)
    when isExpired $ onTimer ctx
  where
    triggerTimerLimited limitDelay = waitDelay limitDelay <|> readTMVar timerWaker
    -- Poll actions until we detect at least one or timeout rings
    awaitSocketActions limitDelay = do
        -- print "collect socket actions"
        actions <- atomically $ do
            socketActivity <- (True <$ peekTQueue socketActionQueue) <|> (False <$ triggerTimerLimited limitDelay)
            if socketActivity
                then flushTQueue socketActionQueue
                else pure mempty
        let acts = nubOrd actions
        -- print [fmt|traversing actions - {length acts} acts|]
        traverse_ (processSocketActionRequest ctx) acts

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
    [C.block|void { curl_multi_remove_handle($(CURLM* multiPtr), $(CURL* easyPtr));}|]
    freeHaskellFunPtr reqHandler.writeResponseBodyPtr

-- print "request cancelled"

completeRequestMulti :: AgentContext -> RequestHandler -> CurlMultiMsg -> IO ()
completeRequestMulti ctx reqHandler msg = withCurlMulti ctx.multi \multiPtr -> withCurlEasy reqHandler.easy \easyPtr -> do
    let requestId = handlerRequestId reqHandler
    VHT.delete ctx.requests requestId
    [C.block|void { curl_multi_remove_handle($(CURLM* multiPtr), $(CURL* easyPtr));}|]
    atomically $ closeTBMQueue reqHandler.responseBodyChan
    freeHaskellFunPtr reqHandler.writeResponseBodyPtr
    completeResponse reqHandler.completedResponse
    void $ tryPutMVar reqHandler.doneRequest msg.result

-- print "request completed"