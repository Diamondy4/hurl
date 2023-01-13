{-# LANGUAGE BlockArguments #-}
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
import Control.Concurrent.Async qualified as Async
import Control.Concurrent.STM
import Control.Concurrent.STM.Delay (newDelay, waitDelay)
import Control.Concurrent.STM.TBMQueue
import Control.Exception
import Control.Monad (forever, void, when)
import Data.Foldable (for_, traverse_)
import Data.HashMap.Strict qualified as HM
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Maybe
import Foreign (freeHaskellFunPtr, newStablePtr)
import Foreign.C.Types (CInt (..))
import GHC.Event
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
    requestsRef <- newIORef mempty
    msgQueueAlive <- newStablePtr msgQueue
    pure $ AgentContext{fdMapRef = socketCtx.fdMapRef, timerWaker = timerCtx.timerWaiter, socketActionQueue = socketCtx.socketActionQueue, ..}

run :: AgentContext -> IO ()
run ctx = forever $ do
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
        requests <- readIORef requestsRef
        if HM.null requests
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
        requests <- readIORef ctx.requestsRef
        for_ (HM.lookup requestId requests) (cancelRequest ctx)
    UnpauseRead requestId -> do
        requests <- readIORef ctx.requestsRef
        case HM.lookup requestId requests of
            Just handler -> withCurlEasy handler.easy unpauseWrite
            Nothing -> pure ()
    UnpauseWrite _ -> error "Not implemented"

doRequest :: AgentContext -> RequestHandler -> IO ()
doRequest AgentContext{..} ci@(RequestHandler{..}) = withCurlMulti multi \multiPtr -> withCurlEasy easy \easyPtr -> do
    [C.block|void { curl_multi_add_handle($(CURLM* multiPtr), $(CURL* easyPtr)); }|]
    modifyIORef' requestsRef (HM.insert (handlerRequestId ci) ci)
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
    _collectSocketAction_old limitDelay = do
        action <- atomically $ (Just <$> readTQueue socketActionQueue) <|> (Nothing <$ triggerTimerLimited limitDelay)
        case action of
            Just action' -> do
                actions <- _collectSocketAction_old limitDelay
                pure (action' : actions)
            Nothing -> do
                return mempty
    -- Poll actions until we detect at least one or timeout rings
    awaitSocketActions limitDelay = do
        -- print "collect socket actions"
        actions <- atomically $ do
            socketActivity <- (True <$ peekTQueue socketActionQueue) <|> (False <$ triggerTimerLimited limitDelay)
            if socketActivity
                then flushTQueue socketActionQueue
                else pure mempty
        -- print [fmt|traversing actions - {length actions} acts|]
        traverse_ (processSocketActionRequest ctx) actions

processMultiMsg :: AgentContext -> CurlMultiMsg -> IO ()
processMultiMsg ac@AgentContext{..} msg = do
    let requestId = easyPtrRequestId msg.easy
    hm <- readIORef requestsRef
    case HM.lookup requestId hm of
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
    _old_completeRequestMulti' rawMsg = do
        -- print "in processing msg"
        msg <- extractMessage (CurlMultiMsgRaw rawMsg)
        processMultiMsg ac msg

cancelRequest :: AgentContext -> RequestHandler -> IO ()
cancelRequest ctx reqHandler = withCurlMulti ctx.multi \multiPtr -> withCurlEasy reqHandler.easy \easyPtr -> do
    modifyIORef' ctx.requestsRef \requests -> do
        HM.delete (handlerRequestId reqHandler) requests
    [C.block|void { curl_multi_remove_handle($(CURLM* multiPtr), $(CURL* easyPtr));}|]
    freeHaskellFunPtr reqHandler.readRequestBodyPtr
    freeHaskellFunPtr reqHandler.writeResponseBodyPtr
    -- print "request cancelled"

completeRequestMulti :: AgentContext -> RequestHandler -> CurlMultiMsg -> IO ()
completeRequestMulti ac reqHandler msg = withCurlMulti ac.multi \multiPtr -> withCurlEasy reqHandler.easy \easyPtr -> do
    let requestId = handlerRequestId reqHandler
    modifyIORef' ac.requestsRef \requests -> do
        HM.delete requestId requests
    [C.block|void { curl_multi_remove_handle($(CURLM* multiPtr), $(CURL* easyPtr));}|]
    atomically $ closeTBMQueue reqHandler.responseBodyChan
    freeHaskellFunPtr reqHandler.readRequestBodyPtr
    freeHaskellFunPtr reqHandler.writeResponseBodyPtr
    completeResponse reqHandler.completedResponse
    void $ tryPutMVar reqHandler.doneRequest msg.result
    -- print "request completed"