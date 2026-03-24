{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Agent where

import Control.Applicative
import Control.Concurrent (getNumCapabilities)
import Control.Concurrent.Async (Async)
import Control.Concurrent.Async qualified as Async
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Exception
import Control.Monad (forever, unless, void)
import Data.Foldable
import Data.IORef.Unboxed (newIORefU)
import Data.Maybe
import Data.RoundRobin (RoundRobin, newRoundRobin)
import Data.Traversable
import Extras
import Foreign (castStablePtrToPtr, freeStablePtr, newStablePtr)
import Foreign.Ptr
import GHC.Event
import GHC.Generics
import Internal.Haskell.Callbacks
import Internal.Haskell.Event qualified as InnerEvent
import Internal.Multi
import Internal.Raw
import Internal.Raw.Extras
import Internal.Raw.MPSC
import Internal.Raw.SocketEvents
import Language.C.Inline qualified as C
import Language.C.Inline.Unsafe qualified as CU
import PyF
import Request
import System.Posix.Types
import Types

C.context (C.baseCtx <> C.funCtx <> C.fptrCtx <> C.bsCtx <> localCtx)

C.include "<string.h>"
C.include "<stdlib.h>"

C.include "<curl/curl.h>"
C.include "HsFFI.h"

C.include "simple_string.h"
C.include "message_chan.h"
C.include "include/waitfree-mpsc-queue/mpscq.h"
C.include "curl_hs.h"

data AgentContext = AgentContext
    { multi :: !(Ptr CurlMulti)
    , timerWaker :: !(TMVar ())
    , outerQueue :: !(TQueue OuterMessage)
    , socketCallbackEnv :: !SocketCallbackEnv
    , timerCallbackEnv :: !TimerCallbackEnv
    }
    deriving (Generic)

data AgentHandle = AgentHandle
    { agentThreadId :: !(Async ())
    , agentContext :: !AgentContext
    }
    deriving (Generic)

data Agent = Single AgentHandle | Threaded (RoundRobin AgentHandle)

spawnThreadedAgent :: Int -> AgentConfig -> IO Agent
spawnThreadedAgent numThreads config = do
    realCapabilities <- getNumCapabilities
    let numThreads' = min realCapabilities numThreads
    handles <- for [0 .. numThreads'] \cap -> do
        multi <- initCurlMulti config
        agentContext <- Agent.new multi
        agentThreadId <- Async.asyncOn cap do
            Agent.run agentContext `catch` \(ex :: SomeException) -> print [fmt|agent died with exception {show ex}|]
        pure $ AgentHandle{agentThreadId, agentContext}
    rr <- newRoundRobin handles
    pure $ Threaded rr

spawnAgent :: AgentConfig -> IO Agent
spawnAgent config = do
    multi <- initCurlMulti config

    agentContext <- Agent.new multi

    agentThreadId <- Async.async $ do
        Agent.run agentContext `catch` \(ex :: SomeException) -> print [fmt|agent died with exception {show ex}|]
    pure . Single $ AgentHandle{agentThreadId, agentContext}

new :: Ptr CurlMulti -> IO AgentContext
new multiPtr = do
    outerQueue <- newTQueueIO
    timerManager <- getSystemTimerManager
    eventManager <- fromJust <$> getSystemEventManager
    tkRef <- newIORefU (-1)
    timerWaker <- newEmptyTMVarIO
    socketEventsState <- initSocketEventState
    let socketCallbackEnv =
            SocketCallbackEnv
                { multi = multiPtr
                , eventManager = eventManager
                , socketEventsState = socketEventsState
                }
        timerCallbackEnv =
            TimerCallbackEnv
                { tkRef = tkRef
                , timerManager = timerManager
                , waker = timerWaker
                }
    socketCallbackEnvPtr <- castStablePtrToPtr <$> newStablePtr socketCallbackEnv
    timerCallbackEnvPtr <- castStablePtrToPtr <$> newStablePtr timerCallbackEnv

    [CU.block|void {
        CURLM *multi = $(CURLM* multiPtr);
        curl_multi_setopt(multi, CURLMOPT_SOCKETFUNCTION, hsSocketFunctionCallback);
        curl_multi_setopt(multi, CURLMOPT_SOCKETDATA, $(void* socketCallbackEnvPtr));

        curl_multi_setopt(multi, CURLMOPT_TIMERFUNCTION, hsTimerFunctionCallback);
        curl_multi_setopt(multi, CURLMOPT_TIMERDATA, $(void* timerCallbackEnvPtr));
    }|]
    pure $ AgentContext{multi = multiPtr, ..}

run :: AgentContext -> IO ()
run !ctx = forever $! loop ctx

waitToFlushTQueue :: TQueue a -> STM [a]
waitToFlushTQueue tq = do
    val <- isEmptyTQueue tq
    if val
        then retry
        else flushTQueue tq

loop :: AgentContext -> IO ()
loop !ctx = do
    !val <-
        atomically $
            (Left <$> takeTMVar ctx.timerWaker)
                <|> (Right . Right <$> waitFlushSocketEventState ctx.socketCallbackEnv.socketEventsState)
                <|> (Right . Left <$> waitToFlushTQueue ctx.outerQueue)
    case val of
        Left _ -> do
            [C.block|void {
                int running_handles;
                curl_multi_socket_action($(CURLM* multi), CURL_SOCKET_TIMEOUT, 0, &running_handles);
            }|]
        Right !z -> case z of
            Left evts -> for_ evts \case
                (Execute !easy) -> do
                    [C.block|void {
                        curl_multi_add_handle($(CURLM* multi), $(CURL* easy));
                    }|]
                _ -> pure ()
            Right evts -> for_ evts \case
                (SocketEvent{..}) -> do
                    let Fd !fd = socket
                        CurlEventsOnSocket !bitmask = processToCurlEvents $ InnerEvent.toGHCEvent event
                    [C.block|void {
                        int running_handles = 0;
                        curl_multi_socket_action($(CURLM* multi), $(int fd), $(int bitmask), &running_handles);
                    }|]
    [C.block| void {
        check_multi_info($(CURLM* multi));
    }|]
  where
    -- print "done socket evts"

    !multi = ctx.multi

processToCurlEvents :: Event -> CurlEventsOnSocket
processToCurlEvents e = flags
  where
    isReadable = if InnerEvent.fromGHCEvent e `InnerEvent.eventIs` InnerEvent.evtRead then CurlCSelectIn else CurlEventsOnSocket 0
    isWritable = if InnerEvent.fromGHCEvent e `InnerEvent.eventIs` InnerEvent.evtWrite then CurlCSelectOut else CurlEventsOnSocket 0
    flags = isReadable <> isWritable

data QueueFull = QueueFull deriving (Show, Exception)

sendMessage :: AgentContext -> OuterMessage -> IO ()
sendMessage ctx outerMessage = atomically $ writeTQueue ctx.outerQueue outerMessage

cancelRequest :: AgentContext -> RequestHandler -> IO ()
cancelRequest ctx reqHandler = do
    let CurlEasy easyPtr = reqHandler.easy
    waker <- newEmptyMVar
    sendMessage ctx $ CancelRequest easyPtr waker
    readMVar waker
    requestWaker <- withEasyData reqHandler.easyData getMVarSPtrC
    void $ simpleStringToBS reqHandler.responseSimpleString
    unless requestWaker.waked do
        freeStablePtr requestWaker.mvarSPtr
