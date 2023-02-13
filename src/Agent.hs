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

import Control.Concurrent (getNumCapabilities)
import Control.Concurrent.Async (Async)
import Control.Concurrent.Async qualified as Async
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad (unless, void)
import Data.RoundRobin (RoundRobin, newRoundRobin)
import Data.Traversable
import Extras
import Foreign (freeStablePtr)
import Foreign.Ptr
import GHC.Generics
import Internal.MPSC
import Internal.Multi
import Internal.Raw
import Internal.Raw.Extras
import Internal.Raw.MPSC
import Internal.Raw.UV
import Language.C.Inline qualified as C
import Language.C.Inline.Unsafe qualified as CU
import PyF
import Request
import Types

C.context (C.baseCtx <> C.funCtx <> C.fptrCtx <> C.bsCtx <> localCtx)

C.include "<string.h>"
C.include "<stdlib.h>"

C.include "<curl/curl.h>"
C.include "HsFFI.h"

C.include "simple_string.h"
C.include "curl_uv.h"
C.include "message_chan.h"
C.include "include/waitfree-mpsc-queue/mpscq.h"

data AgentContext = AgentContext
    { uvLoop :: !UVLoop
    , uvAsync :: !UVAsync
    , multi :: !(Ptr CurlMulti)
    , msgQueue :: !MPSCQ
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
    msgQueue <- initMPSCQ 100000
    uvLoopPtr <-
        [C.block| uv_loop_t* { 
        uv_loop_t *loop = malloc(sizeof(uv_loop_t));
        uv_loop_init(loop);
        return loop;
    }|]
    uvAsyncPtr <- withMPSCQ msgQueue \mpscqPtr ->
        [C.block|uv_async_t* {
        bind_uv_curl_multi($(uv_loop_t* uvLoopPtr), $(CURLM* multiPtr));
        uv_async_t* uv_async = init_async_check_messages($(uv_loop_t* uvLoopPtr), $(mpsc_t* mpscqPtr), $(CURLM* multiPtr));
        return uv_async;
    }
    |]
    pure $ AgentContext{uvAsync = UVAsync uvAsyncPtr, uvLoop = UVLoop uvLoopPtr, multi = multiPtr, ..}

run :: AgentContext -> IO ()
run ctx = do
    let UVLoop uvLoopPtr = ctx.uvLoop
    [C.block|void {
        uv_run($(uv_loop_t* uvLoopPtr), UV_RUN_DEFAULT);
    }|]

data QueueFull = QueueFull deriving (Show, Exception)

sendMessage :: AgentContext -> OuterMessage -> IO ()
sendMessage ctx outerMessage = do
    InternalOuterMessage msgPtr <- toInnerOuterMessage outerMessage
    let UVAsync asyncPtr = ctx.uvAsync
    isEnqueued <- withMPSCQ ctx.msgQueue \mpscPtr ->
        [CU.block|bool {
        bool isEnqueued = mpscq_enqueue($(mpsc_t* mpscPtr), $(outer_message_t* msgPtr));
        if (isEnqueued) {
            uv_async_send($(uv_async_t* asyncPtr));
        }
        return isEnqueued;
    }|]
    unless (toEnum . fromIntegral $ isEnqueued) do
        throwIO QueueFull

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