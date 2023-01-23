{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
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

import Control.Concurrent.Async (Async)
import Control.Concurrent.Async qualified as Async
import Control.Exception
import Control.Monad (unless)
import GHC.Generics
import Internal.MPSC
import Internal.Multi
import Internal.Raw
import Internal.Raw.MPSC
import Internal.Raw.UV
import Language.C.Inline qualified as C
import PyF
import Request

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
    , multi :: !CurlMulti
    , msgQueue :: !MPSCQ
    }
    deriving (Generic)

data AgentHandle = AgentHandle
    { agentThreadId :: !(Async ())
    , agentContext :: !AgentContext
    }
    deriving (Generic)

spawnAgent :: IO AgentHandle
spawnAgent = do
    multi <- initCurlMulti

    agentContext <- Agent.new multi

    agentThreadId <- Async.async $ do
        Agent.run agentContext `catch` \(ex :: SomeException) -> print [fmt|agent died with exception {show ex}|]
    pure AgentHandle{agentThreadId, agentContext}

new :: CurlMulti -> IO AgentContext
new multi = withCurlMulti multi \multiPtr -> do
    msgQueue <- initMPSCQ 10000
    uvLoopPtr <- [C.exp| uv_loop_t* { uv_default_loop() } |]
    uvAsyncPtr <- withMPSCQ msgQueue \mpscqPtr ->
        [C.block|uv_async_t* {
        printf("binding uv curl\n");
        bind_uv_curl_multi($(uv_loop_t* uvLoopPtr), $(CURLM* multiPtr));
        printf("init check messages\n");
        uv_async_t* uv_async = init_async_check_messages($(uv_loop_t* uvLoopPtr), $(mpsc_t* mpscqPtr), $(CURLM* multiPtr));
        return uv_async;
    }
    |]
    print uvLoopPtr
    print uvAsyncPtr
    pure $ AgentContext{uvAsync = UVAsync uvAsyncPtr, uvLoop = UVLoop uvLoopPtr, ..}

run :: AgentContext -> IO ()
run ctx = do
    print "letsStart"
    let UVLoop uvLoopPtr = ctx.uvLoop
    [C.block|void {
        uv_run($(uv_loop_t* uvLoopPtr), UV_RUN_DEFAULT);
    }|]
    print "isItStop?"

data QueueFull = QueueFull deriving (Show, Exception)

sendMessage :: AgentContext -> OuterMessage -> IO ()
sendMessage ctx outerMessage = do
    InternalOuterMessage msgPtr <- toInnerOuterMessage outerMessage
    let UVAsync asyncPtr = ctx.uvAsync
    isEnqueued <- withMPSCQ ctx.msgQueue \mpscPtr ->
        [C.block|bool {
        bool isEnqueued = mpscq_enqueue($(mpsc_t* mpscPtr), $(outer_message_t* msgPtr));
        if (isEnqueued) {
            uv_async_send($(uv_async_t* asyncPtr));
        }
        return isEnqueued;
    }|]
    unless (toEnum . fromIntegral $ isEnqueued) do
        throwIO QueueFull

cancelRequest :: AgentContext -> RequestHandler -> IO ()
cancelRequest ctx reqHandler = withCurlEasy reqHandler.easy \easyPtr -> do
    sendMessage ctx $ CancelRequest easyPtr