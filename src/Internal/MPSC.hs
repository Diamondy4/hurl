{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Internal.MPSC where

import Foreign
import Foreign.C.Types
import Internal.Raw
import Internal.Raw.MPSC
import Language.C.Inline qualified as C

C.context (C.baseCtx <> C.funCtx <> C.fptrCtx <> C.bsCtx <> localCtx)

C.include "message_chan.h"
C.include "include/waitfree-mpsc-queue/mpscq.h"

initMPSCQ :: Int -> IO MPSCQ
initMPSCQ capacity = do
    let capacity' = fromIntegral capacity
    ptr <- [C.exp|mpsc_t* { mpscq_create(NULL, $(int capacity'))}|]
    fptr <- newForeignPtr finalizerMPSCQ ptr
    pure . MPSCQ $ fptr

finalizerMPSCQ :: FunPtr (Ptr MPSCQ -> IO ())
finalizerMPSCQ = [C.funPtr| void mpscq_finalizer(mpsc_t* ptr) { mpscq_destroy(ptr); } |]
