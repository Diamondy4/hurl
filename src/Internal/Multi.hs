{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Internal.Multi where

import Data.Coerce
import Foreign
import Foreign.C.Types
import Internal.Callbacks
import Internal.Raw
import Language.C.Inline qualified as C
import Types

C.context (C.baseCtx <> C.funCtx <> C.fptrCtx <> C.bsCtx <> curlCtx)

C.include "<string.h>"
C.include "<stdlib.h>"

C.include "<curl/curl.h>"
C.include "HsFFI.h"
C.include "curl_hs_c.h"

initCurlMulti :: CurlSocketFunEnv -> CurlTimerFunEnv -> IO CurlMulti
initCurlMulti socketFunEnv timerCtx = do
    -- TODO: Free fun ptrs
    socketFunPtr <- $(C.mkFunPtr [t|Ptr CurlEasy -> CInt -> CInt -> Ptr () -> Ptr () -> IO CInt|]) (curlSocketFunction socketFunEnv)
    timerFunPtr <- $(C.mkFunPtr [t|Ptr CurlMulti -> CLong -> Ptr () -> IO CInt|]) (curlTimerFunction timerCtx)
    multi <-
        [C.block|CURLM* {
        CURLM* multi = curl_multi_init();
        
        curl_multi_setopt(multi, CURLMOPT_SOCKETFUNCTION, $(int (*socketFunPtr)(CURL*, int, int, void*, void*)));
        curl_multi_setopt(multi, CURLMOPT_TIMERFUNCTION, $(int (*timerFunPtr)(CURLM*, long, void*)));

        // TODO: To config
        curl_multi_setopt(multi, CURLMOPT_PIPELINING, CURLPIPE_MULTIPLEX);

        return multi;
    }|]
    coerce $ newForeignPtr cleanupCurlMulti multi

cleanupCurlMulti :: FunPtr (Ptr CurlMulti -> IO ())
cleanupCurlMulti = [C.funPtr| void free_curl_multi(CURLM* ptr){ curl_multi_cleanup(ptr); } |]
