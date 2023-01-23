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
import Internal.Raw
import Language.C.Inline qualified as C

C.context (C.baseCtx <> C.funCtx <> C.fptrCtx <> C.bsCtx <> localCtx)

C.include "<curl/curl.h>"

initCurlMulti :: IO CurlMulti
initCurlMulti = do
    multi <-
        [C.block|CURLM* {
        CURLM* multi = curl_multi_init();
        
        // TODO: To config
        curl_multi_setopt(multi, CURLMOPT_PIPELINING, CURLPIPE_MULTIPLEX);

        return multi;
    }|]
    coerce $ newForeignPtr cleanupCurlMulti multi

cleanupCurlMulti :: FunPtr (Ptr CurlMulti -> IO ())
cleanupCurlMulti = [C.funPtr| void free_curl_multi(CURLM* ptr){ curl_multi_cleanup(ptr); } |]
