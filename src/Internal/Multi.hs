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

import Foreign
import Internal.Raw
import Language.C.Inline qualified as C
import Types

C.context (C.baseCtx <> C.funCtx <> C.fptrCtx <> C.bsCtx <> localCtx)

C.include "<curl/curl.h>"

initCurlMulti :: AgentConfig -> IO (Ptr CurlMulti)
initCurlMulti config =
    [C.block|CURLM* {
        CURLM* multi = curl_multi_init();

        curl_multi_setopt(multi, CURLMOPT_MAX_TOTAL_CONNECTIONS, $(long maxConnectionC));
        curl_multi_setopt(multi, CURLMOPT_MAX_HOST_CONNECTIONS, $(long maxConnectionPerHostC));
        long connection_cache_size = $(long connectionCacheSizeC);
        if (connection_cache_size > 0) {
            curl_multi_setopt(multi, CURLMOPT_MAXCONNECTS, connection_cache_size);
        }

        return multi;
    }|]
  where
    maxConnectionC = fromIntegral $ maxConnection config
    maxConnectionPerHostC = fromIntegral $ maxConnectionPerHost config
    connectionCacheSizeC = fromIntegral $ connectionCacheSize config

cleanupCurlMulti :: FunPtr (Ptr CurlMulti -> IO ())
cleanupCurlMulti = [C.funPtr| void free_curl_multi(CURLM* ptr){ curl_multi_cleanup(ptr); } |]
