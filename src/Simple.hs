{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Simple where

import Data.ByteString.Lazy qualified as BSL
import Language.C.Inline qualified as C

import Internal.Raw

import Agent
import Data.RoundRobin
import Extras
import Internal.Easy
import Internal.Metrics
import Internal.Raw.Extras (getCurlCode)
import Internal.Raw.MPSC (OuterMessage (Execute))
import Language.C.Inline.Unsafe qualified as CU
import Request
import Response
import UnliftIO
import UnliftIO.Resource

C.context (C.baseCtx <> C.funCtx <> C.fptrCtx <> C.bsCtx <> localCtx)

C.include "<string.h>"
C.include "<stdlib.h>"

C.include "<curl/curl.h>"
C.include "HsFFI.h"

initCurl :: IO ()
initCurl = [C.block|void { curl_global_init(CURL_GLOBAL_DEFAULT); }|]

performRequest :: AgentHandle -> RequestHandler -> IO (Either CurlCode (Response BSL.ByteString))
performRequest agent reqHandler = do
    let CurlEasy easyPtr = reqHandler.easy
    sendMessage agent.agentContext $ Execute easyPtr
    readMVar reqHandler.doneRequest
    getCurlCode reqHandler.easyData >>= \case
        Ok -> do
            !responseBS <- simpleStringToBS reqHandler.responseSimpleString
            code <-
                [CU.block|long {
                     long http_code = 0;
                     curl_easy_getinfo($(CURL* easyPtr), CURLINFO_RESPONSE_CODE, &http_code);
                     return http_code;
                 }|]
            metrics <- extractMetrics reqHandler.metricsContext
            pure . Right $! Response{info = HttpParts{statusCode = fromIntegral code, headers = []}, body = BSL.fromStrict responseBS, metrics}
        err -> pure $ Left err

httpLBS :: (MonadResource m, MonadUnliftIO m) => Agent -> Request -> m (Either CurlCode (Response BSL.ByteString))
httpLBS agent request = do
    (releaseKeyEasy, easy) <- allocateEasy
    req <- initRequest request easy
    agentHandle <- case agent of
        Single agentHandle -> pure agentHandle
        Threaded rr -> liftIO $ select rr
    res <- liftIO (performRequest agentHandle req) -- `onException` liftIO (cancelRequest agent.agentContext req)
    release releaseKeyEasy
    pure res
