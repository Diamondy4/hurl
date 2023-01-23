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
import Control.Concurrent (takeMVar)
import Extras
import Internal.Easy
import Internal.Raw.Extras (getCurlCode)
import Internal.Raw.MPSC (OuterMessage (Execute))
import Request
import Response

C.context (C.baseCtx <> C.funCtx <> C.fptrCtx <> C.bsCtx <> localCtx)

C.include "<string.h>"
C.include "<stdlib.h>"

C.include "<curl/curl.h>"
C.include "HsFFI.h"

initCurl :: IO ()
initCurl = [C.block|void { curl_global_init(CURL_GLOBAL_DEFAULT); }|]

performRequest :: AgentHandle -> RequestHandler -> IO (Either CurlCode (Response BSL.ByteString))
performRequest agent reqHandler = withCurlEasy reqHandler.easy \easyPtr -> do
    sendMessage agent.agentContext $ Execute easyPtr
    print "message sent to agent"
    takeMVar reqHandler.doneRequest
    getCurlCode reqHandler.easyData >>= \case
        Ok -> do
            !responseBS <- simpleStringToBS reqHandler.responseSimpleString
            code <-
                [C.block|long {
                     long http_code = 0;
                     curl_easy_getinfo($(CURL* easyPtr), CURLINFO_RESPONSE_CODE, &http_code);
                     return http_code;
                 }|]
            pure . Right $! Response{info = HttpParts{statusCode = fromIntegral code, headers = []}, body = BSL.fromStrict responseBS}
        err -> pure $ Left err

httpLBS :: AgentHandle -> Request -> IO (Either CurlCode (Response BSL.ByteString))
httpLBS agent request = do
    handler <- initRequest request
    performRequest agent handler