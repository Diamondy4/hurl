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

import Control.Concurrent
import Data.ByteString.Builder qualified as BSB
import Data.ByteString.Lazy qualified as BSL
import Language.C.Inline qualified as C

import Internal.Raw

import Control.Applicative
import Control.Concurrent.STM
import Control.Concurrent.STM.TBMQueue
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Builder (byteString)
import Internal.Easy
import Request
import Response
import Resumer
import Types
import UnliftIO (bracketOnError, throwIO)

import Data.Coerce
import Extras
import Foreign (touchForeignPtr)

C.context (C.baseCtx <> C.funCtx <> C.fptrCtx <> C.bsCtx <> curlCtx)

C.include "<string.h>"
C.include "<stdlib.h>"

C.include "<curl/curl.h>"
C.include "HsFFI.h"

C.include "curl_hs_c.h"

initCurl :: IO ()
initCurl = [C.block|void { curl_global_init(CURL_GLOBAL_DEFAULT); }|]

data PipeResult = Closed | Paused | Chunk ByteString

performRequest :: AgentHandle -> RequestHandler -> IO (Response Body)
performRequest agent ci@RequestHandler{..} = do
    atomically . writeTQueue agent.msgQueue $ Execute ci
    takeMVar completedResponse
    !code <- withCurlEasy easy \(!easyPtr) ->
        [C.block|long {
            long http_code = 0;
            curl_easy_getinfo($(CURL* easyPtr), CURLINFO_RESPONSE_CODE, &http_code);
            return http_code;
        }|]
    touchForeignPtr $ coerce easy
    pure $! Response{info = HttpParts{statusCode = fromIntegral code, headers = []}, body = BodyChan responseBodyChan}

collectBody :: AgentHandle -> RequestHandler -> IO BSL.ByteString
collectBody agent handler = do
    bsBody <- BSB.toLazyByteString <$> go mempty
    tryReadMVar handler.doneRequest >>= \case
        Just Ok -> return bsBody
        Nothing -> return bsBody
        Just err -> throwIO err
  where
    go builder = do
        res <-
            atomically $
                ((Just <$> readTBMQueue handler.responseBodyChan) <|> (Nothing <$ waitChanPaused handler.responseBodyStatus)) >>= \case
                    -- Paused
                    Nothing -> do
                        writeTQueue agent.msgQueue (UnpauseWrite $ handlerRequestId handler)
                        resumeChan handler.responseBodyStatus
                        pure Nothing
                    something -> pure something
        case res of
            -- reiterate after resuming chan
            Nothing -> go builder
            -- actual data
            Just (Just chunk) | not (BS.null chunk) -> do
                next <- go builder
                pure $ builder <> byteString chunk <> next
            -- chan closed or returned 0 bytestring end bytes
            _ -> pure builder

performRequestBS :: AgentHandle -> Request -> IO (Response BSL.ByteString)
performRequestBS agent request =
    bracketOnError
        (initRequest request)
        (atomically . writeTQueue agent.msgQueue . CancelRequest . handlerRequestId)
        \handler -> do
            response <- performRequest agent handler
            bsBody <- collectBody agent handler
            pure $ response{body = bsBody}