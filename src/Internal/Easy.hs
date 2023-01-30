{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Internal.Easy where

import Control.Concurrent
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Coerce
import Data.Foldable
import Foreign
import Foreign.C.Types
import Internal.Raw
import Internal.Raw.Extras (mkEasyData, withEasyData)
import Internal.Raw.SimpleString
import Internal.Slist
import Language.C.Inline qualified as C
import Request
import Types
import qualified Language.C.Inline.Unsafe as CU

C.context (C.baseCtx <> C.funCtx <> C.fptrCtx <> C.bsCtx <> localCtx)

C.include "<string.h>"
C.include "<stdlib.h>"

C.include "<curl/curl.h>"
C.include "HsFFI.h"

C.include "simple_string.h"
C.include "extras.h"

initRequest :: Request -> IO RequestHandler
initRequest Request{..} = do
    doneRequest <- newEmptyMVar @()
    easyData <- mkEasyData doneRequest

    let timeoutMS' = fromIntegral timeoutMS
        connectionTimeoutMS' = fromIntegral connectionTimeoutMS
        lowSpeedLimit' = fromIntegral lowSpeedLimit.lowSpeed
        lowSpeedTimeout' = fromIntegral lowSpeedLimit.timeout
    easyPtr <-
        [CU.block|CURL* {
        CURL *easy;
        easy = curl_easy_init();
        if(easy) {
          curl_easy_setopt(easy, CURLOPT_URL, $bs-cstr:host);

          curl_easy_setopt(easy, CURLOPT_HTTP_VERSION, CURL_HTTP_VERSION_2_0);
          curl_easy_setopt(easy, CURLOPT_PIPEWAIT, 1L);

          curl_easy_setopt(easy, CURLOPT_TIMEOUT_MS, $(long timeoutMS'));
          curl_easy_setopt(easy, CURLOPT_CONNECTTIMEOUT_MS, $(long connectionTimeoutMS'));
          curl_easy_setopt(easy, CURLOPT_LOW_SPEED_TIME, $(long lowSpeedTimeout'));
          curl_easy_setopt(easy, CURLOPT_LOW_SPEED_LIMIT, $(long lowSpeedLimit'));

          curl_easy_setopt(easy, CURLOPT_ACCEPT_ENCODING, "");

          curl_easy_setopt(easy, CURLOPT_FOLLOWLOCATION, 1L);

          curl_easy_setopt(easy, CURLOPT_NOSIGNAL, 1L);

          return easy;
        }
    }|]
    easy <- coerce $ newForeignPtr cleanupCurlEasy easyPtr
    responseSimpleString <- mallocForeignPtr @SimpleString

    withEasyData easyData \easyDataPtr ->
        [CU.block|void {
        curl_easy_setopt($(CURL* easyPtr), CURLOPT_PRIVATE, $(hs_easy_data_t* easyDataPtr));
        
        // Request body
        init_simple_string($fptr-ptr:(simple_string_t* responseSimpleString));
        curl_easy_setopt($(CURL* easyPtr), CURLOPT_WRITEFUNCTION, simple_string_writefunc);
        curl_easy_setopt($(CURL* easyPtr), CURLOPT_WRITEDATA, $fptr-ptr:(simple_string_t* responseSimpleString) );
    }|]

    case body of
        Empty ->
            [CU.block|void {
            curl_easy_setopt($(CURL* easyPtr), CURLOPT_POSTFIELDS, "");
            curl_easy_setopt($(CURL* easyPtr), CURLOPT_POSTFIELDSIZE, 0L);
        }|]
        Buffer bs ->
            -- Request body is stored in its RequestHandler instance, thus it should outlive the use.
            [CU.block|void {
            curl_easy_setopt($(CURL* easyPtr), CURLOPT_POSTFIELDS, $bs-ptr:bs);
            curl_easy_setopt($(CURL* easyPtr), CURLOPT_POSTFIELDSIZE, (long)$bs-len:bs);
        }|]

    case method of
        Get -> [CU.block|void { curl_easy_setopt($(CURL* easyPtr), CURLOPT_HTTPGET, 1L); }|]
        Head -> [CU.block|void { curl_easy_setopt($(CURL* easyPtr), CURLOPT_NOBODY, 1L); }|]
        Post -> [CU.block|void { curl_easy_setopt($(CURL* easyPtr), CURLOPT_POST, 1L); }|]
        _ -> do
            let methodBS = httpMethodToBS method
            [CU.block|void {
                curl_easy_setopt($(CURL* easyPtr), CURLOPT_CUSTOMREQUEST, $bs-cstr:methodBS);
            }|]

    slist <-
        if not . null $ headers
            then do
                slist' <- toHeaderSlist headers
                -- TODO: throw exception if failed to make headers
                for_ slist' \slist -> withCurlSlist slist \slistPtr ->
                    [CU.block|void { curl_easy_setopt($(CURL* easyPtr), CURLOPT_HTTPHEADER, $(curl_slist_t* slistPtr)); }|]
                pure slist'
            else pure Nothing

    pure
        RequestHandler
            { easy
            , easyData
            , requestBody = body
            , doneRequest
            , requestHeaders = slist
            , responseSimpleString
            }

takeChunk :: Int -> [ByteString] -> ([ByteString], ByteString)
takeChunk _len [] = ([], "")
takeChunk len ("" : xs) = takeChunk len xs
takeChunk len (chunk : xs) =
    let (now, later) = BS.splitAt len chunk
     in (later : xs, now)

cleanupCurlEasy :: FunPtr (Ptr CurlEasy -> IO ())
cleanupCurlEasy = [C.funPtr| void free_curl_easy(CURL* ptr){ curl_easy_cleanup(ptr); } |]

isBodyEmpty :: Body -> Bool
isBodyEmpty = \case
    Empty -> True
    _ -> False