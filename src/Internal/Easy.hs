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
import Internal.Raw.SimpleString
import Internal.Slist
import Language.C.Inline qualified as C
import Request
import Types

C.context (C.baseCtx <> C.funCtx <> C.fptrCtx <> C.bsCtx <> curlCtx)

C.include "<string.h>"
C.include "<stdlib.h>"

C.include "<curl/curl.h>"
C.include "HsFFI.h"
C.include "curl_hs_c.h"
C.include "curl_hs_c.c"

defaultChunkCount :: Int
defaultChunkCount = 4

initRequest :: Request -> IO RequestHandler
initRequest Request{..} = do
    doneRequest <- newEmptyMVar @CurlCode

    easyPtr <-
        [C.block|CURL* {
        CURL *easy;
        easy = curl_easy_init();
        if(easy) {
          curl_easy_setopt(easy, CURLOPT_URL, $bs-cstr:host);

          curl_easy_setopt(easy, CURLOPT_HTTP_VERSION, CURL_HTTP_VERSION_2_0);
          curl_easy_setopt(easy, CURLOPT_PIPEWAIT, 1L);

          curl_easy_setopt(easy, CURLOPT_TIMEOUT_MS, $(long timeoutMS));

          curl_easy_setopt(easy, CURLOPT_ACCEPT_ENCODING, "");

          curl_easy_setopt(easy, CURLOPT_FOLLOWLOCATION, 1L);

          curl_easy_setopt(easy, CURLOPT_NOSIGNAL, 1L);

          return easy;
        }
    }|]
    easy <- coerce $ newForeignPtr cleanupCurlEasy easyPtr

    responseSimpleString <- mallocForeignPtr @SimpleString

    [C.block|void {
        init_simple_string($fptr-ptr:(simple_string* responseSimpleString));
        curl_easy_setopt($(CURL* easyPtr), CURLOPT_WRITEFUNCTION, simple_string_writefunc);
        curl_easy_setopt($(CURL* easyPtr), CURLOPT_WRITEDATA, $fptr-ptr:(simple_string* responseSimpleString) );
    }|]

    case body of
        Empty ->
            [C.block|void {
            curl_easy_setopt($(CURL* easyPtr), CURLOPT_POSTFIELDS, "");
            curl_easy_setopt($(CURL* easyPtr), CURLOPT_POSTFIELDSIZE, 0L);
        }|]
        Buffer bs ->
            -- Request body is stored in its RequestHandler instance, thus it should outlive the use.
            [C.block|void {
            curl_easy_setopt($(CURL* easyPtr), CURLOPT_POSTFIELDS, $bs-ptr:bs);
            curl_easy_setopt($(CURL* easyPtr), CURLOPT_POSTFIELDSIZE, (long)$bs-len:bs);
        }|]
        Reader _chan _size -> pure () -- TODO: support streaming request body
    slist <-
        if null headers
            then do
                slist' <- toHeaderSlist headers
                -- TODO: throw exception if failed to make headers
                for_ slist' \slist -> withCurlSlist slist \slistPtr ->
                    [C.block|void { curl_easy_setopt($(CURL* easyPtr), CURLOPT_HTTPHEADER, $(curl_slist_t* slistPtr)); }|]
                pure slist'
            else pure Nothing

    case method of
        Get -> [C.block|void { curl_easy_setopt($(CURL* easyPtr), CURLOPT_HTTPGET, 1L); }|]
        Head -> [C.block|void { curl_easy_setopt($(CURL* easyPtr), CURLOPT_NOBODY, 1L); }|]
        Post -> [C.block|void { curl_easy_setopt($(CURL* easyPtr), CURLOPT_POST, 1L); }|]
        _ -> do
            let methodBS = httpMethodToBS method
            [C.block|void {
                curl_easy_setopt($(CURL* easyPtr), CURLOPT_CUSTOMREQUEST, $bs-cstr:methodBS);
            }|]

    pure
        RequestHandler
            { easy
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

-- TODO: error handling
unpauseRead :: Ptr CurlEasy -> IO ()
unpauseRead easy = [C.block|void { curl_easy_pause($(CURL* easy), CURLPAUSE_RECV_CONT); }|]

-- TODO: error handling
unpauseWrite :: Ptr CurlEasy -> IO ()
unpauseWrite easy = [C.block|void { curl_easy_pause($(CURL* easy), CURLPAUSE_SEND_CONT); }|]

isBodyEmpty :: Body -> Bool
isBodyEmpty = \case
    Empty -> True
    _ -> False