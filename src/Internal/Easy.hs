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
import Control.Concurrent.STM
import Control.Concurrent.STM.TBMQueue (TBMQueue, closeTBMQueue, newTBMQueueIO, tryWriteTBMQueue)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Builder (Builder)
import Data.ByteString.Builder qualified as BSB
import Data.ByteString.Internal qualified as BSI
import Data.ByteString.Lazy qualified as BSL
import Data.ByteString.Unsafe qualified as BSU
import Data.Coerce
import Data.Functor
import Data.IORef
import Data.Maybe
import Foreign
import Foreign.C.Types
import Internal.Raw
import Internal.Slist
import Language.C.Inline qualified as C
import PyF
import Request
import Resumer (ChanStatusVar, newChanStatusVarIO, pauseChan)

C.context (C.baseCtx <> C.funCtx <> C.fptrCtx <> C.bsCtx <> curlCtx)

C.include "<string.h>"
C.include "<stdlib.h>"

C.include "<curl/curl.h>"
C.include "HsFFI.h"
C.include "curl_hs_c.h"

defaultChunkCount :: Int
defaultChunkCount = 4

initRequest :: Request -> IO RequestHandler
initRequest Request{..} = do
    slist <- fromJust <$> toHeaderSlist headers

    completedResponse <- newEmptyMVar @()
    doneRequest <- newEmptyMVar @CurlCode

    requestBodyRef <- newIORef $ BSL.toChunks requestBody
    readRequestBodyPtr <- $(C.mkFunPtr [t|Ptr CChar -> CSize -> CSize -> Ptr () -> IO CSize|]) (readFunctionBSL requestBodyRef)

    responseBodyChan <- newTBMQueueIO @ByteString defaultChunkCount
    responseBodyStatus <- newChanStatusVarIO
    writeResponseBodyPtr <- $(C.mkFunPtr [t|Ptr CChar -> CSize -> CSize -> Ptr () -> IO CSize|]) (writeFunctionTBQ responseBodyChan responseBodyStatus completedResponse)

    easyPtr <- withCurlSlist slist \slistPtr ->
        [C.block|CURL* {
        CURL *easy;
        easy = curl_easy_init();
        if(easy) {
          
          curl_easy_setopt(easy, CURLOPT_URL, $bs-cstr:host);
          curl_easy_setopt(easy, CURLOPT_HTTPHEADER, $(curl_slist_t* slistPtr));
          curl_easy_setopt(easy, CURLOPT_TIMEOUT_MS, $(long timeoutMS));
          curl_easy_setopt(easy, CURLOPT_FOLLOWLOCATION, 1L);

          curl_easy_setopt(easy, CURLOPT_POST, 1L);

          curl_easy_setopt(easy, CURLOPT_READFUNCTION, $(size_t (*readRequestBodyPtr)(char*, size_t, size_t, void*)));
          curl_easy_setopt(easy, CURLOPT_WRITEFUNCTION, $(size_t (*writeResponseBodyPtr)(char*, size_t, size_t, void*)));
          
          curl_easy_setopt(easy, CURLOPT_NOSIGNAL, 1L);

          return easy;
        }
    }|]

    easy <- coerce $ newForeignPtr cleanupCurlEasy easyPtr

    pure
        RequestHandler
            { easy
            , doneRequest
            , completedResponse
            , slist
            , readRequestBodyPtr
            , writeResponseBodyPtr
            , responseBodyChan
            , responseBodyStatus
            }

takeChunk :: Int -> [ByteString] -> ([ByteString], ByteString)
takeChunk _len [] = ([], "")
takeChunk len ("" : xs) = takeChunk len xs
takeChunk len (chunk : xs) =
    let (now, later) = BS.splitAt len chunk
     in (later : xs, now)

cleanupCurlEasy :: FunPtr (Ptr CurlEasy -> IO ())
cleanupCurlEasy = [C.funPtr| void free_curl_easy(CURL* ptr){ curl_easy_cleanup(ptr); } |]

readFunctionBSL :: IORef [ByteString] -> CurlEasyReadFunction
readFunctionBSL requestBodyRef buffer size nitems _userdata = do
    -- print "in read function"
    !bsChunks <- readIORef requestBodyRef
    let (newChunks, chunk) = takeChunk (fromIntegral $ size * nitems) bsChunks
    modifyIORef' requestBodyRef (const newChunks)
    BSU.unsafeUseAsCStringLen chunk \(src, srcLen) -> BSI.memcpy (castPtr buffer) (castPtr src) srcLen
    pure $ fromIntegral $ BS.length chunk

writeFunctionBuilder :: IORef Builder -> CurlEasyWriteFunction
writeFunctionBuilder responseBodyRef cStr _size nmemb _userdata = do
    str <- BS.packCStringLen (cStr, fromIntegral nmemb)
    modifyIORef' responseBodyRef (<> BSB.byteString str)
    pure . fromIntegral $ BS.length str

writeFunctionTBQ :: TBMQueue ByteString -> ChanStatusVar -> MVar () -> CurlEasyWriteFunction
writeFunctionTBQ responseBodyChan statusVar completeResponseWaker cStr _size nmemb _userdata = do
    -- print "in write function"
    !str <- BS.packCStringLen (cStr, fromIntegral nmemb)
    res <-
        atomically $
            tryWriteTBMQueue responseBodyChan str >>= \case
                -- queue closed
                Nothing -> pure 0
                -- queue full, we should pause writing body
                Just False -> do
                    pauseChan statusVar
                    pure . fromIntegral . fromEnum $ WritePause
                Just True
                    | BS.null str -> closeTBMQueue responseBodyChan $> 0
                    | otherwise -> pure . fromIntegral $ BS.length str
    completeResponse completeResponseWaker
    pure res

-- TODO: error handling
unpauseRead :: Ptr CurlEasy -> IO ()
unpauseRead easy = [C.block|void { curl_easy_pause($(CURL* easy), CURLPAUSE_RECV_CONT); }|]

-- TODO: error handling
unpauseWrite :: Ptr CurlEasy -> IO ()
unpauseWrite easy = [C.block|void { curl_easy_pause($(CURL* easy), CURLPAUSE_SEND_CONT); }|]
