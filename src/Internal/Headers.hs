{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Internal.Headers where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Data.ByteString.Unsafe qualified as BSU
import Data.CaseInsensitive qualified as CI
import Data.Maybe
import Foreign
import Internal.Raw
import Internal.Raw.Headers
import Language.C.Inline qualified as C
import Language.C.Inline.Unsafe qualified as CU
import Network.HTTP.Types.Header
import UnliftIO

C.context (C.baseCtx <> C.funCtx <> C.fptrCtx <> C.bsCtx <> localCtx)

C.include "<string.h>"
C.include "<stdlib.h>"

C.include "<curl/curl.h>"
C.include "HsFFI.h"

C.include "headers.h"

setHeaderReader :: (MonadIO m) => CurlEasy -> m HeadersData
setHeaderReader (CurlEasy easyPtr) = do
    ptr <- liftIO [C.exp|header_data_t* { header_data_create(1024) }|]
    fptr <- liftIO $ newForeignPtr finalizerHeadersData ptr
    liftIO
        [CU.block|void {
        CURL* easy = $(CURL* easyPtr);
        curl_easy_setopt(easy, CURLOPT_HEADERDATA, $fptr-ptr:(header_data_t* fptr));
        curl_easy_setopt(easy, CURLOPT_HEADERFUNCTION, header_callback);
    }|]
    pure $ HeadersData fptr

finalizerHeadersData :: FunPtr (Ptr HeadersData -> IO ())
finalizerHeadersData = [C.funPtr| void header_data_finalizer(header_data_t *ptr) { free_header_data(ptr); } |]

parseHeaders :: ByteString -> [Header]
parseHeaders bs = mapMaybe parseHeader (BSC.split '\n' bs)

parseHeader :: ByteString -> Maybe Header
parseHeader line =
    if BS.null line || BSC.all (== '\r') line
        then Nothing
        else case BSC.break (== ':') line of
            (key, rest) ->
                if BS.null rest
                    then Nothing
                    else
                        let value = BS.drop 1 rest
                         in Just (CI.mk $ BSC.strip key, BSC.strip value)

extractHeaders :: HeadersData -> IO [Header]
extractHeaders (HeadersData fptr) = withForeignPtr fptr \ptr -> do
    len <- [C.exp|size_t { $(header_data_t* ptr)->size }|]
    strPtr <- [C.exp|char* { $(header_data_t* ptr)->buffer }|]
    bs <- BSU.unsafePackCStringLen (strPtr, fromIntegral len)
    pure $ parseHeaders bs
