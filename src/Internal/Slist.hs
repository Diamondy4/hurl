{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Internal.Slist where

import Control.Exception
import Control.Monad.Cont (ContT (..))
import Control.Monad.Trans
import Control.Monad.Trans.Resource (MonadResource, ReleaseKey, allocate)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Coerce
import Foreign
import Internal.Raw
import Language.C.Inline qualified as C
import Language.C.Inline.Unsafe qualified as CU

C.context (C.baseCtx <> localCtx)

C.include "<curl/curl.h>"
C.include "extras.h"

data CurlSlistError = CurlSlistAppendFailed
    deriving (Show)
    deriving anyclass (Exception)

toHeaderSlistCont :: [ByteString] -> ContT a IO (Ptr CurlSlist)
toHeaderSlistCont headers = do
    headersC <- traverse (ContT . BS.useAsCString) headers
    (headersCArrLen', headersCArr) <- ContT (withArrayLen headersC . curry)
    let headersCArrLen = fromIntegral headersCArrLen'
    ptr <-
        lift
            [CU.block| curl_slist_t* {
                curl_slist_t* slist = NULL;
                curl_slist_t* temp = NULL;

                for (size_t i = 0; i < $(size_t headersCArrLen); i++) {
                  temp = curl_slist_append(temp, $(char** headersCArr)[i]);
                  if (temp == NULL) {
                    return NULL;
                  }
                  slist = temp;
                }

                return slist;
        }|]

    if ptr == nullPtr
        then liftIO $ throwIO CurlSlistAppendFailed
        else pure ptr

toHeaderSlistP :: [ByteString] -> IO (Ptr CurlSlist)
toHeaderSlistP headers = runContT (toHeaderSlistCont headers) pure

finalizeCurlSlist :: FunPtr (Ptr CurlSlist -> IO ())
finalizeCurlSlist = [C.funPtr| void free_slist(curl_slist_t* ptr){ curl_slist_free_all(ptr); } |]

-- | Manage memory with ResourceT
allocateSlist :: (MonadResource m) => [ByteString] -> m (ReleaseKey, Ptr CurlSlist)
allocateSlist headers =
    allocate
        (toHeaderSlistP headers)
        \ptr -> [CU.block|void {curl_slist_free_all($(curl_slist_t* ptr));}|]

-- | Manage memory with ForeignPtr
toHeaderSlist :: [ByteString] -> IO CurlSlist
toHeaderSlist headers = toHeaderSlistP headers >>= coerce . newForeignPtr finalizeCurlSlist
