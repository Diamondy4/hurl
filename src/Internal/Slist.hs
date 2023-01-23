{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Internal.Slist where

import Control.Monad.Cont
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Coerce
import Foreign
import Internal.Raw
import Language.C.Inline qualified as C

C.context (C.baseCtx <> localCtx)

C.include "<curl/curl.h>"
C.include "extras.h"

toHeaderSlistCont :: [ByteString] -> ContT a IO (Maybe (Ptr CurlSlist))
toHeaderSlistCont headers = do
  headersC <- traverse (ContT . BS.useAsCString) headers
  (headersCArrLen', headersCArr) <- ContT (withArrayLen headersC . curry)
  let headersCArrLen = fromIntegral headersCArrLen'
  ptr <-
    lift
      [C.block| curl_slist_t* {
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
  pure if ptr == nullPtr then Nothing else Just ptr

toHeaderSlistP :: [ByteString] -> IO (Maybe (Ptr CurlSlist))
toHeaderSlistP headers = runContT (toHeaderSlistCont headers) return

finalizeCurlSlist :: FunPtr (Ptr CurlSlist -> IO ())
finalizeCurlSlist = [C.funPtr| void free_slist(curl_slist_t* ptr){ curl_slist_free_all(ptr); } |]

toHeaderSlist :: [ByteString] -> IO (Maybe CurlSlist)
toHeaderSlist headers = traverse (coerce . newForeignPtr finalizeCurlSlist) =<< toHeaderSlistP headers