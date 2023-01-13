{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveAnyClass #-}

module Internal.Raw.Curl where

import qualified Data.Map as Map
import           Data.Monoid ((<>), mempty)
import           Foreign.C.Types
import           Foreign.Ptr (Ptr)
import           Foreign.Storable (Storable(..))
import qualified Language.Haskell.TH as TH

import           Language.C.Inline qualified as C
import           Language.C.Inline.Context
import qualified Language.C.Types as C
import GHC.Generics
import GHC.Event
import Foreign
import Control.Exception (Exception)

#include <curl/curl.h>

#include "curl_hs_c.h"

{# context lib="curl" #}

{# pointer *CURLM as CurlMulti foreign newtype #}

{# pointer *CURL as CurlEasy foreign newtype #}


{# pointer *curl_slist_t as CurlSlist foreign newtype #}

{#enum define CurlReadError {CURL_READFUNC_ABORT as ReadAbort, CURL_READFUNC_PAUSE as ReadPause} deriving (Eq, Ord, Show) #}

{#enum define CurlWriteError {CURL_WRITEFUNC_PAUSE as WritePause} deriving (Eq, Ord, Show) #}

{# pointer *CURLMsg as CurlMultiMsgRaw newtype #}

{# enum CURLcode as CurlCode {underscoreToCase} with prefix = "CURLE_" deriving (Eq, Ord, Show) #}

deriving anyclass instance Exception CurlCode

data CurlMultiMsg = CurlMultiMsg
    { easy :: Ptr CurlEasy
    , result :: CurlCode
    }
    deriving (Eq, Show)

extractMessage :: CurlMultiMsgRaw -> IO CurlMultiMsg
extractMessage curlMsg = do
    easy <- {#get CURLMsg->easy_handle#} curlMsg
    result <- {#get CURLMsg->data.result#} curlMsg
    pure $ CurlMultiMsg {easy = easy, result = toEnum . fromIntegral $ result}