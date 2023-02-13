{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveAnyClass #-}

module Internal.Raw.Curl where

import Control.Exception (Exception)
import Control.DeepSeq
import GHC.Generics

#include <curl/curl.h>

#include "simple_string.h"

{# context lib="curl" #}

{# pointer *CURLM as CurlMulti foreign newtype #}

{# pointer *CURL as CurlEasy newtype #}

{# pointer *curl_slist as CurlSlist foreign newtype #}

{# enum CURLcode as CurlCode {underscoreToCase} with prefix = "CURLE_" deriving (Eq, Ord, Show, Generic) #}

deriving anyclass instance Exception CurlCode
deriving anyclass instance NFData CurlCode

instance NFData CurlSlist where
    rnf = rwhnf