{-# OPTIONS_GHC -Wno-name-shadowing #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CApiFFI #-}

module Internal.Raw.Curl where

import Control.Exception (Exception)
import Control.DeepSeq
import GHC.Generics
import Data.Singletons.TH (genSingletons)

#include <curl/curl.h>

#include "simple_string.h"

{# context lib="curl" #}

{# pointer *CURLM as CurlMulti foreign newtype #}

{# pointer *CURL as CurlEasy newtype #}

{# pointer *curl_slist as CurlSlist foreign newtype #}

instance NFData CurlSlist where
    rnf = rwhnf

{# enum CURLoption as CurlOption {underscoreToCase} omit (CURLOPT_OBSOLETE40, CURLOPT_LASTENTRY) with prefix = "CURLOPT_" add prefix = "Easy" deriving (Eq, Ord, Show, Generic) #}

$(genSingletons [''CurlOption])

deriving anyclass instance NFData CurlOption

{# enum CURLcode as CurlCode {underscoreToCase} omit (CURL_LAST) with prefix = "CURLE_" deriving (Eq, Ord, Show, Generic) #}

deriving anyclass instance Exception CurlCode
deriving anyclass instance NFData CurlCode

{# enum define HTTPVersion {CURL_HTTP_VERSION_1_0 as HTTP_1_0, CURL_HTTP_VERSION_1_1 as HTTP_1_1, CURL_HTTP_VERSION_2_0 as HTTP_2, CURL_HTTP_VERSION_2TLS as HTTP_2_TLS, CURL_HTTP_VERSION_2_PRIOR_KNOWLEDGE as HTTP_2_NoUpgrade, CURL_HTTP_VERSION_3 as HTTP_3} deriving (Eq, Ord, Show, Generic) #}
