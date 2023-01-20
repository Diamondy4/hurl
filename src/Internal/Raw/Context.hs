{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Internal.Raw.Context where

import Data.Map qualified as Map
import Language.Haskell.TH qualified as TH

import Internal.Raw.Curl
import Internal.Raw.SimpleString
import Language.C.Inline.Context
import Language.C.Types qualified as C

curlCtx :: Context
curlCtx =
      mempty
            { ctxTypesTable = curlTypesTable
            }

curlTypesTable :: Map.Map C.TypeSpecifier TH.TypeQ
curlTypesTable =
      Map.fromList
            [ (C.TypeName "CURLM", [t|CurlMulti|])
            , (C.TypeName "CURL", [t|CurlEasy|])
            , (C.TypeName "curl_slist_t", [t|CurlSlist|])
            , (C.TypeName "CURLMsg", [t|CurlMultiMsgRaw|])
            , (C.TypeName "simple_string", [t|SimpleString|])
            ]