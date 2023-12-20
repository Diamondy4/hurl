{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Internal.Raw.Context where

import Data.Map qualified as Map
import Language.Haskell.TH qualified as TH

import Internal.Raw.Curl
import Internal.Raw.Extras
import Internal.Raw.MPSC
import Internal.Raw.Metrics
import Internal.Raw.SimpleString
import Internal.Raw.UV
import Language.C.Inline.Context
import Language.C.Types qualified as C

localCtx :: Context
localCtx =
    mempty
        { ctxTypesTable = curlTypesTable <> extraTypesTable <> libuvTypesTable
        }

curlTypesTable :: Map.Map C.TypeSpecifier TH.TypeQ
curlTypesTable =
    Map.fromList
        [ (C.TypeName "CURLM", [t|CurlMulti|])
        , (C.TypeName "CURL", [t|CurlEasy|])
        , (C.TypeName "curl_slist_t", [t|CurlSlist|])
        ]

extraTypesTable :: Map.Map C.TypeSpecifier TH.TypeQ
extraTypesTable =
    Map.fromList
        [ (C.TypeName "hs_easy_data_t", [t|EasyData|])
        , (C.TypeName "simple_string_t", [t|SimpleString|])
        , (C.TypeName "mpsc_t", [t|MPSCQ|])
        , (C.TypeName "outer_message_t", [t|InternalOuterMessage|])
        , (C.TypeName "curl_metrics_context_t", [t|CurlMetricsContext|])
        ]

libuvTypesTable :: Map.Map C.TypeSpecifier TH.TypeQ
libuvTypesTable =
    Map.fromList
        [ (C.TypeName "uv_async_t", [t|UVAsync|])
        , (C.TypeName "uv_loop_t", [t|UVLoop|])
        ]
