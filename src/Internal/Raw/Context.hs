{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}


module Internal.Raw.Context where

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
import Internal.Raw.Curl


curlCtx :: Context
curlCtx = mempty
      { ctxTypesTable = curlTypesTable
      }

curlTypesTable :: Map.Map C.TypeSpecifier TH.TypeQ
curlTypesTable = Map.fromList
  [ (C.TypeName "CURLM", [t| CurlMulti |])
  , (C.TypeName "CURL", [t| CurlEasy |])
  , (C.TypeName "curl_slist_t", [t| CurlSlist |])
  , (C.TypeName "CURLMsg", [t| CurlMultiMsgRaw |])
  ]