{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Internal.Options.Class where

import Data.Kind
import Foreign.Ptr
import Internal.Raw.Curl

class EasyOption option where
    type CurlParamBaseType option :: Type
    setEasyOption :: Ptr CurlEasy -> CurlParamBaseType option -> IO ()