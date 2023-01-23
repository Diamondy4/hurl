{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Types where

import Data.ByteString
import Data.Vector.Hashtables qualified as VHT
import Data.Vector.Mutable qualified as VM
import Data.Vector.Unboxed.Mutable qualified as VUM
import GHC.Generics

type HashTable k v = VHT.Dictionary (VHT.PrimState IO) VUM.MVector k VM.MVector v
data Body where
    Empty :: Body
    Buffer :: !ByteString -> Body
    -- Reader :: !(TBMQueue ByteString) -> !(Maybe Int) -> Body
    deriving (Generic)

data HTTPMethod = Get | Head | Post | Put | Delete
    deriving (Generic)

httpMethodToBS :: HTTPMethod -> ByteString
httpMethodToBS = \case
    Get -> "GET"
    Head -> "HEAD"
    Post -> "POST"
    Put -> "PUT"
    Delete -> "DELETE"
