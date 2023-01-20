{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Types where

import Control.Concurrent.STM (TMVar)
import Control.Concurrent.STM.TBMQueue
import Control.Concurrent.STM.TQueue
import Data.ByteString
import Data.Hashable (Hashable)
import Data.Vector.Generic qualified as VG
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Hashtables qualified as VHT
import Data.Vector.Mutable qualified as VM
import Data.Vector.Primitive qualified as VP
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Base qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM
import Foreign (Ptr)
import Foreign.C.Types
import GHC.Event qualified as GHCEvent
import GHC.Generics
import Internal.Raw
import System.Posix.Types (Fd (..))

type HashTable k v = VHT.Dictionary (VHT.PrimState IO) VUM.MVector k VM.MVector v

data EventManager = EventManager
    { eventManager :: GHCEvent.EventManager
    , socketActionQueue :: !(TQueue SocketActionRequest)
    , fdMap :: !(HashTable Fd FdState)
    }

data CurlTimerFunEnv = CurlTimerFunEnv
    { timerKey :: TMVar GHCEvent.TimeoutKey
    , timerWaiter :: TMVar ()
    , timerManager :: GHCEvent.TimerManager
    }

data CurlSocketFunEnv = CurlSocketFunEnv
    { fdMap :: !(HashTable Fd FdState)
    , eventManager :: EventManager
    , socketActionQueue :: TQueue SocketActionRequest
    }

data FdState = FdState
    { registeredEvents :: !GHCEvent.Event
    , fdKeys :: ![GHCEvent.FdKey]
    }
    deriving (Show)

instance Semigroup FdState where
    fdState1 <> fdState2 =
        FdState
            { registeredEvents = registeredEvents fdState1 <> registeredEvents fdState2
            , fdKeys = fdKeys fdState1 <> fdKeys fdState2
            }

data CurlSocketEvent = CurlSocketEvent
    { socket :: !Fd
    , status :: !CurlSocketEventRequest
    , socketDataPtr :: Ptr ()
    }
    deriving (Show)

data SocketActionRequest = SocketActionRequest
    { fd :: !Fd
    , flags :: !CurlEventsOnSocket
    }
    deriving (Eq, Ord, Show, Generic)

data LoopBreaker = Continue | Stop
    deriving (Eq, Show, Generic)

deriving instance Hashable CInt
deriving instance Hashable Fd

newtype instance VU.MVector s Fd = MV_Fd (VP.MVector s CInt)
newtype instance VU.Vector Fd = V_Fd (VP.Vector CInt)

deriving via (VU.UnboxViaPrim CInt) instance (VGM.MVector VU.MVector Fd)
deriving via (VU.UnboxViaPrim CInt) instance (VG.Vector VU.Vector Fd)

instance VU.Unbox Fd

data Body where
    Empty :: Body
    Buffer :: !ByteString -> Body
    Reader :: !(TBMQueue ByteString) -> !(Maybe Int) -> Body
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
