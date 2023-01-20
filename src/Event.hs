{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Event where

import Data.Bits
import Data.Hashable (Hashable)
import GHC.Event qualified as GHCEvent
import GHC.Generics
import Unsafe.Coerce

newtype Event = Event Int
    deriving (Eq, Generic)
    deriving anyclass (Hashable)

eventIs :: Event -> Event -> Bool
eventIs (Event a) (Event b) = a .&. b /= 0

toGHCEvent :: Event -> GHCEvent.Event
toGHCEvent = unsafeCoerce

fromGHCEvent :: GHCEvent.Event -> Event
fromGHCEvent = unsafeCoerce

evtRead :: Event
evtRead = fromGHCEvent GHCEvent.evtRead

evtWrite :: Event
evtWrite = fromGHCEvent GHCEvent.evtWrite

testEvent :: GHCEvent.Event
testEvent = GHCEvent.evtRead <> GHCEvent.evtWrite

-- >>> (fromGHCEvent testEvent) `eventIs` evtRead
-- True