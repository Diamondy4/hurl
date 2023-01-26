module Types where

import Control.DeepSeq
import Data.ByteString
import Data.Vector.Hashtables qualified as VHT
import Data.Vector.Mutable qualified as VM
import Data.Vector.Unboxed.Mutable qualified as VUM
import GHC.Generics
import Numeric.Natural

type HashTable k v = VHT.Dictionary (VHT.PrimState IO) VUM.MVector k VM.MVector v
data Body where
    Empty :: Body
    Buffer :: !ByteString -> Body
    -- Reader :: !(TBMQueue ByteString) -> !(Maybe Int) -> Body
    deriving (Generic)
    deriving anyclass (NFData)

data HTTPMethod = Get | Head | Post | Put | Delete
    deriving (Generic)
    deriving anyclass (NFData)

httpMethodToBS :: HTTPMethod -> ByteString
httpMethodToBS = \case
    Get -> "GET"
    Head -> "HEAD"
    Post -> "POST"
    Put -> "PUT"
    Delete -> "DELETE"

data AgentConfig = AgentConfig
    { maxConnection :: !Natural
    , maxConnectionPerHost :: !Natural
    , connectionCacheSize :: !Natural
    }
    deriving (Show, Eq, Generic)

defaultConfig :: AgentConfig
defaultConfig =
    AgentConfig
        { maxConnection = 0
        , maxConnectionPerHost = 0
        , connectionCacheSize = 0
        }