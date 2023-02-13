module Types where 

import Control.DeepSeq
import Data.ByteString
import GHC.Generics
import Numeric.Natural

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