module Response where

import Control.Concurrent.STM.TBMQueue (TBMQueue)
import Data.ByteString
import GHC.Generics

data Response body = Response
    { info :: HttpParts
    , body :: body
    }
    deriving (Show, Eq, Generic)

data HttpParts = HttpParts
    { statusCode :: Int
    , headers :: [(ByteString, ByteString)]
    }
    deriving (Show, Eq, Generic)

newtype Body = BodyChan (TBMQueue ByteString)