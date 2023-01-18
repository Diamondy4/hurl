module Response where

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
