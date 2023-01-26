module Response where

import Data.ByteString
import GHC.Generics
import Control.DeepSeq

data Response body = Response
    { info :: HttpParts
    , body :: body
    }
    deriving (Show, Eq, Generic)
    deriving anyclass (NFData)

data HttpParts = HttpParts
    { statusCode :: Int
    , headers :: [(ByteString, ByteString)]
    }
    deriving (Show, Eq, Generic)
    deriving anyclass (NFData)
