module Response where

import Control.DeepSeq
import Data.ByteString
import GHC.Generics
import Internal.Metrics (Metrics)

data Response body = Response
    { info :: !HttpParts
    , body :: !body
    , metrics :: !Metrics
    }
    deriving (Show, Eq, Generic)
    deriving anyclass (NFData)

data HttpParts = HttpParts
    { statusCode :: !Int
    , headers :: ![(ByteString, ByteString)]
    }
    deriving (Show, Eq, Generic)
    deriving anyclass (NFData)
