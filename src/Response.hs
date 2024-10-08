module Response where

import Control.DeepSeq
import GHC.Generics
import Internal.Metrics (Metrics)
import Network.HTTP.Types.Header

data Response body = Response
    { info :: !HttpParts
    , body :: !body
    , metrics :: !Metrics
    }
    deriving (Show, Eq, Generic)
    deriving anyclass (NFData)

data HttpParts = HttpParts
    { statusCode :: !Int
    , headers :: !RequestHeaders
    }
    deriving (Show, Eq, Generic)
    deriving anyclass (NFData)
