{-# LANGUAGE ImpredicativeTypes #-}

module Request where

import Control.Concurrent.MVar
import Control.DeepSeq
import Control.Monad
import Control.Monad.Trans.Resource
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import GHC.Generics
import Internal.Options
import Internal.Raw
import Internal.Raw.Extras
import Internal.Raw.Metrics (CurlMetricsContext)
import Internal.Raw.SimpleString (SimpleStringPtr)
import Types

-- | TODO async request body
data Request = Request
    { host :: !ByteString
    , timeoutMS :: !Int
    , connectionTimeoutMS :: !Int
    , lowSpeedLimit :: !LowSpeedLimit
    , body :: !Body
    , method :: !HTTPMethod
    , headers :: !RequestHeader
    , extraOptions :: ![SomeOption]
    }
    deriving (Generic)
    deriving anyclass (NFData)

data RequestHeader = NoHeaders | HeaderList ![BS.ByteString] | OverrideHeaders !CurlSlist
    deriving (Generic)
    deriving anyclass (NFData)

data LowSpeedLimit = LowSpeedLimit
    { lowSpeed :: !Int
    , timeout :: !Int
    }
    deriving (Generic)
    deriving anyclass (NFData)

-- TODO: support streaming request body
data RequestHandler = RequestHandler
    { easy :: !CurlEasy
    , easyData :: !EasyData
    , doneRequest :: !(MVar ())
    , requestBody :: !Body
    , responseSimpleString :: !SimpleStringPtr
    , metricsContext :: !CurlMetricsContext
    , resources :: ![ReleaseKey]
    }
    deriving (Generic)

completeResponse :: MVar () -> IO ()
completeResponse completeResponseWaker = do
    void $ tryPutMVar completeResponseWaker ()
