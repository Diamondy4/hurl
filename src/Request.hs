{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Request where

import Control.Concurrent.MVar
import Control.Concurrent.STM.TBMQueue (TBMQueue)
import Control.Monad
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BSL
import Data.Coerce
import Data.Hashable (Hashable)
import Foreign.C.Types
import Foreign.ForeignPtr.Unsafe
import Foreign.Ptr
import GHC.Generics
import Internal.Raw
import Resumer (ChanStatusVar)

type CurlEasyReadFunction = Ptr CChar -> CSize -> CSize -> Ptr () -> IO CSize

type CurlEasyWriteFunction = Ptr CChar -> CSize -> CSize -> Ptr () -> IO CSize

-- | TODO async request body
data Request = Request
    { host :: ByteString
    , timeoutMS :: CLong
    , requestBody :: BSL.ByteString
    , headers :: [ByteString]
    }
    deriving (Show, Eq, Generic)

data RequestHandler = RequestHandler
    { easy :: !CurlEasy
    , slist :: !CurlSlist
    , readRequestBodyPtr :: !(FunPtr CurlEasyReadFunction)
    , writeResponseBodyPtr :: !(FunPtr CurlEasyWriteFunction)
    , doneRequest :: !(MVar CurlCode)
    , completedResponse :: !(MVar ())
    , responseBodyChan :: !(TBMQueue ByteString)
    , responseBodyStatus :: !ChanStatusVar
    }
    deriving (Generic)

newtype RequestId = RequestId Int
    deriving (Generic, Eq)
    deriving newtype (Hashable)

handlerRequestId :: RequestHandler -> RequestId
handlerRequestId handler = easyRequestId handler.easy

easyRequestId :: CurlEasy -> RequestId
easyRequestId = easyPtrRequestId . unsafeForeignPtrToPtr . coerce

easyPtrRequestId :: Ptr CurlEasy -> RequestId
easyPtrRequestId easyPtr = RequestId reqId
  where
    IntPtr reqId = ptrToIntPtr easyPtr

completeResponse :: MVar () -> IO ()
completeResponse completeResponseWaker = do
    void $ tryPutMVar completeResponseWaker ()