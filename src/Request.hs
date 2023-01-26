{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}

module Request where

import Control.Concurrent.MVar
import Control.Monad
import Data.ByteString (ByteString)
import Data.Coerce
import Data.Hashable (Hashable)
import Data.Vector.Generic qualified as VG
import Data.Vector.Generic.Mutable qualified as VM
import Data.Vector.Primitive qualified as VP
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Base qualified as VU
import Foreign.C.Types
import Foreign.ForeignPtr.Unsafe
import Foreign.Ptr
import GHC.Generics
import Internal.Raw
import Internal.Raw.SimpleString (SimpleStringPtr)
import Types
import Internal.Raw.Extras
import Control.DeepSeq

-- | TODO async request body
data Request = Request
    { host :: !ByteString
    , timeoutMS :: !Int
    , connectionTimeoutMS :: !Int
    , body :: !Body
    , method :: !HTTPMethod
    , headers :: ![ByteString]
    }
    deriving (Generic)
    deriving anyclass (NFData)

-- TODO: support streaming request body
data RequestHandler = RequestHandler
    { easy :: !CurlEasy
    , easyData :: !EasyData
    , doneRequest :: !(MVar ())
    -- ^ Associated easy handle
    , requestHeaders :: !(Maybe CurlSlist)
    -- ^ Request headers list
    , requestBody :: !Body
    -- ^ Request is done completely and removed from multi handle. Body is consumed and no more data recieved.
    , responseSimpleString :: !SimpleStringPtr
    }
    deriving (Generic)

newtype RequestId = RequestId Int
    deriving (Generic, Eq)
    deriving newtype (Hashable)

newtype instance VU.MVector s RequestId = MV_RequestId (VP.MVector s Int)
newtype instance VU.Vector RequestId = V_RequestId (VP.Vector Int)

deriving via (VU.UnboxViaPrim Int) instance (VM.MVector VU.MVector RequestId)
deriving via (VU.UnboxViaPrim Int) instance (VG.Vector VU.Vector RequestId)

instance VU.Unbox RequestId

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