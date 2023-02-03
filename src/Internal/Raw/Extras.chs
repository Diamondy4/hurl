module Internal.Raw.Extras where

import Foreign
import GHC.Conc
import Control.Concurrent.MVar
import Internal.Raw.Curl

#include "extras.h"

{# pointer *hs_easy_data_t as EasyData foreign newtype #}

mkEasyData :: MVar () -> IO EasyData
mkEasyData waker = do
  (cap, _locked) <- threadCapability =<< myThreadId
  wakerSPtr <- newStablePtrPrimMVar waker
  easyDataFPtr <- mallocForeignPtrBytes {#sizeof hs_easy_data_t#}
  withForeignPtr easyDataFPtr $ \easyDataPtr -> do
     {#set hs_easy_data_t.curl_code#} easyDataPtr 0
     {#set hs_easy_data_t.waker.mvar#} easyDataPtr (castStablePtrToPtr wakerSPtr)
     {#set hs_easy_data_t.waker.waked#} easyDataPtr False
     {#set hs_easy_data_t.waker.capability#} easyDataPtr (fromIntegral cap)
  pure $ EasyData easyDataFPtr

getCurlCode :: EasyData -> IO CurlCode
getCurlCode easyData = withEasyData easyData $ \easyDataPtr -> do
   curlCode' <- {#get hs_easy_data_t.curl_code#} easyDataPtr 
   pure . toEnum . fromIntegral $ curlCode'

data MVarSPtrC = MVarSPtrC {
   mvarSPtr :: StablePtr PrimMVar,
   waked :: Bool
}

getMVarSPtrC :: Ptr EasyData -> IO MVarSPtrC
getMVarSPtrC easyDataPtr = do
   mvarSPtr <- {#get hs_easy_data_t.waker.mvar#} easyDataPtr 
   waked <- {#get hs_easy_data_t.waker.waked#} easyDataPtr
   pure $ MVarSPtrC (castPtrToStablePtr mvarSPtr) waked