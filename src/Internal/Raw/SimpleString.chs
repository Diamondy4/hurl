{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Internal.Raw.SimpleString where

import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable

#include "curl_hs_c.h"

data SimpleString = SimpleString {
    ptr :: !(Ptr CChar),
    len :: !Int
}

{# pointer *simple_string as SimpleStringPtr foreign -> SimpleString  #}

instance Storable SimpleString where
  sizeOf _ = {#sizeof simple_string#}
  alignment _ = {#alignof simple_string#}
  peek p = do
    ptr' <- {#get simple_string.ptr#} p
    len' <- fromIntegral <$> {#get simple_string.len#} p
    pure SimpleString {ptr = ptr', len = len'}
  poke p v = do
    {#set simple_string.ptr#} p (ptr v)
    {#set simple_string.len#} p (fromIntegral . len $ v)