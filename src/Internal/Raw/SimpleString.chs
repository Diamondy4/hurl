{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Internal.Raw.SimpleString where

import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable

#include "simple_string.h"

data SimpleString = SimpleString {
    ptr :: !(Ptr CChar),
    len :: !Int
}

{# pointer *simple_string_t as SimpleStringPtr foreign -> SimpleString  #}

instance Storable SimpleString where
  sizeOf _ = {#sizeof simple_string_t#}
  alignment _ = {#alignof simple_string_t#}
  peek p = do
    ptr' <- {#get simple_string_t.ptr#} p
    len' <- fromIntegral <$> {#get simple_string_t.len#} p
    pure SimpleString {ptr = ptr', len = len'}
  poke p v = do
    {#set simple_string_t.ptr#} p (ptr v)
    {#set simple_string_t.len#} p (fromIntegral . len $ v)