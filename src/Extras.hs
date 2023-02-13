{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Extras where

import Control.Monad.Trans.Resource (MonadResource, ReleaseKey, register)
import Data.ByteString
import Data.ByteString qualified as BS
import Data.ByteString.Internal
import Foreign.C
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable
import Internal.Raw.Context
import Internal.Raw.SimpleString
import Language.C.Inline qualified as C

C.context (C.baseCtx <> C.funCtx <> C.fptrCtx <> C.bsCtx <> localCtx)

C.include "simple_string.h"
C.include "<stdlib.h>"

cPrint :: CString -> IO ()
cPrint ptr = BS.putStr =<< BS.packCString ptr

simpleStringToBS :: SimpleStringPtr -> IO ByteString
simpleStringToBS simpleStringPtr = do
    !simpleString <- withForeignPtr simpleStringPtr peek
    fp <- newForeignPtr finalizeCharPtr simpleString.ptr
    return $ BS (castForeignPtr fp) simpleString.len

finalizeCharPtr :: FunPtr (Ptr CChar -> IO ())
finalizeCharPtr = [C.funPtr| void free_char_ptr(char* s) { free(s); } |]

registerForeignPtr :: MonadResource m => ForeignPtr a -> m ReleaseKey
registerForeignPtr fp = register $ finalizeForeignPtr fp