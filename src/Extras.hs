{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Extras where

import Control.Exception qualified as EUnsafe
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
import UnliftIO

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

bracketOnErrorWithError :: MonadUnliftIO m => m a -> (SomeException -> a -> m b) -> (a -> m c) -> m c
bracketOnErrorWithError before after thing = withRunInIO $ \run -> EUnsafe.mask $ \restore -> do
    x <- run before
    res1 <- EUnsafe.try $ restore $ run $ thing x
    case res1 of
        Left (e1 :: SomeException) -> do
            -- ignore the exception, see bracket for explanation
            _ :: Either SomeException b <-
                EUnsafe.try $ EUnsafe.uninterruptibleMask_ $ run $ after e1 x
            EUnsafe.throwIO e1
        Right y -> return y

