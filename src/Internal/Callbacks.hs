{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Internal.Callbacks where

{-

curlDebugCallback :: Ptr () -> CInt -> CString -> CInt -> Ptr () -> IO CInt
curlDebugCallback _handle infotype str_data size _usrptr = do
    debugStr <- BS.packCStringLen (str_data, fromIntegral size)
    print @String [fmt|{show infotype} => {debugStr}|]
    return 0
 -}