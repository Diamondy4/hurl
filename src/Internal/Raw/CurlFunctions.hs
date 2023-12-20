{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE CPP #-}

module Internal.Raw.CurlFunctions where

import Foreign
import Foreign.C
import Internal.Raw.Curl

-- https://github.com/haskell/haskell-language-server/issues/365
#ifndef __GHCIDE__

foreign import ccall unsafe "curl/curl.h curl_easy_setopt"
    curl_easy_setopt_long :: Ptr CurlEasy -> CInt -> CLong -> IO ()

foreign import ccall unsafe "curl/curl.h curl_easy_setopt"
    curl_easy_setopt_string :: Ptr CurlEasy -> CInt -> Ptr CChar -> IO ()

#else

curl_easy_setopt_long   :: Ptr CurlEasy -> CInt -> CLong -> IO ()
curl_easy_setopt_long = undefined

curl_easy_setopt_string :: Ptr CurlEasy -> CInt -> Ptr CChar -> IO ()
curl_easy_setopt_string = undefined

#endif
