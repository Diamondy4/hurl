module Internal.Raw.Alloc where

import Foreign
import Foreign.C.Types

foreign import ccall unsafe "stdlib.h malloc" c_malloc :: CSize -> IO (Ptr ())

foreign import ccall unsafe "stdlib.h free" c_free :: Ptr () -> IO ()
