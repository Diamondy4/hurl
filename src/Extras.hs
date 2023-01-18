module Extras where

import Control.Concurrent.STM
import Data.ByteString qualified as BS
import Foreign.C

cPrint :: CString -> IO ()
cPrint ptr = BS.putStr =<< BS.packCString ptr

{- | Non-blocking write of a new value to a 'TMVar'
 Puts if empty. Replaces if populated.
-}
writeTMVar :: TMVar a -> a -> STM ()
writeTMVar t new = tryTakeTMVar t >> putTMVar t new

flushTQueueWait :: TQueue a -> STM [a]
flushTQueueWait t = peekTQueue t *> flushTQueue t