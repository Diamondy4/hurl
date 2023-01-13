module Resumer where

import Control.Concurrent.STM
import Data.Typeable

data ChanStatus = Paused | Running

newtype ChanStatusVar = ChanStatusVar (TVar ChanStatus)
    deriving (Eq, Typeable)

newChanStatusVarIO :: IO ChanStatusVar
newChanStatusVarIO = do
    t <- newTVarIO Running
    return (ChanStatusVar t)

waitChanPaused :: ChanStatusVar -> STM ()
waitChanPaused (ChanStatusVar t) = do
    m <- readTVar t
    case m of
        Running -> retry
        Paused -> return ()

pauseChan :: ChanStatusVar -> STM ()
pauseChan (ChanStatusVar t) = writeTVar t Paused

resumeChan :: ChanStatusVar -> STM ()
resumeChan (ChanStatusVar t) = writeTVar t Running

{- data TBMPQueue a = TBMPQueue
    { chan :: TBMQueue a
    , status :: ChanStatusVar
    }

newTBMPQueue :: Int -> IO (TBMPQueue a)
newTBMPQueue bound = do
    chan <- newTBMQueueIO bound
    t <- newChanStatusVarIO Running
    return (TBMPQueue chan t)

tryReadTBMPQueue (TBMPQueue chan status) = do
    (Just <$> readTBMQueue chan) <|> (Nothing <$ waitChanPaused status) -}
