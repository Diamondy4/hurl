{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Internal.Callbacks where

import Foreign hiding (void)
import Foreign.C.Types

import Control.Concurrent.STM
import Data.ByteString qualified as BS
import Data.Vector.Hashtables qualified as VHT
import Foreign.C
import GHC.Event qualified as GHCEvent
import Internal.Raw
import Poller (registerSocket, removeSocket)
import PyF
import System.Posix.Types
import Types
    ( EventManager, CurlSocketFunEnv(..), CurlTimerFunEnv(..) )


curlSocketFunction :: CurlSocketFunEnv -> Ptr CurlEasy -> CInt -> CInt -> Ptr () -> Ptr () -> IO CInt
curlSocketFunction socketEnv _easy s what _userp _socketp = do
    let fd = Fd s
        status = toEnum . fromIntegral $ what
    case status of
        CurlPollRemove -> removeSocket socketEnv.eventManager fd
        curlSocketState -> do
            let ghcSocketEvent = case curlSocketState of
                    CurlPollIn -> GHCEvent.evtRead
                    CurlPollOut -> GHCEvent.evtWrite
                    CurlPollInOut -> GHCEvent.evtRead <> GHCEvent.evtWrite
            registerSocket socketEnv.eventManager fd ghcSocketEvent
    return 0
{-# SCC curlSocketFunction #-}

curlTimerFunction :: CurlTimerFunEnv -> Ptr CurlMulti -> CLong -> Ptr () -> IO CInt
curlTimerFunction !timerEnv _multiPtr !timeout_ms _userp = do
    let tm = timerManager timerEnv
        tkRef = timerKey timerEnv
    case timeout_ms of
        -1 ->
            atomically (tryTakeTMVar tkRef) >>= \case
                Just !tk -> GHCEvent.unregisterTimeout tm tk
                Nothing -> return ()
        _ -> do
            let !timeoutMicro = fromIntegral timeout_ms * 1000
            atomically (tryReadTMVar tkRef) >>= \case
                Just !tk -> GHCEvent.updateTimeout tm tk timeoutMicro
                Nothing -> do
                    !tk <- GHCEvent.registerTimeout tm timeoutMicro do
                        atomically $ writeTMVar (timerWaiter timerEnv) ()
                    atomically $ writeTMVar tkRef tk
    return 0
{-# SCC curlTimerFunction #-}

curlCreateTimerCtx :: IO CurlTimerFunEnv
curlCreateTimerCtx = do
    timerManager <- GHCEvent.getSystemTimerManager
    timerKey <- newEmptyTMVarIO
    timerWaiter <- newEmptyTMVarIO
    pure $ CurlTimerFunEnv{..}

curlCreateSocketCallbackCtx :: EventManager -> IO CurlSocketFunEnv
curlCreateSocketCallbackCtx eventManager = do
    socketActionQueue <- newTQueueIO
    fdMap <- VHT.initialize 100
    pure $ CurlSocketFunEnv{..}

curlDebugCallback :: Ptr () -> CInt -> CString -> CInt -> Ptr () -> IO CInt
curlDebugCallback _handle infotype str_data size _usrptr = do
    debugStr <- BS.packCStringLen (str_data, fromIntegral size)
    print @String [fmt|{show infotype} => {debugStr}|]
    return 0
