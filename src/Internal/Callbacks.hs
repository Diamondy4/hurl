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
import GHC.Event

import Control.Concurrent.STM
import Data.ByteString qualified as BS
import Data.Foldable
import Data.HashMap.Strict qualified as HM
import Data.IORef
import Event qualified as InnerEvent
import Event qualified as UnsafeEvent
import Extras
import Foreign.C
import Internal.Raw
import PyF
import System.Posix.Types
import Types

{- curlSocketFunction1 :: CurlSocketFunEnv -> Ptr CurlEasy -> CInt -> CInt -> Ptr () -> Ptr () -> IO CInt
curlSocketFunction1 socketUpdates _easy s what _userp _socketp = do
    let fd = Fd s
        status = toEnum . fromIntegral $ what
    case status of
        CurlPollRemove -> do
            print "in socket fun - removing socket"
            fdMap <- readIORef $ socketUpdates.fdMapRef
            case HM.lookup fd fdMap of
                Just fdKeys -> do
                    traverse_ (unregisterFd socketUpdates.eventManager) fdKeys
                    atomicModifyIORef' socketUpdates.fdMapRef (\fdMap' -> (HM.delete fd fdMap', ()))
                Nothing -> do return ()
        curlSocketState -> do
            let ghcSocketState = case curlSocketState of
                    CurlPollIn -> evtRead
                    CurlPollOut -> evtWrite
                    CurlPollInOut -> evtRead <> evtWrite
                lifetime = MultiShot
            print [fmt|in socket fun - working on socket - {show fd} - {show ghcSocketState}|]
            fdKey <- registerFd socketUpdates.eventManager (socketCallback socketUpdates.socketActionQueue) fd ghcSocketState lifetime
            atomicModifyIORef' socketUpdates.fdMapRef (\fdMap' -> (HM.insertWith (<>) fd [fdKey] fdMap', ()))
    return 0 -}

curlSocketFunction :: CurlSocketFunEnv -> Ptr CurlEasy -> CInt -> CInt -> Ptr () -> Ptr () -> IO CInt
curlSocketFunction socketUpdates _easy s what _userp _socketp = do
    let fd = Fd s
        status = toEnum . fromIntegral $ what
    case status of
        CurlPollRemove -> do
            -- print "in socket fun - removing socket"
            fdMap <- readIORef $ socketUpdates.fdMapRef
            case HM.lookup fd fdMap of
                Just fdState -> do
                    traverse_ (unregisterFd socketUpdates.eventManager) fdState.fdKeys
                    atomicModifyIORef' socketUpdates.fdMapRef (\fdMap' -> (HM.delete fd fdMap', ()))
                Nothing -> return ()
        curlSocketState -> do
            let ghcSocketEvent = case curlSocketState of
                    CurlPollIn -> evtRead
                    CurlPollOut -> evtWrite
                    CurlPollInOut -> evtRead <> evtWrite
                innerSocketEvent = InnerEvent.fromGHCEvent ghcSocketEvent
                lifetime = MultiShot
            -- print [fmt|in socket fun - working on socket - {show fd} - {show ghcSocketEvent}|]
            fdMap <- readIORef $ socketUpdates.fdMapRef
            case HM.lookup fd fdMap of
                -- event already registered on fd, we shouldn't update it
                Just fdState | InnerEvent.fromGHCEvent fdState.registeredEvents `InnerEvent.eventIs` innerSocketEvent -> pure () -- print [fmt|in socket fun - already known event - {show fdState}|] *> pure ()
                _ -> do
                    -- print "in socket fun - unknown event"
                    fdKey <- registerFd socketUpdates.eventManager (socketCallback socketUpdates.socketActionQueue) fd ghcSocketEvent lifetime
                    let fdState = FdState{registeredEvents = ghcSocketEvent, fdKeys = [fdKey]}
                    atomicModifyIORef' socketUpdates.fdMapRef (\fdMap' -> (HM.insertWith (<>) fd fdState fdMap', ()))
    return 0

curlTimerFunction :: CurlTimerFunEnv -> Ptr CurlMulti -> CLong -> Ptr () -> IO CInt
curlTimerFunction timerData _multiPtr timeout_ms _userp = do
    let tm = timerManager timerData
        tkRef = timerKey timerData
    case timeout_ms of
        -1 ->
            atomically (tryTakeTMVar tkRef) >>= \case
                Just tk -> unregisterTimeout tm tk
                Nothing -> return ()
        _ -> do
            let timeoutMicro = fromIntegral timeout_ms * 1000
            atomically (tryReadTMVar tkRef) >>= \case
                Just tk -> updateTimeout tm tk timeoutMicro
                Nothing -> do
                    tk <- registerTimeout tm timeoutMicro do
                        atomically $ writeTMVar (timerWaiter timerData) ()
                    atomically $ writeTMVar tkRef tk
    return 0

curlCreateTimerCtx :: IO CurlTimerFunEnv
curlCreateTimerCtx = do
    timerManager <- getSystemTimerManager
    timerKey <- newEmptyTMVarIO
    timerWaiter <- newEmptyTMVarIO
    pure $ CurlTimerFunEnv{..}

curlCreateSocketCallbackCtx :: EventManager -> IO CurlSocketFunEnv
curlCreateSocketCallbackCtx eventManager = do
    socketActionQueue <- newTQueueIO
    fdMapRef <- newIORef mempty
    pure $ CurlSocketFunEnv{..}

curlDebugCallback :: Ptr () -> CInt -> CString -> CInt -> Ptr () -> IO CInt
curlDebugCallback _handle infotype str_data size _usrptr = do
    debugStr <- BS.packCStringLen (str_data, fromIntegral size)
    print @String [fmt|{show infotype} => {debugStr}|]
    return 0

socketCallback :: TQueue SocketActionRequest -> FdKey -> Event -> IO ()
socketCallback socketActionQueue fdKey evt = do
    let fd = keyFd fdKey
        fdEvent = UnsafeEvent.fromGHCEvent evt
        isReadable = if fdEvent `UnsafeEvent.eventIs` UnsafeEvent.evtRead then CurlCSelectIn else CurlEventsOnSocket 0
        isWritable = if fdEvent `UnsafeEvent.eventIs` UnsafeEvent.evtWrite then CurlCSelectOut else CurlEventsOnSocket 0
        flags = isReadable <> isWritable
        -- flags = CurlEventsOnSocket 0
        socketActionRequest = SocketActionRequest{fd = fd, flags = flags}
    -- print [fmt|in socket action callback - {show socketActionRequest}|]
    atomically $ writeTQueue socketActionQueue socketActionRequest