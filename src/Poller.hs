{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module Poller where

import Control.Concurrent.STM
import Control.Monad
import Data.Foldable
import Data.Vector.Hashtables qualified as VHT
import Event qualified as InnerEvent
import GHC.Event (registerFd)
import GHC.Event qualified as GHCEvent
import Internal.Raw.SocketEvents
import System.Posix.Types
import Types
import UnliftIO (tryIO)

newEventManager :: IO EventManager
newEventManager = do
    Just eventManager <- GHCEvent.getSystemEventManager
    socketActionQueue <- newTQueueIO
    fdMap <- VHT.initialize 100
    pure $ EventManager{..}

registerSocket :: EventManager -> Fd -> GHCEvent.Event -> IO ()
registerSocket poller fd evt = do
    VHT.lookup poller.fdMap fd >>= \case
        -- event already registered on fd, we shouldn't update it
        Just !fdState | InnerEvent.fromGHCEvent fdState.registeredEvents `InnerEvent.eventIs` innerSocketEvent -> pure ()
        _ -> do
            fdKey <- GHCEvent.registerFd poller.eventManager (socketCallback poller) fd evt lifetime
            let fdState = FdState{registeredEvents = evt, fdKeys = [fdKey]}
                updateFdState (Just fdStateOld) = Just $! fdStateOld <> fdState
                updateFdState Nothing = Just fdState
            VHT.alter poller.fdMap updateFdState fd
  where
    innerSocketEvent = InnerEvent.fromGHCEvent evt
    lifetime = GHCEvent.OneShot
{-# SCC registerSocket #-}

socketCallback :: EventManager -> GHCEvent.FdKey -> GHCEvent.Event -> IO ()
socketCallback poller fdKey evt =
    atomically . writeTQueue poller.socketActionQueue $! fdEventToSocketActionRequest fdKey.keyFd evt
{-# SCC socketCallback #-}

removeSocket :: EventManager -> Fd -> IO ()
removeSocket poller fd =
    VHT.lookup poller.fdMap fd >>= \case
        Just !fdState -> do
            traverse_ (GHCEvent.unregisterFd poller.eventManager) fdState.fdKeys
            VHT.delete poller.fdMap fd
        Nothing -> pure ()
{-# SCC removeSocket #-}

renewSocket :: EventManager -> Fd -> IO ()
renewSocket poller fd =
    VHT.lookup poller.fdMap fd >>= \case
        Just !fdState -> do
            newFdKey' <- tryIO $ registerFd poller.eventManager (socketCallback poller) fd fdState.registeredEvents GHCEvent.OneShot
            case newFdKey' of
                Left _ -> pure ()
                Right newFdKey -> do
                    let newFdState = fdState{fdKeys = [newFdKey]}
                    VHT.insert poller.fdMap fd newFdState
        Nothing -> pure ()

flushEvents :: EventManager -> IO [SocketActionRequest]
flushEvents poller = do
    actions <- atomically $ flushTQueue poller.socketActionQueue
    traverse_ (\socketActRequest -> renewSocket poller socketActRequest.fd) actions
    pure actions
{-# SCC flushEvents #-}

pollForEvents :: EventManager -> STM ()
pollForEvents poller = void $ peekTQueue poller.socketActionQueue

fdEventToSocketActionRequest :: Fd -> GHCEvent.Event -> SocketActionRequest
fdEventToSocketActionRequest fd evt = SocketActionRequest{fd = fd, flags = flags}
  where
    fdEvent = InnerEvent.fromGHCEvent evt
    isReadable = if fdEvent `InnerEvent.eventIs` InnerEvent.evtRead then CurlCSelectIn else CurlEventsOnSocket 0
    isWritable = if fdEvent `InnerEvent.eventIs` InnerEvent.evtWrite then CurlCSelectOut else CurlEventsOnSocket 0
    flags = isReadable <> isWritable
