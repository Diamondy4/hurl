{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant multi-way if" #-}

module Internal.Haskell.Callbacks where

import Control.Concurrent.STM
import Control.Monad
import Data.Foldable
import Data.IORef
import Data.IORef.Unboxed (IORefU, readIORefU, writeIORefU)
import Data.Set
import Data.Set qualified as Set
import Foreign.C.Types
import Foreign.Ptr
import Foreign.StablePtr
import GHC.Event
import GHC.Generics
import Internal.Haskell.Event qualified as InnerEvent
import Internal.Raw
import Internal.Raw.SocketEvents
import Language.C.Inline qualified as C
import Language.C.Inline.Unsafe qualified as CU
import System.Posix.Types
import Unsafe.Coerce

C.context (C.baseCtx <> localCtx)

C.include "HsFFI.h"
C.include "<curl/curl.h>"

newtype InnerEvent = SocketEvent' SocketEvent
    deriving (Show, Eq)

data SocketEvent = SocketEvent
    { socket :: !Fd
    , event :: !InnerEvent.Event
    }
    deriving (Show, Eq, Ord, Generic)

data SocketCallbackEnv = SocketCallbackEnv
    { eventManager :: !EventManager
    , multi :: !(Ptr CurlMulti)
    , socketEventsState :: SocketEventState
    }

data SocketCtx = SocketCtx
    { curEvents :: !Event
    , fdKey :: !FdKey
    }
    deriving (Show)

data SocketEventState = SocketEventState
    { eventsHappened :: !(TVar (Set SocketEvent))
    , eventsOrder :: !(TVar [SocketEvent])
    }

initSocketEventState :: IO SocketEventState
initSocketEventState = do
    eventsHappened <- newTVarIO mempty
    eventsOrder <- newTVarIO []
    pure SocketEventState{..}

waitFlushSocketEventState :: SocketEventState -> STM [SocketEvent]
waitFlushSocketEventState SocketEventState{..} = do
    events <- readTVar eventsOrder
    case events of
        [] -> retry
        _ -> do
            writeTVar eventsHappened mempty
            writeTVar eventsOrder mempty
            pure events

foreign export ccall hsSocketFunctionCallback :: Ptr CurlEasy -> Fd -> Int -> Ptr () -> Ptr () -> IO Int

hsSocketFunctionCallback :: Ptr CurlEasy -> Fd -> Int -> Ptr () -> Ptr () -> IO Int
hsSocketFunctionCallback !_easyPtr !socketFd !action !socketCallbackEnvPtr !socketCtxPtr = do
    !socketCallbackEnv <- deRefStablePtr =<< castPtrToStablePtr @SocketCallbackEnv <$> [CU.exp|void* { $(void* socketCallbackEnvPtr) }|]
    !socketCtxRef <-
        if socketCtxPtr == nullPtr
            then pure Nothing
            else Just <$> deRefStablePtr (castPtrToStablePtr @(IORef SocketCtx) socketCtxPtr)

    let !event = toEnum action
    case event of
        CurlPollRemove -> do
            for_ socketCtxRef (unregisterFd' socketCallbackEnv)
        CurlPollIn -> do
            updateOrRegisterFd' socketCallbackEnv socketCtxRef evtRead socketFd
        CurlPollOut -> do
            updateOrRegisterFd' socketCallbackEnv socketCtxRef evtWrite socketFd
        CurlPollInOut -> do
            updateOrRegisterFd' socketCallbackEnv socketCtxRef (evtRead <> evtWrite) socketFd
    pure 0
  where
    updateOrRegisterFd' !socketCallbackEnv !socketCtxRef !newEvts !fd = case socketCtxRef of
        Nothing -> registerFd' socketCallbackEnv newEvts fd
        Just !socketCtxRef' -> updateFd' socketCallbackEnv socketCtxRef' newEvts fd
    registerFd' (SocketCallbackEnv{..}) !evts !fd = do
        !newFdKey <- registerFd eventManager (onSocketEvent socketEventsState fd) fd evts MultiShot
        let socketCtx =
                SocketCtx
                    { curEvents = evts
                    , fdKey = newFdKey
                    }
            Fd !fdC = fd
        !newSocketCtxRefPtr <- castStablePtrToPtr <$> (newStablePtr =<< newIORef socketCtx)
        [CU.block|void {
                curl_multi_assign($(CURLM* multi), $(int fdC), $(void* newSocketCtxRefPtr));
            }|]
    unregisterFd' !socketCallbackEnv !socketCtxRef = do
        fdKey <- (.fdKey) <$> readIORef socketCtxRef
        unregisterFd socketCallbackEnv.eventManager fdKey
        [CU.block|void {
                hs_free_stable_ptr($(void* socketCtxPtr));
            }|]
    updateFd' (SocketCallbackEnv{..}) !socketCtxRef !newEvts !fd = do
        socketCtx <- readIORef socketCtxRef
        if socketCtx.curEvents == newEvts
            then pure ()
            else do
                !newFdKey <- registerFd eventManager (onSocketEvent socketEventsState fd) fd newEvts MultiShot
                unregisterFd eventManager socketCtx.fdKey
                let !newSocketCtx =
                        SocketCtx
                            { curEvents = newEvts
                            , fdKey = newFdKey
                            }
                writeIORef socketCtxRef newSocketCtx

onSocketEvent :: SocketEventState -> Fd -> IOCallback
onSocketEvent SocketEventState{..} !fd !_fdKey !event = atomically do
    !eventsHappened' <- readTVar eventsHappened
    if Set.member se eventsHappened'
        then pure ()
        else do
            modifyTVar' eventsHappened . Set.insert $! se
            modifyTVar' eventsOrder . (:) $! se
  where
    !se =
        SocketEvent
            { socket = fd
            , event = InnerEvent.fromGHCEvent event
            }

data TimerCallbackEnv = TimerCallbackEnv
    { timerManager :: !TimerManager
    , waker :: !(TMVar ())
    , tkRef :: !(IORefU Int)
    }

foreign export ccall hsTimerFunctionCallback :: Ptr () -> CLong -> Ptr () -> IO Int

hsTimerFunctionCallback :: Ptr () -> CLong -> Ptr () -> IO Int
hsTimerFunctionCallback !_multi !timeoutMillis !timerCbCtx = do
    let !timeoutMicros' = fromIntegral $ timeoutMillis * 1000
        !timeoutMicros = if timeoutMicros' == 0 then 1 else timeoutMicros'
    (TimerCallbackEnv{..}) <- deRefStablePtr =<< castPtrToStablePtr @TimerCallbackEnv <$> [CU.exp|void* { $(void* timerCbCtx) }|]

    !tk' <- readIORefU tkRef
    let !tk :: TimeoutKey = unsafeCoerce tk'
    if
        | timeoutMillis < 0 -> when (tk' /= -1) $ do
            writeIORefU tkRef $ -1
            unregisterTimeout timerManager tk
        | otherwise -> do
            !newTk <- registerTimeout timerManager timeoutMicros (onTimeout' waker)
            writeIORefU tkRef (unsafeCoerce newTk)
            when (tk' /= -1) $ unregisterTimeout timerManager tk

    pure 0

onTimeout' :: TMVar () -> TimeoutCallback
onTimeout' !waker = atomically . void $ tryPutTMVar waker ()
