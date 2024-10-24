{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Internal.Haskell.Callbacks where

import Control.Concurrent.STM
import Control.Monad
import Data.Foldable
import Data.IORef
import Foreign.C.Types
import Foreign.Ptr
import Foreign.StablePtr
import GHC.Event
import Internal.Raw
import Internal.Raw.SocketEvents
import Language.C.Inline qualified as C
import Language.C.Inline.Unsafe qualified as CU
import System.Posix.Types

C.context (C.baseCtx <> localCtx)

C.include "HsFFI.h"
C.include "<curl/curl.h>"

newtype InnerEvent = SocketEvent' SocketEvent
    deriving (Show, Eq)

data SocketEvent = SocketEvent
    { socket :: !Fd
    , event :: !Event
    }
    deriving (Show, Eq)

data SocketCallbackEnv = SocketCallbackEnv
    { eventManager :: !EventManager
    , eventQueue :: !(TQueue InnerEvent)
    , multi :: !(Ptr CurlMulti)
    }

data SocketCtx = SocketCtx
    { curEvents :: Event
    , fdKey :: FdKey
    }
    deriving (Show)

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
        !newFdKey <- registerFd eventManager (onSocketEvent eventQueue fd) fd evts MultiShot
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
                unregisterFd eventManager socketCtx.fdKey
                !newFdKey <- registerFd eventManager (onSocketEvent eventQueue fd) fd newEvts MultiShot
                let !newSocketCtx =
                        SocketCtx
                            { curEvents = newEvts
                            , fdKey = newFdKey
                            }
                writeIORef socketCtxRef newSocketCtx

onSocketEvent :: TQueue InnerEvent -> Fd -> IOCallback
onSocketEvent !events !fd !_fdKey !event =
    atomically . writeTQueue events . SocketEvent' $
        SocketEvent
            { socket = fd
            , event = event
            }

data TimerCallbackEnv = TimerCallbackEnv
    { timerManager :: !TimerManager
    , waker :: TMVar ()
    , tkRef :: IORef (Maybe TimeoutKey)
    }

foreign export ccall hsTimerFunctionCallback :: Ptr () -> CLong -> Ptr () -> IO Int

hsTimerFunctionCallback :: Ptr () -> CLong -> Ptr () -> IO Int
hsTimerFunctionCallback !_multi !timeoutMillis !timerCbCtx = do
    let !timeoutMicros = fromIntegral $ timeoutMillis * 1000
    (TimerCallbackEnv{..}) <- deRefStablePtr =<< castPtrToStablePtr @TimerCallbackEnv <$> [CU.exp|void* { $(void* timerCbCtx) }|]

    let registerTimeout' !timeout = do
            !tk <- registerTimeout timerManager timeout (onTimeout' waker)
            writeIORef tkRef (Just tk)
        unregisterTimeout' !tk = do
            writeIORef tkRef Nothing
            unregisterTimeout timerManager tk
        updateTimeout' !oldTk !timeout = do
            unregisterTimeout timerManager oldTk
            !newTk <- registerTimeout timerManager timeout (onTimeout' waker)
            writeIORef tkRef $ Just newTk

    !tk' <- readIORef tkRef
    if
        | timeoutMillis < 0 -> maybe (pure ()) unregisterTimeout' tk'
        | timeoutMillis == 0 -> do
            maybe (registerTimeout' 1) (\(!tk) -> do updateTimeout' tk 1) tk'
        | otherwise -> do
            maybe (registerTimeout' timeoutMicros) (\(!tk) -> do updateTimeout' tk timeoutMicros) tk'
    pure 0

onTimeout' :: TMVar () -> TimeoutCallback
onTimeout' !waker = atomically . void $ tryPutTMVar waker ()

writeTQueueNoDuplicates :: (Eq a) => TQueue a -> a -> STM ()
writeTQueueNoDuplicates q x = do
    y <- tryPeekTQueue q
    case y of
        Just y' | x == y' -> pure ()
        _ -> writeTQueue q x
