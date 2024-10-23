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

data InnerEvent = SocketEvent' SocketEvent | TimerRing
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
    !socketCtx <-
        if socketCtxPtr == nullPtr
            then pure Nothing
            else Just <$> deRefStablePtr (castPtrToStablePtr @SocketCtx socketCtxPtr)

    let !event = toEnum action
    case event of
        CurlPollRemove -> do
            for_ socketCtx (unregisterFd' socketCallbackEnv . (.fdKey))
        CurlPollIn -> do
            updateOrRegisterFd' socketCallbackEnv socketCtx evtRead socketFd
        CurlPollOut -> do
            updateOrRegisterFd' socketCallbackEnv socketCtx evtWrite socketFd
        CurlPollInOut -> do
            updateOrRegisterFd' socketCallbackEnv socketCtx (evtRead <> evtWrite) socketFd
    pure 0
  where
    updateOrRegisterFd' !socketCallbackEnv !socketCtx !newEvts !fd = case socketCtx of
        Nothing -> registerFd' socketCallbackEnv newEvts fd
        Just !socketCtx' -> updateFd' socketCallbackEnv socketCtx' newEvts fd
    registerFd' (SocketCallbackEnv{..}) !evts !fd = do
        !newFdKey <- registerFd eventManager (onSocketEvent eventQueue fd) fd evts MultiShot
        let socketCtx =
                SocketCtx
                    { curEvents = evts
                    , fdKey = newFdKey
                    }
            Fd !fdC = fd
        !newSocketCtxPtr <- castStablePtrToPtr <$> newStablePtr socketCtx
        [CU.block|void {
                curl_multi_assign($(CURLM* multi), $(int fdC), $(void* newSocketCtxPtr));
            }|]
    unregisterFd' !socketCallbackEnv !fdKey = do
        unregisterFd socketCallbackEnv.eventManager fdKey
        [CU.block|void {
                hs_free_stable_ptr($(void* socketCtxPtr));
            }|]
    updateFd' (SocketCallbackEnv{..}) !socketCtx !newEvts !fd = do
        if socketCtx.curEvents == newEvts
            then pure ()
            else do
                !newFdKey <- registerFd eventManager (onSocketEvent eventQueue fd) fd newEvts MultiShot
                unregisterFd eventManager socketCtx.fdKey
                let !newSocketCtx =
                        SocketCtx
                            { curEvents = newEvts
                            , fdKey = newFdKey
                            }
                    Fd !fdC = fd
                !newSocketCtxPtr <- castStablePtrToPtr <$> newStablePtr newSocketCtx
                [CU.block|void {
                        hs_free_stable_ptr($(void* socketCtxPtr));
                        curl_multi_assign($(CURLM* multi), $(int fdC), $(void* newSocketCtxPtr));
                    }|]

onSocketEvent :: TQueue InnerEvent -> Fd -> IOCallback
onSocketEvent !events !fd !_fdKey !event =
    atomically . writeTQueueNoDuplicates events . SocketEvent' $
        SocketEvent
            { socket = fd
            , event = event
            }

data TimerCallbackEnv = TimerCallbackEnv
    { timerManager :: TimerManager
    , eventQueue :: TQueue InnerEvent
    , tkRef :: IORef (Maybe TimeoutKey)
    }

foreign export ccall hsTimerFunctionCallback :: Ptr () -> CLong -> Ptr () -> IO Int

hsTimerFunctionCallback :: Ptr () -> CLong -> Ptr () -> IO Int
hsTimerFunctionCallback !_multi !timeoutMillis !timerCbCtx = do
    let !timeoutMicros = fromIntegral $ timeoutMillis * 1000
    !timeCallbackEnv <- deRefStablePtr =<< castPtrToStablePtr @TimerCallbackEnv <$> [CU.exp|void* { $(void* timerCbCtx) }|]

    let registerTimeout' timeout = do
            tk <- registerTimeout timeCallbackEnv.timerManager timeout (onTimeout' timeCallbackEnv.eventQueue)
            atomicWriteIORef timeCallbackEnv.tkRef (Just tk)
        unregisterTimeout' tk = do
            atomicWriteIORef timeCallbackEnv.tkRef Nothing
        -- unregisterTimeout timeCallbackEnv.timerManager tk
        updateTimeout' oldTk timeout = do
            newTk <- registerTimeout timeCallbackEnv.timerManager timeout (onTimeout' timeCallbackEnv.eventQueue)
            atomicWriteIORef timeCallbackEnv.tkRef $ Just newTk
    -- unregisterTimeout timeCallbackEnv.timerManager oldTk

    tk' <- readIORef timeCallbackEnv.tkRef
    if
        | timeoutMillis < 0 -> maybe (pure ()) unregisterTimeout' tk'
        | timeoutMillis == 0 -> do
            maybe (registerTimeout' 1) (\tk -> do updateTimeout' tk 1) tk'
        | otherwise -> do
            maybe (registerTimeout' timeoutMicros) (\tk -> do updateTimeout' tk timeoutMicros) tk'
    pure 0

onTimeout' :: TQueue InnerEvent -> TimeoutCallback
onTimeout' !events = do
    atomically . writeTQueueNoDuplicates events $ TimerRing

onTimeout :: TQueue InnerEvent -> IORef (Maybe TimeoutKey) -> TimeoutCallback
onTimeout events tkRef = do
    atomicWriteIORef tkRef Nothing
    atomically . writeTQueueNoDuplicates events $ TimerRing

writeTQueueNoDuplicates :: (Eq a) => TQueue a -> a -> STM ()
writeTQueueNoDuplicates q x = do
    y <- tryPeekTQueue q
    case y of
        Just y' | x == y' -> pure ()
        _ -> writeTQueue q x
