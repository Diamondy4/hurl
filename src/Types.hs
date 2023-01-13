{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Types where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM (STM, TMVar)
import Control.Concurrent.STM.TQueue
import Data.HashMap.Strict
import Data.Hashable (Hashable)
import Data.IORef
import Event qualified as InternalEvent
import Foreign
import Foreign.C.Types
import GHC.Event
import Internal.Raw
import Request
import System.Posix.Types (Fd (..))

data CurlTimerFunEnv = CurlTimerFunEnv
    { timerKey :: TMVar TimeoutKey
    , timerWaiter :: TMVar ()
    , timerManager :: TimerManager
    }

data CurlSocketFunEnv = CurlSocketFunEnv
    { fdMapRef :: IORef (HashMap Fd FdState)
    , eventManager :: EventManager
    , socketActionQueue :: TQueue SocketActionRequest
    }

data FdState = FdState
    { registeredEvents :: !Event
    , fdKeys :: ![FdKey]
    }
    deriving (Show)

instance Semigroup FdState where
    fdState1 <> fdState2 =
        FdState
            { registeredEvents = registeredEvents fdState1 <> registeredEvents fdState2
            , fdKeys = fdKeys fdState1 <> fdKeys fdState2
            }

data CurlSocketEvent = CurlSocketEvent
    { socket :: Fd
    , status :: CurlSocketEventRequest
    , socketDataPtr :: Ptr ()
    }
    deriving (Show)

data AgentMessage where
    Close :: AgentMessage
    Execute :: RequestHandler -> AgentMessage
    UnpauseRead :: RequestId -> AgentMessage
    UnpauseWrite :: RequestId -> AgentMessage
    CancelRequest :: RequestId -> AgentMessage

data SocketActionRequest = SocketActionRequest
    { fd :: Fd
    , flags :: CurlEventsOnSocket
    }
    deriving (Eq, Show)

data AgentContext = AgentContext
    { multi :: !CurlMulti
    , eventManager :: !EventManager
    , msgQueue :: !(TQueue AgentMessage)
    , socketActionQueue :: !(TQueue SocketActionRequest)
    , requestsRef :: !(IORef (HashMap RequestId RequestHandler))
    , fdMapRef :: !(IORef (HashMap Fd FdState))
    , timerWaker :: !(TMVar ())
    , msgQueueAlive :: !(StablePtr (TQueue AgentMessage))
    }

data AgentHandle = AgentHandle
    { msgQueue :: !(TQueue AgentMessage)
    , agentThreadId :: !(Async ())
    , agent :: !AgentContext
    }

data LoopBreaker = Continue | Stop

deriving instance Hashable CInt
deriving instance Hashable Fd
