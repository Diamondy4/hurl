module Main where

import Agent
import Control.Concurrent hiding (throwTo)
import Request
import Simple
import System.TimeIt
import Types
import UnliftIO.Exception
import Control.Monad.Trans.Resource

performRequestTest :: Agent -> IO ()
performRequestTest agent = do
    let headers = ["Transfer-Encoding: chunked", "Accept: application/json", "Content-Type: application/json", "charset: utf-8"]
        body = Empty
        req =
            Request
                { host = "http://dev-avod-rt.getshop.tv:414/body/"
                , timeoutMS = 0
                , connectionTimeoutMS = 0
                , lowSpeedLimit = LowSpeedLimit{lowSpeed = 1, timeout = 1}
                , body
                , headers = HeaderList headers
                , method = Post
                , extraOptions = []
                }
    !response <- timeIt $ runResourceT $ httpLBS agent req
    print response
    pure ()

data Err = Err deriving (Show, Exception)

main :: IO ()
main = do
    initCurl
    let conf =
            AgentConfig
                { connectionCacheSize = 0
                , maxConnectionPerHost = 8
                , maxConnection = 0
                }
    agent <- Agent.spawnAgent conf
    threadDelay 1_000_000
    print "initialized"
    performRequestTest agent
    -- successCounter <- newCounter 0
    --a1 <- async $ forConcurrently_ [1 .. 1000] \_i -> do
    --    performRequestTest agent
    -- incrCounter_ 1 successCounter
    --a2 <- async $ forConcurrently_ [1 .. 1000] \_i -> do
    --    performRequestTest agent
    --    incrCounter_ 1 successCounter
    --_ <- waitBoth a1 a2
    -- print =<< readCounter successCounter
    -- throwTo reqThread Err
    threadDelay 60_000_000
    print "done"