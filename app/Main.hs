module Main where

import Agent
import Control.Concurrent hiding (throwTo)
import Request
import Simple
import System.TimeIt
import Types
import UnliftIO.Async
import UnliftIO.Exception

performRequestTest :: AgentHandle -> IO ()
performRequestTest agent = do
    let headers = ["Transfer-Encoding: chunked", "Accept: application/json", "Content-Type: application/json", "charset: utf-8"]
        body = Empty
        req = Request{host = "https://example.com", timeoutMS = 0, connectionTimeoutMS = 400, body, headers = headers, method = Post}
    !_response <- timeIt $ httpLBS agent req
    -- print response
    pure ()

data Err = Err deriving (Show, Exception)

main :: IO ()
main = do
    initCurl
    let conf =
            AgentConfig
                { connectionCacheSize = 5000
                , maxConnectionPerHost = 100
                , maxConnection = 1000
                }
    agent <- Agent.spawnAgent conf
    threadDelay 1_000_000
    print "initialized"
    -- successCounter <- newCounter 0
    a1 <- async $ forConcurrently_ [1 .. 1000] \_i -> do
        performRequestTest agent
    -- incrCounter_ 1 successCounter
    a2 <- async $ forConcurrently_ [1 .. 1000] \_i -> do
        performRequestTest agent
    --    incrCounter_ 1 successCounter
    _ <- waitBoth a1 a2
    -- print =<< readCounter successCounter
    -- throwTo reqThread Err
    threadDelay 60_000_000
    print "done"