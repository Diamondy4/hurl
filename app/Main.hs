{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Main where
import Agent
import PyFCustom
import Request
import Types
import Simple
import Control.Concurrent

performRequestTest :: AgentHandle -> IO ()
performRequestTest agent = do
    let headers = ["Transfer-Encoding: chunked", "Accept: application/json", "Content-Type: application/json", "charset: utf-8"]
        body = Buffer [cFmt|{ "Its" : "Alive" }|]
        req = Request{host = "https://example.com", timeoutMS = 0, body, headers = headers, method = Post}
    !response <- {- timeIt $ -} httpLBS agent req
    print response
    pure ()


main :: IO ()
main = do
    initCurl
    agent <- Agent.spawnAgent
    print "initialized"
    --performRequestTest agent
    _reqThread <- forkIO $ performRequestTest agent
    --_reqThread2 <- forkIO $ performRequestTest agent
    {-     successCounter <- newCounter 0
        a1 <- async $ forConcurrently_ [1 .. 100] \_i -> do
            performRequestTest agent
            incrCounter_ 1 successCounter
        a2 <- async $ forConcurrently_ [1 .. 100] \_i -> do
            performRequestTest agent
            incrCounter_ 1 successCounter
        _ <- waitBoth a1 a2
        print =<< readCounter successCounter -}
    -- throwTo reqThread Err
    threadDelay 60_000_000
    print "done"