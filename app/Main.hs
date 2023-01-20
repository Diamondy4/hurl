{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Main where

import PyFCustom

import Agent (AgentHandle)
import Agent qualified
import Data.Atomics.Counter
import Request  
import Simple
import System.TimeIt
import Types
import UnliftIO
import UnliftIO.Concurrent (threadDelay)

performRequestTest :: AgentHandle -> IO ()
performRequestTest agent = do
    let headers = ["Transfer-Encoding: chunked", "Accept: application/json", "Content-Type: application/json", "charset: utf-8"]
        body = Buffer [cFmt|{ "Its" : "Alive" }|]
        req = Request{host = "https://example.com", timeoutMS = 400, body, headers = headers, method = Post}
    !response <- timeIt $ httpLBS agent req
    print response
    pure ()

data Err = Err deriving (Show)
instance Exception Err

main :: IO ()
main = do
    initCurl
    agent <- Agent.spawnAgent
    print "initialized"
    -- reqThread <- forkIO $ performRequestTest agent
    successCounter <- newCounter 0
    a1 <- async $ forConcurrently_ [1 .. 10] \_i -> do
        performRequestTest agent
        incrCounter_ 1 successCounter
    a2 <- async $ forConcurrently_ [1 .. 1] \_i -> do
        performRequestTest agent
        incrCounter_ 1 successCounter
    _ <- waitBoth a1 a2
    print =<< readCounter successCounter
    -- throwTo reqThread Err
    threadDelay 60_000_000
    print "done"