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

import Control.Concurrent
import PyFCustom

import Agent (AgentHandle)
import Agent qualified
import Control.Exception
import Control.Monad
import Request
import Simple
import Types

performRequestTest :: AgentHandle -> IO ()
performRequestTest agent = do
    let headers = ["Transfer-Encoding: chunked", "Accept: application/json", "Content-Type: application/json", "charset: utf-8"]
        body = Buffer [cFmt|{ "Its" : "Alive" }|]
        req = Request{host = "https://eoqs3gfexu6bl9.m.pipedream.net", timeoutMS = 0, body, headers = headers, method = Post}
    response <- performRequestBS agent req
    print response

data Err = Err deriving (Show)
instance Exception Err

main :: IO ()
main = do
    initCurl
    agent <- Agent.spawnAgent
    print "initialized"
    --reqThread <- forkIO $ performRequestTest agent
    void . forkIO $ performRequestTest agent
    void . forkIO $ performRequestTest agent
    void . forkIO $ performRequestTest agent
    void . forkIO $ performRequestTest agent
    --void . forkIO $ performRequestTest agent
    threadDelay 100_000
    --throwTo reqThread Err
    threadDelay 60_000_000
    print "done"