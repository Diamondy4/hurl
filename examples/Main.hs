module Main where

import Agent
import Control.Monad.Trans.Resource (runResourceT)
import Request
import Simple
import System.IO
import Types

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    initCurl
    let conf =
            AgentConfig
                { connectionCacheSize = 5000
                , maxConnectionPerHost = 100
                , maxConnection = 1000
                }
    agent <- Agent.spawnAgent conf

    resp <- runResourceT $ httpLBS agent hurlGetRequest

    print resp

hurlGetRequest :: Request
hurlGetRequest =
    Request
        { host = "https://common-cdn.getshop.tv/vitrina-ads/2023-11-testSamolet/vast-test.xml"
        , timeoutMS = 0
        , connectionTimeoutMS = 400
        , lowSpeedLimit = LowSpeedLimit{timeout = 1, lowSpeed = 1}
        , Request.body = Empty
        , Request.headers = NoHeaders
        , extraOptions = []
        , method = Get
        }
