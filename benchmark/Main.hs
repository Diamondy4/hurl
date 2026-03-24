{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import Agent
import Control.DeepSeq
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource (MonadResource, MonadUnliftIO, runResourceT)
import Criterion.Main as Criterion
import Criterion.Types (Config (verbosity), Verbosity (..))
import Data.Atomics.Counter
import Data.ByteString
import Data.ByteString.Lazy qualified as LBS
import Internal.Raw
import Network.HTTP.Client (Response (..))
import Network.HTTP.Client qualified as HTTPClient
import Network.HTTP.Simple qualified as HTTPSimple
import PyF
import Request
import Response qualified as Hurl
import Simple
import Types
import UnliftIO.Async
import UnliftIO.Exception (try)

instance NFData HTTPClient.HttpException where
    rnf !ex = case ex of
        HTTPClient.HttpExceptionRequest _ !_ -> ()
        HTTPClient.InvalidUrlException _ _ -> ()

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
    hurlCounterSuccess <- newCounter 0
    hurlCounterFailure <- newCounter 0
    httpCounterSuccess <- newCounter 0
    httpCounterFailure <- newCounter 0
    let criterionConfig =
            Criterion.defaultConfig
                { verbosity = Verbose
                }
    defaultMainWith
        criterionConfig
        [ bgroup
            "hurl"
            [ -- bench "1" $ nfIO (makeGetRequestHurl hurlCounterSuccess hurlCounterFailure agent)
              -- , bench "10" $ nfIO (makeMultpleParallel 10 (makeGetRequestHurl hurlCounterSuccess hurlCounterFailure agent))
              -- , bench "100" $ nfIO (makeMultpleParallel 100 (makeGetRequestHurl hurlCounterSuccess hurlCounterFailure agent))
              -- , bench "1000" $ nfIO (makeMultpleParallel 1000 (makeGetRequestHurl hurlCounterSuccess hurlCounterFailure agent))
              bench "5000" $ nfIO $ runResourceT $ makeMultpleParallel 5000 (makeGetRequestHurl hurlCounterSuccess hurlCounterFailure agent)
            ]
        , bgroup
            "http-client"
            []
        ]
    -- bench "1" $ nfIO (makeGetRequestHTTPClient httpCounterSuccess httpCounterFailure)
    -- , bench "10" $ nfIO (makeMultpleParallel 10 (makeGetRequestHTTPClient httpCounterSuccess httpCounterFailure))
    -- , bench "100" $ nfIO (makeMultpleParallel 100 (makeGetRequestHTTPClient httpCounterSuccess httpCounterFailure))
    -- , bench "1000" $ nfIO (makeMultpleParallel 1000 (makeGetRequestHTTPClient httpCounter))
    -- , bench "5000" $ nfIO (makeMultpleParallel 10000 (makeGetRequestHTTPClient httpCounter))

    hurlCounterSuccessVal <- readCounter hurlCounterSuccess
    hurlCounterFailureVal <- readCounter hurlCounterFailure
    httpCounterSuccessVal <- readCounter httpCounterSuccess
    httpCounterFailureVal <- readCounter httpCounterFailure
    print
        [fmt|hurl counter - success {hurlCounterSuccessVal} - failed  {hurlCounterFailureVal}
    http counter - {httpCounterSuccessVal} - failed  {httpCounterFailureVal}|]

makeMultpleParallel :: (MonadUnliftIO m) => Int -> m a -> m [a]
makeMultpleParallel n action =
    forConcurrently [1 .. n] $ const action

makeGetRequestHurl :: (MonadResource m, MonadUnliftIO m) => AtomicCounter -> AtomicCounter -> Agent -> m (Either CurlCode (Hurl.Response LBS.ByteString))
makeGetRequestHurl sCounter fCounter agent = do
    !res <- httpLBS agent hurlGetRequest
    case res of
        Left _ -> liftIO $ incrCounter_ 1 fCounter
        Right _ -> liftIO $ incrCounter_ 1 sCounter
    pure $! res

hurlGetRequest :: Request
hurlGetRequest =
    Request
        { host = "https://common-cdn.getshop.tv/vitrina-ads/2023-11-testSamolet/vast-test.xml"
        , timeoutMS = 0
        , connectionTimeoutMS = 400
        , lowSpeedLimit = LowSpeedLimit{timeout = 1, lowSpeed = 1}
        , Request.body = Empty
        , Request.headers = NoHeaders
        , method = Get
        , extraOptions = []
        }

makeGetRequestHTTPClient :: AtomicCounter -> AtomicCounter -> IO (Either HTTPSimple.HttpException ByteString)
makeGetRequestHTTPClient sCounter fCounter = do
    !res <- try @_ @HTTPSimple.HttpException $! HTTPSimple.httpBS httpClientGetRequest
    case res of
        Left _ -> incrCounter_ 1 fCounter
        Right _ -> incrCounter_ 1 sCounter
    pure $! (\(!x) -> x.responseBody) <$> res

httpClientGetRequest :: HTTPSimple.Request
httpClientGetRequest =
    req'
        { HTTPClient.responseTimeout = HTTPClient.responseTimeoutMicro 400000
        }
  where
    req' = HTTPSimple.parseRequest_ "https://example.com/"
