{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Internal.Easy where

import Control.Monad.Trans.Resource
import Data.Foldable
import Foreign
import Foreign.C.Types
import Internal.Metrics
import Internal.Options
import Internal.Raw
import Internal.Raw.Extras
import Internal.Raw.Metrics (CurlMetricsContext (CurlMetricsContext))
import Internal.Raw.SimpleString
import Internal.Slist
import Language.C.Inline qualified as C
import Language.C.Inline.Unsafe qualified as CU
import Request
import Types
import UnliftIO

C.context (C.baseCtx <> C.funCtx <> C.fptrCtx <> C.bsCtx <> localCtx)

C.include "<string.h>"
C.include "<stdlib.h>"

C.include "<curl/curl.h>"
C.include "HsFFI.h"

C.include "simple_string.h"
C.include "extras.h"
C.include "curl_metrics.h"

newEasy :: IO CurlEasy
newEasy = do
    CurlEasy <$> [CU.exp|CURL* { curl_easy_init() }|]

allocateEasy :: MonadResource m => m (ReleaseKey, CurlEasy)
allocateEasy =
    allocate
        newEasy
        \(CurlEasy easyPtr) -> [CU.block|void { curl_easy_cleanup($(CURL* easyPtr)); }|]

setMetrics :: MonadIO m => CurlEasy -> m CurlMetricsContext
setMetrics easy@(CurlEasy easyPtr) = do
    metricsContext@(CurlMetricsContext metricsCtxFptr) <- liftIO $ initCurlMetrics easy
    liftIO
        [CU.block|void {
        CURL* easy = $(CURL* easyPtr);

        curl_easy_setopt(easy, CURLOPT_XFERINFOFUNCTION, metric_xferinfofun_cb);
        curl_easy_setopt(easy, CURLOPT_XFERINFODATA, $fptr-ptr:(curl_metrics_context_t* metricsCtxFptr));
        curl_easy_setopt(easy, CURLOPT_NOPROGRESS, 0L);
    }|]
    pure metricsContext

setEasyData :: MonadIO m => CurlEasy -> m (MVar (), EasyData)
setEasyData (CurlEasy easyPtr) = do
    doneRequest <- newEmptyMVar @_ @()
    easyData@(EasyData easyDataFPtr) <- liftIO $ mkEasyData doneRequest
    liftIO [CU.block|void { curl_easy_setopt($(CURL* easyPtr), CURLOPT_PRIVATE, $fptr-ptr:(hs_easy_data_t* easyDataFPtr)); }|]
    pure (doneRequest, easyData)

setSimpleStringResponse :: MonadIO m => CurlEasy -> m (ForeignPtr SimpleString)
setSimpleStringResponse (CurlEasy easyPtr) = do
    simpleString <- liftIO $ mallocForeignPtr @SimpleString
    liftIO
        [CU.block|void {
        CURL* easy = $(CURL* easyPtr);
        simple_string_t* simple_string = $fptr-ptr:(simple_string_t* simpleString);
        init_simple_string(simple_string);
        curl_easy_setopt(easy, CURLOPT_WRITEDATA, simple_string);
        curl_easy_setopt(easy, CURLOPT_WRITEFUNCTION, simple_string_writefunc);
    }|]
    pure simpleString

setRequestBody :: MonadIO m => Request -> CurlEasy -> m ()
setRequestBody Request{..} (CurlEasy easyPtr) =
    case body of
        Empty ->
            liftIO
                [CU.block|void {
                CURL* easy = $(CURL* easyPtr);
                curl_easy_setopt(easy, CURLOPT_POSTFIELDS, "");
                curl_easy_setopt(easy, CURLOPT_POSTFIELDSIZE, 0L);
            }|]
        Buffer bs ->
            -- Request body is stored in its RequestHandler instance, thus it should outlive the use.
            liftIO
                [CU.block|void {
                CURL* easy = $(CURL* easyPtr);
                curl_easy_setopt(easy, CURLOPT_POSTFIELDS, $bs-ptr:bs);
                curl_easy_setopt(easy, CURLOPT_POSTFIELDSIZE, (long)$bs-len:bs);
            }|]

setUserOptions :: MonadIO m => Foldable t => t SomeOption -> CurlEasy -> m ()
setUserOptions extraOptions (CurlEasy easyPtr) = liftIO $ traverse_ setSomeOption extraOptions
  where
    setSomeOption (SomeOption' (EasyOption' optVal :: EasyOption' opt)) =
        setEasyOption @opt easyPtr optVal

setHTTPMethod :: MonadIO m => HTTPMethod -> CurlEasy -> m ()
setHTTPMethod method (CurlEasy easyPtr) = liftIO case method of
    Get -> [CU.block|void { curl_easy_setopt($(CURL* easyPtr), CURLOPT_HTTPGET, 1L); }|]
    Head -> [CU.block|void { curl_easy_setopt($(CURL* easyPtr), CURLOPT_NOBODY, 1L); }|]
    Post -> [CU.block|void { curl_easy_setopt($(CURL* easyPtr), CURLOPT_POST, 1L); }|]
    _ -> do
        let methodBS = httpMethodToBS method
        [CU.block|void { curl_easy_setopt($(CURL* easyPtr), CURLOPT_CUSTOMREQUEST, $bs-cstr:methodBS); }|]

setDefaultHTTPOptions :: MonadIO m => CurlEasy -> m ()
setDefaultHTTPOptions (CurlEasy easyPtr) =
    liftIO
        [CU.block|void {
        CURL* easy = $(CURL* easyPtr);
        curl_easy_setopt(easy, CURLOPT_HTTP_VERSION, CURL_HTTP_VERSION_2_0);
        curl_easy_setopt(easy, CURLOPT_PIPEWAIT, 1L);
        curl_easy_setopt(easy, CURLOPT_ACCEPT_ENCODING, "");
        curl_easy_setopt(easy, CURLOPT_FOLLOWLOCATION, 1L);
        curl_easy_setopt(easy, CURLOPT_NOSIGNAL, 1L);
    }|]

setRequestOpts :: MonadIO m => Request -> CurlEasy -> m ()
setRequestOpts Request{..} (CurlEasy easyPtr) =
    liftIO
        [CU.block|void {
        CURL* easy = $(CURL* easyPtr);
        curl_easy_setopt(easy, CURLOPT_URL, $bs-cstr:host);
        curl_easy_setopt(easy, CURLOPT_TIMEOUT_MS, $(long timeoutMS'));
        curl_easy_setopt(easy, CURLOPT_CONNECTTIMEOUT_MS, $(long connectionTimeoutMS'));
        curl_easy_setopt(easy, CURLOPT_LOW_SPEED_TIME, $(long lowSpeedTimeout'));
        curl_easy_setopt(easy, CURLOPT_LOW_SPEED_LIMIT, $(long lowSpeedLimit'));
    }|]
  where
    timeoutMS' = fromIntegral timeoutMS
    connectionTimeoutMS' = fromIntegral connectionTimeoutMS
    lowSpeedLimit' = fromIntegral lowSpeedLimit.lowSpeed
    lowSpeedTimeout' = fromIntegral lowSpeedLimit.timeout

setHeaders :: MonadResource m => RequestHeader -> CurlEasy -> m [ReleaseKey]
setHeaders headers (CurlEasy easyPtr) = case headers of
    NoHeaders -> pure mempty
    HeaderList [] -> pure mempty
    HeaderList headers' -> do
        (releaseKeySlist, slistPtr) <- allocateSlist headers'
        liftIO [CU.block|void { curl_easy_setopt($(CURL* easyPtr), CURLOPT_HTTPHEADER, $(curl_slist_t* slistPtr)); }|]
        pure [releaseKeySlist]
    OverrideHeaders (CurlSlist slistFPtr) -> do
        liftIO [CU.block|void { curl_easy_setopt($(CURL* easyPtr), CURLOPT_HTTPHEADER, $fptr-ptr:(curl_slist_t* slistFPtr)); }|]
        pure mempty

initRequest :: MonadResource m => Request -> CurlEasy -> m RequestHandler
initRequest request@Request{..} easy = do
    setDefaultHTTPOptions easy
    setHTTPMethod method easy
    setRequestOpts request easy
    metricsContext <- setMetrics easy
    setRequestBody request easy
    releaseKeySlist <- setHeaders headers easy
    (doneRequest, easyData) <- setEasyData easy
    responseSimpleString <- setSimpleStringResponse easy
    setUserOptions extraOptions easy
    pure
        RequestHandler
            { easy
            , easyData
            , requestBody = body
            , doneRequest
            , responseSimpleString
            , metricsContext
            , resources = releaseKeySlist
            }