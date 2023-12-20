{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Internal.Metrics where

import Control.DeepSeq (NFData)
import Control.Monad.Cont
import Foreign
import GHC.Generics
import Internal.Raw
import Internal.Raw.Metrics
import Language.C.Inline qualified as C
import Language.C.Inline.Unsafe qualified as CU

C.context (C.baseCtx <> C.funCtx <> C.fptrCtx <> C.bsCtx <> localCtx)

C.include "curl_metrics.h"

data Metrics = Metrics
    { uploadProgress :: !Int
    , uploadTotal :: !Int
    , downloadProgress :: !Int
    , downloadTotal :: !Int
    , uploadSpeed :: !Int
    , downloadSpeed :: !Int
    , namelookupTime :: !Int
    , connectTime :: !Int
    , appconnectTime :: !Int
    , pretransferTime :: !Int
    , starttransferTime :: !Int
    , totalTime :: !Int
    , redirectTime :: !Int
    }
    deriving (Show, Eq, Generic)
    deriving anyclass (NFData)

initCurlMetrics :: CurlEasy -> IO CurlMetricsContext
initCurlMetrics (CurlEasy easyPtr) = do
    curlMetricsCtxFPtr <- mallocForeignPtrBytes curlMetricsSize
    withForeignPtr curlMetricsCtxFPtr \curlMetricsCtxPtr ->
        [CU.block|void {
            curl_metrics_context_t* metrics_context = $(curl_metrics_context_t* curlMetricsCtxPtr);
            metrics_context->easy = $(CURL* easyPtr);
            
            metrics_context->metrics.upload_progress = 0;
            metrics_context->metrics.upload_total = 0;
            metrics_context->metrics.download_progress = 0;
            metrics_context->metrics.download_total = 0;
            metrics_context->metrics.upload_speed = 0;
            metrics_context->metrics.download_speed = 0;
            metrics_context->metrics.namelookup_time = 0;
            metrics_context->metrics.connect_time = 0;
            metrics_context->metrics.appconnect_time = 0;
            metrics_context->metrics.pretransfer_time = 0;
            metrics_context->metrics.starttransfer_time = 0;
            metrics_context->metrics.total_time = 0;
            metrics_context->metrics.redirect_time = 0;
        }|]
    pure $ CurlMetricsContext curlMetricsCtxFPtr

extractMetrics :: CurlMetricsContext -> IO Metrics
extractMetrics metricsCtx = runContT (extractMetricsCont metricsCtx) pure

-- We must manually extract values to temp vars and only then get real values because of atomics.
-- It's undefined behavior to pass atomic ptr as non-atomics ptr.
-- TODO: Allocate an array to fill instead if many vars.
extractMetricsCont :: CurlMetricsContext -> ContT a IO Metrics
extractMetricsCont metricsCtx = do
    uploadProgressPtr <- ContT alloca
    uploadTotalPtr <- ContT alloca
    downloadProgressPtr <- ContT alloca
    downloadTotalPtr <- ContT alloca
    uploadSpeedPtr <- ContT alloca
    downloadSpeedPtr <- ContT alloca
    namelookupTimePtr <- ContT alloca
    connectTimePtr <- ContT alloca
    appconnectTimePtr <- ContT alloca
    pretransferTimePtr <- ContT alloca
    starttransferTimePtr <- ContT alloca
    totalTimePtr <- ContT alloca
    redirectTimePtr <- ContT alloca
    metricsCtxPtr <- ContT $ withCurlMetricsContext metricsCtx
    lift
        [CU.block|void {
        *$(long* uploadProgressPtr) = $(curl_metrics_context_t* metricsCtxPtr)->metrics.upload_progress;
        *$(long* uploadTotalPtr) = $(curl_metrics_context_t* metricsCtxPtr)->metrics.upload_total;
        *$(long* downloadProgressPtr) = $(curl_metrics_context_t* metricsCtxPtr)->metrics.download_progress;
        *$(long* downloadTotalPtr) = $(curl_metrics_context_t* metricsCtxPtr)->metrics.download_total;
        *$(long* uploadSpeedPtr) = $(curl_metrics_context_t* metricsCtxPtr)->metrics.upload_speed;
        *$(long* downloadSpeedPtr) = $(curl_metrics_context_t* metricsCtxPtr)->metrics.download_speed;
        *$(long* namelookupTimePtr) = $(curl_metrics_context_t* metricsCtxPtr)->metrics.namelookup_time;
        *$(long* connectTimePtr) = $(curl_metrics_context_t* metricsCtxPtr)->metrics.connect_time;
        *$(long* appconnectTimePtr) = $(curl_metrics_context_t* metricsCtxPtr)->metrics.appconnect_time;
        *$(long* pretransferTimePtr) = $(curl_metrics_context_t* metricsCtxPtr)->metrics.pretransfer_time;
        *$(long* starttransferTimePtr) = $(curl_metrics_context_t* metricsCtxPtr)->metrics.starttransfer_time;
        *$(long* totalTimePtr) = $(curl_metrics_context_t* metricsCtxPtr)->metrics.total_time;
        *$(long* redirectTimePtr) = $(curl_metrics_context_t* metricsCtxPtr)->metrics.redirect_time;
    }|]
    uploadProgress <- liftIO $ fromIntegral <$> peek uploadProgressPtr
    uploadTotal <- liftIO $ fromIntegral <$> peek uploadTotalPtr
    downloadProgress <- liftIO $ fromIntegral <$> peek downloadProgressPtr
    downloadTotal <- liftIO $ fromIntegral <$> peek downloadTotalPtr
    uploadSpeed <- liftIO $ fromIntegral <$> peek uploadSpeedPtr
    downloadSpeed <- liftIO $ fromIntegral <$> peek downloadSpeedPtr
    namelookupTime <- liftIO $ fromIntegral <$> peek namelookupTimePtr
    connectTime <- liftIO $ fromIntegral <$> peek connectTimePtr
    appconnectTime <- liftIO $ fromIntegral <$> peek appconnectTimePtr
    pretransferTime <- liftIO $ fromIntegral <$> peek pretransferTimePtr
    starttransferTime <- liftIO $ fromIntegral <$> peek starttransferTimePtr
    totalTime <- liftIO $ fromIntegral <$> peek totalTimePtr
    redirectTime <- liftIO $ fromIntegral <$> peek redirectTimePtr
    pure $ Metrics{..}
