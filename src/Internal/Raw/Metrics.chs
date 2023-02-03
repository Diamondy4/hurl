module Internal.Raw.Metrics where

#include "curl_metrics.h"

{# pointer *curl_metrics_context_t as CurlMetricsContext foreign newtype #}

curlMetricsSize :: Int
curlMetricsSize = {#sizeof curl_metrics_context_t#}