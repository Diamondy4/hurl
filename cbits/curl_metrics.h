#pragma once

#include <curl/curl.h>
#include <stdatomic.h>

typedef struct curl_metrics_s {
    atomic_long upload_progress;
    atomic_long upload_total;
    atomic_long download_progress;
    atomic_long download_total;

    atomic_long upload_speed;
    atomic_long download_speed;

    atomic_long namelookup_time;
    atomic_long connect_time;
    atomic_long appconnect_time;
    atomic_long pretransfer_time;
    atomic_long starttransfer_time;
    atomic_long total_time;
    atomic_long redirect_time;
} curl_metrics_t;

typedef struct curl_metrics_context_s {
    CURL *easy;
    curl_metrics_t metrics;
} curl_metrics_context_t;

int metric_xferinfofun_cb(curl_metrics_context_t *curl_metrics_context, curl_off_t dltotal, curl_off_t dlnow,
                          curl_off_t ultotal,
                          curl_off_t ulnow);