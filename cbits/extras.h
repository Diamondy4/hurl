#pragma once

#include <curl/curl.h>
#include <HsFFI.h>

typedef struct curl_slist curl_slist_t;

typedef struct hs_easy_data_s {
    CURLcode curl_code;
    HsStablePtr done_request_mvar;
    int capability;
} hs_easy_data_t;
