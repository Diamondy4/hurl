#pragma once

#include <HsFFI.h>
#include <curl/curl.h>

typedef struct curl_slist curl_slist_t;

typedef struct hs_curl_response_data
{
    int status_code;
} hs_curl_response_data;

typedef struct hs_curl_easy_data
{
    HsStablePtr hs_mutex;
    hs_curl_response_data *response_data;
} hs_curl_easy_data;

typedef struct hs_curl_socketdata
{
    HsStablePtr hs_eventmanager;
    CURLM *multi;
} hs_curl_socketdata;

enum HttpRequestType
{
    GET,
    POST
};

// Global curl multi handle. Should be non-global?
CURLM *curl_multi_g;

CURL *curl_easy_test;

int my_test()
{
    int http_code = 0;
    curl_easy_getinfo(curl_easy_test, CURLINFO_RESPONSE_CODE, &http_code);
    return http_code;
}