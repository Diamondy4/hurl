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

typedef void hs_curl_timerdata;

enum HttpRequestType
{
    GET,
    POST
};

typedef enum body_type
{
    EMPTY,
    WRITER_FUNPTR
} body_type;

typedef struct body_data
{
    body_type body_type;
    curl_write_callback cb;
} body_data;