#pragma once

#include <curl/curl.h>
#include <HsFFI.h>
#include <stdbool.h>

typedef struct curl_slist curl_slist_t;

typedef struct hs_waker_s {
    HsStablePtr mvar;
    int capability;
    bool waked;
} hs_waker_t;

typedef struct hs_easy_data_s {
    CURLcode curl_code;
    hs_waker_t waker;
} hs_easy_data_t;

size_t ignore_body_writefunc(void *ptr, size_t size, size_t nmemb, void *userp);

void wake_up_waker(hs_waker_t *waker);