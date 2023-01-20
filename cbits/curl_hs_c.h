#pragma once

#include <HsFFI.h>
#include <curl/curl.h>

typedef struct curl_slist curl_slist_t;

typedef struct simple_string
{
    char *ptr;
    size_t len;
} simple_string;

void init_simple_string(simple_string *s);

size_t simple_string_writefunc(void *ptr, size_t size, size_t nmemb, simple_string *s);