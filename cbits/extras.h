#pragma once

#include <HsFFI.h>
#include <curl/curl.h>
#include <stdbool.h>

typedef struct curl_slist curl_slist_t;

typedef struct hs_waker_s
{
  HsStablePtr mvar;
  int capability;
  bool waked;
} hs_waker_t;

typedef struct socket_context_s
{
  hs_waker_t waker;
  curl_socket_t socket_fd;
  int socket_events;
} socket_context_t;

typedef struct hs_easy_data_s
{
  CURLcode curl_code;
  hs_waker_t waker;
  socket_context_t socket_context;
} hs_easy_data_t;

typedef struct timer_context_s
{
  hs_waker_t waker;
  long timeout_ms;
} timer_context_t;

typedef struct multi_context_s
{
  CURLM *multi;
  timer_context_t timer_context;
} multi_context_t;

size_t ignore_body_writefunc (void *ptr, size_t size, size_t nmemb,
                              void *userp);

void wake_up_waker (hs_waker_t *waker);
