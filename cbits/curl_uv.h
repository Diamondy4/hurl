#pragma once

#include <stdlib.h>
#include <uv.h>
#include <curl/curl.h>

typedef struct multi_context_s {
    uv_loop_t *loop;
    CURLM *multi;
    uv_timer_t timer;
} multi_context_t;

typedef struct socket_context_s {
    uv_poll_t poll_handle;
    curl_socket_t socket_fd;
    CURLM *multi;
} socket_context_t;

static socket_context_t *new_socket_context(multi_context_t *multi_context, curl_socket_t socket_fd);

static void destroy_socket_context_cb(uv_handle_t *handle);

static void destroy_socket_context(socket_context_t *context);

static void check_multi_info(CURLM *multi);

static void socket_callback(uv_poll_t *poll, int status, int events);

static void on_timeout(uv_timer_t *timer);

static int curl_timer_function(CURLM *multi, long timeout_ms, multi_context_t *multi_context);


static int curl_socket_function(CURL *easy, curl_socket_t socket_fd, int action,
                                multi_context_t *multi_context,
                                socket_context_t *socket_context_p);

void bind_uv_curl_multi(uv_loop_t *loop, CURLM *multi);