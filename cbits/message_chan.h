#pragma once

#include "include/waitfree-mpsc-queue/mpscq.h"
#include <uv.h>
#include <curl/curl.h>

typedef struct mpscq mpsc_t;

enum outer_message_types {
    EXECUTE, CANCEL_REQUEST
};

typedef struct outer_message_s {
    enum outer_message_types tag;
    CURL *easy;
} outer_message_t;

typedef struct async_messages_context_s {
    CURL *multi;
    mpsc_t *chan;
} async_messages_context_t;

static void async_check_outer_messages(uv_async_t *async_handle);

uv_async_t *init_async_check_messages(uv_loop_t *loop, mpsc_t* queue, CURLM *multi);