#include "message_chan.h"
#include <uv.h>
#include <curl/curl.h>
#include <stdlib.h>
#include <assert.h>
#include "extras.h"

void async_check_outer_messages(uv_async_t *async_handle) {
    async_messages_context_t *context = async_handle->data;

    assert(context);
    assert(context->multi);
    assert(context->chan);

    outer_message_t *message = NULL;

    while ((message = mpscq_dequeue(context->chan))) {
        assert(message);
        switch (message->tag) {
            case EXECUTE:
                assert(message->execute_payload.easy);
                curl_multi_add_handle(context->multi, message->execute_payload.easy);
                break;
            case CANCEL_REQUEST:
                assert(message->cancel_payload.easy);
                assert(message->cancel_payload.waker.mvar);
                curl_multi_remove_handle(context->multi, message->cancel_payload.easy);
                wake_up_waker(&message->cancel_payload.waker);
                break;
        }
        free(message);
    }
}

uv_async_t *init_async_check_messages(uv_loop_t *loop, mpsc_t *queue, CURLM *multi) {
    uv_async_t *uv_async = malloc(sizeof(uv_async_t));
    async_messages_context_t *async_messages_context = malloc(sizeof(async_messages_context_t));
    async_messages_context->multi = multi;
    async_messages_context->chan = queue;
    uv_async->data = async_messages_context;
    uv_async_init(loop, uv_async, async_check_outer_messages);
    return uv_async;
}