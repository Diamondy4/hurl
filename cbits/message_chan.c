#include "message_chan.h"
#include <uv.h>
#include <curl/curl.h>
#include <stdlib.h>
#include "extras.h"
#include <HsFFI.h>

void async_check_outer_messages(uv_async_t *async_handle) {
    async_messages_context_t *context = async_handle->data;

    outer_message_t *message = NULL;

    while ((message = mpscq_dequeue(context->chan))) {
        switch (message->tag) {
            case EXECUTE:
                curl_multi_add_handle(context->multi, message->easy);
                break;
            case CANCEL_REQUEST:
                curl_multi_remove_handle(context->multi, message->easy);
                hs_easy_data_t easy_data;
                curl_easy_getinfo(message->easy, CURLINFO_PRIVATE, &easy_data);
                if (!easy_data.done_request){
                    easy_data.done_request = true;
                    hs_try_putmvar(easy_data.capability, easy_data.done_request_mvar);
                }
                break;
        }
        free(message);
    }
}

uv_async_t *init_async_check_messages(uv_loop_t *loop, mpsc_t* queue, CURLM *multi) {
    uv_async_t *uv_async = malloc(sizeof(uv_async_t));
    async_messages_context_t *async_messages_context = malloc(sizeof(async_messages_context_t));
    async_messages_context->multi = multi;
    async_messages_context->chan = queue;
    uv_async->data = async_messages_context;
    uv_async_init(loop, uv_async, async_check_outer_messages);
    return uv_async;
}