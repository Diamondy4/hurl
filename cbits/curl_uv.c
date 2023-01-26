#include "curl_uv.h"
#include "extras.h"

socket_context_t *new_socket_context(multi_context_t *multi_context, curl_socket_t socket_fd) {
    socket_context_t *socket_context = malloc(sizeof(socket_context_t));

    socket_context->multi = multi_context->multi;
    socket_context->socket_fd = socket_fd;

    uv_poll_init_socket(multi_context->loop, &socket_context->poll_handle, socket_fd);
    socket_context->poll_handle.data = socket_context;


    return socket_context;
}

void destroy_socket_context_cb(uv_handle_t *handle) {
    socket_context_t *context = handle->data;
    free(context);
}

void destroy_socket_context(socket_context_t *context) {
    uv_close((uv_handle_t *) &context->poll_handle, destroy_socket_context_cb);
}

void check_multi_info(CURLM *multi) {
    CURLMsg *message = NULL;
    int pending = 0;
    CURL *easy_handle = NULL;

    while ((message = curl_multi_info_read(multi, &pending))) {
        switch (message->msg) {
            case CURLMSG_DONE:
                easy_handle = message->easy_handle;

                hs_easy_data_t *hs_easy_data = NULL;
                curl_easy_getinfo(easy_handle, CURLINFO_PRIVATE, &hs_easy_data);

                hs_easy_data->curl_code = message->data.result;
                hs_easy_data->done_request = true;
                hs_try_putmvar(hs_easy_data->capability, hs_easy_data->done_request_mvar);

                curl_multi_remove_handle(multi, easy_handle);

                break;
            default:
                break;
        }
    }
}

void socket_callback(uv_poll_t *poll, int status, int events) {
    (void) status;
    int running_handles = 0;
    int flags = 0;

    if (events & UV_READABLE) {
        flags |= CURL_CSELECT_IN;
    }
    if (events & UV_WRITABLE) {
        flags |= CURL_CSELECT_OUT;
    }

    socket_context_t *socket_context = poll->data;

    curl_multi_socket_action(socket_context->multi, socket_context->socket_fd, flags,
                             &running_handles);
    check_multi_info(socket_context->multi);
}

void on_timeout(uv_timer_t *timer) {
    CURLM *multi = timer->data;
    int running_handles = 0;
    curl_multi_socket_action(multi, CURL_SOCKET_TIMEOUT, 0, &running_handles);
    check_multi_info(multi);
}

int curl_timer_function(CURLM *multi, long timeout_ms, multi_context_t *multi_context) {
    (void) multi;
    if (timeout_ms < 0) {
        uv_timer_stop(&multi_context->timer);
    } else {
        if(timeout_ms == 0){
            timeout_ms = 1;
        }
        uv_timer_start(&multi_context->timer, on_timeout, timeout_ms, 0);
    }
    return 0;
}

int curl_socket_function(CURL *easy, curl_socket_t socket_fd, int action, multi_context_t *multi_context,
                         socket_context_t *socket_context_p) {
    (void) easy;
    socket_context_t *socket_context = NULL;
    int events = 0;

    switch (action) {
        case CURL_POLL_IN:
        case CURL_POLL_OUT:
        case CURL_POLL_INOUT:
            socket_context =
                    socket_context_p ?
                    socket_context_p :
                    new_socket_context(multi_context, socket_fd);

            curl_multi_assign(multi_context->multi, socket_fd, socket_context);

            if (action != CURL_POLL_IN) {
                events |= UV_WRITABLE;
            }
            if (action != CURL_POLL_OUT) {
                events |= UV_READABLE;
            }

            uv_poll_start(&socket_context->poll_handle, events, socket_callback);
            break;
        case CURL_POLL_REMOVE:
            if (socket_context_p) {
                uv_poll_stop(&socket_context_p->poll_handle);
                destroy_socket_context(socket_context_p);
                curl_multi_assign(multi_context->multi, socket_fd, NULL);
            }
            break;
        default:
            abort();
    }

    return 0;
}

void bind_uv_curl_multi(uv_loop_t *loop, CURLM *multi) {
    multi_context_t *multi_context = malloc(sizeof(multi_context_t));
    
    multi_context->multi = multi;
    multi_context->loop = loop;
    
    uv_timer_init(loop, &multi_context->timer);
    multi_context->timer.data = multi;

    curl_multi_setopt(multi, CURLMOPT_SOCKETFUNCTION, curl_socket_function);
    curl_multi_setopt(multi, CURLMOPT_SOCKETDATA, multi_context);

    curl_multi_setopt(multi, CURLMOPT_TIMERFUNCTION, curl_timer_function);
    curl_multi_setopt(multi, CURLMOPT_TIMERDATA, multi_context);
}
