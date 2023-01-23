#include "curl_uv.h"
#include "extras.h"
#include <assert.h>

socket_context_t *new_socket_context(socket_fun_context_t *socket_fun_context, curl_socket_t socket_fd) {
    socket_context_t *socket_context = malloc(sizeof(socket_context_t));

    socket_context->multi = socket_fun_context->multi;
    socket_context->socket_fd = socket_fd;

    uv_poll_init_socket(socket_fun_context->loop, &socket_context->poll_handle, socket_fd);
    socket_context->poll_handle.data = socket_context;

    assert(socket_fun_context);

    return socket_context;
}

void destroy_socket_context_cb(uv_handle_t *handle) {
    assert(handle);
    printf("destroy socket context cb\n");
    socket_context_t *context = handle->data;
    assert(context);
    free(context);
}

void destroy_socket_context(socket_context_t *context) {
    assert(context);
    assert(&context->poll_handle);
    printf("destroy socket context\n");
    uv_close((uv_handle_t *) &context->poll_handle, destroy_socket_context_cb);
}

void check_multi_info(CURLM *multi) {
    assert(multi);
    printf("check multi\n");
    CURLMsg *message = NULL;
    int pending = 0;
    CURL *easy_handle = NULL;

    while ((message = curl_multi_info_read(multi, &pending))) {
        printf("anything\n");
        switch (message->msg) {
            case CURLMSG_DONE:
                printf("multi msg done\n");
                easy_handle = message->easy_handle;

                hs_easy_data_t *hs_easy_data = NULL;
                curl_easy_getinfo(easy_handle, CURLINFO_PRIVATE, &hs_easy_data);

                hs_easy_data->curl_code = message->data.result;
                hs_try_putmvar(hs_easy_data->capability, hs_easy_data->done_request_mvar);

                printf("removing multi handle\n");
                curl_multi_remove_handle(multi, easy_handle);

                break;
            default:
                break;
        }
    }
}

void socket_callback(uv_poll_t *poll, int status, int events) {
    assert(poll);
    printf("socket_callback\n");
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
    assert(socket_context);
    assert(socket_context->multi);

    curl_multi_socket_action(socket_context->multi, socket_context->socket_fd, flags,
                             &running_handles);

    check_multi_info(socket_context->multi);
}

void on_timeout(uv_timer_t *timer) {
    assert(timer);
    printf("timer cb\n");
    CURLM *multi = timer->data;
    assert(multi);
    int running_handles = 0;
    curl_multi_socket_action(multi, CURL_SOCKET_TIMEOUT, 0, &running_handles);
    check_multi_info(multi);
}

int curl_timer_function(CURLM *multi, long timeout_ms, timer_fun_context_t *timer_fun_context) {
    assert(multi);
    assert(timer_fun_context);
    assert(&timer_fun_context->timer);
    printf("timer function\n");
    (void) multi;
    if (timeout_ms < 0) {
        uv_timer_stop(&timer_fun_context->timer);
    } else {
        if(timeout_ms == 0){
            timeout_ms = 1;
        }
        uv_timer_start(&timer_fun_context->timer, on_timeout, timeout_ms, 0);
    }
    return 0;
}

int curl_socket_function(CURL *easy, curl_socket_t socket_fd, int action, socket_fun_context_t *socket_fun_context,
                         socket_context_t *socket_context_p) {
    assert(socket_fun_context);
    char *url = NULL;
    curl_easy_getinfo(easy, CURLINFO_EFFECTIVE_URL, &url);
    (void) easy;
    socket_context_t *socket_context = NULL;
    int events = 0;

    switch (action) {
        case CURL_POLL_IN:
        case CURL_POLL_OUT:
        case CURL_POLL_INOUT:
            printf("curl_socket_function inout - %s\n", url);
            socket_context =
                    socket_context_p ?
                    socket_context_p :
                    new_socket_context(socket_fun_context, socket_fd);

            assert(socket_fun_context->multi);

            curl_multi_assign(socket_fun_context->multi, socket_fd, socket_context);

            if (action != CURL_POLL_IN) {
                events |= UV_WRITABLE;
            }
            if (action != CURL_POLL_OUT) {
                events |= UV_READABLE;
            }

            assert(socket_context);
            assert(&socket_context->poll_handle);
            uv_poll_start(&socket_context->poll_handle, events, socket_callback);
            break;
        case CURL_POLL_REMOVE:
            printf("curl_socket_function remove - %s\n", url);
            if (socket_context_p) {
                assert(&socket_context_p->poll_handle);
                uv_poll_stop(&socket_context_p->poll_handle);
                destroy_socket_context(socket_context_p);
                assert(socket_fun_context->multi);
                curl_multi_assign(socket_fun_context->multi, socket_fd, NULL);
            }
            break;
        default:
            abort();
    }

    return 0;
}

void bind_uv_curl_multi(uv_loop_t *loop, CURLM *multi) {
    printf("binding uv to multi\n");
    timer_fun_context_t *timer_fun_context = malloc(sizeof(timer_fun_context_t));
    uv_timer_init(loop, &timer_fun_context->timer);
    timer_fun_context->timer.data = multi;

    socket_fun_context_t *socket_fun_context = malloc(sizeof(socket_fun_context_t));
    socket_fun_context->multi = multi;
    socket_fun_context->loop = loop;

    curl_multi_setopt(multi, CURLMOPT_SOCKETFUNCTION, curl_socket_function);
    curl_multi_setopt(multi, CURLMOPT_SOCKETDATA, socket_fun_context);

    curl_multi_setopt(multi, CURLMOPT_TIMERFUNCTION, curl_timer_function);
    curl_multi_setopt(multi, CURLMOPT_TIMERDATA, timer_fun_context);
    printf("binded uv to multi\n");
}
