#include "curl_hs.h"
#include "extras.h"
#include <assert.h>

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

                curl_multi_remove_handle(multi, easy_handle);
                wake_up_waker(&hs_easy_data->waker);

                break;
            default:
                break;
        }
    }
}