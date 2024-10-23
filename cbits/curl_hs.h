#pragma once

#include "extras.h"
#include <curl/curl.h>
#include <stdlib.h>

void check_multi_info (CURLM *multi);

int hsSocketFunctionCallback (CURL *easy,      /* easy handle */
                              curl_socket_t s, /* socket */
                              int what,        /* describes the socket */
                              void *clientp,   /* private callback pointer */
                              void *socketp);  /* private socket pointer */

int hsTimerFunctionCallback (CURLM *multi,    /* multi handle */
                             long timeout_ms, /* timeout in number of ms */
                             void *clientp);  /* private callback pointer */