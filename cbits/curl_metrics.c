#include <curl/curl.h>
#include "curl_metrics.h"

int metric_xferinfofun_cb(curl_metrics_context_t *curl_metrics_context, curl_off_t dltotal, curl_off_t dlnow,
                          curl_off_t ultotal, curl_off_t ulnow) {
    CURL *easy = curl_metrics_context->easy;

    curl_metrics_context->metrics.upload_progress = ulnow;
    curl_metrics_context->metrics.upload_total = ultotal;
    curl_metrics_context->metrics.download_progress = dlnow;
    curl_metrics_context->metrics.download_total = dltotal;

    long upload_speed_temp = 0;
    curl_easy_getinfo(easy, CURLINFO_SPEED_UPLOAD_T, &upload_speed_temp);
    curl_metrics_context->metrics.upload_speed = upload_speed_temp;

    long download_speed_temp = 0;
    curl_easy_getinfo(easy, CURLINFO_SPEED_DOWNLOAD_T, &download_speed_temp);
    curl_metrics_context->metrics.download_speed = download_speed_temp;

    long namelookup_time_temp = 0;
    curl_easy_getinfo(easy, CURLINFO_NAMELOOKUP_TIME_T, &namelookup_time_temp);
    curl_metrics_context->metrics.namelookup_time = namelookup_time_temp;

    long connect_time_temp = 0;
    curl_easy_getinfo(easy, CURLINFO_CONNECT_TIME_T, &connect_time_temp);
    curl_metrics_context->metrics.connect_time = connect_time_temp;

    long appconnect_time_temp = 0;
    curl_easy_getinfo(easy, CURLINFO_APPCONNECT_TIME_T, &appconnect_time_temp);
    curl_metrics_context->metrics.appconnect_time = appconnect_time_temp;

    long pretransfer_time_temp = 0;
    curl_easy_getinfo(easy, CURLINFO_PRETRANSFER_TIME_T, &pretransfer_time_temp);
    curl_metrics_context->metrics.pretransfer_time = pretransfer_time_temp;

    long starttransfer_time_temp = 0;
    curl_easy_getinfo(easy, CURLINFO_STARTTRANSFER_TIME_T, &starttransfer_time_temp);
    curl_metrics_context->metrics.starttransfer_time = starttransfer_time_temp;

    long total_time_temp = 0;
    curl_easy_getinfo(easy, CURLINFO_TOTAL_TIME_T, &total_time_temp);
    curl_metrics_context->metrics.total_time = total_time_temp;

    long redirect_time_temp = 0;
    curl_easy_getinfo(easy, CURLINFO_REDIRECT_TIME_T, &redirect_time_temp);
    curl_metrics_context->metrics.redirect_time = redirect_time_temp;

    return 0;
}
