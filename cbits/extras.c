#include <HsFFI.h>
#include <stdbool.h>
#include <stddef.h>
#include "extras.h"

void wake_up_waker(hs_waker_t *waker) {
    if (!waker->waked) {
        hs_try_putmvar(waker->capability, waker->mvar);
        waker->waked = true;
    }
}

size_t ignore_body_writefunc(void *ptr, size_t size, size_t nmemb, void *userp) {
    (void) ptr;
    (void) userp;
    return size * nmemb;
}
