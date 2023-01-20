#pragma once

#include <stddef.h>
#include "curl_hs_c.h"

// TODO: check malloc result
void init_simple_string(simple_string *s)
{
    s->len = 0;
    s->ptr = malloc(s->len + 1);
    s->ptr[0] = '\0';
}

size_t simple_string_writefunc(void *ptr, size_t size, size_t nmemb, simple_string *s)
{
    size_t new_len = s->len + size * nmemb;
    s->ptr = realloc(s->ptr, new_len + 1);

    // fail request when allocation fails
    if (s->ptr == NULL)
    {
        return -1;
    }

    memcpy(s->ptr + s->len, ptr, size * nmemb);
    s->ptr[new_len] = '\0';
    s->len = new_len;

    return size * nmemb;
}