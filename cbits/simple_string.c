#include <stdlib.h>
#include <string.h>
#include "simple_string.h"
#include <assert.h>
#include <stdio.h>

// TODO: check malloc result
void init_simple_string(simple_string_t *str) {
    str->len = 0;
    str->ptr = malloc(str->len + 1);
    str->ptr[0] = '\0';
}

size_t simple_string_writefunc(void *ptr, size_t size, size_t nmemb, simple_string_t *str) {
    assert(ptr);
    assert(str);
    printf("writing response body\n");
    size_t new_len = str->len + size * nmemb;
    str->ptr = realloc(str->ptr, new_len + 1);

    // fail request when allocation fails
    if (str->ptr == NULL) {
        return -1;
    }

    memcpy(str->ptr + str->len, ptr, size * nmemb);
    str->ptr[new_len] = '\0';
    str->len = new_len;

    return size * nmemb;
}