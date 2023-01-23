#pragma once

#include <stddef.h>

typedef struct simple_string_s {
    char *ptr;
    size_t len;
} simple_string_t;

void init_simple_string(simple_string_t *str);

size_t simple_string_writefunc(void *ptr, size_t size, size_t nmemb, simple_string_t *str);