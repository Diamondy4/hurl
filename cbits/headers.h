#pragma once

#include <stddef.h>
#include <stdlib.h>
#include <string.h>

typedef struct header_data_s
{
    char *buffer;
    size_t size;
    size_t capacity;
} header_data_t;

void free_header_data(header_data_t *hd);

header_data_t* header_data_create(size_t initial_capacity);

size_t header_callback(char *buffer, size_t size, size_t nitems, void *userdata);