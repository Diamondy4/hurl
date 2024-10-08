#include <curl/curl.h>
#include <stdlib.h>
#include <string.h>
#include "headers.h"

void free_header_data(header_data_t *hd)
{
    if (hd)
    {
        free(hd->buffer);
        free(hd);
    }
}


header_data_t* header_data_create(size_t initial_capacity) {
    header_data_t *hd = malloc(sizeof(header_data_t));
    if (!hd) {
        return NULL;
    }

    hd->buffer = malloc(initial_capacity);
    if (!hd->buffer) {
        free(hd);
        return NULL;
    }

    hd->size = 0;
    hd->capacity = initial_capacity;
    return hd;
}


size_t header_callback(char *buffer, size_t size, size_t nitems, void *userdata)
{
    size_t total_size = nitems; // 'size' is always 1 for headers
    header_data_t *hd = (header_data_t*)userdata;

    // Calculate the new required capacity (+1 for the separator)
    size_t required_capacity = hd->size + total_size + 1;

    // Resize the buffer if necessary
    if (required_capacity > hd->capacity)
    {
        size_t new_capacity = (hd->capacity * 2 > required_capacity) ? hd->capacity * 2 : required_capacity;
        char *new_buffer = realloc(hd->buffer, new_capacity);
        if (!new_buffer)
        {
            // Memory allocation failed
            return 0; // Returning 0 will abort the transfer
        }
        hd->buffer = new_buffer;
        hd->capacity = new_capacity;
    }

    // Copy the header data into the buffer
    memcpy(hd->buffer + hd->size, buffer, total_size);
    hd->size += total_size;

    // Append a '\n' separator
    hd->buffer[hd->size] = '\n';
    hd->size += 1;

    return total_size;
}
