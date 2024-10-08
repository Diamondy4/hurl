module Internal.Raw.Headers where

#include "headers.h"

{# pointer *header_data_t as HeadersData foreign newtype #}
