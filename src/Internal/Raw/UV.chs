module Internal.Raw.UV where

#include <uv.h>

{# pointer *uv_loop_t as UVLoop newtype #}

{# pointer *uv_async_t as UVAsync newtype #}