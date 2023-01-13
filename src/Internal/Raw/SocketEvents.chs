{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Internal.Raw.SocketEvents where

import Foreign.C.Types
import Data.Bits

#include <curl/curl.h>

#include "curl_hs_c.h"

{# context lib="curl" #}

{- | Notification of the events that have happened on a socket.

This type is passed as an argument to the `curl_multi_socket_action` method on a multi handle
to indicate what events have occurred on a socket.
-}
newtype CurlEventsOnSocket = CurlEventsOnSocket
    { bits :: CInt
    }
    deriving (Eq, Ord, Show)
    deriving newtype (Bits)

pattern CurlCSelectIn :: CurlEventsOnSocket
pattern CurlCSelectIn = CurlEventsOnSocket {#const CURL_CSELECT_IN#}

pattern CurlCSelectOut :: CurlEventsOnSocket
pattern CurlCSelectOut = CurlEventsOnSocket {#const CURL_CSELECT_OUT#}

pattern CurlCSelectErr :: CurlEventsOnSocket
pattern CurlCSelectErr = CurlEventsOnSocket {#const CURL_CSELECT_ERR#}


instance Semigroup CurlEventsOnSocket where
    (<>) = (.|.)

{-|
Notification of events that are requested on a socket.

This type is yielded to the `CURLMOPT_SOCKETFUNCTION` callback to indicate what
events are requested on a socket.
-}
{#enum define CurlSocketEventRequest {CURL_POLL_IN as CurlPollIn, CURL_POLL_OUT as CurlPollOut, CURL_POLL_INOUT as CurlPollInOut, CURL_POLL_REMOVE as CurlPollRemove} deriving (Eq, Ord, Show) #}