module Internal.Raw.MPSC where

import Internal.Raw.Alloc
import Internal.Raw.Curl
import Foreign 


#include "message_chan.h"
#include <uv.h>

{# pointer *mpsc_t as MPSCQ foreign newtype #}

{#enum outer_message_types as InternalOuterMessageTag {underscoreToCase} add prefix = "Internal" deriving (Eq)#}

{#pointer *outer_message_t as InternalOuterMessage newtype #}

data OuterMessage = Execute (Ptr CurlEasy) | CancelRequest (Ptr CurlEasy)

toInnerOuterMessage :: OuterMessage -> IO InternalOuterMessage
toInnerOuterMessage msg = do
    ptr' <- c_malloc {# sizeof outer_message_t #}
    let cMsg = InternalOuterMessage $ castPtr ptr'
    case msg of
        Execute easy -> do
            {#set outer_message_t.tag#} cMsg (fromIntegral . fromEnum $ InternalExecute)
            {#set outer_message_t.easy#} cMsg (castPtr easy)
        CancelRequest easy -> do
            {#set outer_message_t.tag#} cMsg (fromIntegral . fromEnum $ InternalCancelRequest)
            {#set outer_message_t.easy#} cMsg (castPtr easy)
    pure cMsg