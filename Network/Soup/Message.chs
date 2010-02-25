{-# LANGUAGE ForeignFunctionInterface #-}

{# context lib="libsoup" prefix="soup_" #}

module Network.Soup.Message
    ( Message

    , messageGetType

    -- Properties --------------------------------------------------------------

    -- Signals -----------------------------------------------------------------

    ) where

#include <libsoup/soup-message.h>

import Foreign.C
import GHC.Ptr
import System.Glib.FFI

import System.Glib.GType
import System.Glib.Properties

import Control.Monad

import Graphics.UI.Gtk.Abstract.Object
    ( makeNewObject
    )

import Graphics.UI.Gtk.Signals

{#import Network.Soup.General.Types#}
    ( Message 

    , withMessage
    , mkMessage
    )

messageGetType :: IO GType
messageGetType =
    {#call message_get_type#}

{- TODO
GType soup_message_get_type (void);
SoupMessage *soup_message_new (const char *method, CONST CHAr *uri_string);
SoupMessage *soup_message_new_from_uri (const char *method, SoupURI *uri);
void soup_message_set_request (SoupMessage *msg, const char *content_type, SoupMemoryUse req_use, const char *req_body, gsize req_length);
void soup_message_set_response (SoupMessage *msg, const char *content_type, SoupMemoryUse resp_use, const char *resp_body, gsize resp_length);
void soup_message_set_http_version (SoupMessage *msg, SoupHTTPVersion version);
SoupHTTPVersion soup_message_get_http_version (SoupMessage *msg);
gboolean soup_message_is_keepalive (SoupMessage *msg);
SoupURI *soup_message_get_uri (SoupMessage *msg);
void soup_message_set_uri (SoupMessage *msg, SoupURI *uri);
SoupAddress *soup_message_get_address (SoupMessage *msg);
void soup_message_set_flags (SoupMessage *msg, SoupMessageFlags flags);
SoupMessageFlags soup_message_get_flags (SoupMessage *msg);
guint soup_message_add_header_handler (SoupMessage *msg, const char *signal, const char *header, GCallback callback, gpointer user_data);
guint soup_message_add_status_code_handler ( SoupMessage *msg, const char *signal, guint status_code, GCallback callback, gpointer user_data);
void soup_message_set_status (SoupMessage *msg, guint status_code);
void soup_message_set_status_full (SoupMessage *msg, guint status_code, const char *reason_phrase);
void soup_message_set_chunk_allocator (SoupMessage *msg, SoupChunkAllocator allocator, gpointer user_data, GDestroyNotify destroy_notify);
void soup_message_disable_feature (SoupMessage *msg, GType feature_type);
-}
