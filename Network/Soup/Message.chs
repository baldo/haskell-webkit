{-# LANGUAGE ForeignFunctionInterface #-}

{# context lib="libsoup" prefix="soup_" #}

module Network.Soup.Message
    ( Message

    , messageGetType

    , messageNew
    -- , messageNewFromUri -- TODO

    -- , messageSetRequest -- TODO
    -- , messageSetResponse -- TODO

    , messageSetHttpVersion
    , messageGetHttpVersion

    , messageIsKeepalive

    -- , messageGetUri -- TODO
    -- , messageSetUri -- TODO

    -- , messageGetAddress -- TODO

    , messageSetFlags
    , messageGetFlags

    -- , messageAddHeaderHandler -- TODO
    -- , messageAddStatusCodeHandler -- TODO

    -- , messageSetStatus -- TODO
    -- , messageSetStatusFull -- TODO

    -- , messageSetChunkAllocator -- TODO

    -- , messageDisableFeature -- TODO

    -- Properties --------------------------------------------------------------

    -- Signals -----------------------------------------------------------------

    ) where

#include <libsoup/soup-message.h>

import Foreign.C
import System.Glib.FFI

import System.Glib.GType

import Control.Monad

{#import Network.Soup.General.Types#}
    ( Message 

    , withMessage
    , makeMessage
    )

{#import Network.Soup.General.Enums#}
    ( HttpVersion (..)
    , MessageFlags (..)
    )
messageGetType :: IO GType
messageGetType =
    {#call message_get_type#}

messageNew :: String -> String -> IO Message
messageNew method uri =
    withCString method $ \c_method ->
        withCString uri $ \c_uri -> do
            ptr <- {#call message_new#} c_method c_uri
            let ptr' = castPtr ptr
            makeMessage (return ptr')

{- TODO
SoupMessage *soup_message_new_from_uri (const char *method, SoupURI *uri);
void soup_message_set_request (SoupMessage *msg, const char *content_type, SoupMemoryUse req_use, const char *req_body, gsize req_length);
void soup_message_set_response (SoupMessage *msg, const char *content_type, SoupMemoryUse resp_use, const char *resp_body, gsize resp_length);
-}

messageSetHttpVersion :: Message -> HttpVersion -> IO ()
messageSetHttpVersion message version =
    withMessage message $ \ptr ->
        {#call message_set_http_version#} ptr
            ((fromIntegral . fromEnum) version)

messageGetHttpVersion :: Message -> IO HttpVersion
messageGetHttpVersion message =
    withMessage message $ \ptr ->
        liftM (toEnum . fromIntegral) $
            {#call message_get_http_version#} ptr

messageIsKeepalive :: Message -> IO Bool
messageIsKeepalive message =
    withMessage message $ \ptr ->
        liftM toBool $ 
            {#call message_is_keepalive#} ptr

{- TODO
SoupURI *soup_message_get_uri (SoupMessage *msg);
void soup_message_set_uri (SoupMessage *msg, SoupURI *uri);
SoupAddress *soup_message_get_address (SoupMessage *msg);
-}

messageSetFlags
    :: Message
    -> [MessageFlags]
    -> IO ()
messageSetFlags message flags =
    withMessage message $ \ptr ->
        {#call message_set_flags#} ptr
            (fromIntegral $ foldl (\x y -> x .|. fromEnum y)  0 flags)

toBits :: Integral a => a -> [a]
toBits m = toBits' m 1
    where
        toBits' 0 _ = []
        toBits' n b
            | n `mod` 2 == 0 =     toBits' (n `div` 2) (b * 2)
            | otherwise      = b : toBits' (n `div` 2) (b * 2)

-- TODO/FIXME  -> IO [MessageFlags]
messageGetFlags :: Message -> IO [MessageFlags]
messageGetFlags message =
    withMessage message $ \ptr ->
        liftM ((map toEnum) . toBits . fromIntegral) $
            {#call message_get_flags#} ptr

{- TODO
guint soup_message_add_header_handler (SoupMessage *msg, const char *signal, const char *header, GCallback callback, gpointer user_data);
guint soup_message_add_status_code_handler ( SoupMessage *msg, const char *signal, guint status_code, GCallback callback, gpointer user_data);
void soup_message_set_status (SoupMessage *msg, guint status_code);
void soup_message_set_status_full (SoupMessage *msg, guint status_code, const char *reason_phrase);
void soup_message_set_chunk_allocator (SoupMessage *msg, SoupChunkAllocator allocator, gpointer user_data, GDestroyNotify destroy_notify);
void soup_message_disable_feature (SoupMessage *msg, GType feature_type);
-}
