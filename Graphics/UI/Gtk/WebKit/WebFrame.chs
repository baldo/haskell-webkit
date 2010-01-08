{-# LANGUAGE ForeignFunctionInterface #-}

{# context lib="libwebkit" prefix="webkit_" #}

module Graphics.UI.Gtk.WebKit.WebFrame
    ( WebFrame

    , webFrameGetWebView

    , webFrameGetName
    , webFrameGetTitle
    , webFrameGetUri

    , webFrameGetParent

    , webFrameLoadUri
    , webFrameLoadString
    , webFrameLoadAlternateString

    , webFrameStopLoading
    , webFrameReload

    , webFrameFindFrame

    , webFramePrint

    , webFrameGetLoadStatus
    ) where

#include <webkit/webkitwebframe.h>

import Foreign.C
import GHC.Ptr
import System.Glib.FFI

import Control.Monad

import Graphics.UI.Gtk
    ( makeNewObject
    )

{#import Graphics.UI.Gtk.WebKit.General.Types#}
    ( WebFrame
    , WebView

    , mkWebFrame
    , unWebFrame
    , mkWebView
    , unWebView
    )

{#import Graphics.UI.Gtk.WebKit.General.Enums#}
    ( LoadStatus (..)
    )

webFrameGetWebView :: WebFrame -> IO WebView
webFrameGetWebView frame =
    withForeignPtr (unWebFrame frame) $ \ptr ->
        makeNewObject mkWebView $ {#call web_frame_get_web_view#} ptr

webFrameGetName :: WebFrame -> IO (Maybe String)
webFrameGetName frame =
    withForeignPtr (unWebFrame frame) $ \ptr ->
        {#call web_frame_get_name#} ptr
            >>= maybePeek peekCString

webFrameGetTitle :: WebFrame -> IO (Maybe String)
webFrameGetTitle frame =
    withForeignPtr (unWebFrame frame) $ \ptr ->
        {#call web_frame_get_title#} ptr
            >>= maybePeek peekCString

webFrameGetUri :: WebFrame -> IO (Maybe String)
webFrameGetUri frame =
    withForeignPtr (unWebFrame frame) $ \ptr ->
        {#call web_frame_get_uri#} ptr
            >>= maybePeek peekCString

webFrameGetParent :: WebFrame -> IO WebFrame
webFrameGetParent frame =
    withForeignPtr (unWebFrame frame) $ \ptr ->
        makeNewObject mkWebFrame $ {#call web_frame_get_parent#} ptr

webFrameLoadUri :: WebFrame -> String -> IO ()
webFrameLoadUri frame uri = do
    withCString uri $ \c_uri ->
        withForeignPtr (unWebFrame frame) $ \ptr ->
            {#call web_frame_load_uri#} ptr c_uri

webFrameLoadString :: WebFrame -> String -> String -> String -> String -> IO ()
webFrameLoadString frame content mime_type encoding base_uri = do
    withCString content $ \c_content ->
        withCString mime_type $ \c_mime_type ->
            withCString encoding $ \c_encoding ->
                withCString base_uri $ \c_base_uri ->
                    withForeignPtr (unWebFrame frame) $ \ptr ->
                        {#call web_frame_load_string#}
                            ptr c_content c_mime_type c_encoding c_base_uri

webFrameLoadAlternateString :: WebFrame -> String -> String -> String -> IO ()
webFrameLoadAlternateString frame content base_url unreachable_url = do
    withCString content $ \c_content ->
        withCString base_url $ \c_base_url->
            withCString unreachable_url $ \c_unreachable_url ->
                withForeignPtr (unWebFrame frame) $ \ptr ->
                    {#call web_frame_load_alternate_string#}
                        ptr c_content c_base_url c_unreachable_url

{- TODO
void web_frame_load_request (WebKitWebFrame *frame, WebKitNetworkRequest *request);
-}

webFrameStopLoading :: WebFrame -> IO ()
webFrameStopLoading frame =
    withForeignPtr (unWebFrame frame) $ \ptr ->
        {#call web_frame_stop_loading#} ptr

webFrameReload :: WebFrame -> IO ()
webFrameReload frame =
    withForeignPtr (unWebFrame frame) $ \ptr ->
        {#call web_frame_reload#} ptr

webFrameFindFrame :: WebFrame -> String -> IO WebFrame
webFrameFindFrame frame name = do
    withCString name $ \c_name ->
        withForeignPtr (unWebFrame frame) $ \ptr ->
            makeNewObject mkWebFrame $
                {#call web_frame_find_frame#} ptr c_name

{- TODO
JSGlobalContextRef web_frame_get_global_context (WebKitWebFrame *frame);
GtkPrintOperationResult web_frame_print_full (WebKitWebFrame *frame, GtkPrintOperation *operation, GtkPrintOperationAction action, GError **error);
-}

webFramePrint :: WebFrame -> IO ()
webFramePrint frame =
    withForeignPtr (unWebFrame frame) $ \ptr ->
        {#call web_frame_print#} ptr

webFrameGetLoadStatus :: WebFrame -> IO LoadStatus
webFrameGetLoadStatus frame =
    withForeignPtr (unWebFrame frame) $ \ptr ->
        liftM (toEnum . fromIntegral) $
            {#call web_frame_get_load_status#} ptr

