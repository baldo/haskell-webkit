{-# LANGUAGE ForeignFunctionInterface #-}

{# context lib="libwebkit" prefix="webkit_" #}

module Graphics.UI.Gtk.WebKit.Download
    ( Download

    ) where

#include <webkit/webkitdownload.h>

import Foreign.C
import GHC.Ptr
import System.Glib.FFI
import System.Glib.GType

import Control.Monad

import Graphics.UI.Gtk
    ( makeNewObject
    )

{#import Graphics.UI.Gtk.WebKit.General.Types#}
    ( Download
    , NetworkResponse
    , NetworkRequest

    , withDownload
    , mkDownload
    
    , withNetworkRequest
    , mkNetworkRequest
    , mkNetworkResponse

    )

{- TODO
void                webkit_download_cancel              (WebKitDownload *download);
guint64             webkit_download_get_current_size    (WebKitDownload *download);
const gchar *       webkit_download_get_destination_uri (WebKitDownload *download);
gdouble             webkit_download_get_elapsed_time    (WebKitDownload *download);
WebKitNetworkRequest * webkit_download_get_network_request
                                                        (WebKitDownload *download);
WebKitNetworkResponse * webkit_download_get_network_response
                                                        (WebKitDownload *download);
gdouble             webkit_download_get_progress        (WebKitDownload *download);
WebKitDownloadStatus  webkit_download_get_status        (WebKitDownload *download);
const gchar *       webkit_download_get_suggested_filename
                                                        (WebKitDownload *download);
guint64             webkit_download_get_total_size      (WebKitDownload *download);
const gchar *       webkit_download_get_uri             (WebKitDownload *download);
WebKitDownload *    webkit_download_new                 (WebKitNetworkRequest *request);
void                webkit_download_set_destination_uri (WebKitDownload *download,
                                                         const gchar *destination_uri);
void                webkit_download_start               (WebKitDownload *download);
-}
