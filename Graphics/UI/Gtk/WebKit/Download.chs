{-# LANGUAGE ForeignFunctionInterface #-}

{# context lib="libwebkit" prefix="webkit_" #}

module Graphics.UI.Gtk.WebKit.Download
    ( Download

    , downloadCancel
    , downloadStart
    , downloadGetCurrentSize
    , downloadSetDestinationUri
    , downloadGetElapsedTime
    , downloadGetUri
    , downloadGetTotalSize
    , downloadGetSuggestedFileName
    , downloadGetNetworkResponse
    , downloadGetNetworkRequest
    , downloadGetStatus

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

{#import Graphics.UI.Gtk.WebKit.General.Enums#}
    ( DownloadStatus
    )


downloadCancel :: Download -> IO ()
downloadCancel download =
    withDownload download $ \ptr ->
        {#call download_cancel#} ptr

downloadGetCurrentSize :: Download -> IO Integer
downloadGetCurrentSize download =
    withDownload download $ \ptr ->
        liftM toInteger $ {#call download_get_current_size#} ptr

downloadGetDestinationUri :: Download -> IO String
downloadGetDestinationUri download =
     withDownload download $ \ptr ->
        {#call download_get_destination_uri#} ptr >>= peekCString

downloadGetElapsedTime :: Download -> IO Double
downloadGetElapsedTime download = 
    withDownload download $ \ptr ->
        liftM realToFrac $ {#call download_get_elapsed_time#} ptr

downloadGetNetworkRequest :: Download -> IO NetworkRequest
downloadGetNetworkRequest download =
    withDownload download $ \ptr ->
        makeNewObject mkNetworkRequest $ {#call download_get_network_request#} ptr

downloadGetNetworkResponse :: Download -> IO NetworkResponse
downloadGetNetworkResponse download =
    withDownload download $ \ptr ->
        makeNewObject mkNetworkResponse $ {#call download_get_network_response#} ptr

downloadGetProgress :: Download -> IO Double
downloadGetProgress download =
    withDownload download $ \ptr ->
        liftM realToFrac $ {#call download_get_progress#} ptr

downloadGetStatus :: Download -> IO DownloadStatus 
downloadGetStatus download =
    withDownload download $ \ptr ->
        liftM (toEnum . fromIntegral) $ {#call download_get_status#} ptr

downloadGetSuggestedFileName :: Download -> IO String
downloadGetSuggestedFileName download =
    withDownload download $ \ptr ->
        {#call download_get_suggested_filename#} ptr >>= peekCString

downloadGetTotalSize :: Download -> IO Integer
downloadGetTotalSize download =
    withDownload download $ \ptr ->
        liftM toInteger $ {#call download_get_current_size#} ptr

downloadGetUri :: Download -> IO String
downloadGetUri download =
    withDownload download $ \ptr ->
        {#call download_get_uri#} ptr >>= peekCString

downloadNew :: NetworkRequest -> IO Download
downloadNew request =
    withNetworkRequest request $ \ptr ->
        makeNewObject mkDownload $ 
            {#call download_new#} ptr 

downloadSetDestinationUri :: Download -> String -> IO ()
downloadSetDestinationUri download uri =
    withDownload download $ \pDownload ->
        withCString uri $ \pUri ->
            {#call download_set_destination_uri#} pDownload pUri

downloadStart :: Download -> IO ()
downloadStart download =
    withDownload download $ \ptr ->
        {#call download_start#} ptr
