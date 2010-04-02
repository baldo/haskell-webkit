{-# LANGUAGE ForeignFunctionInterface #-}

{# context lib="libwebkit" prefix="webkit_" #}

{-| 'Download' is used to communicate with the application when downloading.

'Download' carries information about a download request, including a 
NetworkRequest object. The application may use this object to control the 
download process, or to simply figure out what is to be downloaded, and 
do it itself.
-}

module Graphics.UI.Gtk.WebKit.Download
    ( Download

    , downloadGetType
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
    , downloadGetProgress
    ) where

#include <webkit/webkitdownload.h>

import Foreign.C
import GHC.Ptr
import System.Glib.FFI
import System.Glib.GType

import Control.Monad

import Graphics.UI.Gtk.Abstract.Object
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

downloadGetType :: IO GType
downloadGetType =
    {#call download_get_type#}

{- | Cancels the download. Calling this will not free the 'Download'
     object, so you still need to call g_object_unref() on it, if you are 
     the owner of a reference. Notice that cancelling the download provokes
     the emission of the 'Download::error' signal, reporting that the 
     download was cancelled.
-}
downloadCancel
    :: Download -- ^ the 'Download' to be canceled
    -> IO ()
downloadCancel download =
    withDownload download $ \ptr ->
        {#call download_cancel#} ptr

-- | Current already downloaded size.
downloadGetCurrentSize
    :: Download   -- ^ 'Download' to determine size of
    -> IO Integer -- ^ size of the given 'Download'
downloadGetCurrentSize download =
    withDownload download $ \ptr ->
        liftM toInteger $ {#call download_get_current_size#} ptr

{- | Obtains the URI to which the downloaded file will be written. This must 
     have been set by the application before calling 'downloadStart', 
     and may be 'Nothing'.
-}
downloadGetDestinationUri
    :: Download          -- ^ 'Download' to get the URI of
    -> IO (Maybe String) -- ^ 'Just' the URI or 'Nothing' if unset
downloadGetDestinationUri download =
     withDownload download $ \ptr ->
        {#call download_get_destination_uri#} ptr
            >>= maybePeek peekCString

{- | Elapsed time for the download in seconds, including any fractional part. 
     If the download is finished, had an error or was cancelled this is the 
     time between its start and the event.
-}
downloadGetElapsedTime
    :: Download  -- ^ the 'Download' to examine
    -> IO Double -- ^ elapsed time
downloadGetElapsedTime download = 
    withDownload download $ \ptr ->
        liftM realToFrac $ {#call download_get_elapsed_time#} ptr

-- | Retrieves the 'NetworkRequest' datatype that backs the download process.
downloadGetNetworkRequest
    :: Download          -- ^ the 'Download'
    -> IO NetworkRequest -- ^ the corresponding 'NetworkRequest'
downloadGetNetworkRequest download =
    withDownload download $ \ptr ->
        makeNewObject mkNetworkRequest $ {#call download_get_network_request#} ptr

-- | Retrieves the 'NetworkResponse' datatype that backs the download process.
downloadGetNetworkResponse
    :: Download           -- ^ the 'Download'
    -> IO NetworkResponse -- ^ the corresponding 'NetworkResponse'
downloadGetNetworkResponse download =
    withDownload download $ \ptr ->
        makeNewObject mkNetworkResponse $ {#call download_get_network_response#} ptr


{- | Determines the current progress of the download.
     The returning 'Double' ranging from 0.0 to 1.0.
-}
downloadGetProgress
    :: Download  -- ^ the 'Download'
    -> IO Double -- ^ the progress
downloadGetProgress download =
    withDownload download $ \ptr ->
        liftM realToFrac $ {#call download_get_progress#} ptr

-- | Obtains the current status of the 'Download', as a 'DownloadStatus'
downloadGetStatus
    :: Download          -- ^ the 'Download'
    -> IO DownloadStatus -- ^ the current status
downloadGetStatus download =
    withDownload download $ \ptr ->
        liftM (toEnum . fromIntegral) $ {#call download_get_status#} ptr

{- | Retrieves the filename that was suggested by the server, or the one 
     derived by WebKit from the URI.
-}
downloadGetSuggestedFileName
    :: Download  -- ^ the 'Download'
    -> IO String -- ^ the suggested filename
downloadGetSuggestedFileName download =
    withDownload download $ \ptr ->
        {#call download_get_suggested_filename#} ptr >>= peekCString

{- | Returns the expected total size of the 'Download'. This is expected 
     because the server may provide incorrect or missing Content-Length. 
     Notice that this may grow over time, as it will be always the same as 
     current_size in the cases where current size surpasses it.
-}
downloadGetTotalSize
    :: Download   -- ^ the 'Download'
    -> IO Integer -- ^ expected total size
downloadGetTotalSize download =
    withDownload download $ \ptr ->
        liftM toInteger $ {#call download_get_total_size#} ptr

{- | Convenience method to retrieve the URI from the 'NetworkRequest'
     which is being downloaded.
-}
downloadGetUri
    :: Download  -- ^ the 'Download'
    -> IO String -- ^ the URI being downloaded
downloadGetUri download =
    withDownload download $ \ptr ->
        {#call download_get_uri#} ptr >>= peekCString

-- | Creates a new 'Download' datatype for the given 'NetworkRequest' datatype.
downloadNew
    :: NetworkRequest -- ^ the 'NetworkRequest'
    -> IO Download    -- ^ the new 'Download'
downloadNew request =
    withNetworkRequest request $ \ptr ->
        makeNewObject mkDownload $ 
            {#call download_new#} ptr 

-- | Defines the URI that should be used to save the downloaded file to.
downloadSetDestinationUri
    :: Download -- ^ the 'Download'
    -> String   -- ^ destination URI
    -> IO ()
downloadSetDestinationUri download uri =
    withDownload download $ \pDownload ->
        withCString uri $ \pUri ->
            {#call download_set_destination_uri#} pDownload pUri

{- | Initiates the download. Notice that you must have set the destination-uri 
     property before calling this method.
-}
downloadStart
    :: Download -- ^ the 'Download' to start
    -> IO ()
downloadStart download =
    withDownload download $ \ptr ->
        {#call download_start#} ptr
