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
    , downloadGetDestinationUri
    , downloadSetDestinationUri
    , downloadGetElapsedTime
    , downloadGetUri
    , downloadNew
    , downloadGetTotalSize
    , downloadGetSuggestedFileName
    , downloadGetNetworkResponse
    , downloadGetNetworkRequest
    , downloadGetStatus
    , downloadGetProgress
    ) where

#include <webkit/webkitdownload.h>

import System.Glib.FFI
import System.Glib.GType
    ( GType
    )

import Control.Monad.Trans
    ( MonadIO
    , liftIO
    )

{#import Graphics.UI.Gtk.WebKit.General.Types#}
    ( Download
    , NetworkResponse
    , NetworkRequest

    , withDownload
    , makeDownload

    , withNetworkRequest
    , makeNetworkRequest
    , makeNetworkResponse
    )

{#import Graphics.UI.Gtk.WebKit.General.Enums#}
    ( DownloadStatus (..)
    )

downloadGetType
    :: MonadIO m
    => m GType
downloadGetType = liftIO $
    {#call download_get_type#}

{- | Cancels the download. Calling this will not free the 'Download'
     object, so you still need to call g_object_unref() on it, if you are
     the owner of a reference. Notice that cancelling the download provokes
     the emission of the 'Download::error' signal, reporting that the
     download was cancelled.
-}
downloadCancel
    :: MonadIO m
    => Download -- ^ the 'Download' to be canceled
    -> m ()
downloadCancel download = liftIO $
    withDownload download $ \ptr ->
        {#call download_cancel#} ptr

-- | Current already downloaded size.
downloadGetCurrentSize
    :: MonadIO m
    => Download  -- ^ 'Download' to determine size of
    -> m Integer -- ^ size of the given 'Download'
downloadGetCurrentSize download = liftIO $
    withDownload download $ \ptr ->
        {#call download_get_current_size#} ptr >>=
            return . toInteger

{- | Obtains the URI to which the downloaded file will be written. This must
     have been set by the application before calling 'downloadStart',
     and may be 'Nothing'.
-}
downloadGetDestinationUri
    :: MonadIO m
    => Download         -- ^ 'Download' to get the URI of
    -> m (Maybe String) -- ^ 'Just' the URI or 'Nothing' if unset
downloadGetDestinationUri download = liftIO $
     withDownload download $ \ptr ->
        {#call download_get_destination_uri#} ptr
            >>= maybePeek peekCString

{- | Elapsed time for the download in seconds, including any fractional part.
     If the download is finished, had an error or was cancelled this is the
     time between its start and the event.
-}
downloadGetElapsedTime
    :: MonadIO m
    => Download -- ^ the 'Download' to examine
    -> m Double -- ^ elapsed time
downloadGetElapsedTime download = liftIO $
    withDownload download $ \ptr ->
        {#call download_get_elapsed_time#} ptr >>=
            return . realToFrac

-- | Retrieves the 'NetworkRequest' datatype that backs the download process.
downloadGetNetworkRequest
    :: MonadIO m
    => Download         -- ^ the 'Download'
    -> m NetworkRequest -- ^ the corresponding 'NetworkRequest'
downloadGetNetworkRequest download = liftIO $
    withDownload download $ \ptr ->
        makeNetworkRequest $ {#call download_get_network_request#} ptr

-- | Retrieves the 'NetworkResponse' datatype that backs the download process.
downloadGetNetworkResponse
    :: MonadIO m
    => Download          -- ^ the 'Download'
    -> m NetworkResponse -- ^ the corresponding 'NetworkResponse'
downloadGetNetworkResponse download = liftIO $
    withDownload download $ \ptr ->
        makeNetworkResponse $ {#call download_get_network_response#} ptr


{- | Determines the current progress of the download.
     The returning 'Double' ranging from 0.0 to 1.0.
-}
downloadGetProgress
    :: MonadIO m
    => Download -- ^ the 'Download'
    -> m Double -- ^ the progress
downloadGetProgress download = liftIO $
    withDownload download $ \ptr ->
        {#call download_get_progress#} ptr >>=
            return . realToFrac

-- | Obtains the current status of the 'Download', as a 'DownloadStatus'
downloadGetStatus
    :: MonadIO m
    => Download         -- ^ the 'Download'
    -> m DownloadStatus -- ^ the current status
downloadGetStatus download = liftIO $
    withDownload download $ \ptr ->
        {#call download_get_status#} ptr >>=
            return . toEnum . fromIntegral

{- | Retrieves the filename that was suggested by the server, or the one
     derived by WebKit from the URI.
-}
downloadGetSuggestedFileName
    :: MonadIO m
    => Download -- ^ the 'Download'
    -> m String -- ^ the suggested filename
downloadGetSuggestedFileName download = liftIO $
    withDownload download $ \ptr ->
        {#call download_get_suggested_filename#} ptr >>= peekCString

{- | Returns the expected total size of the 'Download'. This is expected
     because the server may provide incorrect or missing Content-Length.
     Notice that this may grow over time, as it will be always the same as
     current_size in the cases where current size surpasses it.
-}
downloadGetTotalSize
    :: MonadIO m
    => Download  -- ^ the 'Download'
    -> m Integer -- ^ expected total size
downloadGetTotalSize download = liftIO $
    withDownload download $ \ptr ->
        {#call download_get_total_size#} ptr >>=
            return . toInteger

{- | Convenience method to retrieve the URI from the 'NetworkRequest'
     which is being downloaded.
-}
downloadGetUri
    :: MonadIO m
    => Download -- ^ the 'Download'
    -> m String -- ^ the URI being downloaded
downloadGetUri download = liftIO $
    withDownload download $ \ptr ->
        {#call download_get_uri#} ptr >>= peekCString

-- | Creates a new 'Download' datatype for the given 'NetworkRequest' datatype.
downloadNew
    :: MonadIO m
    => NetworkRequest -- ^ the 'NetworkRequest'
    -> m Download     -- ^ the new 'Download'
downloadNew request = liftIO $
    withNetworkRequest request $ \ptr ->
        makeDownload $
            {#call download_new#} ptr

-- | Defines the URI that should be used to save the downloaded file to.
downloadSetDestinationUri
    :: MonadIO m
    => Download -- ^ the 'Download'
    -> String   -- ^ destination URI
    -> m ()
downloadSetDestinationUri download uri = liftIO $
    withDownload download $ \pDownload ->
        withCString uri $ \pUri ->
            {#call download_set_destination_uri#} pDownload pUri

{- | Initiates the download. Notice that you must have set the destination-uri
     property before calling this method.
-}
downloadStart
    :: MonadIO m
    => Download -- ^ the 'Download' to start
    -> m ()
downloadStart download = liftIO $
    withDownload download $ \ptr ->
        {#call download_start#} ptr
