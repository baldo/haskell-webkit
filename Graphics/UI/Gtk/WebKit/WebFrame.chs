{-# LANGUAGE ForeignFunctionInterface #-}

{# context lib="libwebkit" prefix="webkit_" #}

{-| The content of a 'WebView'.

A 'WebView' contains a main 'WebFrame'. A 'WebFrame' contains the content of one
URI. The URI and name of the frame can be retrieved, the load status and
progress can be observed using the signals and can be controlled
using the methods of the 'WebFrame'. A 'WebFrame' can have any number of
children and one child can be found by using 'webFrameFindFrame'.
-}

module Graphics.UI.Gtk.WebKit.WebFrame
    ( WebFrame

    , webFrameGetType

    , webFrameGetWebView

    , webFrameGetName
    , webFrameGetTitle
    , webFrameGetUri

    , webFrameGetParent

    , webFrameGetNetworkResponse

    , webFrameLoadUri
    , webFrameLoadString
    , webFrameLoadAlternateString
    , webFrameLoadRequest

    , webFrameStopLoading
    , webFrameReload

    , webFrameFindFrame

    , webFrameGetGlobalContext

    --, webFramePrintFull -- TODO
    , webFramePrint

    , webFrameGetLoadStatus

    , webFrameGetHorizontalScrollbarPolicy
    , webFrameGetVerticalScrollbarPolicy

    , webFrameGetDataSource
    , webFrameGetProvisionalDataSource

    , webFrameGetSecurityOrigin
    ) where

#include <webkit/webkitwebframe.h>

import System.Glib.FFI
import System.Glib.GType
    ( GType
    )
import Graphics.UI.Gtk.General.Enums
    ( PolicyType (..)
    )

import Control.Monad.Trans
    ( MonadIO
    , liftIO
    )

{#import Graphics.UI.Gtk.WebKit.General.Types#}
    ( WebFrame
    , WebView
    , NetworkRequest
    , NetworkResponse
    , WebDataSource
    , SecurityOrigin

    , makeWebFrame
    , makeWebView
    , makeNetworkResponse
    , makeWebDataSource
    , makeSecurityOrigin

    , withWebFrame
    , withNetworkRequest
    )

{#import Graphics.UI.Gtk.WebKit.General.Enums#}
    ( LoadStatus (..)
    )

{#import Language.JavaScript.JavaScriptCore.General.Types#}
    ( GlobalContextRef (..)
    , makeGlobalContextRef
    )

webFrameGetType
    :: MonadIO m
    => m GType
webFrameGetType = liftIO $
    {#call web_frame_get_type#}

{- DEPRECATED
webkit_web_frame_new
webkit_web_view_open
-}

{- | Returns the 'WebView' that manages the 'WebFrame'.

     The 'WebView' returned manages the entire hierarchy of 'WebFrame' objects
     that contains the frame.
-}
webFrameGetWebView
    :: MonadIO m
    => WebFrame  -- ^ a frame
    -> m WebView -- ^ the 'WebView' managing the frame
webFrameGetWebView frame = liftIO $
    withWebFrame frame $ \ptr ->
        makeWebView $ {#call web_frame_get_web_view#} ptr

-- | Returns the frame's name.
webFrameGetName
    :: MonadIO m
    => WebFrame -- ^ a frame
    -> m String -- ^ its name
webFrameGetName frame = liftIO $
    withWebFrame frame $ \ptr ->
        {#call web_frame_get_name#} ptr
            >>= peekCString

-- | Returns the frame's document title.
webFrameGetTitle
    :: MonadIO m
    => WebFrame -- ^ a frame
    -> m String -- ^ its title
webFrameGetTitle frame = liftIO $
    withWebFrame frame $ \ptr ->
        {#call web_frame_get_title#} ptr
            >>= peekCString

-- | Returns the current URI of the contents displayed by the frame.
webFrameGetUri
    :: MonadIO m
    => WebFrame -- ^ a frame
    -> m String -- ^ the URI
webFrameGetUri frame = liftIO $
    withWebFrame frame $ \ptr ->
        {#call web_frame_get_uri#} ptr
            >>= peekCString

-- | Returns the frame's parent frame, or 'Nothing' if it has none.
webFrameGetParent
    :: MonadIO m
    => WebFrame           -- ^ a frame
    -> m (Maybe WebFrame) -- ^ 'Just' its parent or 'Nothing'
webFrameGetParent frame = liftIO $
    withWebFrame frame $ maybePeek $ \ptr ->
        makeWebFrame $ {#call web_frame_get_parent#} ptr

{- | Returns a 'NetworkResponse' object representing the response that was
     given to the request for the given frame, or 'Nothing' if the frame was
     not created by a load. You must unref the object when you are done with it.
-}
webFrameGetNetworkResponse
    :: MonadIO m
    => WebFrame                  -- ^ a frame
    -> m (Maybe NetworkResponse) -- ^ 'Just' the 'NetworkResponse' or 'Nothing'
webFrameGetNetworkResponse frame = liftIO $
    withWebFrame frame $ maybePeek $ \ptr ->
        makeNetworkResponse $ {#call web_frame_get_network_response#} ptr

-- | Requests loading of the specified URI string.
webFrameLoadUri
    :: MonadIO m
    => WebFrame -- ^ a frame
    -> String   -- ^ an URI string
    -> m ()
webFrameLoadUri frame uri = liftIO $
    withCString uri $ \c_uri ->
        withWebFrame frame $ \ptr ->
            {#call web_frame_load_uri#} ptr c_uri

{- | Requests loading of the given content with the specified MIME type,
     encoding and base URI.

     If no MIME type is given, \"text/html\" is assumed.

     If no encoding is given, \"UTF-8\" is assumed.
-}
webFrameLoadString
    :: MonadIO m
    => WebFrame     -- ^ a frame
    -> String       -- ^ an URI string
    -> Maybe String -- ^ 'Just' MIME type or 'Nothing'
    -> Maybe String -- ^ 'Just' encoding or 'Nothing'
    -> String       -- ^ the base URI for relative locations
    -> m ()
webFrameLoadString frame content mime_type encoding base_uri = liftIO $
    withCString content $ \c_content ->
        maybeWith withCString mime_type $ \c_mime_type ->
            maybeWith withCString encoding $ \c_encoding ->
                withCString base_uri $ \c_base_uri ->
                    withWebFrame frame $ \ptr ->
                        {#call web_frame_load_string#}
                            ptr c_content c_mime_type c_encoding c_base_uri

{- | Request loading of an alternate content for a URL that is unreachable.
     Using this method will preserve the back-forward list.
-}
webFrameLoadAlternateString
    :: MonadIO m
    => WebFrame -- ^ a frame
    -> String   -- ^ the alternate content to display as the main page
                --   of the frame
    -> String   -- ^ the base URI for relative locations, has to be an
                --   absolute URI
    -> String   -- ^ the URL for the alternate page content
    -> m ()
webFrameLoadAlternateString frame content base_url unreachable_url = liftIO $
    withCString content $ \c_content ->
        withCString base_url $ \c_base_url->
            withCString unreachable_url $ \c_unreachable_url ->
                withWebFrame frame $ \ptr ->
                    {#call web_frame_load_alternate_string#}
                        ptr c_content c_base_url c_unreachable_url

{- | Connects to a given URI by initiating an asynchronous client request.

     Creates a provisional data source that will transition to a committed data
     source once any data has been received. Use 'webFrameStopLoading' to
     stop the load. This function is typically invoked on the main frame.
-}
webFrameLoadRequest
    :: MonadIO m
    => WebFrame       -- ^ a frame
    -> NetworkRequest -- ^ a request
    -> m ()
webFrameLoadRequest frame request = liftIO $
    withWebFrame frame $ \pFrame ->
        withNetworkRequest request $ \pRequest ->
            {#call web_frame_load_request#} pFrame pRequest

-- | Stops any pending loads on frame's data source, and those of its children.
webFrameStopLoading
    :: MonadIO m
    => WebFrame -- ^ a frame
    -> m ()
webFrameStopLoading frame = liftIO $
    withWebFrame frame $ \ptr ->
        {#call web_frame_stop_loading#} ptr

-- | Reloads the initial request.
webFrameReload
    :: MonadIO m
    => WebFrame -- ^ a frame
    -> m ()
webFrameReload frame = liftIO $
    withWebFrame frame $ \ptr ->
        {#call web_frame_reload#} ptr

{- | For pre-defined names, returns the 'WebFrame' if the 'String' is \"_self\"
     or \"_current\", returns the frame's parent frame if the 'String' is
     \"_parent\", and returns the main frame if the 'String' is \"_top\". Also
     returns the 'WebFrame' if it is the main frame and the 'String' is either
     \"_parent\" or \"_top\". For other 'String's, this function returns the
     first frame that matches. This function searches the 'WebFrame' and its
     descendents first, then its parent and its children moving up the
     hierarchy until a match is found. If no match is found in the hierarchy,
     this function will search for a matching frame in other main frame
     hierarchies. Returns 'Nothing' if no match is found.
-}
webFrameFindFrame
    :: MonadIO m
    => WebFrame           -- ^ the 'WebFrame' to search in
    -> String             -- ^ the name of the frame to search for
    -> m (Maybe WebFrame) -- ^ the matching frame
webFrameFindFrame frame name = liftIO $
    withCString name $ maybePeek $ \c_name ->
        withWebFrame frame $ \ptr ->
            makeWebFrame $
                {#call web_frame_find_frame#} ptr c_name

{- TODO
Prints the given WebKitWebFrame, using the given GtkPrintOperation and
GtkPrintOperationAction. This function wraps a call to gtk_print_operation_run()
for printing the contents of the WebKitWebFrame.

frame :
    a WebKitWebFrame to be printed

operation :
    the GtkPrintOperation to be carried

action :
    the GtkPrintOperationAction to be performed

error :
    GError for error return

Returns :

GtkPrintOperationResult web_frame_print_full (WebKitWebFrame *frame, GtkPrintOperation *operation, GtkPrintOperationAction action, GError **error);
-}

{- | Gets the global JavaScript execution context. Use this function to bridge
     between the WebKit and JavaScriptCore APIs.
-}
webFrameGetGlobalContext
    :: MonadIO m
    => WebFrame           -- ^ a frame
    -> m GlobalContextRef -- ^ the global JavaScript context
webFrameGetGlobalContext frame = liftIO $
    withWebFrame frame $ \ptr ->
       makeGlobalContextRef $
            webkit_web_frame_get_global_context ptr

-- need this self written import because c2hs has convertion problems
foreign import ccall safe "webkitwebframe.h webkit_web_frame_get_global_context"
    webkit_web_frame_get_global_context :: ((Ptr (WebFrame)) -> (IO (Ptr (GlobalContextRef))))

{- | Prints the given 'WebFrame', by presenting a print dialog to the user.
     If you need more control over the printing process, see 'webFramePrintFull'.
-}
webFramePrint
    :: MonadIO m
    => WebFrame -- ^ a frame
    -> m ()
webFramePrint frame = liftIO $
    withWebFrame frame $ \ptr ->
        {#call web_frame_print#} ptr

{- TODO GtkPrintOperationResult  webkit_web_frame_print_full    (WebKitWebFrame *frame,
                                                         GtkPrintOperation *operation,
                                                         GtkPrintOperationAction action,
                                                         GError **error);
    This needs implementation of the printing part of gtk in gtk2hs..
-}


-- | Determines the current status of the load.
webFrameGetLoadStatus
    :: MonadIO m
    => WebFrame     -- ^ a frame
    -> m LoadStatus -- ^ the 'LoadStatus'
webFrameGetLoadStatus frame = liftIO $
    withWebFrame frame $ \ptr ->
        {#call web_frame_get_load_status#} ptr >>=
            return . toEnum . fromIntegral

webFrameGetHorizontalScrollbarPolicy
    :: MonadIO m
    => WebFrame     -- ^ a frame
    -> m PolicyType -- ^ horizontal scrollbar policy
webFrameGetHorizontalScrollbarPolicy frame = liftIO $
    withWebFrame frame $ \ptr ->
        {#call web_frame_get_horizontal_scrollbar_policy#} ptr >>=
            return . toEnum . fromIntegral

webFrameGetVerticalScrollbarPolicy
    :: MonadIO m
    => WebFrame     -- ^ a frame
    -> m PolicyType -- ^ vertical scrollbar policy
webFrameGetVerticalScrollbarPolicy frame = liftIO $
    withWebFrame frame $ \ptr ->
        {#call web_frame_get_vertical_scrollbar_policy#} ptr >>=
            return . toEnum . fromIntegral



-- | Returns the committed data source.
webFrameGetDataSource
    :: MonadIO m
    => WebFrame        -- ^ a frame
    -> m WebDataSource -- ^ the committed 'WebDataSource'
webFrameGetDataSource frame = liftIO $
    withWebFrame frame $ \ptr ->
        makeWebDataSource $ {#call web_frame_get_data_source#} ptr

{- | You use 'webFrameLoadRequest' to initiate a request that creates a
     provisional data source. The provisional data source will transition
     to a committed data source once any data has been received. Use
     'webFrameGetDataSource' to get the committed data source.
-}
webFrameGetProvisionalDataSource
    :: MonadIO m
    => WebFrame        -- ^ a frame
    -> m WebDataSource -- ^ the provisional data source
webFrameGetProvisionalDataSource frame = liftIO $
     withWebFrame frame $ \ptr ->
        makeWebDataSource $ {#call web_frame_get_provisional_data_source#} ptr

-- | Returns the frame's 'SecurityOrigin'.
webFrameGetSecurityOrigin
    :: MonadIO m
    => WebFrame         -- ^ a frame
    -> m SecurityOrigin -- ^ its 'SecurityOrigin'
webFrameGetSecurityOrigin frame = liftIO $
    withWebFrame frame $ \ptr ->
        makeSecurityOrigin $ {#call web_frame_get_security_origin#} ptr
