{-# LANGUAGE ForeignFunctionInterface #-}

{# context lib="libwebkit" prefix="webkit_" #}

{-| The central datatype of Haskell WebKit

'WebView' is the central datatype of Haskell WebKit. It is an instance of
'WidgetClass' implementing the scrolling interface which means you can embed in
a 'ScrolledWindow'. It is responsible for managing the drawing of the content,
forwarding of events. You can load any URI into the 'WebView' or any kind of
data string. With 'WebSettings' you can control various aspects of the
rendering and loading of the content. Each 'WebView' has exactly one 'WebFrame'
as main frame. A 'WebFrame' can have n children.
-}

module Graphics.UI.Gtk.WebKit.WebView
    ( WebView

    -- * Functions

    , webViewGetType

    , webViewNew

    , webViewGetTitle
    , webViewGetUri

    , webViewSetMaintainsBackForwardList
    , webViewGetBackForwardList
    , webViewGoToBackForwardItem

    , webViewCanGoBack
    , webViewCanGoBackOrForward
    , webViewCanGoForward

    , webViewGoBack
    , webViewGoBackOrForward
    , webViewGoForward

    , webViewStopLoading
    , webViewReload
    , webViewReloadBypassCache

    , webViewLoadUri
    , webViewLoadString
    -- , webViewLoadHtmlString -- DEPRECATED
    , webViewLoadRequest

    , webViewSearchText
    , webViewMarkTextMatches
    , webViewSetHighlightTextMatches
    , webViewUnmarkTextMatches

    , webViewGetMainFrame
    , webViewGetFocusedFrame

    , webViewExecuteScript

    , webViewCanCutClipboard
    , webViewCanCopyClipboard
    , webViewCanPasteClipboard

    , webViewCutClipboard
    , webViewCopyClipboard
    , webViewPasteClipboard

    , webViewDeleteSelection
    , webViewHasSelection
    , webViewSelectAll

    , webViewGetEditable
    , webViewSetEditable

    , webViewGetCopyTargetList
    , webViewGetPasteTargetList

    , webViewSetSettings
    , webViewGetSettings

    , webViewGetInspector

    , webViewCanShowMimeType

    , webViewGetTransparent
    , webViewSetTransparent

    , webViewGetZoomLevel
    , webViewSetZoomLevel

    , webViewZoomIn
    , webViewZoomOut

    , webViewGetFullContentZoom
    , webViewSetFullContentZoom

    , getDefaultSession

    , webViewGetEncoding

    , webViewSetCustomEncoding
    , webViewGetCustomEncoding

    , webViewMoveCursor

    , webViewGetLoadStatus
    , webViewGetProgress

    , webViewUndo
    , webViewCanUndo

    , webViewRedo
    , webViewCanRedo

    , webViewSetViewSourceMode
    , webViewGetViewSourceMode

    --, webViewGetHitTestResult -- TODO

    -- * Properties

    , webViewGetIconUri

    , webViewGetIMContext

    , webViewGetWindowFeatures
    , webViewSetWindowFeatures

    -- * Signals

    , onWebViewCopyClipboard
    , afterWebViewCopyClipboard

    , onWebViewConsoleMessage
    , afterWebViewConsoleMessage

    , onWebViewCutClipboard
    , afterWebViewCutClipboard

    , onWebViewDatabaseQuotaExceeded
    , afterWebViewDatabaseQuotaExceeded

    , onWebViewDocumentLoadFinished
    , afterWebViewDocumentLoadFinished

    , onWebViewHoveringOverLink
    , afterWebViewHoveringOverLink

    , onWebViewIconLoaded
    , afterWebViewIconLoaded

    , onWebViewCloseWebView
    , afterWebViewCloseWebView

    , onWebViewPasteClipboard
    , afterWebViewPasteClipboard

    , onWebViewPopulatePopup
    , afterWebViewPopulatePopup

    , onWebViewSelectAll
    , afterWebViewSelectAll

    , onWebViewSelectionChanged
    , afterWebViewSelectionChanged

    , onWebViewStatusbarTextChanged
    , afterWebViewStatusbarTextChanged

    , onWebViewPrintRequested
    , afterWebViewPrintRequested

    , onWebViewPrintRequestedWrapper

    , onWebViewRedo
    , afterWebViewRedo

    , onWebViewResourceRequestStarting
    , afterWebViewResourceRequestStarting

    , onWebViewCreateWebView
    , afterWebViewCreateWebView

    , onWebViewScriptAlert
    , afterWebViewScriptAlert

    , onWebViewScriptConfirm
    , afterWebViewScriptConfirm

    , onWebViewDownloadRequested
    , afterWebViewDownloadRequested

    , onWebViewSetCrollAdjustments
    , afterWebViewSetCrollAdjustments

    , onWebViewScriptPrompt
    , afterWebViewScriptPrompt

    , onWebViewReady
    , afterWebViewReady

    , onWebViewUndo
    , afterWebViewUndo

    , onWebViewNavigationPolicyDecisionRequested
    , afterWebViewNavigationPolicyDecisionRequested

    , afterWebViewMimeTypePolicyDecisionRequested
    , onWebViewMimeTypePolicyDecisionRequested

    ) where

#include <webkit/webkitwebview.h>
#include <gtk/gtkimcontext.h>

import System.Glib.Signals
    ( Signal (..)
    , ConnectId

    , after
    , on

    , connectGeneric
    )
import System.Glib.GObject
    ( objectUnref
    )
import System.Glib.FFI
import System.Glib.GType
    ( GType
    )
import System.Glib.Properties
    ( objectGetPropertyGObject
    , objectSetPropertyGObject

    , objectGetPropertyString
    )

import Control.Monad.Trans
    ( MonadIO
    , liftIO
    )

import Graphics.UI.Gtk.Types
    ( Adjustment (..)
    , IMContext
    , Menu (..)
    )
import Graphics.UI.Gtk.Signals
    ( connect_NONE__NONE
    , connect_STRING_STRING__NONE
    )
import Graphics.UI.Gtk.General.DNDTypes
    ( TargetList
    , mkTargetList
    )
import Graphics.UI.Gtk.General.Enums
    ( MovementStep
    )
import Graphics.UI.Gtk.Abstract.Object
    ( makeNewObject
    )

{#import Graphics.UI.Gtk.WebKit.General.Types#}
    ( NetworkRequest
    , WebFrame
    , WebView
    , WebSettings
    , WebBackForwardList
    , WebHistoryItem
    , WebInspector
    , WebResource
    , NetworkResponse
    , Download
    , WebDatabase
    , WebNavigationAction
    , WebPolicyDecision

    , withNetworkRequest
    , makeNetworkRequest
    , makeWebFrame
    , withWebView
    , makeWebView
    , withWebSettings
    , makeWebSettings
    , makeWebBackForwardList
    , withWebHistoryItem
    , withWebView
    , makeWebView
    , withWebSettings
    , makeWebSettings
    , makeWebInspector
    , makeNetworkResponse
    , makeNetworkRequest
    , makeWebResource
    , makeDownload
    , makeWebDatabase
    , makeWebNavigationAction
    , makeWebPolicyDecision

    )

{#import Graphics.UI.Gtk.WebKit.General.Enums#}
    ( LoadStatus (..)
    )

{#import Graphics.UI.Gtk.WebKit.WebWindowFeatures#}
    ( WebWindowFeatures
    , webWindowFeaturesGetType
    )

{#import Network.Soup.General.Types#}
    ( SoupSession

    , makeSoupSession
    )

webViewGetType
    :: MonadIO m
    => m GType
webViewGetType = liftIO $
    {#call web_view_get_type#}
-- | Create a new Instance of a WebView Object
webViewNew
    :: MonadIO m
    => m WebView -- ^ new instance
webViewNew = liftIO $ do
    ptr <- {#call web_view_new#}
    let ptr' = castPtr ptr
    makeWebView (return ptr')

-- | Returns the web_view's document title
webViewGetTitle
    :: MonadIO m
    => WebView          -- ^ a WebView
    -> m (Maybe String) -- ^ 'Just String' the title of web_view or 'Nothing'
webViewGetTitle web_view = liftIO $
    withWebView web_view $ \ptr ->
        {#call web_view_get_title#} ptr
            >>= maybePeek peekCString

-- | Returns the current URI of the contents displayed by the 'WebView'
webViewGetUri
    :: MonadIO m
    => WebView          -- ^ a 'WebView'
    -> m (Maybe String) -- ^ 'Just' the URI of 'WebView' or 'Nothing'
webViewGetUri web_view = liftIO $
    withWebView web_view $ \ptr ->
        {#call web_view_get_uri#} ptr
            >>= maybePeek peekCString

-- | Set the view to maintain a back or forward list of history items.
webViewSetMaintainsBackForwardList
    :: MonadIO m
    => WebView -- ^ a 'WebView'
    -> Bool    -- ^ to tell the 'WebView' to maintain a back or forward list
    -> m ()
webViewSetMaintainsBackForwardList web_view flag = liftIO $
    withWebView web_view $ \ptr ->
        {#call web_view_set_maintains_back_forward_list#}
            ptr $ fromBool flag

-- | Returns the 'WebBackForwardList' for the given 'WebView'.
webViewGetBackForwardList
    :: MonadIO m
    => WebView              -- ^ the 'WebView'
    -> m WebBackForwardList -- ^ the 'WebBackForwardList'
webViewGetBackForwardList view = liftIO $
    withWebView view $ \ptr ->
        makeWebBackForwardList $
            {#call web_view_get_back_forward_list#} ptr

-- | Go to the specified 'WebHistoryItem' on a given 'WebView'.
webViewGoToBackForwardItem
    :: MonadIO m
    => WebView        -- ^ the 'WebView'
    -> WebHistoryItem -- ^ the 'WebHistoryItem'
    -> m Bool         -- ^ 'True' if loading of item is successful,
                      --   'False' if not
webViewGoToBackForwardItem view item = liftIO $
    withWebView view $ \ptr ->
        withWebHistoryItem item $ \iptr ->
            {#call web_view_go_to_back_forward_item#} ptr iptr >>=
                return . toBool

-- | Determines whether the given 'WebView' has a previous history item.
webViewCanGoBack
    :: MonadIO m
    => WebView -- ^ lookup history for this 'WebView'
    -> m Bool  -- ^ 'True' if able to move back, 'False' otherwise
webViewCanGoBack web_view = liftIO $
    withWebView web_view $ \ptr ->
        {#call web_view_can_go_back#} ptr >>=
            return . toBool

{- | Determines whether the given 'WebView' has a history item a given number
     of steps away. Negative values represent steps backward while positive
     values represent steps forward.
-}
webViewCanGoBackOrForward
    :: MonadIO m
    => WebView -- ^ lookup history for this 'WebView'
    -> Int     -- ^ the number of steps
    -> m Bool  -- ^ 'True' if able to move back or forward the given number of
               --   steps, 'False' otherwise
webViewCanGoBackOrForward web_view steps = liftIO $
    withWebView web_view $ \ptr ->
        {#call web_view_can_go_back_or_forward#}
            ptr (fromIntegral steps) >>=
                return . toBool

-- | Determines whether the given 'WebView' has a next history item.
webViewCanGoForward
    :: MonadIO m
    => WebView -- ^ lookup history for this 'WebView'
    -> m Bool  -- ^ 'True' if able to move forward, 'False' otherwise
webViewCanGoForward web_view = liftIO $
    withWebView web_view $ \ptr ->
        {#call web_view_can_go_forward#} ptr >>=
            return . toBool

-- | Loads the previous history item.
webViewGoBack
    :: MonadIO m
    => WebView -- ^ the 'WebView' that should go back
    -> m ()
webViewGoBack web_view = liftIO $
    withWebView web_view $ \ptr ->
        {#call web_view_go_back#} ptr

{- | Loads the history item that is the number of steps away from the current
     item. Negative values represent steps backward while positive values
     represent steps forward.
-}
webViewGoBackOrForward
    :: MonadIO m
    => WebView -- ^ the 'WebView' that should go back or forward
    -> Int     -- ^ number of steps
    -> m ()
webViewGoBackOrForward web_view steps = liftIO $
    withWebView web_view $ \ptr ->
        {#call web_view_go_back_or_forward#} ptr (fromIntegral steps)

-- | Loads the next history item.
webViewGoForward
    :: MonadIO m
    => WebView -- ^ the 'WebView' that should go forward
    -> m ()
webViewGoForward web_view = liftIO $
    withWebView web_view $ \ptr ->
        {#call web_view_go_forward#} ptr


-- | Stops any ongoing load in the 'WebView'.
webViewStopLoading
    :: MonadIO m
    => WebView -- ^ the 'WebView'
    -> m ()
webViewStopLoading web_view = liftIO $
    withWebView web_view $ \ptr ->
        {#call web_view_stop_loading#} ptr

{- DEPRECATED since 1.1.1
webViewOpen
    :: MonadIO m
    => WebView
    -> String
    -> m ()
webViewOpen web_view uri = liftIO $
    withCString uri $ \c_uri ->
        withWebView web_view $ \ptr ->
            {#call web_view_open#} ptr c_uri
-}

-- | Reloads the 'WebView' without using any cached data.
webViewReload
    :: MonadIO m
    => WebView -- ^ the 'WebView'
    -> m ()
webViewReload web_view = liftIO $
    withWebView web_view $ \ptr ->
        {#call web_view_reload#} ptr

-- | Reloads the 'WebView' without using any cached data.
webViewReloadBypassCache
    :: MonadIO m
    => WebView -- ^ the 'WebView' to reload
    -> m ()
webViewReloadBypassCache web_view = liftIO $
    withWebView web_view $ \ptr ->
        {#call web_view_reload_bypass_cache#} ptr

-- | Requests loading of the specified URI string.
webViewLoadUri
    :: MonadIO m
    => WebView -- ^ load in this 'WebView'
    -> String  -- ^ URI to load
    -> m ()
webViewLoadUri web_view uri = liftIO $
    withCString uri $ \c_uri ->
        withWebView web_view $ \ptr ->
            {#call web_view_load_uri#} ptr c_uri

webViewLoadString
    :: MonadIO m
    => WebView
    -> String
    -> String
    -> String
    -> String
    -> m ()
webViewLoadString web_view content mime_type encoding base_uri = liftIO $
    withCString content $ \c_content ->
        withCString mime_type $ \c_mime_type ->
            withCString encoding $ \c_encoding ->
                withCString base_uri $ \c_base_uri ->
                    withWebView web_view $ \ptr ->
                        {#call web_view_load_string#}
                            ptr c_content c_mime_type c_encoding c_base_uri

{- DEPRECATED
webViewLoadHtmlString
    :: MonadIO m
    => WebView
    -> String
    -> String
    -> m ()
webViewLoadHtmlString web_view content base_uri = liftIO $
    withCString content $ \c_content ->
        withCString base_uri $ \c_base_uri ->
            withWebView web_view $ \ptr ->
                {#call web_view_load_html_string#}
                    ptr c_content c_base_uri
-}

{- | Requests loading of the specified asynchronous client 'NetworkRequest'.

     Creates a provisional data source that will transition to a committed
     data source once any data has been received. Use 'webViewStopLoading'
     to stop the load.
-}
webViewLoadRequest
    :: MonadIO m
    => WebView        -- ^ load in this 'WebView'
    -> NetworkRequest -- ^ 'NetworkRequest' to load
    -> m ()
webViewLoadRequest web_view request = liftIO $
    withWebView web_view $ \wv_ptr ->
        withNetworkRequest request $ \r_ptr ->
            {#call web_view_load_request#} wv_ptr r_ptr

webViewSearchText
    :: MonadIO m
    => WebView
    -> String
    -> Bool
    -> Bool
    -> Bool
    -> m Bool
webViewSearchText web_view text case_sensitive forward wrap = liftIO $
    withCString text $ \c_text ->
        withWebView web_view $ \ptr ->
            {#call web_view_search_text#}
                ptr c_text (fromBool case_sensitive)
                (fromBool forward) (fromBool wrap) >>=
                    return . toBool

webViewMarkTextMatches
    :: MonadIO m
    => WebView
    -> String
    -> Bool
    -> Int
    -> m Int
webViewMarkTextMatches web_view string case_sensitive limit = liftIO $
    withCString string $ \c_string ->
        withWebView web_view $ \ptr ->
            {#call web_view_mark_text_matches#}
                ptr c_string (fromBool case_sensitive) (fromIntegral limit) >>=
                    return . fromIntegral

webViewSetHighlightTextMatches
    :: MonadIO m
    => WebView
    -> Bool
    -> m ()
webViewSetHighlightTextMatches web_view highlight = liftIO $
    withWebView web_view $ \ptr ->
        {#call web_view_set_highlight_text_matches#} ptr $ fromBool highlight

webViewUnmarkTextMatches
    :: MonadIO m
    => WebView
    -> m ()
webViewUnmarkTextMatches web_view = liftIO $
    withWebView web_view $ \ptr ->
        {#call web_view_unmark_text_matches#} ptr

webViewGetMainFrame
    :: MonadIO m
    => WebView
    -> m WebFrame
webViewGetMainFrame web_view = liftIO $
    withWebView web_view $ \ptr ->
        makeWebFrame $ {#call web_view_get_main_frame#} ptr

webViewGetFocusedFrame
    :: MonadIO m
    => WebView
    -> m WebFrame
webViewGetFocusedFrame web_view = liftIO $
    withWebView web_view $ \ptr ->
        makeWebFrame $ {#call web_view_get_focused_frame#} ptr

webViewExecuteScript
    :: MonadIO m
    => WebView
    -> String
    -> m ()
webViewExecuteScript web_view script = liftIO $
    withCString script $ \c_script ->
        withWebView web_view $ \ptr ->
            {#call web_view_execute_script#} ptr c_script

webViewCanCutClipboard
    :: MonadIO m
    => WebView
    -> m Bool
webViewCanCutClipboard web_view = liftIO $
    withWebView web_view $ \ptr ->
        {#call web_view_can_cut_clipboard#} ptr >>=
            return . toBool

webViewCanCopyClipboard
    :: MonadIO m
    => WebView
    -> m Bool
webViewCanCopyClipboard web_view = liftIO $
    withWebView web_view $ \ptr ->
        {#call web_view_can_copy_clipboard#} ptr >>=
            return . toBool

webViewCanPasteClipboard
    :: MonadIO m
    => WebView
    -> m Bool
webViewCanPasteClipboard web_view = liftIO $
    withWebView web_view $ \ptr ->
        {#call web_view_can_paste_clipboard#} ptr >>=
            return . toBool

-- | Cuts the current selection inside the 'WebView' to the clipboard.
webViewCutClipboard
    :: MonadIO m
    => WebView  -- ^ the 'WebView' to cut from
    -> m ()
webViewCutClipboard web_view = liftIO $
    withWebView web_view $ \ptr ->
        {#call web_view_cut_clipboard#} ptr

-- | Copies the current selection inside the 'WebView' to the clipboard.
webViewCopyClipboard
    :: MonadIO m
    => WebView -- ^ the 'WebView' to copy from
    -> m ()
webViewCopyClipboard web_view = liftIO $
    withWebView web_view $ \ptr ->
        {#call web_view_copy_clipboard#} ptr

-- | Pastes the current contents of the clipboard to the 'WebView'.
webViewPasteClipboard
    :: MonadIO m
    => WebView -- ^ the 'WebView' to paste to
    -> m ()
webViewPasteClipboard web_view = liftIO $
    withWebView web_view $ \ptr ->
        {#call web_view_paste_clipboard#} ptr

-- | Deletes the current selection inside the 'WebView'.
webViewDeleteSelection
    :: MonadIO m
    => WebView -- ^ the 'WebView' to delete from
    -> m ()
webViewDeleteSelection web_view = liftIO $
    withWebView web_view $ \ptr ->
        {#call web_view_delete_selection#} ptr

webViewHasSelection
    :: MonadIO m
    => WebView
    -> m Bool
webViewHasSelection web_view = liftIO $
    withWebView web_view $ \ptr ->
        {#call web_view_has_selection#} ptr >>=
            return . toBool

webViewSelectAll
    :: MonadIO m
    => WebView
    -> m ()
webViewSelectAll web_view = liftIO $
    withWebView web_view $ \ptr ->
        {#call web_view_select_all#} ptr

{- | Returns whether the user is allowed to edit the document.

     Returns 'True' if 'WebView' allows the user to edit the HTML document,
     'False' if it doesn't. You can change the document programmatically
     regardless of this setting.
-}
webViewGetEditable
    :: MonadIO m
    => WebView -- ^ the 'WebView'
    -> m Bool  -- ^ indicates the editable state
webViewGetEditable web_view = liftIO $
    withWebView web_view $ \ptr ->
        {#call web_view_get_editable#} ptr >>=
            return . toBool

webViewSetEditable
    :: MonadIO m
    => WebView
    -> Bool
    -> m ()
webViewSetEditable web_view flag = liftIO $
    withWebView web_view $ \ptr ->
        {#call web_view_set_editable#} ptr $ fromBool flag

-- TODO: Understand this stuff and check wether this does work as it should...
webViewGetCopyTargetList
    :: MonadIO m
    => WebView
    -> m TargetList
webViewGetCopyTargetList web_view = liftIO $
    withWebView web_view $ \ptr ->
        {#call web_view_get_copy_target_list#} ptr
            >>= mkTargetList . castPtr -- TODO: is this okay?

-- TODO: Understand this stuff and check wether this does work as it should...
webViewGetPasteTargetList
    :: MonadIO m
    => WebView
    -> m TargetList
webViewGetPasteTargetList web_view = liftIO $
    withWebView web_view $ \ptr ->
        {#call web_view_get_paste_target_list#} ptr
            >>= mkTargetList . castPtr -- TODO: is this okay?

webViewSetSettings
    :: MonadIO m
    => WebView
    -> WebSettings
    -> m ()
webViewSetSettings web_view settings = liftIO $
    withWebView web_view $ \vptr ->
        withWebSettings settings $ \sptr ->
            {#call web_view_set_settings#} vptr sptr

webViewGetSettings
    :: MonadIO m
    => WebView
    -> m WebSettings
webViewGetSettings web_view = liftIO $
    withWebView web_view $ \ptr ->
        makeWebSettings $ {#call web_view_get_settings#} ptr

webViewGetInspector
    :: MonadIO m
    => WebView
    -> m WebInspector
webViewGetInspector web_view = liftIO $
    withWebView web_view $ \ptr ->
        makeWebInspector $ {#call web_view_get_inspector#} ptr

{- | This functions returns whether or not a MIME type can be displayed using
     this view.
-}
webViewCanShowMimeType
    :: MonadIO m
    => WebView -- ^ the 'WebView' to check
    -> String  -- ^ the MIME type
    -> m Bool  -- ^ 'Bool' indicating if MIME type can be displayed
webViewCanShowMimeType web_view mime_type = liftIO $
    withCString mime_type $ \c_mime_type ->
        withWebView web_view $ \ptr ->
            {#call web_view_can_show_mime_type#} ptr c_mime_type >>=
                return . toBool

-- | Returns whether the 'WebView' has a transparent background.
webViewGetTransparent
    :: MonadIO m
    => WebView -- ^ the 'WebView'
    -> m Bool  -- ^ 'False' when the 'WebView' draws a solid background
               --   (the default), otherwise 'True'
webViewGetTransparent web_view = liftIO $
    withWebView web_view $ \ptr ->
        {#call web_view_get_transparent#} ptr >>=
            return . toBool

webViewSetTransparent
    :: MonadIO m
    => WebView
    -> Bool
    -> m ()
webViewSetTransparent web_view flag = liftIO $
    withWebView web_view $ \ptr ->
        {#call web_view_set_transparent#} ptr $
            fromBool flag

{- | Returns the zoom level of the given 'WebView', i.e. the factor by which
     elements in the page are scaled with respect to their original size. If
     the "full-content-zoom" property is set to 'False' (the default) the zoom
     level changes the text size, or if 'True', scales all elements in the
     page.
-}
webViewGetZoomLevel
    :: MonadIO m
    => WebView -- ^ the 'WebView'
    -> m Float -- ^ the zoom level
webViewGetZoomLevel web_view = liftIO $
    withWebView web_view $ \ptr ->
        {#call web_view_get_zoom_level#} ptr >>=
            return . realToFrac

webViewSetZoomLevel
    :: MonadIO m
    => WebView
    -> Float
    -> m ()
webViewSetZoomLevel web_view zoom_level = liftIO $
    withWebView web_view $ \ptr ->
        {#call web_view_set_zoom_level#} ptr $
            realToFrac zoom_level

webViewZoomIn
    :: MonadIO m
    => WebView
    -> m ()
webViewZoomIn web_view = liftIO $
    withWebView web_view $ \ptr ->
        {#call web_view_zoom_in#} ptr

webViewZoomOut
    :: MonadIO m
    => WebView
    -> m ()
webViewZoomOut web_view = liftIO $
    withWebView web_view $ \ptr ->
        {#call web_view_zoom_out#} ptr

webViewGetFullContentZoom
    :: MonadIO m
    => WebView
    -> m Bool
webViewGetFullContentZoom web_view = liftIO $
    withWebView web_view $ \ptr ->
        {#call web_view_get_full_content_zoom#} ptr >>=
            return . toBool

webViewSetFullContentZoom
    :: MonadIO m
    => WebView
    -> Bool
    -> m ()
webViewSetFullContentZoom web_view full_content_zoom = liftIO $
    withWebView web_view $ \ptr ->
        {#call web_view_set_full_content_zoom#} ptr $
            fromBool full_content_zoom

getDefaultSession
    :: MonadIO m
    => m SoupSession
getDefaultSession = liftIO $
    makeSoupSession {#call get_default_session#}

webViewGetEncoding
    :: MonadIO m
    => WebView
    -> m (Maybe String)
webViewGetEncoding web_view = liftIO $
    withWebView web_view $ \ptr ->
        {#call web_view_get_encoding#} ptr
            >>= maybePeek peekCString

webViewSetCustomEncoding
    :: MonadIO m
    => WebView
    -> String
    -> m ()
webViewSetCustomEncoding web_view encoding = liftIO $
    withCString encoding $ \c_encoding ->
        withWebView web_view $ \ptr ->
            {#call web_view_set_custom_encoding#} ptr c_encoding

{- | Returns the current encoding of the 'WebView', not the default-encoding
     of 'WebSettings'.
-}
webViewGetCustomEncoding
    :: MonadIO m
    => WebView          -- ^ the 'WebView'
    -> m (Maybe String) -- ^ 'Just' encoding if set, otherwise 'Nothing'
webViewGetCustomEncoding web_view = liftIO $
    withWebView web_view $ \ptr ->
        {#call web_view_get_custom_encoding#} ptr
            >>= maybePeek peekCString

webViewMoveCursor
    :: MonadIO m
    => WebView
    -> MovementStep
    -> Int
    -> m ()
webViewMoveCursor web_view movement_step count = liftIO $
    withWebView web_view $ \wvptr ->
        {#call web_view_move_cursor#} wvptr
            ((fromIntegral . fromEnum) movement_step) (fromIntegral count)

webViewGetLoadStatus
    :: MonadIO m
    => WebView
    -> m LoadStatus
webViewGetLoadStatus web_view = liftIO $
    withWebView web_view $ \ptr ->
        {#call web_view_get_load_status#} ptr >>=
            return . toEnum . fromIntegral

webViewGetProgress
    :: MonadIO m
    => WebView
    -> m Double
webViewGetProgress web_view = liftIO $
    withWebView web_view $ \ptr ->
        {#call web_view_get_progress#} ptr >>=
            return . realToFrac

webViewUndo
    :: MonadIO m
    => WebView
    -> m ()
webViewUndo web_view = liftIO $
    withWebView web_view $ \ptr ->
        {#call web_view_undo#} ptr

{- | Determines whether or not it is currently possible to undo the last editing
     command in the view.
-}
webViewCanUndo
    :: MonadIO m
    => WebView -- a 'WebView'
    -> m Bool  -- 'True' if possible to undo last editing command
webViewCanUndo web_view = liftIO $
    withWebView web_view $ \ptr ->
        {#call web_view_can_undo#} ptr >>=
            return . toBool

webViewRedo
    :: MonadIO m
    => WebView
    -> m ()
webViewRedo web_view = liftIO $
    withWebView web_view $ \ptr ->
        {#call web_view_redo#} ptr

{- | Determines whether or not it is currently possible to redo the last editing
     command in the view.
-}
webViewCanRedo
    :: MonadIO m
    => WebView -- ^ a 'WebView'
    -> m Bool  -- ^ 'True' if possible to redo last editing command
webViewCanRedo web_view = liftIO $
    withWebView web_view $ \ptr ->
        {#call web_view_can_redo#} ptr >>=
            return . toBool

webViewSetViewSourceMode
    :: MonadIO m
    => WebView
    -> Bool
    -> m ()
webViewSetViewSourceMode web_view source_mode = liftIO $
    withWebView web_view $ \ptr ->
        {#call web_view_set_view_source_mode#} ptr
            (fromBool source_mode)

webViewGetViewSourceMode
    :: MonadIO m
    => WebView
    -> m Bool
webViewGetViewSourceMode web_view = liftIO $
    withWebView web_view $ \ptr ->
        {#call web_view_get_view_source_mode#} ptr >>=
            return . toBool

{- TODO
WebKitHitTestResult* webkit_web_view_get_hit_test_result (WebKitWebView *webView, GdkEventButton *event);

webViewGetHitTestResult
    :: MonadIO m
    => WebView
    -> EventButton
    -> m HitTestResult
webViewGetHitTestResult web_view event_button = liftIO $
    withWebView web_view $ \wptr ->
        makeHitTestResult $ do
            {#call web_view_get_hit_test_result#} wptr ???

-}

-- Properties ------------------------------------------------------------------

webViewGetIconUri
    :: MonadIO m
    => WebView
    -> m String
webViewGetIconUri wv = liftIO $
    objectGetPropertyString
        "icon-uri" wv

webViewGetIMContext
    :: MonadIO m
    => WebView
    -> m IMContext
webViewGetIMContext web_view = liftIO $ do
    -- TODO this call a gtk function directly, this should happen in gtk2hs!
    imct <- {#call gtk_im_context_get_type#}
    objectGetPropertyGObject imct "im-context" web_view

webViewSetWindowFeatures
    :: MonadIO m
    => WebView
    -> WebWindowFeatures
    -> m ()
webViewSetWindowFeatures web_view web_window_features = liftIO $ do
    wwft <- webWindowFeaturesGetType
    objectSetPropertyGObject wwft "window-features" web_view web_window_features

webViewGetWindowFeatures
    :: MonadIO m
    => WebView
    -> m WebWindowFeatures
webViewGetWindowFeatures web_view = liftIO $ do
    wwft <- webWindowFeaturesGetType
    objectGetPropertyGObject wwft "window-features" web_view

-- Signals ---------------------------------------------------------------------

onWebViewCloseWebView, afterWebViewCloseWebView
    :: MonadIO m
    => WebView
    -> (WebView -> IO Bool)
    -> m (ConnectId WebView)
onWebViewCloseWebView web_view f = liftIO $
    on web_view (Signal (connectGeneric "close-web-view")) $
        \ webViewPtr -> do
            x1 <- makeWebView $ return webViewPtr
            f x1
afterWebViewCloseWebView web_view f = liftIO $
    on web_view (Signal (connectGeneric "close-web-view")) $
        \ webViewPtr -> do
            x1 <- makeWebView $ return webViewPtr
            f x1

onWebViewConsoleMessage, afterWebViewConsoleMessage
    :: MonadIO m
    => WebView
    -> (WebView -> String -> Int -> String -> IO Bool)
    -> m (ConnectId WebView)
onWebViewConsoleMessage web_view f = liftIO $
    on web_view (Signal (connectGeneric "console-message"))
        (webViewConsoleMessageWrapper f)
afterWebViewConsoleMessage web_view f = liftIO $
    after web_view (Signal (connectGeneric "console-message"))
        (webViewConsoleMessageWrapper f)

webViewConsoleMessageWrapper
    :: (WebView -> String -> Int -> String -> IO Bool)
    -> Ptr WebView
    -> CString
    -> CInt
    -> CString
    -> IO Bool
webViewConsoleMessageWrapper f webViewPtr message line source_id = do
    x1 <- makeWebView $ return webViewPtr
    x2 <- peekCString message
    x4 <- peekCString source_id
    f x1 x2 (fromIntegral line) x4

onWebViewCopyClipboard, afterWebViewCopyClipboard
    :: MonadIO m
    => WebView
    -> IO ()
    -> m (ConnectId WebView)
onWebViewCopyClipboard wv h = liftIO $
    connect_NONE__NONE "copy-clipboard" False wv h
afterWebViewCopyClipboard wv h = liftIO $
    connect_NONE__NONE "copy-clipboard" True wv h

{- TODO "create-plugin-widget" : GtkWidget* user_function (WebKitWebView *web_view, gchar *mime_type, gchar *uri, GHashTable *param, gpointer user_data) : Run Last / Action -}

onWebViewCreateWebView, afterWebViewCreateWebView
    :: MonadIO m
    => WebView
    -> (WebView -> WebFrame -> IO WebView)
    -> m (ConnectId WebView)
onWebViewCreateWebView web_view f = liftIO $
    on web_view (Signal (connectGeneric "create-web-view"))
        (webViewCreateWebViewWrapper f)
afterWebViewCreateWebView web_view f = liftIO $
    after web_view (Signal (connectGeneric "create-web-view"))
        (webViewCreateWebViewWrapper f)

webViewCreateWebViewWrapper
    :: (WebView -> WebFrame -> IO WebView)
    -> Ptr WebView
    -> Ptr WebFrame
    -> IO WebView
webViewCreateWebViewWrapper f webViewPtr webFramePtr = do
    x1 <- makeWebView $ return webViewPtr
    x2 <- makeWebFrame $ return webFramePtr
    f x1 x2

onWebViewCutClipboard, afterWebViewCutClipboard
    :: MonadIO m
    => WebView
    -> IO ()
    -> m (ConnectId WebView)
onWebViewCutClipboard wv h = liftIO $
    connect_NONE__NONE "cut-clipboard" False wv h
afterWebViewCutClipboard wv h = liftIO $
    connect_NONE__NONE "cut-clipboard" True wv h

onWebViewDatabaseQuotaExceeded, afterWebViewDatabaseQuotaExceeded
    :: MonadIO m
    => WebView
    -> (WebView -> WebFrame -> WebDatabase -> IO ())
    -> m (ConnectId WebView)
onWebViewDatabaseQuotaExceeded web_view f = liftIO $
    on web_view (Signal (connectGeneric "database-quota-exceeded"))
        (webViewDatabaseQuotaExceededWrapper f)
afterWebViewDatabaseQuotaExceeded web_view f = liftIO $
    after web_view (Signal (connectGeneric "database-quota-exceeded"))
        (webViewDatabaseQuotaExceededWrapper f)

webViewDatabaseQuotaExceededWrapper
    :: (WebView -> WebFrame -> WebDatabase -> IO ())
    -> Ptr WebView
    -> Ptr WebFrame
    -> Ptr WebDatabase
    -> IO ()
webViewDatabaseQuotaExceededWrapper f webViewPtr webFramePtr databasePtr = do
    x1 <- makeWebView $ return webViewPtr
    x2 <- makeWebFrame $ return webFramePtr
    x3 <- makeWebDatabase $ return databasePtr
    f x1 x2 x3

onWebViewDocumentLoadFinished, afterWebViewDocumentLoadFinished
    :: MonadIO m
    => WebView
    -> (WebView -> WebFrame -> IO ())
    -> m (ConnectId WebView)
onWebViewDocumentLoadFinished web_view f = liftIO $
    on web_view (Signal (connectGeneric "document-load-finished"))
        (webViewDocumentLoadFinishedWrapper f)
afterWebViewDocumentLoadFinished web_view f = liftIO $
    after web_view (Signal (connectGeneric "document-load-finished"))
        (webViewDocumentLoadFinishedWrapper f)

webViewDocumentLoadFinishedWrapper
    :: (WebView -> WebFrame -> IO ())
    -> Ptr WebView
    -> Ptr WebFrame
    -> IO ()
webViewDocumentLoadFinishedWrapper f webViewPtr webFramePtr = do
    x1 <- makeWebView $ return webViewPtr
    x2 <- makeWebFrame $ return webFramePtr
    f x1 x2

onWebViewDownloadRequested, afterWebViewDownloadRequested
    :: MonadIO m
    => WebView
    -> (WebView -> Download -> IO Bool)
    -> m (ConnectId WebView)
onWebViewDownloadRequested web_view f = liftIO $
    on web_view (Signal (connectGeneric "download-requested"))
        (webViewDownloadRequestedWrapper f)
afterWebViewDownloadRequested web_view f = liftIO $
    after web_view (Signal (connectGeneric "download-requested"))
        (webViewDownloadRequestedWrapper f)

webViewDownloadRequestedWrapper
    :: (WebView -> Download -> IO Bool)
    -> Ptr WebView
    -> Ptr Download
    -> IO Bool
webViewDownloadRequestedWrapper f webViewPtr downloadPtr = do
    x1 <- makeWebView $ return webViewPtr
    x2 <- makeDownload $ return downloadPtr
    f x1 x2

onWebViewHoveringOverLink, afterWebViewHoveringOverLink
    :: MonadIO m
    => WebView
    -> (String -> String -> IO ())
    -> m (ConnectId WebView)
onWebViewHoveringOverLink wv h = liftIO $
    connect_STRING_STRING__NONE "hovering-over-link" False wv h
afterWebViewHoveringOverLink wv h = liftIO $
    connect_STRING_STRING__NONE "hovering-over-link" True wv h

onWebViewIconLoaded, afterWebViewIconLoaded
    :: MonadIO m
    => WebView
    -> IO ()
    -> m (ConnectId WebView)
onWebViewIconLoaded wv h = liftIO $
    connect_NONE__NONE "icon-loaded" False wv h
afterWebViewIconLoaded wv h = liftIO $
    connect_NONE__NONE "icon-loaded" True wv h

{- DEPRECATED
onWebViewLoadCommitted, afterWebViewLoadCommitted
    :: MonadIO m
    => WebView
    -> (WebFrame -> IO ())
    -> m (ConnectId WebView)
onWebViewLoadCommitted = liftIO $
    connect_OBJECT__NONE "load-committed" False
afterWebViewLoadCommitted = liftIO $
    connect_OBJECT__NONE "load-committed" True
-}

{- TODO
"load-error" : gboolean user_function (WebKitWebView *web_view, WebKitWebFrame *web_frame, gchar *uri, gpointer web_error, gpointer user_data) : Run Last
"move-cursor" : gboolean user_function (WebKitWebView *web_view, GtkMovementStep step, gint count, gpointer user_data) : Run Last / Action
"new-window-policy-decision-requested" : gboolean user_function (WebKitWebView *web_view, WebKitWebFrame *frame, WebKitNetworkRequest *request, WebKitWebNavigationAction *navigation_action, WebKitWebPolicyDecision *policy_decision, gpointer user_data) : Run Last
-}

onWebViewMimeTypePolicyDecisionRequested, afterWebViewMimeTypePolicyDecisionRequested
    :: MonadIO m
    => WebView
    -> (WebView -> WebFrame -> NetworkRequest -> String -> WebPolicyDecision -> IO Bool)
    -> m (ConnectId WebView)
onWebViewMimeTypePolicyDecisionRequested web_view f = liftIO $
    on web_view (Signal (connectGeneric "mime-type-policy-decision-requested"))
        (webViewMimeTypePolicyDecisionRequestedWrapper f)
afterWebViewMimeTypePolicyDecisionRequested web_view f = liftIO $
    after web_view (Signal (connectGeneric "mime-type-policy-decision-requested"))
        (webViewMimeTypePolicyDecisionRequestedWrapper f)

webViewMimeTypePolicyDecisionRequestedWrapper
    :: (WebView -> WebFrame -> NetworkRequest -> String -> WebPolicyDecision -> IO Bool)
    -> Ptr WebView
    -> Ptr WebFrame
    -> Ptr NetworkRequest
    -> CString
    -> Ptr WebPolicyDecision
    -> IO Bool
webViewMimeTypePolicyDecisionRequestedWrapper f z1 z2 z3 z4 z5 = do
    x1 <- makeWebView $ return z1
    x2 <- makeWebFrame $ return z2
    x3 <- makeNetworkRequest $ return z3
    x4 <- peekCString z4
    x5 <- makeWebPolicyDecision $ return z5
    f x1 x2 x3 x4 x5


onWebViewNavigationPolicyDecisionRequested, afterWebViewNavigationPolicyDecisionRequested
    :: MonadIO m
    => WebView
    -> (WebView -> WebFrame -> NetworkRequest -> WebNavigationAction -> WebPolicyDecision -> IO ())
    -> m (ConnectId WebView)
onWebViewNavigationPolicyDecisionRequested web_view f = liftIO $
    on web_view (Signal (connectGeneric "navigation-policy-decision-requested"))
        (webViewNavigationPolicyDecisionRequestedWrapper f)
afterWebViewNavigationPolicyDecisionRequested web_view f = liftIO $
    after web_view (Signal (connectGeneric "navigation-policy-decision-requested"))
        (webViewNavigationPolicyDecisionRequestedWrapper f)

webViewNavigationPolicyDecisionRequestedWrapper
    :: (WebView -> WebFrame -> NetworkRequest -> WebNavigationAction -> WebPolicyDecision -> IO ())
    -> Ptr WebView
    -> Ptr WebFrame
    -> Ptr NetworkRequest
    -> Ptr WebNavigationAction
    -> Ptr WebPolicyDecision
    -> IO ()
webViewNavigationPolicyDecisionRequestedWrapper f x1 x2 x3 x4 x5 = do
    y1 <- makeWebView $ return x1
    y2 <- makeWebFrame $ return x2
    y3 <- makeNetworkRequest $ return x3
    y4 <- makeWebNavigationAction $ return x4
    y5 <- makeWebPolicyDecision $ return x5
    f y1 y2 y3 y4 y5

onWebViewPasteClipboard, afterWebViewPasteClipboard
    :: MonadIO m
    => WebView
    -> IO ()
    -> m (ConnectId WebView)
onWebViewPasteClipboard wv h = liftIO $
    connect_NONE__NONE "paste-clipboard" False wv h
afterWebViewPasteClipboard wv h = liftIO $
    connect_NONE__NONE "paste-clipboard" True wv h

onWebViewPopulatePopup, afterWebViewPopulatePopup
    :: MonadIO m
    => WebView
    -> (WebView -> Menu -> IO ())
    -> m (ConnectId WebView)
onWebViewPopulatePopup web_view f = liftIO $
    on web_view (Signal (connectGeneric "populater-popup"))
        (webViewPopulatePopupWrapper f)
afterWebViewPopulatePopup web_view f = liftIO $
    after web_view (Signal (connectGeneric "populater-popup"))
        (webViewPopulatePopupWrapper f)

webViewPopulatePopupWrapper
    :: (WebView -> Menu -> IO ())
    -> Ptr WebView
    -> Ptr Menu
    -> IO ()
webViewPopulatePopupWrapper f webViewPtr menuPtr = do
    x1 <- makeWebView $ return webViewPtr
    x2 <- makeNewObject (Menu, objectUnref) $ return menuPtr
    f x1 x2

onWebViewPrintRequested, afterWebViewPrintRequested
    :: MonadIO m
    => WebView
    -> (WebView -> WebFrame -> IO Bool)
    -> m (ConnectId WebView)
onWebViewPrintRequested web_view f = liftIO $
    on web_view (Signal (connectGeneric "print-requested"))
    (onWebViewPrintRequestedWrapper f)
afterWebViewPrintRequested web_view f = liftIO $
    after web_view (Signal (connectGeneric "print-requested"))
    (onWebViewPrintRequestedWrapper f)

onWebViewPrintRequestedWrapper
    :: (WebView -> WebFrame -> IO Bool)
    -> Ptr WebView
    -> Ptr WebFrame
    -> IO Bool
onWebViewPrintRequestedWrapper f webViewPtr webFramePtr = do
    x1 <- makeWebView $ return webViewPtr
    x2 <- makeWebFrame $ return webFramePtr
    f x1 x2

onWebViewRedo, afterWebViewRedo
    :: MonadIO m
    => WebView
    -> (WebView -> IO ())
    -> m (ConnectId WebView)
onWebViewRedo web_view f = liftIO $
    on web_view (Signal (connectGeneric "redo")) $ \ wv -> do
        x <-  makeWebView $ return wv
        f x
afterWebViewRedo web_view f = liftIO $
    after web_view (Signal (connectGeneric "redo")) $ \ wv -> do
        x <-  makeWebView $ return wv
        f x
{- | Emitted when a request is about to be sent. You can modify the request
     while handling this signal. You can set the URI in the 'NetworkRequest'
     object itself, and add/remove/replace headers using the 'Message' object it
     carries, if it is present. See 'networkRequestGetMessage'. Setting the
     request URI to "about:blank" will effectively cause the request to load nothing,
     and can be used to disable the loading of specific resources.

     Notice that information about an eventual redirect is available in response's
     'Message', not in the 'Message' carried by the request. If response is 'Nothing',
     then this is not a redirected request.

     The 'Resource' object will be the same throughout all the lifetime of the
     resource, but the contents may change from inbetween signal emissions.
-}
onWebViewResourceRequestStarting,afterWebViewResourceRequestStarting
    :: MonadIO m
    => WebView -- ^ the 'WebView' to bind on
    -> (WebView -> WebFrame -> WebResource -> NetworkRequest -> Maybe NetworkResponse -> IO ())
    -> m (ConnectId WebView)
onWebViewResourceRequestStarting wV f = liftIO $
    on wV
        (Signal (connectGeneric "resource-request-starting"))
            (webViewResourceRequestStartingWrapper f)
afterWebViewResourceRequestStarting wV f = liftIO $
    after wV
        (Signal (connectGeneric "resource-request-starting"))
            (webViewResourceRequestStartingWrapper f)

webViewResourceRequestStartingWrapper
    :: (WebView -> WebFrame -> WebResource -> NetworkRequest -> Maybe NetworkResponse -> IO ())
    -> Ptr WebView
    -> Ptr WebFrame
    -> Ptr WebResource
    -> Ptr NetworkRequest
    -> Ptr NetworkResponse
    -> IO ()
webViewResourceRequestStartingWrapper
    f web_view web_frame web_resource network_request network_response = do
    x1 <- makeWebView $ return web_view
    x2 <- makeWebFrame $ return web_frame
    x3 <- makeWebResource $ return web_resource
    x4 <- makeNetworkRequest $ return network_request
    if nullPtr /= network_response then do
        x5 <- makeNetworkResponse  $ return network_response
        f x1 x2 x3 x4 (Just x5)
      else
        f x1 x2 x3 x4 Nothing

onWebViewScriptPrompt, afterWebViewScriptPrompt
    :: MonadIO m
    => WebView
    -> (WebView -> WebFrame -> String -> String -> String -> IO Bool)
    -> m (ConnectId WebView)
onWebViewScriptPrompt web_view f = liftIO $
    on web_view (Signal (connectGeneric "script-prompt"))
        (webViewScriptPromptWrapper f)
afterWebViewScriptPrompt web_view f = liftIO $
    after web_view (Signal (connectGeneric "script-prompt"))
        (webViewScriptPromptWrapper f)

webViewScriptPromptWrapper
    :: (WebView -> WebFrame -> String -> String -> String -> IO Bool)
    -> Ptr WebView
    -> Ptr WebFrame
    -> CString
    -> CString
    -> CString
    -> IO Bool
webViewScriptPromptWrapper f webViewPtr webFramePtr message def text = do
    x1 <- makeWebView $ return webViewPtr
    x2 <- makeWebFrame $ return webFramePtr
    x3 <- peekCString message
    x4 <- peekCString def
    x5 <- peekCString text
    f x1 x2 x3 x4 x5

onWebViewScriptAlert, afterWebViewScriptAlert
    :: MonadIO m
    => WebView
    -> (WebView -> WebFrame -> String -> IO Bool)
    -> m (ConnectId WebView)
onWebViewScriptAlert web_view f = liftIO $
    on web_view (Signal (connectGeneric "script-alert"))
        (webViewScriptAlertWrapper f)
afterWebViewScriptAlert web_view f = liftIO $
    after web_view (Signal (connectGeneric "script-alert"))
        (webViewScriptAlertWrapper f)

webViewScriptAlertWrapper
    :: (WebView -> WebFrame -> String -> IO Bool)
    -> Ptr WebView
    -> Ptr WebFrame
    -> CString
    -> IO Bool
webViewScriptAlertWrapper f webViewPtr webFramePtr message = do
    x1 <- makeWebView $ return webViewPtr
    x2 <- makeWebFrame $ return webFramePtr
    x3 <- peekCString message
    f x1 x2 x3

onWebViewScriptConfirm, afterWebViewScriptConfirm
    :: MonadIO m
    => WebView
    -> (WebView -> WebFrame -> String -> Bool -> IO Bool)
    -> m (ConnectId WebView)
onWebViewScriptConfirm web_view f = liftIO $
    on web_view (Signal (connectGeneric "script-confirm"))
        (webViewScriptConfirmWrapper f)
afterWebViewScriptConfirm web_view f = liftIO $
    after web_view (Signal (connectGeneric "script-confirm"))
        (webViewScriptConfirmWrapper f)

webViewScriptConfirmWrapper
    :: (WebView -> WebFrame -> String -> Bool -> IO Bool)
    -> Ptr WebView
    -> Ptr WebFrame
    -> CString
    -> Bool
    -> IO Bool
webViewScriptConfirmWrapper f webViewPtr webFramePtr message confirm = do
    x1 <- makeWebView $ return webViewPtr
    x2 <- makeWebFrame $ return webFramePtr
    x3 <- peekCString message
    f x1 x2 x3 confirm

onWebViewSelectAll, afterWebViewSelectAll
    :: MonadIO m
    => WebView
    -> IO ()
    -> m (ConnectId WebView)
onWebViewSelectAll web_view f = liftIO $
    on web_view (Signal (connectGeneric "select-all")) (\_ -> f)
afterWebViewSelectAll web_view f = liftIO $
    after web_view (Signal (connectGeneric "select-all")) (\_ -> f)

onWebViewSelectionChanged, afterWebViewSelectionChanged
    :: MonadIO m
    => WebView
    -> IO ()
    -> m (ConnectId WebView)
onWebViewSelectionChanged wv h = liftIO $
    connect_NONE__NONE "selection-changed" False wv h
afterWebViewSelectionChanged wv h = liftIO $
    connect_NONE__NONE "selection-changed" True wv h

onWebViewSetCrollAdjustments,afterWebViewSetCrollAdjustments
    :: MonadIO m
    => WebView
    -> (WebView -> Adjustment -> Adjustment -> IO ())
    -> m (ConnectId WebView)
onWebViewSetCrollAdjustments web_view f = liftIO $
    on web_view (Signal (connectGeneric "set-scroll-adjustments"))
        (webViewSetCrollAdjustmentsWrapper f)
afterWebViewSetCrollAdjustments web_view f = liftIO $
    after web_view (Signal (connectGeneric "set-scroll-adjustments"))
        (webViewSetCrollAdjustmentsWrapper f)

webViewSetCrollAdjustmentsWrapper
    :: (WebView -> Adjustment -> Adjustment -> IO ())
    -> Ptr WebView
    -> Ptr Adjustment
    -> Ptr Adjustment
    -> IO ()
webViewSetCrollAdjustmentsWrapper f webViewPtr gtkAdj1Ptr gtkAdj2Ptr = do
    x1 <- makeWebView $ return webViewPtr
    x2 <- makeNewObject (Adjustment, objectUnref)  $ return gtkAdj1Ptr
    x3 <- makeNewObject (Adjustment, objectUnref)  $ return gtkAdj2Ptr
    f x1 x2 x3

onWebViewStatusbarTextChanged, afterWebViewStatusbarTextChanged
    :: MonadIO m
    => WebView
    -> (String -> IO ())
    -> m (ConnectId WebView)
onWebViewStatusbarTextChanged web_view f = liftIO $
    on web_view (Signal (connectGeneric "status-bar-text-changed")) (\_ -> f)
afterWebViewStatusbarTextChanged web_view f = liftIO $
     after web_view (Signal (connectGeneric "status-bar-text-changed")) (\_ -> f)

{- DEPRECATED
onWebViewTitleChanged, afterWebViewTitleChanged
    :: MonadIO m
    =>
    WebView -> (WebFrame -> String -> IO ()) -> IO (ConnectId WebView)
onWebViewTitleChanged = liftIO $
    connect_OBJECT_STRING__NONE "title-changed" False
afterWebViewTitleChanged = liftIO $
    connect_OBJECT_STRING__NONE "title-changed" True
-}

{- TODO
"web-view-ready" : gboolean user_function (WebKitWebView *web_view, gpointer user_data) : Run Last
"window-object-cleared" : void user_function (WebKitWebView *web_view, WebKitWebFrame *frame, gpointer context, gpointer arg3, gpointer user_data) : Run Last / Action
-}

onWebViewReady,afterWebViewReady
    :: MonadIO m
    => WebView
    -> (WebView -> IO ())
    -> m (ConnectId WebView)
onWebViewReady web_view f = liftIO $
    on web_view (Signal (connectGeneric "web-view-ready")) $ \ wv -> do
        x <-  makeWebView $ return wv
        f x
afterWebViewReady web_view f = liftIO $
    after web_view (Signal (connectGeneric "web-view-ready")) $ \ wv -> do
        x <-  makeWebView $ return wv
        f x

onWebViewUndo, afterWebViewUndo
    :: MonadIO m
    => WebView
    -> (WebView -> IO ())
    -> m (ConnectId WebView)
onWebViewUndo web_view f = liftIO $
    on web_view (Signal (connectGeneric "undo")) $ \ wv -> do
        x <-  makeWebView $ return wv
        f x
afterWebViewUndo web_view f = liftIO $
    after web_view (Signal (connectGeneric "undo")) $ \ wv -> do
        x <-  makeWebView $ return wv
        f x

