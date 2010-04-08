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

    -- , webViewGetImContext -- TODO

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
 
    ) where
 
#include <webkit/webkitwebview.h>

import Foreign.C
import Control.Monad

import System.Glib.Signals
import System.Glib.GObject
import System.Glib.FFI
import System.Glib.GType
import System.Glib.Properties

import Graphics.UI.Gtk.Types
import Graphics.UI.Gtk.Signals
import Graphics.UI.Gtk.General.DNDTypes
import Graphics.UI.Gtk.General.Enums 
    ( MovementStep )
import Graphics.UI.Gtk.Abstract.Object  
    ( makeNewObject )

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

webViewGetType :: IO GType
webViewGetType =
    {#call web_view_get_type#}

webViewNew :: IO WebView
webViewNew = do
    ptr <- {#call web_view_new#}
    let ptr' = castPtr ptr
    makeWebView (return ptr')

webViewGetTitle :: WebView -> IO (Maybe String)
webViewGetTitle web_view =
    withWebView web_view $ \ptr ->
        {#call web_view_get_title#} ptr
            >>= maybePeek peekCString

webViewGetUri :: WebView -> IO (Maybe String)
webViewGetUri web_view = do
    withWebView web_view $ \ptr ->
        {#call web_view_get_uri#} ptr
            >>= maybePeek peekCString

webViewSetMaintainsBackForwardList :: WebView -> Bool -> IO ()
webViewSetMaintainsBackForwardList web_view flag =
    withWebView web_view $ \ptr ->
        {#call web_view_set_maintains_back_forward_list#}
            ptr $ fromBool flag

-- | Returns the 'WebBackForwardList' for the given 'WebView'.
webViewGetBackForwardList
    :: WebView               -- ^ the 'WebView'
    -> IO WebBackForwardList -- ^ the 'WebBackForwardList'
webViewGetBackForwardList view =
    withWebView view $ \ptr ->
        makeWebBackForwardList $
            {#call web_view_get_back_forward_list#} ptr

webViewGoToBackForwardItem :: WebView -> WebHistoryItem -> IO Bool
webViewGoToBackForwardItem view item =
    withWebView view $ \ptr ->
        withWebHistoryItem item $ \iptr ->
            liftM toBool $
                {#call web_view_go_to_back_forward_item#} ptr iptr 
 
-- | Determines whether the given 'WebView' has a previous history item.
webViewCanGoBack
    :: WebView -- ^ lookup history for this 'WebView'
    -> IO Bool -- ^ 'True' if able to move back, 'False' otherwise
webViewCanGoBack web_view = do
    withWebView web_view $ \ptr ->
        liftM toBool $ {#call web_view_can_go_back#} ptr

{- | Determines whether the given 'WebView' has a history item a given number
     of steps away. Negative values represent steps backward while positive
     values represent steps forward.
-}
webViewCanGoBackOrForward
    :: WebView -- ^ lookup history for this 'WebView'
    -> Int     -- ^ the number of steps 
    -> IO Bool -- ^ 'True' if able to move back or forward the given number of
               --   steps, 'False' otherwise 
webViewCanGoBackOrForward web_view steps = do
    withWebView web_view $ \ptr ->
        liftM toBool $
            {#call web_view_can_go_back_or_forward#}
                ptr (fromIntegral steps)

-- | Determines whether the given 'WebView' has a next history item.
webViewCanGoForward
    :: WebView -- ^ lookup history for this 'WebView'
    -> IO Bool -- ^ 'True' if able to move forward, 'False' otherwise
webViewCanGoForward web_view = do
    withWebView web_view $ \ptr ->
        liftM toBool $ {#call web_view_can_go_forward#} ptr

-- | Loads the previous history item.
webViewGoBack
    :: WebView -- ^ the 'WebView' that should go back
    -> IO ()
webViewGoBack web_view =
    withWebView web_view $ \ptr ->
        {#call web_view_go_back#} ptr

{- | Loads the history item that is the number of steps away from the current
     item. Negative values represent steps backward while positive values
     represent steps forward.
-}
webViewGoBackOrForward
    :: WebView -- ^ the 'WebView' that should go back or forward
    -> Int     -- ^ number of steps
    -> IO ()
webViewGoBackOrForward web_view steps =
    withWebView web_view $ \ptr ->
        {#call web_view_go_back_or_forward#} ptr (fromIntegral steps)

-- | Loads the next history item.
webViewGoForward
    :: WebView -- ^ the 'WebView' that should go forward
    -> IO ()
webViewGoForward web_view =
    withWebView web_view $ \ptr ->
        {#call web_view_go_forward#} ptr

webViewStopLoading :: WebView -> IO ()
webViewStopLoading web_view =
    withWebView web_view $ \ptr ->
        {#call web_view_stop_loading#} ptr

{- DEPRECATED since 1.1.1
webViewOpen :: WebView -> String -> IO ()
webViewOpen web_view uri = do
    withCString uri $ \c_uri ->
        withWebView web_view $ \ptr ->
            {#call web_view_open#} ptr c_uri
-}

webViewReload :: WebView -> IO ()
webViewReload web_view =
    withWebView web_view $ \ptr ->
        {#call web_view_reload#} ptr

webViewReloadBypassCache :: WebView -> IO ()
webViewReloadBypassCache web_view =
    withWebView web_view $ \ptr ->
        {#call web_view_reload_bypass_cache#} ptr

-- | Requests loading of the specified URI string.
webViewLoadUri
    :: WebView -- ^ load in this 'WebView'
    -> String  -- ^ URI to load
    -> IO ()
webViewLoadUri web_view uri = do
    withCString uri $ \c_uri ->
        withWebView web_view $ \ptr ->
            {#call web_view_load_uri#} ptr c_uri

webViewLoadString :: WebView -> String -> String -> String -> String -> IO ()
webViewLoadString web_view content mime_type encoding base_uri = do
    withCString content $ \c_content ->
        withCString mime_type $ \c_mime_type ->
            withCString encoding $ \c_encoding ->
                withCString base_uri $ \c_base_uri ->
                    withWebView web_view $ \ptr ->
                        {#call web_view_load_string#}
                            ptr c_content c_mime_type c_encoding c_base_uri

{- DEPRECATED
webViewLoadHtmlString :: WebView -> String -> String -> IO ()
webViewLoadHtmlString web_view content base_uri = do
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
    :: WebView        -- ^ load in this 'WebView'
    -> NetworkRequest -- ^ 'NetworkRequest' to load
    -> IO ()
webViewLoadRequest web_view request =
    withWebView web_view $ \wv_ptr ->
        withNetworkRequest request $ \r_ptr ->
            {#call web_view_load_request#} wv_ptr r_ptr

webViewSearchText :: WebView -> String -> Bool -> Bool -> Bool -> IO Bool
webViewSearchText web_view text case_sensitive forward wrap =
    withCString text $ \c_text ->
        withWebView web_view $ \ptr ->
            liftM toBool $
                {#call web_view_search_text#}
                    ptr c_text (fromBool case_sensitive)
                    (fromBool forward) (fromBool wrap)

webViewMarkTextMatches :: WebView -> String -> Bool -> Int -> IO Int
webViewMarkTextMatches web_view string case_sensitive limit =
    withCString string $ \c_string ->
        withWebView web_view $ \ptr ->
            liftM fromIntegral $
                {#call web_view_mark_text_matches#}
                    ptr c_string (fromBool case_sensitive) (fromIntegral limit)

webViewSetHighlightTextMatches :: WebView -> Bool -> IO ()
webViewSetHighlightTextMatches web_view highlight =
    withWebView web_view $ \ptr ->
        {#call web_view_set_highlight_text_matches#} ptr $ fromBool highlight

webViewUnmarkTextMatches :: WebView -> IO ()
webViewUnmarkTextMatches web_view =
    withWebView web_view $ \ptr ->
        {#call web_view_unmark_text_matches#} ptr

webViewGetMainFrame :: WebView -> IO WebFrame
webViewGetMainFrame web_view =
    withWebView web_view $ \ptr ->
        makeWebFrame $ {#call web_view_get_main_frame#} ptr

webViewGetFocusedFrame :: WebView -> IO WebFrame
webViewGetFocusedFrame web_view =
    withWebView web_view $ \ptr ->
        makeWebFrame $ {#call web_view_get_focused_frame#} ptr

webViewExecuteScript :: WebView -> String -> IO ()
webViewExecuteScript web_view script = do
    withCString script $ \c_script ->
        withWebView web_view $ \ptr ->
            {#call web_view_execute_script#} ptr c_script

webViewCanCutClipboard :: WebView -> IO Bool
webViewCanCutClipboard web_view = do
    withWebView web_view $ \ptr ->
        liftM toBool $ {#call web_view_can_cut_clipboard#} ptr

webViewCanCopyClipboard :: WebView -> IO Bool
webViewCanCopyClipboard web_view = do
    withWebView web_view $ \ptr ->
        liftM toBool $ {#call web_view_can_copy_clipboard#} ptr

webViewCanPasteClipboard :: WebView -> IO Bool
webViewCanPasteClipboard web_view = do
    withWebView web_view $ \ptr ->
        liftM toBool $ {#call web_view_can_paste_clipboard#} ptr

-- | Cuts the current selection inside the 'WebView' to the clipboard.
webViewCutClipboard
    :: WebView  -- ^ the 'WebView' to cut from
    -> IO ()
webViewCutClipboard web_view =
    withWebView web_view $ \ptr ->
        {#call web_view_cut_clipboard#} ptr

-- | Copies the current selection inside the 'WebView' to the clipboard.
webViewCopyClipboard
    :: WebView -- ^ the 'WebView' to copy from
    -> IO ()
webViewCopyClipboard web_view =
    withWebView web_view $ \ptr ->
        {#call web_view_copy_clipboard#} ptr

-- | Pastes the current contents of the clipboard to the 'WebView'.
webViewPasteClipboard
    :: WebView -- ^ the 'WebView' to paste to
    -> IO ()
webViewPasteClipboard web_view =
    withWebView web_view $ \ptr ->
        {#call web_view_paste_clipboard#} ptr

-- | Deletes the current selection inside the 'WebView'.
webViewDeleteSelection
    :: WebView -- ^ the 'WebView' to delete from
    -> IO ()
webViewDeleteSelection web_view =
    withWebView web_view $ \ptr ->
        {#call web_view_delete_selection#} ptr

webViewHasSelection :: WebView -> IO Bool
webViewHasSelection web_view = do
    withWebView web_view $ \ptr ->
        liftM toBool $ {#call web_view_has_selection#} ptr

webViewSelectAll :: WebView -> IO ()
webViewSelectAll web_view =
    withWebView web_view $ \ptr ->
        {#call web_view_select_all#} ptr

{- | Returns whether the user is allowed to edit the document.

     Returns 'True' if 'WebView' allows the user to edit the HTML document,
     'False' if it doesn't. You can change the document programmatically
     regardless of this setting.
-}
webViewGetEditable
    :: WebView -- ^ the 'WebView'
    -> IO Bool -- ^ indicates the editable state
webViewGetEditable web_view = do
    withWebView web_view $ \ptr ->
        liftM toBool $ {#call web_view_get_editable#} ptr

webViewSetEditable :: WebView -> Bool -> IO ()
webViewSetEditable web_view flag =
    withWebView web_view $ \ptr ->
        {#call web_view_set_editable#} ptr $ fromBool flag

-- TODO: Understand this stuff and check wether this does work as it should...
webViewGetCopyTargetList :: WebView -> IO TargetList
webViewGetCopyTargetList web_view =
    withWebView web_view $ \ptr ->
        {#call web_view_get_copy_target_list#} ptr
            >>= mkTargetList . castPtr -- TODO: is this okay?

-- TODO: Understand this stuff and check wether this does work as it should...
webViewGetPasteTargetList :: WebView -> IO TargetList
webViewGetPasteTargetList web_view =
    withWebView web_view $ \ptr ->
        {#call web_view_get_paste_target_list#} ptr
            >>= mkTargetList . castPtr -- TODO: is this okay?

webViewSetSettings :: WebView -> WebSettings -> IO ()
webViewSetSettings web_view settings = 
    withWebView web_view $ \vptr ->
        withWebSettings settings $ \sptr ->
            {#call web_view_set_settings#} vptr sptr

webViewGetSettings :: WebView -> IO WebSettings 
webViewGetSettings web_view =
    withWebView web_view $ \ptr ->
        makeWebSettings $ {#call web_view_get_settings#} ptr

webViewGetInspector :: WebView -> IO WebInspector
webViewGetInspector web_view =
    withWebView web_view $ \ptr ->
        makeWebInspector $ {#call web_view_get_inspector#} ptr

{- | This functions returns whether or not a MIME type can be displayed using
     this view.
-}
webViewCanShowMimeType
    :: WebView -- ^ the 'WebView' to check
    -> String  -- ^ the MIME type
    -> IO Bool -- ^ 'Bool' indicating if MIME type can be displayed
webViewCanShowMimeType web_view mime_type = do
    withCString mime_type $ \c_mime_type ->
        withWebView web_view $ \ptr ->
            liftM toBool $
                {#call web_view_can_show_mime_type#} ptr c_mime_type

-- | Returns whether the 'WebView' has a transparent background.
webViewGetTransparent
    :: WebView -- ^ the 'WebView'
    -> IO Bool -- ^ 'False' when the 'WebView' draws a solid background
               -- ^ (the default), otherwise 'True'
webViewGetTransparent web_view = do
    withWebView web_view $ \ptr ->
        liftM toBool $
            {#call web_view_get_transparent#} ptr

webViewSetTransparent :: WebView -> Bool -> IO ()
webViewSetTransparent web_view flag =
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
    :: WebView  -- ^ the 'WebView'
    -> IO Float -- ^ the zoom level
webViewGetZoomLevel web_view =
    withWebView web_view $ \ptr ->
        liftM realToFrac $
            {#call web_view_get_zoom_level#} ptr

webViewSetZoomLevel :: WebView -> Float -> IO ()
webViewSetZoomLevel web_view zoom_level =
    withWebView web_view $ \ptr ->
        {#call web_view_set_zoom_level#} ptr $
            realToFrac zoom_level

webViewZoomIn :: WebView -> IO ()
webViewZoomIn web_view =
    withWebView web_view $ \ptr ->
        {#call web_view_zoom_in#} ptr

webViewZoomOut :: WebView -> IO ()
webViewZoomOut web_view =
    withWebView web_view $ \ptr ->
        {#call web_view_zoom_out#} ptr

webViewGetFullContentZoom :: WebView -> IO Bool
webViewGetFullContentZoom web_view = do
    withWebView web_view $ \ptr ->
        liftM toBool $
            {#call web_view_get_full_content_zoom#} ptr

webViewSetFullContentZoom :: WebView -> Bool -> IO ()
webViewSetFullContentZoom web_view full_content_zoom =
    withWebView web_view $ \ptr ->
        {#call web_view_set_full_content_zoom#} ptr $
            fromBool full_content_zoom

getDefaultSession :: IO SoupSession
getDefaultSession =
    makeSoupSession {#call get_default_session#}

webViewGetEncoding :: WebView -> IO (Maybe String)
webViewGetEncoding web_view =
    withWebView web_view $ \ptr ->
        {#call web_view_get_encoding#} ptr
            >>= maybePeek peekCString

webViewSetCustomEncoding :: WebView -> String -> IO ()
webViewSetCustomEncoding web_view encoding = do
    withCString encoding $ \c_encoding ->
        withWebView web_view $ \ptr ->
            {#call web_view_set_custom_encoding#} ptr c_encoding

{- | Returns the current encoding of the 'WebView', not the default-encoding
     of 'WebSettings'.
-}
webViewGetCustomEncoding
    :: WebView           -- ^ the 'WebView'
    -> IO (Maybe String) -- ^ 'Just' encoding if set, otherwise 'Nothing'
webViewGetCustomEncoding web_view =
    withWebView web_view $ \ptr ->
        {#call web_view_get_custom_encoding#} ptr
            >>= maybePeek peekCString

webViewMoveCursor :: WebView -> MovementStep -> Int -> IO ()
webViewMoveCursor web_view movement_step count =
    withWebView web_view $ \wvptr ->
        {#call web_view_move_cursor#} wvptr 
            ((fromIntegral . fromEnum) movement_step) (fromIntegral count)

webViewGetLoadStatus :: WebView -> IO LoadStatus
webViewGetLoadStatus web_view =
    withWebView web_view $ \ptr ->
        liftM (toEnum . fromIntegral) $
            {#call web_view_get_load_status#} ptr

webViewGetProgress :: WebView -> IO Double
webViewGetProgress web_view =
    withWebView web_view $ \ptr ->
        liftM realToFrac $
            {#call web_view_get_progress#} ptr

webViewUndo :: WebView -> IO ()
webViewUndo web_view =
    withWebView web_view $ \ptr ->
        {#call web_view_undo#} ptr 

{- | Determines whether or not it is currently possible to undo the last editing
     command in the view.
-}
webViewCanUndo
    :: WebView -- a 'WebView'
    -> IO Bool -- 'True' if possible to undo last editing command
webViewCanUndo web_view =
    withWebView web_view $ \ptr ->
        liftM toBool $ 
            {#call web_view_can_undo#} ptr

webViewRedo :: WebView -> IO ()
webViewRedo web_view =
    withWebView web_view $ \ptr ->
        {#call web_view_redo#} ptr 

{- | Determines whether or not it is currently possible to redo the last editing
     command in the view.
-}
webViewCanRedo
    :: WebView -- ^ a 'WebView'
    -> IO Bool -- ^ 'True' if possible to redo last editing command
webViewCanRedo web_view =
    withWebView web_view $ \ptr ->
        liftM toBool $ 
            {#call web_view_can_redo#} ptr

webViewSetViewSourceMode :: WebView -> Bool -> IO ()
webViewSetViewSourceMode web_view source_mode =
    withWebView web_view $ \ptr ->
        {#call web_view_set_view_source_mode#} ptr 
            (fromBool source_mode) 

webViewGetViewSourceMode :: WebView -> IO Bool
webViewGetViewSourceMode web_view =
    withWebView web_view $ \ptr ->
        liftM toBool $ 
            {#call web_view_get_view_source_mode#} ptr

{- TODO
WebKitHitTestResult* webkit_web_view_get_hit_test_result (WebKitWebView *webView, GdkEventButton *event);

webViewGetHitTestResult :: WebView -> EventButton -> IO HitTestResult 
webViewGetHitTestResult web_view event_button =
    withWebView web_view $ \wptr ->
        makeHitTestResult $ do
            {#call web_view_get_hit_test_result#} wptr ??? 

-}

-- Properties ------------------------------------------------------------------

webViewGetIconUri :: WebView -> IO String
webViewGetIconUri =
    objectGetPropertyString
        "icon-uri"

{- TODO / new in 1.1.20
"im-context" GtkIMContext* : Read
-}

webViewSetWindowFeatures :: WebView -> WebWindowFeatures -> IO ()
webViewSetWindowFeatures web_view web_window_features = do
    wwft <- webWindowFeaturesGetType
    objectSetPropertyGObject wwft "window-features" web_view web_window_features

webViewGetWindowFeatures :: WebView -> IO WebWindowFeatures
webViewGetWindowFeatures web_view = do
    wwft <- webWindowFeaturesGetType
    objectGetPropertyGObject wwft "window-features" web_view

-- Signals ---------------------------------------------------------------------

onWebViewCloseWebView, afterWebViewCloseWebView :: 
    WebView -> (WebView -> IO Bool) -> IO (ConnectId WebView)
onWebViewCloseWebView web_view f = 
    on web_view (Signal (connectGeneric "close-web-view")) $
        \ webViewPtr -> do 
            x1 <- makeWebView $ return webViewPtr 
            f x1
afterWebViewCloseWebView web_view f = 
    on web_view (Signal (connectGeneric "close-web-view")) $
        \ webViewPtr -> do 
            x1 <- makeWebView $ return webViewPtr 
            f x1

onWebViewConsoleMessage,afterWebViewConsoleMessage :: 
    WebView -> (WebView -> String -> Int -> String -> IO Bool) -> IO (ConnectId WebView)
onWebViewConsoleMessage web_view f = 
    on web_view (Signal (connectGeneric "console-message")) 
        (webViewConsoleMessageWrapper f)
afterWebViewConsoleMessage web_view f = 
    after web_view (Signal (connectGeneric "console-message")) 
        (webViewConsoleMessageWrapper f)

webViewConsoleMessageWrapper ::
    (WebView -> String -> Int -> String -> IO Bool)
    -> Ptr WebView -> CString -> CInt -> CString -> IO Bool
webViewConsoleMessageWrapper f webViewPtr message line source_id = do
    x1 <- makeWebView $ return webViewPtr
    x2 <- peekCString message
    x4 <- peekCString source_id
    f x1 x2 (fromIntegral line) x4 

onWebViewCopyClipboard, afterWebViewCopyClipboard ::
    WebView -> IO () -> IO (ConnectId WebView)
onWebViewCopyClipboard =
    connect_NONE__NONE "copy-clipboard" False
afterWebViewCopyClipboard =
    connect_NONE__NONE "copy-clipboard" True

{- TODO "create-plugin-widget" : GtkWidget* user_function (WebKitWebView *web_view, gchar *mime_type, gchar *uri, GHashTable *param, gpointer user_data) : Run Last / Action -}

onWebViewCreateWebView, afterWebViewCreateWebView::
    WebView -> (WebView -> WebFrame -> IO WebView) -> IO (ConnectId WebView)
onWebViewCreateWebView web_view f = 
    on web_view (Signal (connectGeneric "create-web-view")) 
        (webViewCreateWebViewWrapper f) 
afterWebViewCreateWebView web_view f = 
    after web_view (Signal (connectGeneric "create-web-view"))
        (webViewCreateWebViewWrapper f)

webViewCreateWebViewWrapper :: 
    (WebView -> WebFrame -> IO WebView) 
    -> Ptr WebView -> Ptr WebFrame -> IO WebView
webViewCreateWebViewWrapper f webViewPtr webFramePtr = do
            x1 <- makeWebView $ return webViewPtr
            x2 <- makeWebFrame $ return webFramePtr
            f x1 x2 

onWebViewCutClipboard, afterWebViewCutClipboard ::
    WebView -> IO () -> IO (ConnectId WebView)
onWebViewCutClipboard =
    connect_NONE__NONE "cut-clipboard" False
afterWebViewCutClipboard =
    connect_NONE__NONE "cut-clipboard" True

onWebViewDatabaseQuotaExceeded, afterWebViewDatabaseQuotaExceeded ::
    WebView 
    -> (WebView -> WebFrame -> WebDatabase -> IO ())
    -> IO (ConnectId WebView)
onWebViewDatabaseQuotaExceeded web_view f =
    on web_view (Signal (connectGeneric "database-quota-exceeded"))
        (webViewDatabaseQuotaExceededWrapper f)
afterWebViewDatabaseQuotaExceeded web_view f =
    after web_view (Signal (connectGeneric "database-quota-exceeded"))
        (webViewDatabaseQuotaExceededWrapper f)

webViewDatabaseQuotaExceededWrapper :: 
    (WebView -> WebFrame -> WebDatabase -> IO ())
    -> Ptr WebView -> Ptr WebFrame -> Ptr WebDatabase -> IO ()
webViewDatabaseQuotaExceededWrapper f webViewPtr webFramePtr databasePtr = do
    x1 <- makeWebView $ return webViewPtr
    x2 <- makeWebFrame $ return webFramePtr 
    x3 <- makeWebDatabase $ return databasePtr
    f x1 x2 x3 

onWebViewDownloadRequested, afterWebViewDownloadRequested :: 
    WebView -> (WebView -> Download -> IO Bool) -> IO (ConnectId WebView)
onWebViewDownloadRequested web_view f =
    on web_view (Signal (connectGeneric "download-requested")) 
        (webViewDownloadRequestedWrapper f)
afterWebViewDownloadRequested web_view f =
    after web_view (Signal (connectGeneric "download-requested")) 
        (webViewDownloadRequestedWrapper f)
  
webViewDownloadRequestedWrapper ::
     (WebView -> Download -> IO Bool) -> Ptr WebView -> Ptr Download -> IO Bool
webViewDownloadRequestedWrapper f webViewPtr downloadPtr = do
    x1 <- makeWebView $ return webViewPtr 
    x2 <- makeDownload $ return downloadPtr 
    f x1 x2 

onWebViewHoveringOverLink, afterWebViewHoveringOverLink ::
    WebView -> (String -> String -> IO ()) -> IO (ConnectId WebView)
onWebViewHoveringOverLink =
    connect_STRING_STRING__NONE "hovering-over-link" False
afterWebViewHoveringOverLink =
    connect_STRING_STRING__NONE "hovering-over-link" True

onWebViewIconLoaded, afterWebViewIconLoaded ::
    WebView -> IO () -> IO (ConnectId WebView)
onWebViewIconLoaded =
    connect_NONE__NONE "icon-loaded" False
afterWebViewIconLoaded =
    connect_NONE__NONE "icon-loaded" True

{- DEPRECATED
onWebViewLoadCommitted, afterWebViewLoadCommitted ::
    WebView -> (WebFrame -> IO ()) -> IO (ConnectId WebView)
onWebViewLoadCommitted =
    connect_OBJECT__NONE "load-committed" False
afterWebViewLoadCommitted =
    connect_OBJECT__NONE "load-committed" True
-}

{- TODO
"load-error" : gboolean user_function (WebKitWebView *web_view, WebKitWebFrame *web_frame, gchar *uri, gpointer web_error, gpointer user_data) : Run Last
"mime-type-policy-decision-requested" : gboolean user_function (WebKitWebView *web_view, WebKitWebFrame *frame, WebKitNetworkRequest *request, gchar *mimetype, WebKitWebPolicyDecision *policy_decision, gpointer user_data) : Run Last
"move-cursor" : gboolean user_function (WebKitWebView *web_view, GtkMovementStep step, gint count, gpointer user_data) : Run Last / Action
"new-window-policy-decision-requested" : gboolean user_function (WebKitWebView *web_view, WebKitWebFrame *frame, WebKitNetworkRequest *request, WebKitWebNavigationAction *navigation_action, WebKitWebPolicyDecision *policy_decision, gpointer user_data) : Run Last
-}

onWebViewNavigationPolicyDecisionRequested, afterWebViewNavigationPolicyDecisionRequested ::
    WebView -> (WebView -> WebFrame -> NetworkRequest -> WebNavigationAction -> WebPolicyDecision -> IO ()) -> IO (ConnectId WebView)
onWebViewNavigationPolicyDecisionRequested web_view f =
    on web_view (Signal (connectGeneric "navigation-policy-decision-requested")) 
        (webViewNavigationPolicyDecisionRequestedWrapper f)
afterWebViewNavigationPolicyDecisionRequested web_view f =
    after web_view (Signal (connectGeneric "navigation-policy-decision-requested")) 
        (webViewNavigationPolicyDecisionRequestedWrapper f)

webViewNavigationPolicyDecisionRequestedWrapper ::
    (WebView -> WebFrame -> NetworkRequest -> WebNavigationAction -> WebPolicyDecision -> IO ()) 
    -> Ptr WebView -> Ptr WebFrame -> Ptr NetworkRequest -> Ptr WebNavigationAction -> Ptr WebPolicyDecision -> IO ()
webViewNavigationPolicyDecisionRequestedWrapper f x1 x2 x3 x4 x5 = do
        y1 <- makeWebView $ return x1
        y2 <- makeWebFrame $ return x2
        y3 <- makeNetworkRequest $ return x3
        y4 <- makeWebNavigationAction $ return x4
        y5 <- makeWebPolicyDecision $ return x5
        f y1 y2 y3 y4 y5 

onWebViewPasteClipboard, afterWebViewPasteClipboard ::
    WebView -> IO () -> IO (ConnectId WebView)
onWebViewPasteClipboard =
    connect_NONE__NONE "paste-clipboard" False
afterWebViewPasteClipboard =
    connect_NONE__NONE "paste-clipboard" True

onWebViewPopulatePopup, afterWebViewPopulatePopup ::
    WebView -> (WebView -> Menu -> IO ()) -> IO (ConnectId WebView)
onWebViewPopulatePopup web_view f =
    on web_view (Signal (connectGeneric "populater-popup")) 
        (webViewPopulatePopupWrapper f)
afterWebViewPopulatePopup web_view f =
    after web_view (Signal (connectGeneric "populater-popup")) 
        (webViewPopulatePopupWrapper f)

webViewPopulatePopupWrapper :: 
    (WebView -> Menu -> IO ()) 
    -> Ptr WebView -> Ptr Menu -> IO ()
webViewPopulatePopupWrapper f webViewPtr menuPtr = do
    x1 <- makeWebView $ return webViewPtr 
    x2 <- makeNewObject (Menu, objectUnref) $ return menuPtr
    f x1 x2 

onWebViewPrintRequested, afterWebViewPrintRequested ::
    WebView -> (WebView -> WebFrame -> IO Bool) -> IO (ConnectId WebView)
onWebViewPrintRequested web_view f =
    on web_view (Signal (connectGeneric "print-requested"))
    (onWebViewPrintRequestedWrapper f)
afterWebViewPrintRequested web_view f =
    after web_view (Signal (connectGeneric "print-requested"))
    (onWebViewPrintRequestedWrapper f)

onWebViewPrintRequestedWrapper ::
    (WebView -> WebFrame -> IO Bool)
    -> Ptr WebView -> Ptr WebFrame -> IO Bool
onWebViewPrintRequestedWrapper f webViewPtr webFramePtr = do
    x1 <- makeWebView $ return webViewPtr
    x2 <- makeWebFrame $ return webFramePtr
    f x1 x2

onWebViewRedo, afterWebViewRedo :: 
    WebView -> (WebView -> IO ()) -> IO (ConnectId WebView) 
onWebViewRedo web_view f =
    on web_view (Signal (connectGeneric "redo")) $ \ wv -> do 
        x <-  makeWebView $ return wv 
        f x 
afterWebViewRedo web_view f =
    after web_view (Signal (connectGeneric "redo")) $ \ wv -> do 
        x <-  makeWebView $ return wv 
        f x 

onWebViewResourceRequestStarting,afterWebViewResourceRequestStarting :: 
   WebView
   -> (WebView -> WebFrame -> WebResource -> NetworkRequest -> Maybe NetworkResponse -> IO ()) 
   -> IO (ConnectId WebView)
onWebViewResourceRequestStarting wV f = 
    on wV 
        (Signal (connectGeneric "resource-request-starting")) 
            (webViewResourceRequestStartingWrapper f) 
afterWebViewResourceRequestStarting wV f = 
    after wV 
        (Signal (connectGeneric "resource-request-starting")) 
            (webViewResourceRequestStartingWrapper f) 

webViewResourceRequestStartingWrapper :: 
   (WebView -> WebFrame -> WebResource -> NetworkRequest -> Maybe NetworkResponse -> IO ()) 
   -> Ptr WebView -> Ptr WebFrame -> Ptr WebResource -> Ptr NetworkRequest -> Ptr NetworkResponse
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

onWebViewScriptPrompt, afterWebViewScriptPrompt :: 
    WebView -> (WebView -> WebFrame -> String -> String -> String -> IO Bool) 
    -> IO (ConnectId WebView)
onWebViewScriptPrompt web_view f = 
    on web_view (Signal (connectGeneric "script-prompt")) 
        (webViewScriptPromptWrapper f)
afterWebViewScriptPrompt web_view f = 
    after web_view (Signal (connectGeneric "script-prompt")) 
        (webViewScriptPromptWrapper f)

webViewScriptPromptWrapper :: 
    (WebView -> WebFrame -> String -> String -> String -> IO Bool) 
    -> Ptr WebView -> Ptr WebFrame -> CString -> CString -> CString -> IO Bool     
webViewScriptPromptWrapper f webViewPtr webFramePtr message def text = do
    x1 <- makeWebView $ return webViewPtr
    x2 <- makeWebFrame $ return webFramePtr
    x3 <- peekCString message
    x4 <- peekCString def
    x5 <- peekCString text
    f x1 x2 x3 x4 x5

onWebViewScriptAlert, afterWebViewScriptAlert :: 
    WebView -> (WebView -> WebFrame -> String -> IO Bool) 
    -> IO (ConnectId WebView)
onWebViewScriptAlert web_view f =
    on web_view (Signal (connectGeneric "script-alert"))
        (webViewScriptAlertWrapper f)
afterWebViewScriptAlert web_view f =
    after web_view (Signal (connectGeneric "script-alert"))
        (webViewScriptAlertWrapper f)

webViewScriptAlertWrapper :: 
    (WebView -> WebFrame -> String -> IO Bool) 
    -> Ptr WebView -> Ptr WebFrame -> CString -> IO Bool
webViewScriptAlertWrapper f webViewPtr webFramePtr message = do
    x1 <- makeWebView $ return webViewPtr    
    x2 <- makeWebFrame $ return webFramePtr
    x3 <- peekCString message
    f x1 x2 x3

onWebViewScriptConfirm, afterWebViewScriptConfirm :: 
    WebView -> (WebView -> WebFrame -> String -> Bool -> IO Bool)
    -> IO (ConnectId WebView)
onWebViewScriptConfirm web_view f =
    on web_view (Signal (connectGeneric "script-confirm"))
        (webViewScriptConfirmWrapper f)
afterWebViewScriptConfirm web_view f =
    after web_view (Signal (connectGeneric "script-confirm"))
        (webViewScriptConfirmWrapper f)

webViewScriptConfirmWrapper ::
    (WebView -> WebFrame -> String -> Bool -> IO Bool)
    -> Ptr WebView -> Ptr WebFrame -> CString -> Bool -> IO Bool
webViewScriptConfirmWrapper f webViewPtr webFramePtr message confirm = do
    x1 <- makeWebView $ return webViewPtr
    x2 <- makeWebFrame $ return webFramePtr 
    x3 <- peekCString message
    f x1 x2 x3 confirm

onWebViewSelectAll, afterWebViewSelectAll ::
    WebView -> IO () -> IO (ConnectId WebView)
onWebViewSelectAll web_view f = 
    on web_view (Signal (connectGeneric "select-all")) (\_ -> f)
afterWebViewSelectAll web_view f =
    after web_view (Signal (connectGeneric "select-all")) (\_ -> f)

onWebViewSelectionChanged, afterWebViewSelectionChanged ::
    WebView -> IO () -> IO (ConnectId WebView)
onWebViewSelectionChanged =
    connect_NONE__NONE "selection-changed" False
afterWebViewSelectionChanged =
    connect_NONE__NONE "selection-changed" True

onWebViewSetCrollAdjustments,afterWebViewSetCrollAdjustments ::
    WebView -> (WebView -> Adjustment -> Adjustment -> IO ()) 
    -> IO (ConnectId WebView)
onWebViewSetCrollAdjustments web_view f =
    on web_view (Signal (connectGeneric "set-scroll-adjustments"))
        (webViewSetCrollAdjustmentsWrapper f)
afterWebViewSetCrollAdjustments web_view f =
    after web_view (Signal (connectGeneric "set-scroll-adjustments"))
        (webViewSetCrollAdjustmentsWrapper f)

webViewSetCrollAdjustmentsWrapper ::
    (WebView -> Adjustment -> Adjustment -> IO ()) 
    -> Ptr WebView -> Ptr Adjustment -> Ptr Adjustment -> IO ()
webViewSetCrollAdjustmentsWrapper f webViewPtr gtkAdj1Ptr gtkAdj2Ptr = do
    x1 <- makeWebView $ return webViewPtr
    x2 <- makeNewObject (Adjustment, objectUnref)  $ return gtkAdj1Ptr
    x3 <- makeNewObject (Adjustment, objectUnref)  $ return gtkAdj2Ptr
    f x1 x2 x3 

onWebViewStatusbarTextChanged, afterWebViewStatusbarTextChanged ::
    WebView -> (String -> IO ()) -> IO (ConnectId WebView)
onWebViewStatusbarTextChanged web_view f =
    on web_view (Signal (connectGeneric "status-bar-text-changed")) (\_ -> f)
afterWebViewStatusbarTextChanged web_view f =
     after web_view (Signal (connectGeneric "status-bar-text-changed")) (\_ -> f)

{- DEPRECATED
onWebViewTitleChanged, afterWebViewTitleChanged ::
    WebView -> (WebFrame -> String -> IO ()) -> IO (ConnectId WebView)
onWebViewTitleChanged =
    connect_OBJECT_STRING__NONE "title-changed" False
afterWebViewTitleChanged =
    connect_OBJECT_STRING__NONE "title-changed" True
-}

{- TODO
"web-view-ready" : gboolean user_function (WebKitWebView *web_view, gpointer user_data) : Run Last
"window-object-cleared" : void user_function (WebKitWebView *web_view, WebKitWebFrame *frame, gpointer context, gpointer arg3, gpointer user_data) : Run Last / Action
-}

onWebViewReady,afterWebViewReady :: 
    WebView -> (WebView -> IO ())
    -> IO (ConnectId WebView)
onWebViewReady web_view f =
    on web_view (Signal (connectGeneric "web-view-ready")) $ \ wv -> do 
        x <-  makeWebView $ return wv 
        f x 
afterWebViewReady web_view f =
    after web_view (Signal (connectGeneric "web-view-ready")) $ \ wv -> do 
        x <-  makeWebView $ return wv 
        f x 

onWebViewUndo, afterWebViewUndo :: 
    WebView -> (WebView -> IO ()) -> IO (ConnectId WebView) 
onWebViewUndo web_view f =
    on web_view (Signal (connectGeneric "undo")) $ \ wv -> do 
        x <-  makeWebView $ return wv 
        f x 
afterWebViewUndo web_view f =
    after web_view (Signal (connectGeneric "undo")) $ \ wv -> do 
        x <-  makeWebView $ return wv 
        f x 
