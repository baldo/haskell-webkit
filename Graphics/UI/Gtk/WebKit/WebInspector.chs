{-# LANGUAGE ForeignFunctionInterface #-}

{# context lib="libwebkit" prefix="webkit_" #}

{-| Access to the WebKit 'WebInspector'.

The 'WebInspector' is a graphical tool to inspect and change the content of a
'WebView'. It also includes an interactive JavaScriptDebugger. Using
'WebInspector' one can get a 'Widget' which can be embedded into an application
to show the inspector.

The inspector is enabled by applying 'webSettingsSetEnableDeveloperExtras' to
the 'WebSettings' of the 'WebView' and 'True'.
-}

module Graphics.UI.Gtk.WebKit.WebInspector
    ( WebInspector

    -- * Functions
    , webInseptorGetType
    , webInspectorClose
    , webInspectorShow
    , webInspectorInspectCoordinates
    , webInspectorGetWebView
    , webInspectorGetInspectedUri

    -- * Properties
    , webInspectorSetJavascriptProfilingEnabled
    , webInspectorGetJavascriptProfilingEnabled

    , webInspectorSetTimeLineProfilingEnabled
    , webInspectorGetTimeLineProfilingEnabled

    -- * Signals
    , onWebInspectorShowWindow
    , afterWebInspectorShowWindow

    , onWebInspectorFinished
    , afterWebInspectorFinished

    , onWebInspectorDetachWindow
    , afterWebInspectorDetachWindow

    , onWebInspectorCloseWindow
    , afterWebInspectorCloseWindow

    , onWebInspectorAttachWindow
    , afterWebInspectorAttachWindow

    , onWebInspectorInspectWebView
    , afterWebInspectorInspectWebView
    ) where

#include <webkit/webkitwebinspector.h>

import System.Glib.FFI
import System.Glib.GType
    ( GType
    )
import System.Glib.Signals
    ( Signal (..)
    , ConnectId

    , after
    , on

    , connectGeneric
    )
import System.Glib.Properties
    ( objectGetPropertyBool
    , objectSetPropertyBool
    )

import Control.Monad.Trans
    ( MonadIO
    , liftIO
    )

{#import Graphics.UI.Gtk.WebKit.General.Types#}
    ( WebInspector
    , WebView

    , makeWebInspector
    , withWebInspector
    , makeWebView
    )

webInseptorGetType
    :: MonadIO m
    => m GType
webInseptorGetType = liftIO $
    {#call web_inspector_get_type#}

-- | Obtains the URI that is currently being inspected.
webInspectorGetInspectedUri
    :: MonadIO m
    => WebInspector -- ^ the inspector
    -> m String     -- ^ the URI
webInspectorGetInspectedUri inspector = liftIO $
    withWebInspector inspector $ \ptr ->
        {#call web_inspector_get_inspected_uri#} ptr >>= peekCString

{- | Obtains the 'WebView' that is used to render the inspector. The 'WebView'
     is created by the application, by handling the \"inspect-web-view\" signal
     ('onWebInspectorInspectWebView', 'afterWebInspectorInspectWebView').

     This means that this function may return 'Nothing' if the user hasn't
     inspected anything.
-}
webInspectorGetWebView
    :: MonadIO m
    => WebInspector      -- ^ an inspector
    -> m (Maybe WebView) -- ^ 'Just' its 'WebView' or 'Nothing'
webInspectorGetWebView inspector = liftIO $
    withWebInspector inspector $
        maybePeek $ \ptr ->
            makeWebView $
                {#call web_inspector_get_web_view#} ptr

{- | Causes the 'WebInspector' to inspect the node that is located at the given
     coordinates of the 'Widget'. The coordinates should be relative to the
     'WebView' widget, not to the scrollable content, and may be obtained from
     an 'Event' directly.

     This means x, and y being zero doesn't guarantee you will hit the left-most
     top corner of the content, since the contents may have been scrolled.
-}
webInspectorInspectCoordinates
    :: MonadIO m
    => WebInspector -- ^ the 'WebInspector' that will do the inspection
    -> Double       -- ^ the x coordinate of the node to be inspected
    -> Double       -- ^ the y coordinate of the node to be inspected
    -> m ()
webInspectorInspectCoordinates inspector x y = liftIO $
    withWebInspector inspector $ \ptr ->
        {#call web_inspector_inspect_coordinates#}
            ptr (realToFrac x) (realToFrac y)

-- | Causes the 'WebInspector' to be shown.
webInspectorShow
    :: MonadIO m
    => WebInspector -- ^ the inspector to show
    -> m ()
webInspectorShow inspector = liftIO $
    withWebInspector inspector $ \ptr ->
        {#call  web_inspector_show#} ptr

-- | Causes the 'WebInspector' to be closed.
webInspectorClose
    :: MonadIO m
    => WebInspector -- ^ the inspector to close
    -> m ()
webInspectorClose inspector = liftIO $
    withWebInspector inspector $ \ptr ->
        {#call  web_inspector_close#} ptr

-- Properties -----------------------------------------------------------------

{- | Enable or disable JavaScript profiling in the inspector. This means that
     Console.profiles will return the profiles.

     Disabled by default.
-}
webInspectorSetJavascriptProfilingEnabled
    :: MonadIO m
    => WebInspector -- ^ an inspector
    -> Bool         -- ^ 'True' to enable, 'False' to disable profiling
    -> m ()
webInspectorSetJavascriptProfilingEnabled wi b = liftIO $
    objectSetPropertyBool
        "javascript-profiling-enabled" wi b

{- | Determines whether JavaScript profiling is enabled in the inspector.

     Disabled by default.
-}
webInspectorGetJavascriptProfilingEnabled
    :: MonadIO m
    => WebInspector -- ^ an inspector
    -> m Bool       -- ^ 'True' if profiling is enabled, 'False' otherwise
webInspectorGetJavascriptProfilingEnabled wi = liftIO $
    objectGetPropertyBool
        "javascript-profiling-enabled" wi

{- | Enable or disable Timeline profiling in the inspector.

     Disabled by default.
-}
webInspectorSetTimeLineProfilingEnabled
    :: MonadIO m
    => WebInspector -- ^ an inspector
    -> Bool         -- ^ 'True' to enable, 'False' to disable profiling
    -> m ()
webInspectorSetTimeLineProfilingEnabled wi b = liftIO $
    objectSetPropertyBool
        "timeline-profiling-enabled" wi b

{- | Determines whether Timeline profiling is enabled in the inspector.

     Disabled by default.
-}
webInspectorGetTimeLineProfilingEnabled
    :: MonadIO m
    => WebInspector -- ^ an inspector
    -> m Bool       -- ^ 'True' if profiling is enabled, 'False' otherwise
webInspectorGetTimeLineProfilingEnabled wi = liftIO $
    objectGetPropertyBool
        "timeline-profiling-enabled" wi

-- Signals --------------------------------------------------------------------

{- | Emitted when the inspector should appear at the same 'Window' as the
     'WebView' being inspected.
-}
onWebInspectorAttachWindow, afterWebInspectorAttachWindow
    :: MonadIO m
    => WebInspector               -- ^ an inspector
    -> (WebInspector -> IO ())    -- ^ the signal handler
    -> m (ConnectId WebInspector)
onWebInspectorAttachWindow web_inspector f = liftIO $
    on web_inspector (Signal (connectGeneric "attach-window"))
        (webInspectorSignalWrapper f)
afterWebInspectorAttachWindow web_inspector f = liftIO $
    after web_inspector (Signal (connectGeneric "attach-window"))
        (webInspectorSignalWrapper f)

{- | Emitted when the inspector window should be closed. You can destroy the
     'Window' or hide it so that it can be displayed again by handling
     \"show-window\" later on.

     Notice that the inspected 'WebView' may no longer exist when this signal
     is emitted.

     Notice, too, that if you decide to destroy the 'Window',
     \"inspect-web-view\" will be emmited again, when the user inspects an
     element.
-}
onWebInspectorCloseWindow, afterWebInspectorCloseWindow
    :: MonadIO m
    => WebInspector               -- ^ an inspector
    -> (WebInspector -> IO ())    -- ^ the signal handler
    -> m (ConnectId WebInspector)
onWebInspectorCloseWindow web_inspector f = liftIO $
    on web_inspector (Signal (connectGeneric "close-window"))
        (webInspectorSignalWrapper f)
afterWebInspectorCloseWindow web_inspector f = liftIO $
    after web_inspector (Signal (connectGeneric "close-window"))
        (webInspectorSignalWrapper f)

-- | Emitted when the inspector should appear in a separate 'Window'.
onWebInspectorDetachWindow, afterWebInspectorDetachWindow
    :: MonadIO m
    => WebInspector               -- ^ an inspector
    -> (WebInspector -> IO ())    -- ^ the signal handler
    -> m (ConnectId WebInspector)
onWebInspectorDetachWindow web_inspector f = liftIO $
    on web_inspector (Signal (connectGeneric "detach-window"))
        (webInspectorSignalWrapper f)
afterWebInspectorDetachWindow web_inspector f = liftIO $
    after web_inspector (Signal (connectGeneric "detach-window"))
        (webInspectorSignalWrapper f)

{- | Emitted when the inspection is done. You should release your references on
     the inspector at this time. The inspected 'WebView' may no longer exist
     when this signal is emitted.
-}
onWebInspectorFinished, afterWebInspectorFinished
    :: MonadIO m
    => WebInspector               -- ^ an inspector
    -> (WebInspector -> IO ())    -- ^ the signal handler
    -> m (ConnectId WebInspector)
onWebInspectorFinished web_inspector f = liftIO $
    on web_inspector (Signal (connectGeneric "finished"))
        (webInspectorSignalWrapper f)
afterWebInspectorFinished web_inspector f = liftIO $
    after web_inspector (Signal (connectGeneric "finished"))
        (webInspectorSignalWrapper f)

{- | Emitted when the inspector window should be displayed. Notice that the
     'Window' must have been created already by handling \"inspect-web-view\".
-}
onWebInspectorShowWindow, afterWebInspectorShowWindow
    :: MonadIO m
    => WebInspector               -- ^ an inspector
    -> (WebInspector -> IO ())    -- ^ the signal handler
    -> m (ConnectId WebInspector)
onWebInspectorShowWindow web_inspector f = liftIO $
    on web_inspector (Signal (connectGeneric "show-window"))
        (webInspectorSignalWrapper f)
afterWebInspectorShowWindow web_inspector f = liftIO $
    after web_inspector (Signal (connectGeneric "show-window"))
        (webInspectorSignalWrapper f)

webInspectorSignalWrapper
    :: (WebInspector -> IO ())
    -> Ptr WebInspector
    -> IO ()
webInspectorSignalWrapper f webInspectorPtr = do
    x1 <- makeWebInspector $ return webInspectorPtr
    f x1

{- | Emitted when the user activates the \"inspect\" context menu item to
     inspect a 'WebView'. The application which is interested in the inspector
     should create a 'Window', or otherwise add the 'WebView' it creates to an
     existing 'Window'.

     You don't need to handle the reference count of the 'WebView' you create;
     the 'Widget' to which you add it will do that.
-}
onWebInspectorInspectWebView, afterWebInspectorInspectWebView
    :: MonadIO m
    => WebInspector                       -- ^ an inspector
    -> (WebInspector -> WebView -> IO ()) -- ^ the signal handler
    -> m (ConnectId WebInspector)
onWebInspectorInspectWebView web_inspector f = liftIO $
    on web_inspector (Signal (connectGeneric "inspect-web-view"))
        (webInspectorWebViewSignalWrapper f)
afterWebInspectorInspectWebView web_inspector f = liftIO $
    after web_inspector (Signal (connectGeneric "inspect-web-view"))
        (webInspectorWebViewSignalWrapper f)

webInspectorWebViewSignalWrapper
    :: (WebInspector -> WebView -> IO ())
    -> Ptr WebInspector
    -> Ptr WebView
    -> IO ()
webInspectorWebViewSignalWrapper f webInspectorPtr webViewPtr = do
    x1 <- makeWebInspector $ return webInspectorPtr
    x2 <- makeWebView $ return webViewPtr
    f x1 x2
