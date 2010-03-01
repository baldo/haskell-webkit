{-# LANGUAGE ForeignFunctionInterface #-}

{# context lib="libwebkit" prefix="webkit_" #}

module Graphics.UI.Gtk.WebKit.WebInspector
    ( WebInspector

    -- , webInspectorClose -- new in webkit 1.1.17
    -- , webInspectorShow  -- new in webkit 1.1.17
    -- , webInspectorInspectCoordinates -- new in webkit 1.1.17
    , webInspectorGetWebView
    , webInspectorGetInspectedUri

    -- Properties -------------------------------------------------------------
    , webInspectorSetJavascriptProfilingEnabled 
    , webInspectorGetJavascriptProfilingEnabled 

    -- , webInspectorSetTimeLineProfilingEnabled -- new in webkit 1.1.17
    -- , webInspectorGetTimeLineProfilingEnabled -- new in webkit 1.1.17

    -- Signals ----------------------------------------------------------------
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

import Foreign.C
import GHC.Ptr
import System.Glib.FFI
import System.Glib.Signals
import System.Glib.Properties

import Control.Monad

import Graphics.UI.Gtk.Abstract.Object
    ( makeNewObject
    )

{#import Graphics.UI.Gtk.WebKit.General.Types#}
    ( WebInspector
    , WebView

    , mkWebInspector
    , withWebInspector
    , makeWebInspector
    , makeWebView
    )

webInspectorGetInspectedUri :: WebInspector -> IO String
webInspectorGetInspectedUri inspector =
    withWebInspector inspector $ \ptr ->
        {#call web_inspector_get_inspected_uri#} ptr >>= peekCString

webInspectorGetWebView :: WebInspector -> IO WebView
webInspectorGetWebView inspector = 
    withWebInspector inspector $ \ptr ->
        makeWebView $ 
            {#call web_inspector_get_web_view#} ptr 

{- new in webkit 1.1.17
webInspectorInspectCoordinates :: WebInspector -> Double -> Double -> IO ()
webInspectorInspectCoordinates inspector x y =
    withWebInspector inspector $ \ptr ->
        {#call web_inspector_inspect_coordinates#} ptr (toRational x) (toRational y)

webInspectorShow :: WebInspector -> IO ()
webInspectorShow inspector =
    withWebInspector inspector $ \ptr ->
        {#call  web_inspector_show#} ptr

webInspectorClose :: WebInspector -> IO ()
webInspectorClose inspector =
    withWebInspector inspector $ \ptr ->
        {#call  web_inspector_close#} ptr
-} 

-- Properties -----------------------------------------------------------------

webInspectorSetJavascriptProfilingEnabled :: WebInspector -> Bool -> IO ()
webInspectorSetJavascriptProfilingEnabled =
    objectSetPropertyBool 
        "javascript-profiling-enabled" 

webInspectorGetJavascriptProfilingEnabled :: WebInspector -> IO Bool
webInspectorGetJavascriptProfilingEnabled =
    objectGetPropertyBool 
        "javascript-profiling-enabled" 


{- new in webkit 1.1.17
webInspectorSetTimeLineProfilingEnabled :: WebInspector -> Bool -> IO ()
webInspectorSetTimeLineProfilingEnabled =
    objectSetPropertyBool 
        "timeline-profiling-enabled" 

webInspectorGetTimeLineProfilingEnabled :: WebInspector -> IO Bool
webInspectorGetTimeLineProfilingEnabled =
    objectGetPropertyBool 
        "timeline-profiling-enabled" 
-}

-- Signals --------------------------------------------------------------------

onWebInspectorAttachWindow, afterWebInspectorAttachWindow ::
    WebInspector -> (WebInspector -> IO ()) -> IO (ConnectId WebInspector)
onWebInspectorAttachWindow web_inspector f =
    on web_inspector (Signal (connectGeneric "attach-window"))
        (webInspectorSignalWrapper f) 
afterWebInspectorAttachWindow web_inspector f =
    after web_inspector (Signal (connectGeneric "attach-window"))
        (webInspectorSignalWrapper f)

onWebInspectorCloseWindow, afterWebInspectorCloseWindow ::
    WebInspector -> (WebInspector -> IO ()) -> IO (ConnectId WebInspector)
onWebInspectorCloseWindow web_inspector f =
    on web_inspector (Signal (connectGeneric "close-window"))
        (webInspectorSignalWrapper f) 
afterWebInspectorCloseWindow web_inspector f =
    after web_inspector (Signal (connectGeneric "close-window"))
        (webInspectorSignalWrapper f)

onWebInspectorDetachWindow, afterWebInspectorDetachWindow ::
    WebInspector -> (WebInspector -> IO ()) -> IO (ConnectId WebInspector)
onWebInspectorDetachWindow web_inspector f =
    on web_inspector (Signal (connectGeneric "detach-window"))
        (webInspectorSignalWrapper f) 
afterWebInspectorDetachWindow web_inspector f =
    after web_inspector (Signal (connectGeneric "detach-window"))
        (webInspectorSignalWrapper f)

onWebInspectorFinished, afterWebInspectorFinished ::
    WebInspector -> (WebInspector -> IO ()) -> IO (ConnectId WebInspector)
onWebInspectorFinished web_inspector f =
    on web_inspector (Signal (connectGeneric "finished"))
        (webInspectorSignalWrapper f) 
afterWebInspectorFinished web_inspector f =
    after web_inspector (Signal (connectGeneric "finished"))
        (webInspectorSignalWrapper f)

onWebInspectorShowWindow, afterWebInspectorShowWindow ::
    WebInspector -> (WebInspector -> IO ()) -> IO (ConnectId WebInspector)
onWebInspectorShowWindow web_inspector f =
    on web_inspector (Signal (connectGeneric "show-window"))
        (webInspectorSignalWrapper f) 
afterWebInspectorShowWindow web_inspector f =
    after web_inspector (Signal (connectGeneric "show-window"))
        (webInspectorSignalWrapper f)

webInspectorSignalWrapper :: 
    (WebInspector -> IO ()) 
    -> Ptr WebInspector -> IO ()
webInspectorSignalWrapper f webInspectorPtr = do
    x1 <- makeWebInspector $ return webInspectorPtr 
    f x1 

onWebInspectorInspectWebView, afterWebInspectorInspectWebView ::
    WebInspector -> (WebInspector -> WebView -> IO ()) -> IO (ConnectId WebInspector)
onWebInspectorInspectWebView web_inspector f =
    on web_inspector (Signal (connectGeneric "inspect-web-view"))
        (webInspectorWebViewSignalWrapper f) 
afterWebInspectorInspectWebView web_inspector f =
    after web_inspector (Signal (connectGeneric "inspect-web-view"))
        (webInspectorWebViewSignalWrapper f)

webInspectorWebViewSignalWrapper :: 
    (WebInspector -> WebView -> IO ()) 
    -> Ptr WebInspector -> Ptr WebView -> IO ()
webInspectorWebViewSignalWrapper f webInspectorPtr webViewPtr = do
    x1 <- makeWebInspector $ return webInspectorPtr 
    x2 <- makeWebView $ return webViewPtr
    f x1 x2
