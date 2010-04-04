{-# LANGUAGE ForeignFunctionInterface #-}

{# context lib="libwebkit" prefix="webkit_" #}

{-| Window properties of a 'WebView'

The content of a 'WebView' can request to change certain properties of a 
'WebView'. This can include the x, y position of the window, the width and 
height but also if a toolbar, scrollbar, statusbar, locationbar should be 
visible to the user, the request to show the 'WebView' fullscreen.

In the normal case one will use 'webViewGetWindowFeatures' to get 
the 'WebWindowFeatures' and then monitor the property changes. Be aware that 
the WebWindowFeatures might change change before "web-view-ready" signal is 
emitted. To be safe listen to the notify::window-features signal of the 
'WebView' and reconnect the signals whenever the 'WebWindowFeatures' of a 
'WebView' changes.
-}

module Graphics.UI.Gtk.WebKit.WebWindowFeatures
    ( WebWindowFeatures

    -- * Functions

    , webWindowFeaturesGetType

    , webWindowFeaturesNew
    , webWindowFeaturesEqual

    -- * Properties

    , webWindowFeaturesGetFullscreen
    , webWindowFeaturesSetFullscreen

    , webWindowFeaturesGetHeight
    , webWindowFeaturesSetHeight

    , webWindowFeaturesGetLocationbarVisible
    , webWindowFeaturesSetLocationbarVisible

    , webWindowFeaturesGetMenubarVisible
    , webWindowFeaturesSetMenubarVisible

    , webWindowFeaturesGetScrollbarVisible
    , webWindowFeaturesSetScrollbarVisible

    , webWindowFeaturesGetStatusbarVisible
    , webWindowFeaturesSetStatusbarVisible

    , webWindowFeaturesGetToolbarVisible
    , webWindowFeaturesSetToolbarVisible

    , webWindowFeaturesGetWidth
    , webWindowFeaturesSetWidth

    , webWindowFeaturesGetX
    , webWindowFeaturesSetX

    , webWindowFeaturesGetY
    , webWindowFeaturesSetY

    ) where

#include <webkit/webkitwebwindowfeatures.h>

import Foreign.C
import System.Glib.FFI
import System.Glib.GType
import System.Glib.Properties

import Control.Monad

{#import Graphics.UI.Gtk.WebKit.General.Types#}
    ( WebWindowFeatures

    , makeWebWindowFeatures
    , withWebWindowFeatures
    )

webWindowFeaturesGetType :: IO GType
webWindowFeaturesGetType =
    {#call web_window_features_get_type#}

{- | Decides if a 'WebWindowFeatures' object equals another, as in has the same
     values.
-}
webWindowFeaturesEqual
    :: WebWindowFeatures -- ^ a 'WebWindowFeatures' object
    -> WebWindowFeatures -- ^ another 'WebWindowFeatures' object
    -> IO Bool           -- ^ 'True' if both have the same values, 'False'
                         --   otherwise
webWindowFeaturesEqual f1 f2 =
    withWebWindowFeatures f1 $ \pf1 ->
        withWebWindowFeatures f2 $ \pf2 ->
            liftM toBool $ 
                {#call web_window_features_equal#} pf1 pf2

{- | Creates a new 'WebWindowFeatures' object with default values. 
     It must be manually attached to a 'WebView'.
-}
webWindowFeaturesNew
    :: IO WebWindowFeatures -- ^ a new 'WebWindowFeatures' object
webWindowFeaturesNew =
    makeWebWindowFeatures $
        {#call web_window_features_new#} 

-- Properties -----------------------------------------------------------------

{- | Returns whether window will be displayed fullscreen.

     Default value: 'False'
-}
webWindowFeaturesGetFullscreen
    :: WebWindowFeatures -- ^ window featuress
    -> IO Bool           -- ^ 'True' if window will be displayed fullscreen
webWindowFeaturesGetFullscreen = 
  objectGetPropertyBool
    "fullscreen"

{- | Sets whether window will be displayed fullscreen.

     Default value: 'False'
-}
webWindowFeaturesSetFullscreen
    :: WebWindowFeatures -- ^ window featuress
    -> Bool              -- ^ 'True' to display window fullscreen
    -> IO ()
webWindowFeaturesSetFullscreen = 
  objectSetPropertyBool
    "fullscreen"

{- | Returns the height of the window on the screen.

     Allowed values: >= G_MAXULONG

     Default value: -1
-}
webWindowFeaturesGetHeight
    :: WebWindowFeatures -- ^ window featuress
    -> IO Int            -- ^ the height
webWindowFeaturesGetHeight = 
  objectGetPropertyInt
    "height"

{- | Sets the height of the window on the screen.

     Allowed values: >= G_MAXULONG

     Default value: -1
-}
webWindowFeaturesSetHeight
    :: WebWindowFeatures -- ^ window featuress
    -> Int               -- ^ the height
    -> IO ()
webWindowFeaturesSetHeight = 
  objectSetPropertyInt
    "height"

{- | Returns whether the locationbar should be visible for the window.

     Default value: 'True'
-}
webWindowFeaturesGetLocationbarVisible
    :: WebWindowFeatures -- ^ window featuress
    -> IO Bool           -- ^ 'True' if locationbar should be visible
webWindowFeaturesGetLocationbarVisible = 
  objectGetPropertyBool
    "locationbar-visible"

{- | Sets whether the locationbar should be visible for the window.

     Default value: 'True'
-}
webWindowFeaturesSetLocationbarVisible
    :: WebWindowFeatures -- ^ window featuress
    -> Bool              -- ^ 'False' if locationbar should not be visible
    -> IO ()
webWindowFeaturesSetLocationbarVisible = 
  objectSetPropertyBool
    "locationbar-visible"

{- | Returns whether the menubar should be visible for the window.

     Default value: 'True'
-}
webWindowFeaturesGetMenubarVisible
    :: WebWindowFeatures -- ^ window featuress
    -> IO Bool           -- ^ 'True' if menubar should be visible
webWindowFeaturesGetMenubarVisible = 
  objectGetPropertyBool
    "menubar-visible"

{- | Sets whether the menubar should be visible for the window.

     Default value: 'True'
-}
webWindowFeaturesSetMenubarVisible
    :: WebWindowFeatures -- ^ window featuress
    -> Bool              -- ^ 'False' if menubar should no be visible
    -> IO ()
webWindowFeaturesSetMenubarVisible = 
  objectSetPropertyBool
    "menubar-visible"

{- | Returns whether the scrollbars should be visible for the window.

     Default value: 'True'
-}
webWindowFeaturesGetScrollbarVisible
    :: WebWindowFeatures -- ^ window featuress
    -> IO Bool           -- ^ 'True' if scrollbars should be visible
webWindowFeaturesGetScrollbarVisible = 
  objectGetPropertyBool
    "scrollbar-visible"

{- | Sets whether the scrollbars should be visible for the window.

     Default value: 'True'
-}
webWindowFeaturesSetScrollbarVisible
    :: WebWindowFeatures -- ^ window featuress
    -> Bool              -- ^ 'True' if scrollbars should be visible
    -> IO ()
webWindowFeaturesSetScrollbarVisible = 
  objectSetPropertyBool
    "scrollbar-visible"

{- | Returns whether the statusbar should be visible for the window.

     Default value: 'True'
-}
webWindowFeaturesGetStatusbarVisible
    :: WebWindowFeatures -- ^ window featuress
    -> IO Bool           -- ^ 'True' if statusbar should be visible
webWindowFeaturesGetStatusbarVisible = 
  objectGetPropertyBool
    "statusbar-visible"

{- | Sets whether the statusbar should be visible for the window.

     Default value: 'True'
-}
webWindowFeaturesSetStatusbarVisible
    :: WebWindowFeatures -- ^ window featuress
    -> Bool              -- ^ 'True' if statusbar should be visible
    -> IO ()
webWindowFeaturesSetStatusbarVisible = 
  objectSetPropertyBool
    "statusbar-visible"

{- | Returns whether the toolbar should be visible for the window.

     Default value: 'True'
-}
webWindowFeaturesGetToolbarVisible
    :: WebWindowFeatures -- ^ window featuress
    -> IO Bool           -- ^ 'True' if toolbar should be visible
webWindowFeaturesGetToolbarVisible = 
  objectGetPropertyBool
    "toolbar-visible"

{- | Sets whether the toolbar should be visible for the window.

     Default value: 'True'
-}
webWindowFeaturesSetToolbarVisible
    :: WebWindowFeatures -- ^ window featuress
    -> Bool              -- ^ 'True' if toolbar should be visible
    -> IO ()
webWindowFeaturesSetToolbarVisible = 
  objectSetPropertyBool
    "toolbar-visible"

{- | Returns the width of the window on the screen.

     Allowed values: >= G_MAXULONG

     Default value: -1
-}
webWindowFeaturesGetWidth
    :: WebWindowFeatures -- ^ window featuress
    -> IO Int            -- ^ the width
webWindowFeaturesGetWidth = 
  objectGetPropertyInt
    "width"

{- | Sets the width of the window on the screen.

     Allowed values: >= G_MAXULONG

     Default value: -1
-}
webWindowFeaturesSetWidth
    :: WebWindowFeatures -- ^ window featuress
    -> Int               -- ^ the width
    -> IO ()
webWindowFeaturesSetWidth = 
  objectSetPropertyInt
    "width"

{- | Returns the starting x position of the window on the screen.

     Allowed values: >= G_MAXULONG

     Default value: -1
-}
webWindowFeaturesGetX
    :: WebWindowFeatures -- ^ window featuress
    -> IO Int            -- ^ the starting x position
webWindowFeaturesGetX = 
  objectGetPropertyInt
    "x"

{- | Sets the starting x position of the window on the screen.

     Allowed values: >= G_MAXULONG

     Default value: -1
-}
webWindowFeaturesSetX
    :: WebWindowFeatures -- ^ window featuress
    -> Int               -- ^ the starting x position
    -> IO ()
webWindowFeaturesSetX = 
  objectSetPropertyInt
    "x"

{- | Returns the starting y position of the window on the screen.

     Allowed values: >= G_MAXULONG

     Default value: -1
-}
webWindowFeaturesGetY
    :: WebWindowFeatures -- ^ window featuress
    -> IO Int            -- ^ the starting y position
webWindowFeaturesGetY = 
  objectGetPropertyInt
    "y"

{- | Sets the starting y position of the window on the screen.

     Allowed values: >= G_MAXULONG

     Default value: -1
-}
webWindowFeaturesSetY
    :: WebWindowFeatures -- ^ window featuress
    -> Int               -- ^ the starting y position
    -> IO ()
webWindowFeaturesSetY = 
  objectSetPropertyInt
    "y"

