{-# LANGUAGE ForeignFunctionInterface #-}

{# context lib="libwebkit" prefix="webkit_" #}

{-| Window properties of a 'WebView'

The content of a 'WebView' can request to change certain properties of a 
'WebView'. This can include the x, y position of the window, the width and 
height but also if a toolbar, scrollbar, statusbar, locationbar should be 
visible to the user, the request to show the 'WebView' fullscreen.

In the normal case one will use 'WebView.webViewGetWindowFeatures' to get 
the 'WebWindowFeatures' and then monitor the property changes. Be aware that 
the WebWindowFeatures might change change before "web-view-ready" signal is 
emitted. To be safe listen to the notify::window-features signal of the 
WebView and reconnect the signals whenever the WebWindowFeatures of a 
WebView changes.
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
import GHC.Ptr
import System.Glib.FFI
import System.Glib.GType
import System.Glib.Properties

import Control.Monad

import Graphics.UI.Gtk.Abstract.Object
    ( makeNewObject
    )

{#import Graphics.UI.Gtk.WebKit.General.Types#}
    ( WebWindowFeatures

    , mkWebWindowFeatures
    , unWebWindowFeatures

    , withWebWindowFeatures
    )

webWindowFeaturesGetType :: IO GType
webWindowFeaturesGetType =
    {#call web_window_features_get_type#}

-- | Decides if a 'WebWindowFeatures' object equals another, as in has the same
--   values.
webWindowFeaturesEqual :: WebWindowFeatures -- ^ a 'WebWindowFeatures' object
                       -> WebWindowFeatures -- ^ another 'WebWindowFeatures'
                                            --   object
                       -> IO Bool           -- ^ 'True' if both have the same
                                            --   values, 'False' otherwise
webWindowFeaturesEqual f1 f2 =
    withWebWindowFeatures f1 $ \pf1 ->
        withWebWindowFeatures f2 $ \pf2 ->
            liftM toBool $ 
                {#call web_window_features_equal#} pf1 pf2

-- | Creates a new 'WebWindowFeatures' object with default values. 
--   It must be manually attached to a 'WebView'.
webWindowFeaturesNew :: IO WebWindowFeatures -- ^ a new 'WebWindowFeatures'
                                             --   object
webWindowFeaturesNew =
    makeNewObject mkWebWindowFeatures $
        {#call web_window_features_new#} 

-- Properties -----------------------------------------------------------------


{- TODO figure out what Construct means, setter implemted
  "fullscreen"               gboolean              : Read / Write / Construct
  "height"                   gint                  : Read / Write / Construct
  "locationbar-visible"      gboolean              : Read / Write / Construct
  "menubar-visible"          gboolean              : Read / Write / Construct
  "scrollbar-visible"        gboolean              : Read / Write / Construct
  "statusbar-visible"        gboolean              : Read / Write / Construct
  "toolbar-visible"          gboolean              : Read / Write / Construct
  "width"                    gint                  : Read / Write / Construct
  "x"                        gint                  : Read / Write / Construct
  "y"                        gint                  : Read / Write / Construct
-}

webWindowFeaturesGetFullscreen :: WebWindowFeatures -> IO Bool
webWindowFeaturesGetFullscreen = 
  objectGetPropertyBool
    "fullscreen"

webWindowFeaturesSetFullscreen :: WebWindowFeatures -> Bool -> IO ()
webWindowFeaturesSetFullscreen = 
  objectSetPropertyBool
    "fullscreen"

webWindowFeaturesGetHeight :: WebWindowFeatures -> IO Int
webWindowFeaturesGetHeight = 
  objectGetPropertyInt
    "height"

webWindowFeaturesSetHeight :: WebWindowFeatures -> Int -> IO ()
webWindowFeaturesSetHeight = 
  objectSetPropertyInt
    "height"

webWindowFeaturesGetLocationbarVisible :: WebWindowFeatures -> IO Bool
webWindowFeaturesGetLocationbarVisible = 
  objectGetPropertyBool
    "locationbar-visible"

webWindowFeaturesSetLocationbarVisible :: WebWindowFeatures -> Bool -> IO ()
webWindowFeaturesSetLocationbarVisible = 
  objectSetPropertyBool
    "locationbar-visible"

webWindowFeaturesGetMenubarVisible :: WebWindowFeatures -> IO Bool
webWindowFeaturesGetMenubarVisible = 
  objectGetPropertyBool
    "menubar-visible"

webWindowFeaturesSetMenubarVisible :: WebWindowFeatures -> Bool -> IO ()
webWindowFeaturesSetMenubarVisible = 
  objectSetPropertyBool
    "menubar-visible"

webWindowFeaturesGetScrollbarVisible :: WebWindowFeatures -> IO Bool
webWindowFeaturesGetScrollbarVisible = 
  objectGetPropertyBool
    "scrollbar-visible"

webWindowFeaturesSetScrollbarVisible :: WebWindowFeatures -> Bool -> IO ()
webWindowFeaturesSetScrollbarVisible = 
  objectSetPropertyBool
    "scrollbar-visible"

webWindowFeaturesGetStatusbarVisible :: WebWindowFeatures -> IO Bool
webWindowFeaturesGetStatusbarVisible = 
  objectGetPropertyBool
    "statusbar-visible"

webWindowFeaturesSetStatusbarVisible :: WebWindowFeatures -> Bool -> IO ()
webWindowFeaturesSetStatusbarVisible = 
  objectSetPropertyBool
    "statusbar-visible"

webWindowFeaturesGetToolbarVisible :: WebWindowFeatures -> IO Bool
webWindowFeaturesGetToolbarVisible = 
  objectGetPropertyBool
    "toolbar-visible"

webWindowFeaturesSetToolbarVisible :: WebWindowFeatures -> Bool -> IO ()
webWindowFeaturesSetToolbarVisible = 
  objectSetPropertyBool
    "toolbar-visible"

webWindowFeaturesGetWidth :: WebWindowFeatures -> IO Int
webWindowFeaturesGetWidth = 
  objectGetPropertyInt
    "width"

webWindowFeaturesSetWidth :: WebWindowFeatures -> Int -> IO ()
webWindowFeaturesSetWidth = 
  objectSetPropertyInt
    "width"

webWindowFeaturesGetX :: WebWindowFeatures -> IO Int
webWindowFeaturesGetX = 
  objectGetPropertyInt
    "x"

webWindowFeaturesSetX :: WebWindowFeatures -> Int -> IO ()
webWindowFeaturesSetX = 
  objectSetPropertyInt
    "x"

webWindowFeaturesGetY :: WebWindowFeatures -> IO Int
webWindowFeaturesGetY = 
  objectGetPropertyInt
    "y"

webWindowFeaturesSetY :: WebWindowFeatures -> Int -> IO ()
webWindowFeaturesSetY = 
  objectSetPropertyInt
    "y"

