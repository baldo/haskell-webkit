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

    , webWindowFeaturesNew
    , webWindowFeaturesEqual

    ) where

#include <webkit/webkitwebwindowfeatures.h>

import Foreign.C
import GHC.Ptr
import System.Glib.FFI
import System.Glib.GType

import Control.Monad

import Graphics.UI.Gtk
    ( makeNewObject
    )

{#import Graphics.UI.Gtk.WebKit.General.Types#}
    ( WebWindowFeatures

    , mkWebWindowFeatures
    , unWebWindowFeatures

    , withWebWindowFeatures
    )

-- |Decides if a WebKitWebWindowFeatures instance equals another, 
--  as in has the same values.
webWindowFeaturesEqual :: WebWindowFeatures -- ^ a 'WebWindowFeatures' instance
                       -> WebWindowFeatures -- ^ another 'WebWindowFeatures' instance
                       -> IO Bool           -- ^ True if the instances have 
                                            --   the same values, False otherwise
webWindowFeaturesEqual f1 f2 =
    withWebWindowFeatures f1 $ \pf1 ->
        withWebWindowFeatures f2 $ \pf2 ->
            liftM toBool $ 
                {#call web_window_features_equal#} pf1 pf2

-- |Creates a new 'WebWindowFeatures' instance with default values. 
--  It must be manually attached to a 'WebView'.
webWindowFeaturesNew :: IO WebWindowFeatures -- ^ a new 'WebWindowFeatures' instance
webWindowFeaturesNew =
    makeNewObject mkWebWindowFeatures $
        {#call web_window_features_new#} 

-- Properties -----------------------------------------------------------------


{- TODO figure out what Construct means
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
