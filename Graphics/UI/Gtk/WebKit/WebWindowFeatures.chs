{-# LANGUAGE ForeignFunctionInterface #-}

{# context lib="libwebkit" prefix="webkit_" #}

module Graphics.UI.Gtk.WebKit.WebWindowFeatures
    ( WebWindowFeatures

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

{- TODO 
gboolean            webkit_web_window_features_equal    (WebKitWebWindowFeatures *features1,
                                                         WebKitWebWindowFeatures *features2);
WebKitWebWindowFeatures * webkit_web_window_features_new
                                                        (void);
-}

-- Properties -----------------------------------------------------------------

{- TODO 
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
