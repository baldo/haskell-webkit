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

import System.Glib.FFI
import System.Glib.GType
    ( GType
    )
import System.Glib.Properties
    ( objectGetPropertyBool
    , objectSetPropertyBool

    , objectGetPropertyInt
    , objectSetPropertyInt
    )

import Control.Monad.Trans
    ( MonadIO
    , liftIO
    )

{#import Graphics.UI.Gtk.WebKit.General.Types#}
    ( WebWindowFeatures

    , makeWebWindowFeatures
    , withWebWindowFeatures
    )

webWindowFeaturesGetType
    :: MonadIO m
    => m GType
webWindowFeaturesGetType = liftIO $
    {#call web_window_features_get_type#}

{- | Decides if a 'WebWindowFeatures' object equals another, as in has the same
     values.
-}
webWindowFeaturesEqual
    :: MonadIO m
    => WebWindowFeatures -- ^ a 'WebWindowFeatures' object
    -> WebWindowFeatures -- ^ another 'WebWindowFeatures' object
    -> m Bool            -- ^ 'True' if both have the same values, 'False'
                         --   otherwise
webWindowFeaturesEqual f1 f2 = liftIO $
    withWebWindowFeatures f1 $ \pf1 ->
        withWebWindowFeatures f2 $ \pf2 ->
            {#call web_window_features_equal#} pf1 pf2 >>=
                return . toBool

{- | Creates a new 'WebWindowFeatures' object with default values.
     It must be manually attached to a 'WebView'.
-}
webWindowFeaturesNew
    :: MonadIO m
    => m WebWindowFeatures -- ^ a new 'WebWindowFeatures' object
webWindowFeaturesNew = liftIO $
    makeWebWindowFeatures $
        {#call web_window_features_new#}

-- Properties -----------------------------------------------------------------

{- | Returns whether window will be displayed fullscreen.

     Default value: 'False'
-}
webWindowFeaturesGetFullscreen
    :: MonadIO m
    => WebWindowFeatures -- ^ window featuress
    -> m Bool            -- ^ 'True' if window will be displayed fullscreen
webWindowFeaturesGetFullscreen wf = liftIO $
  objectGetPropertyBool
    "fullscreen" wf

{- | Sets whether window will be displayed fullscreen.

     Default value: 'False'
-}
webWindowFeaturesSetFullscreen
    :: MonadIO m
    => WebWindowFeatures -- ^ window featuress
    -> Bool              -- ^ 'True' to display window fullscreen
    -> m ()
webWindowFeaturesSetFullscreen wf b = liftIO $
  objectSetPropertyBool
    "fullscreen" wf b

{- | Returns the height of the window on the screen.

     Allowed values: >= G_MAXULONG

     Default value: -1
-}
webWindowFeaturesGetHeight
    :: MonadIO m
    => WebWindowFeatures -- ^ window featuress
    -> m Int             -- ^ the height
webWindowFeaturesGetHeight wf = liftIO $
  objectGetPropertyInt
    "height" wf

{- | Sets the height of the window on the screen.

     Allowed values: >= G_MAXULONG

     Default value: -1
-}
webWindowFeaturesSetHeight
    :: MonadIO m
    => WebWindowFeatures -- ^ window featuress
    -> Int               -- ^ the height
    -> m ()
webWindowFeaturesSetHeight wf i = liftIO $
  objectSetPropertyInt
    "height" wf i

{- | Returns whether the locationbar should be visible for the window.

     Default value: 'True'
-}
webWindowFeaturesGetLocationbarVisible
    :: MonadIO m
    => WebWindowFeatures -- ^ window featuress
    -> m Bool            -- ^ 'True' if locationbar should be visible
webWindowFeaturesGetLocationbarVisible wf = liftIO $
  objectGetPropertyBool
    "locationbar-visible" wf

{- | Sets whether the locationbar should be visible for the window.

     Default value: 'True'
-}
webWindowFeaturesSetLocationbarVisible
    :: MonadIO m
    => WebWindowFeatures -- ^ window featuress
    -> Bool              -- ^ 'False' if locationbar should not be visible
    -> m ()
webWindowFeaturesSetLocationbarVisible wf b = liftIO $
  objectSetPropertyBool
    "locationbar-visible" wf b

{- | Returns whether the menubar should be visible for the window.

     Default value: 'True'
-}
webWindowFeaturesGetMenubarVisible
    :: MonadIO m
    => WebWindowFeatures -- ^ window featuress
    -> m Bool            -- ^ 'True' if menubar should be visible
webWindowFeaturesGetMenubarVisible wf = liftIO $
  objectGetPropertyBool
    "menubar-visible" wf

{- | Sets whether the menubar should be visible for the window.

     Default value: 'True'
-}
webWindowFeaturesSetMenubarVisible
    :: MonadIO m
    => WebWindowFeatures -- ^ window featuress
    -> Bool              -- ^ 'False' if menubar should no be visible
    -> m ()
webWindowFeaturesSetMenubarVisible wf b = liftIO $
  objectSetPropertyBool
    "menubar-visible" wf b

{- | Returns whether the scrollbars should be visible for the window.

     Default value: 'True'
-}
webWindowFeaturesGetScrollbarVisible
    :: MonadIO m
    => WebWindowFeatures -- ^ window featuress
    -> m Bool            -- ^ 'True' if scrollbars should be visible
webWindowFeaturesGetScrollbarVisible wf = liftIO $
  objectGetPropertyBool
    "scrollbar-visible" wf

{- | Sets whether the scrollbars should be visible for the window.

     Default value: 'True'
-}
webWindowFeaturesSetScrollbarVisible
    :: MonadIO m
    => WebWindowFeatures -- ^ window featuress
    -> Bool              -- ^ 'True' if scrollbars should be visible
    -> m ()
webWindowFeaturesSetScrollbarVisible wf b = liftIO $
  objectSetPropertyBool
    "scrollbar-visible" wf b

{- | Returns whether the statusbar should be visible for the window.

     Default value: 'True'
-}
webWindowFeaturesGetStatusbarVisible
    :: MonadIO m
    => WebWindowFeatures -- ^ window featuress
    -> m Bool            -- ^ 'True' if statusbar should be visible
webWindowFeaturesGetStatusbarVisible wf = liftIO $
  objectGetPropertyBool
    "statusbar-visible" wf

{- | Sets whether the statusbar should be visible for the window.

     Default value: 'True'
-}
webWindowFeaturesSetStatusbarVisible
    :: MonadIO m
    => WebWindowFeatures -- ^ window featuress
    -> Bool              -- ^ 'True' if statusbar should be visible
    -> m ()
webWindowFeaturesSetStatusbarVisible wf b = liftIO $
  objectSetPropertyBool
    "statusbar-visible" wf b

{- | Returns whether the toolbar should be visible for the window.

     Default value: 'True'
-}
webWindowFeaturesGetToolbarVisible
    :: MonadIO m
    => WebWindowFeatures -- ^ window featuress
    -> m Bool            -- ^ 'True' if toolbar should be visible
webWindowFeaturesGetToolbarVisible wf = liftIO $
  objectGetPropertyBool
    "toolbar-visible" wf

{- | Sets whether the toolbar should be visible for the window.

     Default value: 'True'
-}
webWindowFeaturesSetToolbarVisible
    :: MonadIO m
    => WebWindowFeatures -- ^ window featuress
    -> Bool              -- ^ 'True' if toolbar should be visible
    -> m ()
webWindowFeaturesSetToolbarVisible wf b = liftIO $
  objectSetPropertyBool
    "toolbar-visible" wf b

{- | Returns the width of the window on the screen.

     Allowed values: >= G_MAXULONG

     Default value: -1
-}
webWindowFeaturesGetWidth
    :: MonadIO m
    => WebWindowFeatures -- ^ window featuress
    -> m Int             -- ^ the width
webWindowFeaturesGetWidth wf = liftIO $
  objectGetPropertyInt
    "width" wf

{- | Sets the width of the window on the screen.

     Allowed values: >= G_MAXULONG

     Default value: -1
-}
webWindowFeaturesSetWidth
    :: MonadIO m
    => WebWindowFeatures -- ^ window featuress
    -> Int               -- ^ the width
    -> m ()
webWindowFeaturesSetWidth wf i = liftIO $
  objectSetPropertyInt
    "width" wf i

{- | Returns the starting x position of the window on the screen.

     Allowed values: >= G_MAXULONG

     Default value: -1
-}
webWindowFeaturesGetX
    :: MonadIO m
    => WebWindowFeatures -- ^ window featuress
    -> m Int             -- ^ the starting x position
webWindowFeaturesGetX wf = liftIO $
  objectGetPropertyInt
    "x" wf

{- | Sets the starting x position of the window on the screen.

     Allowed values: >= G_MAXULONG

     Default value: -1
-}
webWindowFeaturesSetX
    :: MonadIO m
    => WebWindowFeatures -- ^ window featuress
    -> Int               -- ^ the starting x position
    -> m ()
webWindowFeaturesSetX wf i = liftIO $
  objectSetPropertyInt
    "x" wf i

{- | Returns the starting y position of the window on the screen.

     Allowed values: >= G_MAXULONG

     Default value: -1
-}
webWindowFeaturesGetY
    :: MonadIO m
    => WebWindowFeatures -- ^ window featuress
    -> m Int             -- ^ the starting y position
webWindowFeaturesGetY wf = liftIO $
  objectGetPropertyInt
    "y" wf

{- | Sets the starting y position of the window on the screen.

     Allowed values: >= G_MAXULONG

     Default value: -1
-}
webWindowFeaturesSetY
    :: MonadIO m
    => WebWindowFeatures -- ^ window featuress
    -> Int               -- ^ the starting y position
    -> m ()
webWindowFeaturesSetY wf i = liftIO $
  objectSetPropertyInt
    "y" wf i

