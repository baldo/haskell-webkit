{-# LANGUAGE ForeignFunctionInterface #-}

{# context lib="glib" prefix="g_" #}

module Graphics.UI.Gtk.WebKit.General.General
    ( initWebKit
    ) where
 
#include <glib/gthread.h>

import System.Glib.FFI

import Control.Monad.Trans
    ( MonadIO
    , liftIO
    )

import Graphics.UI.Gtk
    ( initGUI
    )

initWebKit :: MonadIO m => m ()
initWebKit = liftIO $ do
    {#call unsafe thread_init#} nullPtr
    _ <- initGUI
    return ()

