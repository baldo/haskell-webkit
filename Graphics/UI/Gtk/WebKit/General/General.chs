{-# LANGUAGE ForeignFunctionInterface #-}

{# context lib="glib" prefix="g_" #}

module Graphics.UI.Gtk.WebKit.General.General
    ( initWebKit
    ) where
 
#include <glib/gthread.h>

import System.Glib.FFI

import Graphics.UI.Gtk
    ( initGUI
    )

initWebKit :: IO ()
initWebKit = do
    {#call unsafe thread_init#} nullPtr
    initGUI
    return ()

