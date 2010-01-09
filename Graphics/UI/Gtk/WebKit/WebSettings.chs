{-# LANGUAGE ForeignFunctionInterface #-}

{# context lib="libwebkit" prefix="webkit_" #}

module Graphics.UI.Gtk.WebKit.WebSettings 
    ( WebSettings 

    , webSettingsNew
    , webSettingsCopy
    , webSettingsGetUserAgent
    ) where

#include <webkit/webkitwebsettings.h>

import Foreign.C
import GHC.Ptr
import System.Glib.FFI

import Control.Monad

import Graphics.UI.Gtk
    ( makeNewObject
    )

{#import Graphics.UI.Gtk.WebKit.General.Types#}
    ( WebSettings

    , mkWebSettings
    , unWebSettings 
    )


{#import Graphics.UI.Gtk.WebKit.General.Enums#}
    ( WebKitEditingBehavior (..)
    )

webSettingsCopy :: WebSettings -> IO WebSettings
webSettingsCopy settings = 
    withForeignPtr (unWebSettings settings) $ \ptr ->
        makeNewObject mkWebSettings $ {#call web_settings_copy#} ptr 

webSettingsNew :: IO WebSettings 
webSettingsNew =
    makeNewObject mkWebSettings $ {#call web_settings_new#}

webSettingsGetUserAgent :: WebSettings -> IO String 
webSettingsGetUserAgent settings = 
    withForeignPtr (unWebSettings settings) $ \ptr ->
       {#call web_settings_get_user_agent#} ptr >>= peekCString
