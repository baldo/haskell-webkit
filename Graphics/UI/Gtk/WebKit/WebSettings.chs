{-# LANGUAGE ForeignFunctionInterface #-}

-- TODO: Understand construct stuff with properties...

{# context lib="libwebkit" prefix="webkit_" #}

module Graphics.UI.Gtk.WebKit.WebSettings 
    ( WebSettings 

    , webSettingsNew
    , webSettingsCopy
    , webSettingsGetUserAgent

    -- Properties --------------------------------------------------------------

    , webSettingsGetAutoLoadImages
    , webSettingsSetAutoLoadImages

    , webSettingsGetAutoShrinkImages
    , webSettingsSetAutoShrinkImages

    , webSettingsGetCursiveFontFamily
    , webSettingsSetCursiveFontFamily

    , webSettingsGetDefaultEncoding
    , webSettingsSetDefaultEncoding

    , webSettingsGetDefaultFontFamily
    , webSettingsSetDefaultFontFamily

    , webSettingsGetDefaultFontSize
    , webSettingsSetDefaultFontSize

    , webSettingsGetDefaultMonospaceFontSize
    , webSettingsSetDefaultMonospaceFontSize

    , webSettingsGetEditingBehavior
    , webSettingsSetEditingBehavior

    , webSettingsGetEnableCaretBrowsing
    , webSettingsSetEnableCaretBrowsing

    , webSettingsGetEnableDefaultContextMenu
    , webSettingsSetEnableDefaultContextMenu

    , webSettingsGetEnableDeveloperExtras
    , webSettingsSetEnableDeveloperExtras

    --, webSettingsGetEnableDomPaste
    --, webSettingsSetEnableDomPaste

    --, webSettingsGetEnableHtml5Database
    --, webSettingsSetEnableHtml5Database

    --, webSettingsGetEnableHtml5LocalStorage
    --, webSettingsSetEnableHtml5LocalStorage

    --, webSettingsGetEnableOfflineWebApplicationCache
    --, webSettingsSetEnableOfflineWebApplicationCache

    --, webSettingsGetEnablePageCache
    --, webSettingsSetEnablePageCache

    --, webSettingsGetEnablePlugins
    --, webSettingsSetEnablePlugins

    --, webSettingsGetEnablePrivateBrowsing
    --, webSettingsSetEnablePrivateBrowsing

    , webSettingsGetEnableScripts 
    , webSettingsSetEnableScripts 

    --, webSettingsGetEnableSiteSpecificQuirks
    --, webSettingsSetEnableSiteSpecificQuirks

    --, webSettingsGetEnableSpellChecking
    --, webSettingsSetEnableSpellChecking

    --, webSettingsGetEnableUniversalAccessFromFileUris
    --, webSettingsSetEnableUniversalAccessFromFileUris

    --, webSettingsGetEnableXssAuditor
    --, webSettingsSetEnableXssAuditor

    --, webSettingsGetEnforce96Dpi
    --, webSettingsSetEnforce96Dpi

    --, webSettingsGetFantasyFontFamily
    --, webSettingsSetFantasyFontFamily

    --, webSettingsGetJavascriptCanOpenWindowsAutomatically
    --, webSettingsSetJavascriptCanOpenWindowsAutomatically

    --, webSettingsGetMinimumFontSize
    --, webSettingsSetMinimumFontSize

    --, webSettingsGetMinimumLogicalFontSize
    --, webSettingsSetMinimumLogicalFontSize

    --, webSettingsGetMonospaceFontFamily
    --, webSettingsSetMonospaceFontFamily

    --, webSettingsGetPrintBackgrounds
    --, webSettingsSetPrintBackgrounds

    --, webSettingsGetResizableTextAreas
    --, webSettingsSetResizableTextAreas

    --, webSettingsGetSansSerifFontFamily
    --, webSettingsSetSansSerifFontFamily

    --, webSettingsGetSerifFontFamily
    --, webSettingsSetSerifFontFamily

    --, webSettingsGetSpellCheckingLanguages
    --, webSettingsSetSpellCheckingLanguages

    --, webSettingsGetTabKeyCyclesThroughElements
    --, webSettingsSetTabKeyCyclesThroughElements

    --, webSettingsGetUserAgent
    --, webSettingsSetUserAgent

    --, webSettingsGetUserStylesheetUri
    --, webSettingsSetUserStylesheetUri

    --, webSettingsGetZoomStep
    --, webSettingsSetZoomStep
    ) where

#include <webkit/webkitwebsettings.h>

import Foreign.C
import GHC.Ptr
import System.Glib.FFI

import System.Glib.Properties

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
    ( EditingBehavior (..)
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

-- Properties -----------------------------------------------------------------

webSettingsGetAutoLoadImages :: WebSettings -> IO Bool
webSettingsGetAutoLoadImages =
    objectGetPropertyBool
        "auto-load-images"

webSettingsSetAutoLoadImages :: WebSettings -> Bool -> IO ()
webSettingsSetAutoLoadImages =
    objectSetPropertyBool
        "auto-load-images"

{- TODO
"auto-load-images" gboolean : Read / Write / Construct
-}

webSettingsGetAutoShrinkImages :: WebSettings -> IO Bool
webSettingsGetAutoShrinkImages =
    objectGetPropertyBool
        "auto-shrink-images"

webSettingsSetAutoShrinkImages :: WebSettings -> Bool -> IO ()
webSettingsSetAutoShrinkImages =
    objectSetPropertyBool
        "auto-shrink-images"

{- TODO
"auto-shrink-images" gboolean : Read / Write / Construct
-}

webSettingsGetCursiveFontFamily :: WebSettings -> IO String
webSettingsGetCursiveFontFamily =
    objectGetPropertyString
        "cursive-font-family"

webSettingsSetCursiveFontFamily :: WebSettings -> String -> IO ()
webSettingsSetCursiveFontFamily =
    objectSetPropertyString
        "cursive-font-family"

{- TODO
"cursive-font-family" gchar* : Read / Write / Construct
-}

webSettingsGetDefaultEncoding :: WebSettings -> IO String
webSettingsGetDefaultEncoding =
    objectGetPropertyString
        "default-encoding"

webSettingsSetDefaultEncoding :: WebSettings -> String -> IO ()
webSettingsSetDefaultEncoding =
    objectSetPropertyString
        "default-encoding"

{- TODO
"default-encoding" gchar* : Read / Write / Construct
-}

webSettingsGetDefaultFontFamily :: WebSettings -> IO String
webSettingsGetDefaultFontFamily =
    objectGetPropertyString
        "default-font-family"

webSettingsSetDefaultFontFamily :: WebSettings -> String -> IO ()
webSettingsSetDefaultFontFamily =
    objectSetPropertyString
        "default-font-family"

{- TODO
"default-font-family" gchar* : Read / Write / Construct
-}

webSettingsGetDefaultFontSize :: WebSettings -> IO Int
webSettingsGetDefaultFontSize =
    objectGetPropertyInt
        "default-font-size"

webSettingsSetDefaultFontSize :: WebSettings -> Int -> IO ()
webSettingsSetDefaultFontSize =
    objectSetPropertyInt
        "default-font-size"

{- TODO
"default-font-size" gint : Read / Write / Construct
-}

webSettingsGetDefaultMonospaceFontSize :: WebSettings -> IO Int
webSettingsGetDefaultMonospaceFontSize =
    objectGetPropertyInt
        "default-monospace-font-size"

webSettingsSetDefaultMonospaceFontSize :: WebSettings -> Int -> IO ()
webSettingsSetDefaultMonospaceFontSize =
    objectSetPropertyInt
        "default-monospace-font-size"

{- TODO
"default-monospace-font-size" gint : Read / Write / Construct
-}

webSettingsGetEditingBehavior :: WebSettings -> IO EditingBehavior
webSettingsGetEditingBehavior ws =
    (objectGetPropertyInt
        "editing-behavior" ws)
            >>= (return . toEnum)

webSettingsSetEditingBehavior :: WebSettings -> EditingBehavior -> IO ()
webSettingsSetEditingBehavior ws enum =
    objectSetPropertyInt
        "editing-behavior" ws
            $ fromEnum enum

{- TODO
"editing-behavior" WebKitEditingBehavior : Read / Write / Construct
-}

webSettingsGetEnableCaretBrowsing :: WebSettings -> IO Bool
webSettingsGetEnableCaretBrowsing =
    objectGetPropertyBool
        "enable-caret-browsing"

webSettingsSetEnableCaretBrowsing :: WebSettings -> Bool -> IO ()
webSettingsSetEnableCaretBrowsing =
    objectSetPropertyBool
        "enable-caret-browsing"

{- TODO
"enable-caret-browsing" gboolean : Construct
-}

webSettingsGetEnableDefaultContextMenu :: WebSettings -> IO Bool
webSettingsGetEnableDefaultContextMenu =
    objectGetPropertyBool
        "enable-default-context-menu"

webSettingsSetEnableDefaultContextMenu :: WebSettings -> Bool -> IO ()
webSettingsSetEnableDefaultContextMenu =
    objectSetPropertyBool
        "enable-default-context-menu"

{- TODO
"enable-default-context-menu" gboolean : Read / Write / Construct
-}

webSettingsGetEnableDeveloperExtras :: WebSettings -> IO Bool
webSettingsGetEnableDeveloperExtras =
    objectGetPropertyBool
        "enable-developer-extras"

webSettingsSetEnableDeveloperExtras :: WebSettings -> Bool -> IO ()
webSettingsSetEnableDeveloperExtras =
    objectSetPropertyBool
        "enable-developer-extras"

{- TODO
"enable-developer-extras" gboolean : Read / Write / Construct
-}

{- TODO
"enable-dom-paste" gboolean : Read / Write / Construct
-}

{- TODO
"enable-html5-database" gboolean : Read / Write / Construct
-}

{- TODO
"enable-html5-local-storage" gboolean : Read / Write / Construct
-}

{- TODO
"enable-offline-web-application-cache" gboolean : Read / Write / Construct
-}

{- TODO
"enable-page-cache" gboolean : Read / Write / Construct
-}

{- TODO
"enable-plugins" gboolean : Read / Write / Construct
-}

{- TODO
"enable-private-browsing" gboolean : Read / Write / Construct
-}

webSettingsGetEnableScripts :: WebSettings -> IO Bool
webSettingsGetEnableScripts =
    objectGetPropertyBool
        "enable-scripts"

webSettingsSetEnableScripts :: WebSettings -> Bool -> IO ()
webSettingsSetEnableScripts =
    objectSetPropertyBool
        "enable-scripts"

{- TODO
"enable-scripts" gboolean : Read / Write / Construct
-}

{- TODO
"enable-site-specific-quirks" gboolean : Read / Write / Construct
-}

{- TODO
"enable-spell-checking" gboolean : Read / Write / Construct
-}

{- TODO
"enable-universal-access-from-file-uris" gboolean : Read / Write / Construct
-}

{- TODO
"enable-xss-auditor" gboolean : Read / Write / Construct
-}

{- TODO
"enforce-96-dpi" gboolean : Read / Write / Construct
-}

{- TODO
"fantasy-font-family" gchar* : Read / Write / Construct
-}

{- TODO
"javascript-can-open-windows-automatically" gboolean : Read / Write / Construct
-}

{- TODO
"minimum-font-size" gint : Read / Write / Construct
-}

{- TODO
"minimum-logical-font-size" gint : Read / Write / Construct
-}

{- TODO
"monospace-font-family" gchar* : Read / Write / Construct
-}

{- TODO
"print-backgrounds" gboolean : Read / Write / Construct
-}

{- TODO
"resizable-text-areas" gboolean : Read / Write / Construct
-}

{- TODO
"sans-serif-font-family" gchar* : Read / Write / Construct
-}

{- TODO
"serif-font-family" gchar* : Read / Write / Construct
-}

{- TODO
"spell-checking-languages" gchar* : Read / Write / Construct
-}

{- TODO
"tab-key-cycles-through-elements" gboolean : Read / Write / Construct
-}

{- TODO
"user-agent" gchar* : Read / Write / Construct
-}

{- TODO
"user-stylesheet-uri" gchar* : Read / Write / Construct
-}

{- TODO
"zoom-step" gfloat : Read / Write / Construct
-}

