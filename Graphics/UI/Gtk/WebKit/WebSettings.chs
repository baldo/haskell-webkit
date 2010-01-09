{-# LANGUAGE ForeignFunctionInterface #-}

-- TODO: Understand construct stuff with properties...

{# context lib="libwebkit" prefix="webkit_" #}

module Graphics.UI.Gtk.WebKit.WebSettings 
    ( WebSettings 

    , webSettingsNew
    , webSettingsCopy
    --, webSettingsGetUserAgent

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

    , webSettingsGetEnableDomPaste
    , webSettingsSetEnableDomPaste

    , webSettingsGetEnableHtml5Database
    , webSettingsSetEnableHtml5Database

    , webSettingsGetEnableHtml5LocalStorage
    , webSettingsSetEnableHtml5LocalStorage

    , webSettingsGetEnableOfflineWebApplicationCache
    , webSettingsSetEnableOfflineWebApplicationCache

    , webSettingsGetEnablePageCache
    , webSettingsSetEnablePageCache

    , webSettingsGetEnablePlugins
    , webSettingsSetEnablePlugins

    , webSettingsGetEnablePrivateBrowsing
    , webSettingsSetEnablePrivateBrowsing

    , webSettingsGetEnableScripts 
    , webSettingsSetEnableScripts 

    , webSettingsGetEnableSiteSpecificQuirks
    , webSettingsSetEnableSiteSpecificQuirks

    , webSettingsGetEnableSpellChecking
    , webSettingsSetEnableSpellChecking

    , webSettingsGetEnableUniversalAccessFromFileUris
    , webSettingsSetEnableUniversalAccessFromFileUris

    , webSettingsGetEnableXssAuditor
    , webSettingsSetEnableXssAuditor

    , webSettingsGetEnforce96Dpi
    , webSettingsSetEnforce96Dpi

    , webSettingsGetFantasyFontFamily
    , webSettingsSetFantasyFontFamily

    , webSettingsGetJavascriptCanOpenWindowsAutomatically
    , webSettingsSetJavascriptCanOpenWindowsAutomatically

    , webSettingsGetMinimumFontSize
    , webSettingsSetMinimumFontSize

    , webSettingsGetMinimumLogicalFontSize
    , webSettingsSetMinimumLogicalFontSize

    , webSettingsGetMonospaceFontFamily
    , webSettingsSetMonospaceFontFamily

    , webSettingsGetPrintBackgrounds
    , webSettingsSetPrintBackgrounds

    , webSettingsGetResizableTextAreas
    , webSettingsSetResizableTextAreas

    , webSettingsGetSansSerifFontFamily
    , webSettingsSetSansSerifFontFamily

    , webSettingsGetSerifFontFamily
    , webSettingsSetSerifFontFamily

    , webSettingsGetSpellCheckingLanguages
    , webSettingsSetSpellCheckingLanguages

    , webSettingsGetTabKeyCyclesThroughElements
    , webSettingsSetTabKeyCyclesThroughElements

    , webSettingsGetUserAgent
    , webSettingsSetUserAgent

    , webSettingsGetUserStylesheetUri
    , webSettingsSetUserStylesheetUri

    , webSettingsGetZoomStep
    , webSettingsSetZoomStep
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

{- Same as property function...
webSettingsGetUserAgent :: WebSettings -> IO String 
webSettingsGetUserAgent settings = 
    withForeignPtr (unWebSettings settings) $ \ptr ->
       {#call web_settings_get_user_agent#} ptr >>= peekCString
-}

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

webSettingsGetEnableDomPaste :: WebSettings -> IO Bool
webSettingsGetEnableDomPaste =
    objectGetPropertyBool
        "enable-dom-paste"

webSettingsSetEnableDomPaste :: WebSettings -> Bool -> IO ()
webSettingsSetEnableDomPaste =
    objectSetPropertyBool
        "enable-dom-paste"

{- TODO
"enable-dom-paste" gboolean : Read / Write / Construct
-}

webSettingsGetEnableHtml5Database :: WebSettings -> IO Bool
webSettingsGetEnableHtml5Database =
    objectGetPropertyBool
        "enable-html5-database"

webSettingsSetEnableHtml5Database :: WebSettings -> Bool -> IO ()
webSettingsSetEnableHtml5Database =
    objectSetPropertyBool
        "enable-html5-database"

{- TODO
"enable-html5-database" gboolean : Read / Write / Construct
-}

webSettingsGetEnableHtml5LocalStorage :: WebSettings -> IO Bool
webSettingsGetEnableHtml5LocalStorage =
    objectGetPropertyBool
        "enable-html5-local-storage"

webSettingsSetEnableHtml5LocalStorage :: WebSettings -> Bool -> IO ()
webSettingsSetEnableHtml5LocalStorage =
    objectSetPropertyBool
        "enable-html5-local-storage"

{- TODO
"enable-html5-local-storage" gboolean : Read / Write / Construct
-}

webSettingsGetEnableOfflineWebApplicationCache :: WebSettings -> IO Bool
webSettingsGetEnableOfflineWebApplicationCache =
    objectGetPropertyBool
        "enable-offline-web-application-cache"

webSettingsSetEnableOfflineWebApplicationCache :: WebSettings -> Bool -> IO ()
webSettingsSetEnableOfflineWebApplicationCache =
    objectSetPropertyBool
        "enable-offline-web-application-cache"

{- TODO
"enable-offline-web-application-cache" gboolean : Read / Write / Construct
-}

webSettingsGetEnablePageCache :: WebSettings -> IO Bool
webSettingsGetEnablePageCache =
    objectGetPropertyBool
        "enable-page-cache"

webSettingsSetEnablePageCache :: WebSettings -> Bool -> IO ()
webSettingsSetEnablePageCache =
    objectSetPropertyBool
        "enable-page-cache"

{- TODO
"enable-page-cache" gboolean : Read / Write / Construct
-}

webSettingsGetEnablePlugins :: WebSettings -> IO Bool
webSettingsGetEnablePlugins =
    objectGetPropertyBool
        "enable-plugins"

webSettingsSetEnablePlugins :: WebSettings -> Bool -> IO ()
webSettingsSetEnablePlugins =
    objectSetPropertyBool
        "enable-plugins"

{- TODO
"enable-plugins" gboolean : Read / Write / Construct
-}

webSettingsGetEnablePrivateBrowsing :: WebSettings -> IO Bool
webSettingsGetEnablePrivateBrowsing =
    objectGetPropertyBool
        "enable-private-browsing"

webSettingsSetEnablePrivateBrowsing :: WebSettings -> Bool -> IO ()
webSettingsSetEnablePrivateBrowsing =
    objectSetPropertyBool
        "enable-private-browsing"

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

webSettingsGetEnableSiteSpecificQuirks :: WebSettings -> IO Bool
webSettingsGetEnableSiteSpecificQuirks =
    objectGetPropertyBool
        "enable-site-specific-quirks"

webSettingsSetEnableSiteSpecificQuirks :: WebSettings -> Bool -> IO ()
webSettingsSetEnableSiteSpecificQuirks =
    objectSetPropertyBool
        "enable-site-specific-quirks"

{- TODO
"enable-site-specific-quirks" gboolean : Read / Write / Construct
-}

webSettingsGetEnableSpellChecking :: WebSettings -> IO Bool
webSettingsGetEnableSpellChecking =
    objectGetPropertyBool
        "enable-spell-checking"

webSettingsSetEnableSpellChecking :: WebSettings -> Bool -> IO ()
webSettingsSetEnableSpellChecking =
    objectSetPropertyBool
        "enable-spell-checking"

{- TODO
"enable-spell-checking" gboolean : Read / Write / Construct
-}

webSettingsGetEnableUniversalAccessFromFileUris :: WebSettings -> IO Bool
webSettingsGetEnableUniversalAccessFromFileUris =
    objectGetPropertyBool
        "enable-universal-access-from-file-uris"

webSettingsSetEnableUniversalAccessFromFileUris :: WebSettings -> Bool -> IO ()
webSettingsSetEnableUniversalAccessFromFileUris =
    objectSetPropertyBool
        "enable-universal-access-from-file-uris"

{- TODO
"enable-universal-access-from-file-uris" gboolean : Read / Write / Construct
-}

webSettingsGetEnableXssAuditor :: WebSettings -> IO Bool
webSettingsGetEnableXssAuditor =
    objectGetPropertyBool
        "enable-xss-auditor"

webSettingsSetEnableXssAuditor :: WebSettings -> Bool -> IO ()
webSettingsSetEnableXssAuditor =
    objectSetPropertyBool
        "enable-xss-auditor"

{- TODO
"enable-xss-auditor" gboolean : Read / Write / Construct
-}

webSettingsGetEnforce96Dpi :: WebSettings -> IO Bool
webSettingsGetEnforce96Dpi =
    objectGetPropertyBool
        "enforce-96-dpi"

webSettingsSetEnforce96Dpi :: WebSettings -> Bool -> IO ()
webSettingsSetEnforce96Dpi =
    objectSetPropertyBool
        "enforce-96-dpi"

{- TODO
"enforce-96-dpi" gboolean : Read / Write / Construct
-}

webSettingsGetFantasyFontFamily :: WebSettings -> IO Bool
webSettingsGetFantasyFontFamily =
    objectGetPropertyBool
        "fantasy-font-family"

webSettingsSetFantasyFontFamily :: WebSettings -> Bool -> IO ()
webSettingsSetFantasyFontFamily =
    objectSetPropertyBool
        "fantasy-font-family"

{- TODO
"fantasy-font-family" gchar* : Read / Write / Construct
-}

webSettingsGetJavascriptCanOpenWindowsAutomatically :: WebSettings -> IO Bool
webSettingsGetJavascriptCanOpenWindowsAutomatically =
    objectGetPropertyBool
        "javascript-can-open-windows-automatically"

webSettingsSetJavascriptCanOpenWindowsAutomatically :: WebSettings -> Bool
                                                       -> IO ()
webSettingsSetJavascriptCanOpenWindowsAutomatically =
    objectSetPropertyBool
        "javascript-can-open-windows-automatically"

{- TODO
"javascript-can-open-windows-automatically" gboolean : Read / Write / Construct
-}

webSettingsGetMinimumFontSize :: WebSettings -> IO Int
webSettingsGetMinimumFontSize =
    objectGetPropertyInt
        "minimum-font-size"

webSettingsSetMinimumFontSize :: WebSettings -> Int -> IO ()
webSettingsSetMinimumFontSize =
    objectSetPropertyInt
        "minimum-font-size"

{- TODO
"minimum-font-size" gint : Read / Write / Construct
-}

webSettingsGetMinimumLogicalFontSize :: WebSettings -> IO Int
webSettingsGetMinimumLogicalFontSize =
    objectGetPropertyInt
        "minimum-logical-font-size"

webSettingsSetMinimumLogicalFontSize :: WebSettings -> Int -> IO ()
webSettingsSetMinimumLogicalFontSize =
    objectSetPropertyInt
        "minimum-logical-font-size"

{- TODO
"minimum-logical-font-size" gint : Read / Write / Construct
-}

webSettingsGetMonospaceFontFamily :: WebSettings -> IO String
webSettingsGetMonospaceFontFamily =
    objectGetPropertyString
        "monospace-font-family"

webSettingsSetMonospaceFontFamily :: WebSettings -> String -> IO ()
webSettingsSetMonospaceFontFamily =
    objectSetPropertyString
        "monospace-font-family"

{- TODO
"monospace-font-family" gchar* : Read / Write / Construct
-}

webSettingsGetPrintBackgrounds :: WebSettings -> IO Bool
webSettingsGetPrintBackgrounds =
    objectGetPropertyBool
        "print-backgrounds"

webSettingsSetPrintBackgrounds :: WebSettings -> Bool -> IO ()
webSettingsSetPrintBackgrounds =
    objectSetPropertyBool
        "print-backgrounds"

{- TODO
"print-backgrounds" gboolean : Read / Write / Construct
-}

webSettingsGetResizableTextAreas :: WebSettings -> IO Bool
webSettingsGetResizableTextAreas =
    objectGetPropertyBool
        "resizable-text-areas"

webSettingsSetResizableTextAreas :: WebSettings -> Bool -> IO ()
webSettingsSetResizableTextAreas =
    objectSetPropertyBool
        "resizable-text-areas"

{- TODO
"resizable-text-areas" gboolean : Read / Write / Construct
-}

webSettingsGetSansSerifFontFamily :: WebSettings -> IO String
webSettingsGetSansSerifFontFamily =
    objectGetPropertyString
        "sans-serif-font-family"

webSettingsSetSansSerifFontFamily :: WebSettings -> String -> IO ()
webSettingsSetSansSerifFontFamily =
    objectSetPropertyString
        "sans-serif-font-family"

{- TODO
"sans-serif-font-family" gchar* : Read / Write / Construct
-}

webSettingsGetSerifFontFamily :: WebSettings -> IO String
webSettingsGetSerifFontFamily =
    objectGetPropertyString
        "serif-font-family"

webSettingsSetSerifFontFamily :: WebSettings -> String -> IO ()
webSettingsSetSerifFontFamily =
    objectSetPropertyString
        "serif-font-family"

{- TODO
"serif-font-family" gchar* : Read / Write / Construct
-}

webSettingsGetSpellCheckingLanguages :: WebSettings -> IO String
webSettingsGetSpellCheckingLanguages =
    objectGetPropertyString
        "spell-checking-languages"

webSettingsSetSpellCheckingLanguages :: WebSettings -> String -> IO ()
webSettingsSetSpellCheckingLanguages =
    objectSetPropertyString
        "spell-checking-languages"

{- TODO
"spell-checking-languages" gchar* : Read / Write / Construct
-}

webSettingsGetTabKeyCyclesThroughElements :: WebSettings -> IO Bool
webSettingsGetTabKeyCyclesThroughElements =
    objectGetPropertyBool
        "tab-key-cycles-through-elements"

webSettingsSetTabKeyCyclesThroughElements :: WebSettings -> Bool -> IO ()
webSettingsSetTabKeyCyclesThroughElements =
    objectSetPropertyBool
        "tab-key-cycles-through-elements"

{- TODO
"tab-key-cycles-through-elements" gboolean : Read / Write / Construct
-}

webSettingsGetUserAgent :: WebSettings -> IO String
webSettingsGetUserAgent =
    objectGetPropertyString
        "user-agent"

webSettingsSetUserAgent :: WebSettings -> String -> IO ()
webSettingsSetUserAgent =
    objectSetPropertyString
        "user-agent"

{- TODO
"user-agent" gchar* : Read / Write / Construct
-}

webSettingsGetUserStylesheetUri :: WebSettings -> IO String
webSettingsGetUserStylesheetUri =
    objectGetPropertyString
        "user-stylesheet-uri"

webSettingsSetUserStylesheetUri :: WebSettings -> String -> IO ()
webSettingsSetUserStylesheetUri =
    objectSetPropertyString
        "user-stylesheet-uri"

{- TODO
"user-stylesheet-uri" gchar* : Read / Write / Construct
-}

webSettingsGetZoomStep :: WebSettings -> IO Float
webSettingsGetZoomStep =
    objectGetPropertyFloat
        "zoom-step"

webSettingsSetZoomStep :: WebSettings -> Float -> IO ()
webSettingsSetZoomStep =
    objectSetPropertyFloat
        "zoom-step"

{- TODO
"zoom-step" gfloat : Read / Write / Construct
-}

