{-# LANGUAGE ForeignFunctionInterface #-}

{-| Control the behavior of a 'WebView'.

'WebSettings' can be applied to a 'WebView' to control the to be used
text encoding, color, font sizes, printing mode, script support, loading of
images and various other things.
-}

{# context lib="libwebkit" prefix="webkit_" #}

module Graphics.UI.Gtk.WebKit.WebSettings
    ( WebSettings

    -- * Functions

    , webSettingsNew
    , webSettingsCopy

    -- * Properties

    , webSettingsGetAutoLoadImages
    , webSettingsSetAutoLoadImages

    , webSettingsGetAutoResizeWindow
    , webSettingsSetAutoResizeWindow

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

    , webSettingsGetEnableFileAccessFromFileUris
    , webSettingsSetEnableFileAccessFromFileUris

    , webSettingsGetEnableHtml5Database
    , webSettingsSetEnableHtml5Database

    , webSettingsGetEnableHtml5LocalStorage
    , webSettingsSetEnableHtml5LocalStorage

    , webSettingsGetEnableJavaApplet
    , webSettingsSetEnableJavaApplet

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

import System.Glib.FFI
import System.Glib.Properties

{#import Graphics.UI.Gtk.WebKit.General.Types#}
    ( WebSettings

    , makeWebSettings
    , withWebSettings
    )


{#import Graphics.UI.Gtk.WebKit.General.Enums#}
    ( EditingBehavior (..)
    )

-- Functions ------------------------------------------------------------------

-- | Creates a copy of the given 'WebSettings'.
webSettingsCopy
    :: WebSettings    -- ^ settings
    -> IO WebSettings -- ^ the copy
webSettingsCopy settings =
    withWebSettings settings $ \ptr ->
        makeWebSettings $ {#call web_settings_copy#} ptr

{- | Creates new 'WebSettings' with default values. It must be manually attached
     to a 'WebView'.
-}
webSettingsNew
    :: IO WebSettings -- ^ the new 'WebSettings'
webSettingsNew =
    makeWebSettings $ {#call web_settings_new#}

-- Properties -----------------------------------------------------------------

{- | Get whether images are automatically loaded.

     Default value: 'True'
-}
webSettingsGetAutoLoadImages
    :: WebSettings -- ^ settings
    -> IO Bool     -- ^ 'True' if images are automatically loaded
webSettingsGetAutoLoadImages =
    objectGetPropertyBool
        "auto-load-images"

{- | Set whether to load images automatically.

     Default value: 'True'
-}
webSettingsSetAutoLoadImages
    :: WebSettings -- ^ settings
    -> Bool        -- ^ 'True' if images shall be automatically loaded
    -> IO ()
webSettingsSetAutoLoadImages =
    objectSetPropertyBool
        "auto-load-images"

{- | Gets the value of this property. For more information look at
     'webSettingsSetAutoResizeWindow'.

     Default value: 'False'
-}
webSettingsGetAutoResizeWindow
    :: WebSettings -- ^ settings
    -> IO Bool     -- ^ 'True' if automatic resizing is enabled
webSettingsGetAutoResizeWindow =
    objectGetPropertyBool
        "auto-resize-window"

{- | Web pages can request to modify the size and position of the window
     containing the 'WebView' through various DOM methods (resizeTo, moveTo,
     resizeBy, moveBy). By default WebKit will not honor this requests, but you
     can set this property to 'True' if you'd like it to do so. If you wish to
     handle this manually, you can connect to the notify signal for the
     'WebWindowFeatures' of your 'WebView'.

     Default value: 'False'
-}
webSettingsSetAutoResizeWindow
    :: WebSettings -- ^ settings
    -> Bool        -- ^ 'True' if automatic resizing shall be enabled
    -> IO ()
webSettingsSetAutoResizeWindow =
    objectSetPropertyBool
        "auto-resize-window"

{- | Returns whether standalone images are automatically shrinked to fit.

     Default value: 'True'
-}
webSettingsGetAutoShrinkImages
    :: WebSettings -- ^ settings
    -> IO Bool     -- ^ 'True' if automatic shrinking is enabled
webSettingsGetAutoShrinkImages =
    objectGetPropertyBool
        "auto-shrink-images"

{- | Sets whether to automatically shrink standalone images to fit.

     Default value: 'True'
-}
webSettingsSetAutoShrinkImages
    :: WebSettings -- ^ settings
    -> Bool        -- ^ 'False' to disable automatic shrinking
    -> IO ()
webSettingsSetAutoShrinkImages =
    objectSetPropertyBool
        "auto-shrink-images"

{- | Returns the default Cursive font family used to display text.

     Default value: \"serif\"
-}
webSettingsGetCursiveFontFamily
    :: WebSettings -- ^ settings
    -> IO String   -- ^ the font family
webSettingsGetCursiveFontFamily =
    objectGetPropertyString
        "cursive-font-family"

{- | Sets the default Cursive font family used to display text.

     Default value: \"serif\"
-}
webSettingsSetCursiveFontFamily
    :: WebSettings -- ^ settings
    -> String      -- ^ the font family
    -> IO ()
webSettingsSetCursiveFontFamily =
    objectSetPropertyString
        "cursive-font-family"

{- | Returns the default encoding used to display text.

     Default value: \"iso-8859-1\"
-}
webSettingsGetDefaultEncoding
    :: WebSettings -- ^ settings
    -> IO String   -- ^ the encoding
webSettingsGetDefaultEncoding =
    objectGetPropertyString
        "default-encoding"

{- | Sets the default encoding used to display text.

     Default value: \"iso-8859-1\"
-}
webSettingsSetDefaultEncoding
    :: WebSettings -- ^ settings
    -> String      -- ^ the encoding
    -> IO ()
webSettingsSetDefaultEncoding =
    objectSetPropertyString
        "default-encoding"

{- | Returns the default font family used to display text.

     Default value: \"sans-serif\"
-}
webSettingsGetDefaultFontFamily
    :: WebSettings -- ^ settings
    -> IO String   -- ^ the font family
webSettingsGetDefaultFontFamily =
    objectGetPropertyString
        "default-font-family"

{- | Sets the default font family used to display text.

     Default value: \"sans-serif\"
-}
webSettingsSetDefaultFontFamily
    :: WebSettings -- ^ settings
    -> String      -- ^ the font family
    -> IO ()
webSettingsSetDefaultFontFamily =
    objectSetPropertyString
        "default-font-family"

{- | Returns the default font size used to display text.

     Allowed values: >= 5

     Default value: 12
-}
webSettingsGetDefaultFontSize
    :: WebSettings -- ^ settings
    -> IO Int      -- ^ the font size
webSettingsGetDefaultFontSize =
    objectGetPropertyInt
        "default-font-size"

{- | Sets the default font size used to display text.

     Allowed values: >= 5

     Default value: 12
-}
webSettingsSetDefaultFontSize
    :: WebSettings -- ^ settings
    -> Int         -- ^ the font size
    -> IO ()
webSettingsSetDefaultFontSize =
    objectSetPropertyInt
        "default-font-size"

{- | Returns the default font size used to display monospace text.

     Allowed values: >= 5

     Default value: 10
-}
webSettingsGetDefaultMonospaceFontSize
    :: WebSettings -- ^ settings
    -> IO Int      -- ^ the font size
webSettingsGetDefaultMonospaceFontSize =
    objectGetPropertyInt
        "default-monospace-font-size"

{- | Sets the default font size used to display monospace text.

     Allowed values: >= 5

     Default value: 10
-}
webSettingsSetDefaultMonospaceFontSize
    :: WebSettings -- ^ settings
    -> Int         -- ^ the font size
    -> IO ()
webSettingsSetDefaultMonospaceFontSize =
    objectSetPropertyInt
        "default-monospace-font-size"

{- | Returns the 'EditingBehavior'. For more information have a look at
     'webSettingsSetEditingBehavior'.
-}
webSettingsGetEditingBehavior
    :: WebSettings        -- ^ settings
    -> IO EditingBehavior -- ^ the editing behavior
webSettingsGetEditingBehavior ws =
    (objectGetPropertyInt
        "editing-behavior" ws)
            >>= (return . toEnum)

{- | This setting controls various editing behaviors that differ between
     platforms and that have been combined in two groups, 'Mac' and 'Windows'.
     Some examples:

       1. Clicking below the last line of an editable area puts the caret at
          the end of the last line on Mac, but in the middle of the last line
          on Windows.

       2. Pushing down the arrow key on the last line puts the caret at the
          end of the last line on Mac, but does nothing on Windows. A similar
          case exists on the top line.

     Default value: 'EditingBehaviorMac'
-}
webSettingsSetEditingBehavior
    :: WebSettings     -- ^ settings
    -> EditingBehavior -- ^ the editing behavior
    -> IO ()
webSettingsSetEditingBehavior ws enum =
    objectSetPropertyInt
        "editing-behavior" ws
            $ fromEnum enum

{- | Returns whether caret browsing mode is enabled.

     Default value: 'False'
-}
webSettingsGetEnableCaretBrowsing
    :: WebSettings -- ^ settings
    -> IO Bool     -- ^ 'True' if caret browsing mode is enabled
webSettingsGetEnableCaretBrowsing =
    objectGetPropertyBool
        "enable-caret-browsing"

{- | Sets whether to enable caret browsing mode.

     Default value: 'False'
-}
webSettingsSetEnableCaretBrowsing
    :: WebSettings -- ^ settings
    -> Bool        -- ^ 'True' to enable caret browsing mode
    -> IO ()
webSettingsSetEnableCaretBrowsing =
    objectSetPropertyBool
        "enable-caret-browsing"

{- | Returns whether context menu is enabled by default. For more information
     have a look at 'webSettingsSetEnableDefaultContextMenu'.
-}
webSettingsGetEnableDefaultContextMenu
    :: WebSettings -- ^ settings
    -> IO Bool     -- ^ 'False' if context menu is disabled by default
webSettingsGetEnableDefaultContextMenu =
    objectGetPropertyBool
        "enable-default-context-menu"

{- | Whether right-clicks should be handled automatically to create, and display
     the context menu. Turning this off will make WebKitGTK+ not emit the
     populate-popup signal. Notice that the default button press event handler
     may still handle right clicks for other reasons, such as in-page context
     menus, or right-clicks that are handled by the page itself.

     Default value: 'True'
-}
webSettingsSetEnableDefaultContextMenu
    :: WebSettings -- ^ settings
    -> Bool        -- ^ 'False' to disable context menu by default
    -> IO ()
webSettingsSetEnableDefaultContextMenu =
    objectSetPropertyBool
        "enable-default-context-menu"

{- | Returns whether developer extensions are enabled. For more information have
     a look at 'webSettingsSetEnableDeveloperExtras'.
-}
webSettingsGetEnableDeveloperExtras
    :: WebSettings -- ^ settings
    -> IO Bool     -- ^ 'True' if developer extensions are enabled
webSettingsGetEnableDeveloperExtras =
    objectGetPropertyBool
        "enable-developer-extras"

{- | Whether developer extensions should be enabled. This enables, for now, the
     Web Inspector, which can be controlled using the 'WebInspector' held by the
     'WebView' this setting is enabled for.

     Default value: 'False'
-}
webSettingsSetEnableDeveloperExtras
    :: WebSettings -- ^ settings
    -> Bool        -- ^ 'True' to enable developer extensions
    -> IO ()
webSettingsSetEnableDeveloperExtras =
    objectSetPropertyBool
        "enable-developer-extras"

{- | Returns whether DOM paste is enabled. For further information have a look
     at 'webSettingsSetEnableDomPaste'.
-}
webSettingsGetEnableDomPaste
    :: WebSettings -- ^ settings
    -> IO Bool     -- ^ 'True' if DOM paste is enabled
webSettingsGetEnableDomPaste =
    objectGetPropertyBool
        "enable-dom-paste"

{- | Sets whether to enable DOM paste. If set to 'True',
     document.execCommand(\"Paste\") will correctly execute and paste content
     of the clipboard.

     Default value: 'False'
-}
webSettingsSetEnableDomPaste
    :: WebSettings -- ^ settings
    -> Bool        -- ^ 'True' to enable DOM paste
    -> IO ()
webSettingsSetEnableDomPaste =
    objectSetPropertyBool
        "enable-dom-paste"

{- | Returns whether enable-file-access-from-file-uris property is enabled. For
     more information look at 'webSettingsSetEnableFileAccessFromFileUris'.
-}
webSettingsGetEnableFileAccessFromFileUris
    :: WebSettings -- ^ settings
    -> IO Bool     -- ^ 'True' if enabled
webSettingsGetEnableFileAccessFromFileUris =
    objectGetPropertyBool
        "enable-file-access-from-file-uris"

{- | Boolean property to control file access for file:// URIs. If this option is
     enabled every file:// will have its own security unique domain.

     Default value: 'False'
-}
webSettingsSetEnableFileAccessFromFileUris
    :: WebSettings -- ^ settings
    -> Bool        -- ^ 'True' to enable
    -> IO ()
webSettingsSetEnableFileAccessFromFileUris =
    objectSetPropertyBool
        "enable-file-access-from-file-uris"

{- | Returns whether HTML5 client-side SQL database support is enabled. For
     further information look at 'webSettingsSetEnableHtml5Database'.
-}
webSettingsGetEnableHtml5Database
    :: WebSettings -- ^ settings
    -> IO Bool     -- ^ 'True' if client-side SQL database support is enabled
webSettingsGetEnableHtml5Database =
    objectGetPropertyBool
        "enable-html5-database"

{- | Whether to enable HTML5 client-side SQL database support. Client-side SQL
     database allows web pages to store structured data and be able to use SQL
     to manipulate that data asynchronously.

     Default value: 'True'
-}
webSettingsSetEnableHtml5Database
    :: WebSettings -- ^ settings
    -> Bool        -- ^ 'False' to disable client-side SQL database support
    -> IO ()
webSettingsSetEnableHtml5Database =
    objectSetPropertyBool
        "enable-html5-database"

{- | Returns whether HTML5 localStorage support is enabled. localStorage
     provides simple synchronous storage access.

     Default value: 'True'
-}
webSettingsGetEnableHtml5LocalStorage
    :: WebSettings -- ^ settings
    -> IO Bool     -- ^ 'True' if localStorage support is enabled
webSettingsGetEnableHtml5LocalStorage =
    objectGetPropertyBool
        "enable-html5-local-storage"

{- | Sets whether to enable HTML5 localStorage support. localStorage
     provides simple synchronous storage access.

     Default value: 'True'
-}
webSettingsSetEnableHtml5LocalStorage
    :: WebSettings -- ^ settings
    -> Bool        -- ^ 'False' to disable localStorage support
    -> IO ()
webSettingsSetEnableHtml5LocalStorage =
    objectSetPropertyBool
        "enable-html5-local-storage"

{- | Returns whether the Java \<applet\> tag is enabled. For further information
     have a look at 'webSettingsSetEnableJavaApplet'.
-}
webSettingsGetEnableJavaApplet
    :: WebSettings -- ^ settings
    -> IO Bool     -- ^ 'True' if \<applet\> tag is enabled
webSettingsGetEnableJavaApplet =
    objectGetPropertyBool
        "enable-java-applet"

{- | Enable or disable support for the Java \<applet\> tag. Keep in mind that
     Java content can be still shown in the page through \<object\> or
     \<embed\>, which are the preferred tags for this task.

     Default value: 'True'
-}
webSettingsSetEnableJavaApplet
    :: WebSettings -- ^ settings
    -> Bool        -- ^ 'False' to disable the Java \<applet\> tag
    -> IO ()
webSettingsSetEnableJavaApplet =
    objectSetPropertyBool
        "enable-java-applet"

{- | Returns wether offline web application cache support is enabled. For more
     information look at 'webSettingsSetEnableOfflineWebApplicationCache'.
-}
webSettingsGetEnableOfflineWebApplicationCache
    :: WebSettings -- ^ settings
    -> IO Bool     -- ^ 'True' if offline web application cache is enabled
webSettingsGetEnableOfflineWebApplicationCache =
    objectGetPropertyBool
        "enable-offline-web-application-cache"

{- | Whether to enable HTML5 offline web application cache support. Offline Web
     Application Cache ensures web applications are available even when the user
     is not connected to the network.

     Default value: 'True'
-}
webSettingsSetEnableOfflineWebApplicationCache
    :: WebSettings -- ^ settings
    -> Bool        -- ^ 'False' to disable offline web application cache support
    -> IO ()
webSettingsSetEnableOfflineWebApplicationCache =
    objectSetPropertyBool
        "enable-offline-web-application-cache"

{- | Returns whether page cache is enabled. For further information have a look
     at 'webSettingsSetEnablePageCache'.
-}
webSettingsGetEnablePageCache
    :: WebSettings -- ^ settings
    -> IO Bool     -- ^ 'True' if page cache is enabled
webSettingsGetEnablePageCache =
    objectGetPropertyBool
        "enable-page-cache"

{- | Enable or disable the page cache. Disabling the page cache is generally
     only useful for special circumstances like low-memory scenarios or special
     purpose applications like static HTML viewers. This setting only controls
     the Page Cache, this cache is different than the disk-based or memory-based
     traditional resource caches, its point is to make going back and forth
     between pages much faster. For details about the different types of caches
     and their purposes see:
     <http://webkit.org/blog/427/webkit-page-cache-i-the-basics/>

     Default value: 'False'
-}
webSettingsSetEnablePageCache
    :: WebSettings -- ^ settings
    -> Bool        -- ^ 'True' to enable page cache
    -> IO ()
webSettingsSetEnablePageCache =
    objectSetPropertyBool
        "enable-page-cache"

{- | Returns whether embedded plugin objects are enabled.

     Default value: 'True'
-}
webSettingsGetEnablePlugins
    :: WebSettings -- ^ settings
    -> IO Bool     -- ^ 'True' if embedded plugin objects are enabled
webSettingsGetEnablePlugins =
    objectGetPropertyBool
        "enable-plugins"

{- | Sets whether to enable embedded plugin objects.

     Default value: 'True'
-}
webSettingsSetEnablePlugins
    :: WebSettings -- ^ settings
    -> Bool        -- ^ 'False' to disable embedded plugin objects
    -> IO ()
webSettingsSetEnablePlugins =
    objectSetPropertyBool
        "enable-plugins"

{- | Returns wheter private browsing mode is enabled. For further information
     have a look at 'webSettingsSetEnablePrivateBrowsing'.
-}
webSettingsGetEnablePrivateBrowsing
    :: WebSettings -- ^ settings
    -> IO Bool     -- ^ 'True' if private browsing mode is enabled
webSettingsGetEnablePrivateBrowsing =
    objectGetPropertyBool
        "enable-private-browsing"

{- | Whether to enable private browsing mode. Private browsing mode prevents
     WebKit from updating the global history and storing any session information
     e.g., on-disk cache, as well as suppressing any messages from being printed
     into the (javascript) console.

     This is currently experimental for WebKitGtk.

     Default value: 'False'
-}
webSettingsSetEnablePrivateBrowsing
    :: WebSettings -- ^ settings
    -> Bool        -- ^ 'True' to enable private browsing mode
    -> IO ()
webSettingsSetEnablePrivateBrowsing =
    objectSetPropertyBool
        "enable-private-browsing"

{- | Returns wheter embedded scripting languages are enabled.

     Default value: 'True'
-}
webSettingsGetEnableScripts
    :: WebSettings -- ^ settings
    -> IO Bool     -- ^ 'True' if embedded scripting languages are enabled
webSettingsGetEnableScripts =
    objectGetPropertyBool
        "enable-scripts"

{- | Sets wether to enable embedded scripting languages.

     Default value: 'True'
-}
webSettingsSetEnableScripts
    :: WebSettings -- ^ settings
    -> Bool        -- ^ 'False' to disable embedded scripting languages
    -> IO ()
webSettingsSetEnableScripts =
    objectSetPropertyBool
        "enable-scripts"

{- | Returns wheter the site-specific compatibility workarounds are enabled.

     Default value: 'False'
-}
webSettingsGetEnableSiteSpecificQuirks
    :: WebSettings -- ^ settings
    -> IO Bool     -- ^ 'True' if site-specific workarounds are enabled
webSettingsGetEnableSiteSpecificQuirks =
    objectGetPropertyBool
        "enable-site-specific-quirks"

{- | Sets wheter to enable the site-specific compatibility workarounds.

     Default value: 'False'
-}
webSettingsSetEnableSiteSpecificQuirks
    :: WebSettings -- ^ settings
    -> Bool        -- ^ 'True' to enable site-specific workarounds
    -> IO ()
webSettingsSetEnableSiteSpecificQuirks =
    objectSetPropertyBool
        "enable-site-specific-quirks"

-- TODO: new in 1.1.23 - "enable-spatial-navigation"

{- | Returns whether spell checking while typing is enabled.

     Default value: 'False'
-}
webSettingsGetEnableSpellChecking
    :: WebSettings -- ^ settings
    -> IO Bool     -- ^ 'True' if spell checking while typing is enabled
webSettingsGetEnableSpellChecking =
    objectGetPropertyBool
        "enable-spell-checking"

{- | Sets whether to enable spell checking while typing.

     Default value: 'False'
-}
webSettingsSetEnableSpellChecking
    :: WebSettings -- ^ settings
    -> Bool        -- ^ 'True' to enable spell checking while typing
    -> IO ()
webSettingsSetEnableSpellChecking =
    objectSetPropertyBool
        "enable-spell-checking"

{- | Returns whether it is allowed for files loaded through file:// URIs
     to have universal access to all pages.

     Default value: 'False'
-}
webSettingsGetEnableUniversalAccessFromFileUris
    :: WebSettings -- ^ settings
    -> IO Bool     -- ^ 'True' if universal access is granted
webSettingsGetEnableUniversalAccessFromFileUris =
    objectGetPropertyBool
        "enable-universal-access-from-file-uris"

{- | Sets whether to allow files loaded through file:// URIs universal access
     to all pages.

     Default value: 'False'
-}
webSettingsSetEnableUniversalAccessFromFileUris
    :: WebSettings -- ^ settings
    -> Bool        -- ^ 'True' to grant universal access
    -> IO ()
webSettingsSetEnableUniversalAccessFromFileUris =
    objectSetPropertyBool
        "enable-universal-access-from-file-uris"

{- | Returns whether the XSS Auditor is enabled. This feature filters some kinds
     of reflective XSS attacks on vulnerable web sites.

     Default value: 'True'
-}
webSettingsGetEnableXssAuditor
    :: WebSettings -- ^ settings
    -> IO Bool     -- ^ 'True' if XSS Auditor is enabled
webSettingsGetEnableXssAuditor =
    objectGetPropertyBool
        "enable-xss-auditor"

{- | Sets whether to enable the XSS Auditor. This feature filters some kinds of
     reflective XSS attacks on vulnerable web sites.

     Default value: 'True'
-}
webSettingsSetEnableXssAuditor
    :: WebSettings -- ^ settings
    -> Bool        -- ^ 'False' to disable XSS Auditor
    -> IO ()
webSettingsSetEnableXssAuditor =
    objectSetPropertyBool
        "enable-xss-auditor"

{- | Returns whether a resolution of 96 DPI is enforced. For further information
     have a look at 'webSettingsSetEnforce96Dpi'.
-}
webSettingsGetEnforce96Dpi
    :: WebSettings -- ^ settings
    -> IO Bool     -- ^ 'True' if resolution of 96 DPI is enforced
webSettingsGetEnforce96Dpi =
    objectGetPropertyBool
        "enforce-96-dpi"

{- | Enforce a resolution of 96 DPI. This is meant for compatibility with web
     pages which cope badly with different screen resolutions and for automated
     testing. Web browsers and applications that typically display arbitrary
     content from the web should provide a preference for this.

     Default value: 'False'
-}
webSettingsSetEnforce96Dpi
    :: WebSettings -- ^ settings
    -> Bool        -- ^ 'True' to enforce resolution of 96 DPI
    -> IO ()
webSettingsSetEnforce96Dpi =
    objectSetPropertyBool
        "enforce-96-dpi"

{- | Returns the default Fantasy font family used to display text.

     Default value: \"serif\"
-}
webSettingsGetFantasyFontFamily
    :: WebSettings -- ^ settings
    -> IO String   -- ^ the font family
webSettingsGetFantasyFontFamily =
    objectGetPropertyString
        "fantasy-font-family"

{- | Sets the default Fantasy font family used to display text.

     Default value: \"serif\"
-}
webSettingsSetFantasyFontFamily
    :: WebSettings -- ^ settings
    -> String      -- ^ the font family
    -> IO ()
webSettingsSetFantasyFontFamily =
    objectSetPropertyString
        "fantasy-font-family"

{- | Returns whether JavaScript can open popup windows automatically without
     user intervention.

     Default value: 'False'
-}
webSettingsGetJavascriptCanOpenWindowsAutomatically
    :: WebSettings -- ^ settings
    -> IO Bool     -- ^ 'True' if allowed
webSettingsGetJavascriptCanOpenWindowsAutomatically =
    objectGetPropertyBool
        "javascript-can-open-windows-automatically"

{- | Sets whether JavaScript can open popup windows automatically without user
     intervention.

     Default value: 'False'
-}
webSettingsSetJavascriptCanOpenWindowsAutomatically
    :: WebSettings -- ^ settings
    -> Bool        -- ^ 'True' to allow
    -> IO ()
webSettingsSetJavascriptCanOpenWindowsAutomatically =
    objectSetPropertyBool
        "javascript-can-open-windows-automatically"

{- | Returns the minimum font size used to display text.

     Allowed values: >= 1

     Default value: 5
-}
webSettingsGetMinimumFontSize
    :: WebSettings -- ^ settings
    -> IO Int      -- ^ minimum font size
webSettingsGetMinimumFontSize =
    objectGetPropertyInt
        "minimum-font-size"

{- | Sets the minimum font size used to display text.

     Allowed values: >= 1

     Default value: 5
-}
webSettingsSetMinimumFontSize
    :: WebSettings -- ^ settings
    -> Int         -- ^ minimum font size
    -> IO ()
webSettingsSetMinimumFontSize =
    objectSetPropertyInt
        "minimum-font-size"

{- | Returns the minimum logical font size used to display text.

     Allowed values: >= 1

     Default value: 5
-}
webSettingsGetMinimumLogicalFontSize
    :: WebSettings -- ^ settings
    -> IO Int      -- ^ minimum logical font size
webSettingsGetMinimumLogicalFontSize =
    objectGetPropertyInt
        "minimum-logical-font-size"

{- | Sets the minimum logical font size used to display text.

     Allowed values: >= 1

     Default value: 5
-}
webSettingsSetMinimumLogicalFontSize
    :: WebSettings -- ^ settings
    -> Int         -- ^ minimum logical font size
    -> IO ()
webSettingsSetMinimumLogicalFontSize =
    objectSetPropertyInt
        "minimum-logical-font-size"

{- | Returns the default font family used to display monospace text.

     Default value: \"monospace\"
-}
webSettingsGetMonospaceFontFamily
    :: WebSettings -- ^ settings
    -> IO String   -- ^ the font family
webSettingsGetMonospaceFontFamily =
    objectGetPropertyString
        "monospace-font-family"

{- | Sets the default font family used to display monospace text.

     Default value: \"monospace\"
-}
webSettingsSetMonospaceFontFamily
    :: WebSettings -- ^ settings
    -> String      -- ^ the font family
    -> IO ()
webSettingsSetMonospaceFontFamily =
    objectSetPropertyString
        "monospace-font-family"

{- | Returns whether background images should be printed.

     Default value: 'True'
-}
webSettingsGetPrintBackgrounds
    :: WebSettings -- ^ settings
    -> IO Bool     -- ^ 'True' if background images should be printed
webSettingsGetPrintBackgrounds =
    objectGetPropertyBool
        "print-backgrounds"

{- | Sets whether background images should be printed.

     Default value: 'True'
-}
webSettingsSetPrintBackgrounds
    :: WebSettings -- ^ settings
    -> Bool        -- ^ 'False' to prevent background images from being printed
    -> IO ()
webSettingsSetPrintBackgrounds =
    objectSetPropertyBool
        "print-backgrounds"

{- | Returns whether text areas are resizable.

     Default value: 'True'
-}
webSettingsGetResizableTextAreas
    :: WebSettings -- ^ settings
    -> IO Bool     -- ^ 'True' if text areas are resizable
webSettingsGetResizableTextAreas =
    objectGetPropertyBool
        "resizable-text-areas"

{- | Sets whether text areas are resizable.

     Default value: 'True'
-}
webSettingsSetResizableTextAreas
    :: WebSettings -- ^ settings
    -> Bool        -- ^ 'False' if text areas should not be resizable
    -> IO ()
webSettingsSetResizableTextAreas =
    objectSetPropertyBool
        "resizable-text-areas"

{- | Returns the default Sans Serif font family used to display text.

     Default value: \"sans-serif\"
-}
webSettingsGetSansSerifFontFamily
    :: WebSettings -- ^ settings
    -> IO String   -- ^ the font family
webSettingsGetSansSerifFontFamily =
    objectGetPropertyString
        "sans-serif-font-family"

{- | Sets the default Sans Serif font family used to display text.

     Default value: \"sans-serif\"
-}
webSettingsSetSansSerifFontFamily
    :: WebSettings -- ^ settings
    -> String      -- ^ the font family
    -> IO ()
webSettingsSetSansSerifFontFamily =
    objectSetPropertyString
        "sans-serif-font-family"

{- | Returns the default Serif font family used to display text.

     Default value: \"serif\"
-}
webSettingsGetSerifFontFamily
    :: WebSettings -- ^ settings
    -> IO String   -- ^ the font family
webSettingsGetSerifFontFamily =
    objectGetPropertyString
        "serif-font-family"

{- | Sets the default Serif font family used to display text.

     Default value: \"serif\"
-}
webSettingsSetSerifFontFamily
    :: WebSettings -- ^ settings
    -> String      -- ^ the font family
    -> IO ()
webSettingsSetSerifFontFamily =
    objectSetPropertyString
        "serif-font-family"

{- | Returns the languages to be used for spell checking. For further
     information have a look at 'webSettingsSetSpellCheckingLanguages'.
-}
webSettingsGetSpellCheckingLanguages
    :: WebSettings       -- ^ settings
    -> IO (Maybe String) -- ^ 'Just' the languages or 'Nothing'
webSettingsGetSpellCheckingLanguages =
    objectGetPropertyMaybeString
        "spell-checking-languages"

{- | The languages to be used for spell checking, separated by commas.

     The locale string typically is in the form lang_COUNTRY, where lang is an
     ISO-639 language code, and COUNTRY is an ISO-3166 country code. For
     instance, sv_FI for Swedish as written in Finland or pt_BR for Portuguese
     as written in Brazil.

     If 'Nothing' is specified then the value returned by
     gtk_get_default_language will be used.

     Default value: 'Nothing'
-}
webSettingsSetSpellCheckingLanguages
    :: WebSettings  -- ^ settings
    -> Maybe String -- ^ 'Just' the languages to use or 'Nothing'
    -> IO ()
webSettingsSetSpellCheckingLanguages =
    objectSetPropertyMaybeString
        "spell-checking-languages"

{- | Returns whether the tab key cycles through elements on the page. For
     further information look at 'webSettingsSetTabKeyCyclesThroughElements'.
-}
webSettingsGetTabKeyCyclesThroughElements
    :: WebSettings -- ^ settings
    -> IO Bool     -- ^ 'True' if tab key cycles through elements
webSettingsGetTabKeyCyclesThroughElements =
    objectGetPropertyBool
        "tab-key-cycles-through-elements"

{- | Whether the tab key cycles through elements on the page.

     If flag is 'True', pressing the tab key will focus the next element in the
     'WebView'. If flag is 'False', the 'WebView' will interpret tab key presses
     as normal key presses. If the selected element is editable, the tab key
     will cause the insertion of a tab character.

     Default value: 'True'
-}
webSettingsSetTabKeyCyclesThroughElements
    :: WebSettings -- ^ settings
    -> Bool        -- ^ 'False' if tab key should not cycle through elements
    -> IO ()
webSettingsSetTabKeyCyclesThroughElements =
    objectSetPropertyBool
        "tab-key-cycles-through-elements"

{- | Get the User-Agent string used by WebKitGtk. For further information have a
     look at 'webSettingsSetUserAgent'.
-}
webSettingsGetUserAgent
    :: WebSettings -- ^ settings
    -> IO String   -- ^ User-Agent in use
webSettingsGetUserAgent =
    objectGetPropertyString
        "user-agent"

{- | Set the User-Agent string used by WebKitGtk.

     This will be a default User-Agent string if a custom string isn't provided
     by the application. Setting this property to a an empty string will result
     in the User-Agent string being reset to the default value.

     Default value: \"Mozilla\/5.0 (X11; U; Linux x86_64; c) AppleWebKit/531.2+
                      (KHTML, like Gecko) Safari\/531.2+\"
     (I guess it doesn't always look exactly like this.)
-}
webSettingsSetUserAgent
    :: WebSettings -- ^ settings
    -> String      -- ^ User-Agent string to use
    -> IO ()
webSettingsSetUserAgent =
    objectSetPropertyString
        "user-agent"

{- | Returns 'Just' the URI of a stylesheet that is applied to every page or
     'Nothing' if none is set.

     Default value: 'Nothing'
-}
webSettingsGetUserStylesheetUri
    :: WebSettings       -- ^ settings
    -> IO (Maybe String) -- ^ 'Just' the URI of the stylesheet or 'Nothing'
webSettingsGetUserStylesheetUri =
    objectGetPropertyMaybeString
        "user-stylesheet-uri"

{- | Set the URI of a stylesheet that is applied to every page. Use 'Nothing' to
     set none.

     Default value: 'Nothing'
-}
webSettingsSetUserStylesheetUri
    :: WebSettings  -- ^ settings
    -> Maybe String -- ^ 'Just' the URI of the stylesheet or 'Nothing'
    -> IO ()
webSettingsSetUserStylesheetUri =
    objectSetPropertyMaybeString
        "user-stylesheet-uri"

{- | Returns the value by which the zoom level is changed when zooming in
     or out.

     Allowed values: >= 0

     Default value: 0.1
-}
webSettingsGetZoomStep
    :: WebSettings -- ^ settings
    -> IO Float    -- ^ the step
webSettingsGetZoomStep =
    objectGetPropertyFloat
        "zoom-step"

{- | Sets the value by which the zoom level is changed when zooming in or out.

     Allowed values: >= 0

     Default value: 0.1
-}
webSettingsSetZoomStep
    :: WebSettings -- ^ settings
    -> Float       -- ^ the step
    -> IO ()
webSettingsSetZoomStep =
    objectSetPropertyFloat
        "zoom-step"

