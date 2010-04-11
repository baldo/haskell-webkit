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

    , webSettingsSetEnableSpartialNavigation
    , webSettingsGetEnableSpartialNavigation

    ) where

#include <webkit/webkitwebsettings.h>

import System.Glib.FFI
import System.Glib.Properties
    ( objectGetPropertyBool
    , objectSetPropertyBool

    , objectGetPropertyFloat
    , objectSetPropertyFloat

    , objectGetPropertyInt
    , objectSetPropertyInt

    , objectGetPropertyMaybeString
    , objectSetPropertyMaybeString

    , objectGetPropertyString
    , objectSetPropertyString
    )

import Control.Monad.Trans
    ( MonadIO
    , liftIO
    )

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
    :: MonadIO m
    => WebSettings   -- ^ settings
    -> m WebSettings -- ^ the copy
webSettingsCopy settings = liftIO $
    withWebSettings settings $ \ptr ->
        makeWebSettings $ {#call web_settings_copy#} ptr

{- | Creates new 'WebSettings' with default values. It must be manually attached
     to a 'WebView'.
-}
webSettingsNew
    :: MonadIO m
    => m WebSettings -- ^ the new 'WebSettings'
webSettingsNew = liftIO $
    makeWebSettings $ {#call web_settings_new#}

-- Properties -----------------------------------------------------------------

{- | Get whether images are automatically loaded.

     Default value: 'True'
-}
webSettingsGetAutoLoadImages
    :: MonadIO m
    => WebSettings -- ^ settings
    -> m Bool      -- ^ 'True' if images are automatically loaded
webSettingsGetAutoLoadImages ws = liftIO $
    objectGetPropertyBool
        "auto-load-images" ws

{- | Set whether to load images automatically.

     Default value: 'True'
-}
webSettingsSetAutoLoadImages
    :: MonadIO m
    => WebSettings -- ^ settings
    -> Bool        -- ^ 'True' if images shall be automatically loaded
    -> m ()
webSettingsSetAutoLoadImages ws b = liftIO $
    objectSetPropertyBool
        "auto-load-images" ws b

{- | Gets the value of this property. For more information look at
     'webSettingsSetAutoResizeWindow'.

     Default value: 'False'
-}
webSettingsGetAutoResizeWindow
    :: MonadIO m
    => WebSettings -- ^ settings
    -> m Bool      -- ^ 'True' if automatic resizing is enabled
webSettingsGetAutoResizeWindow ws = liftIO $
    objectGetPropertyBool
        "auto-resize-window" ws

{- | Web pages can request to modify the size and position of the window
     containing the 'WebView' through various DOM methods (resizeTo, moveTo,
     resizeBy, moveBy). By default WebKit will not honor this requests, but you
     can set this property to 'True' if you'd like it to do so. If you wish to
     handle this manually, you can connect to the notify signal for the
     'WebWindowFeatures' of your 'WebView'.

     Default value: 'False'
-}
webSettingsSetAutoResizeWindow
    :: MonadIO m
    => WebSettings -- ^ settings
    -> Bool        -- ^ 'True' if automatic resizing shall be enabled
    -> m ()
webSettingsSetAutoResizeWindow ws b = liftIO $
    objectSetPropertyBool
        "auto-resize-window" ws b

{- | Returns whether standalone images are automatically shrinked to fit.

     Default value: 'True'
-}
webSettingsGetAutoShrinkImages
    :: MonadIO m
    => WebSettings -- ^ settings
    -> m Bool      -- ^ 'True' if automatic shrinking is enabled
webSettingsGetAutoShrinkImages ws = liftIO $
    objectGetPropertyBool
        "auto-shrink-images" ws

{- | Sets whether to automatically shrink standalone images to fit.

     Default value: 'True'
-}
webSettingsSetAutoShrinkImages
    :: MonadIO m
    => WebSettings -- ^ settings
    -> Bool        -- ^ 'False' to disable automatic shrinking
    -> m ()
webSettingsSetAutoShrinkImages ws b = liftIO $
    objectSetPropertyBool
        "auto-shrink-images" ws b

{- | Returns the default Cursive font family used to display text.

     Default value: \"serif\"
-}
webSettingsGetCursiveFontFamily
    :: MonadIO m
    => WebSettings -- ^ settings
    -> m String    -- ^ the font family
webSettingsGetCursiveFontFamily ws = liftIO $
    objectGetPropertyString
        "cursive-font-family" ws

{- | Sets the default Cursive font family used to display text.

     Default value: \"serif\"
-}
webSettingsSetCursiveFontFamily
    :: MonadIO m
    => WebSettings -- ^ settings
    -> String      -- ^ the font family
    -> m ()
webSettingsSetCursiveFontFamily ws s = liftIO $
    objectSetPropertyString
        "cursive-font-family" ws s

{- | Returns the default encoding used to display text.

     Default value: \"iso-8859-1\"
-}
webSettingsGetDefaultEncoding
    :: MonadIO m
    => WebSettings -- ^ settings
    -> m String    -- ^ the encoding
webSettingsGetDefaultEncoding ws = liftIO $
    objectGetPropertyString
        "default-encoding" ws 

{- | Sets the default encoding used to display text.

     Default value: \"iso-8859-1\"
-}
webSettingsSetDefaultEncoding
    :: MonadIO m
    => WebSettings -- ^ settings
    -> String      -- ^ the encoding
    -> m ()
webSettingsSetDefaultEncoding ws s = liftIO $
    objectSetPropertyString
        "default-encoding" ws s

{- | Returns the default font family used to display text.

     Default value: \"sans-serif\"
-}
webSettingsGetDefaultFontFamily
    :: MonadIO m
    => WebSettings -- ^ settings
    -> m String    -- ^ the font family
webSettingsGetDefaultFontFamily ws = liftIO $
    objectGetPropertyString
        "default-font-family" ws

{- | Sets the default font family used to display text.

     Default value: \"sans-serif\"
-}
webSettingsSetDefaultFontFamily
    :: MonadIO m
    => WebSettings -- ^ settings
    -> String      -- ^ the font family
    -> m ()
webSettingsSetDefaultFontFamily ws s = liftIO $
    objectSetPropertyString
        "default-font-family" ws s

{- | Returns the default font size used to display text.

     Allowed values: >= 5

     Default value: 12
-}
webSettingsGetDefaultFontSize
    :: MonadIO m
    => WebSettings -- ^ settings
    -> m Int       -- ^ the font size
webSettingsGetDefaultFontSize ws = liftIO $
    objectGetPropertyInt
        "default-font-size" ws

{- | Sets the default font size used to display text.

     Allowed values: >= 5

     Default value: 12
-}
webSettingsSetDefaultFontSize
    :: MonadIO m
    => WebSettings -- ^ settings
    -> Int         -- ^ the font size
    -> m ()
webSettingsSetDefaultFontSize ws i = liftIO $
    objectSetPropertyInt
        "default-font-size" ws i

{- | Returns the default font size used to display monospace text.

     Allowed values: >= 5

     Default value: 10
-}
webSettingsGetDefaultMonospaceFontSize
    :: MonadIO m
    => WebSettings -- ^ settings
    -> m Int       -- ^ the font size
webSettingsGetDefaultMonospaceFontSize ws = liftIO $
    objectGetPropertyInt
        "default-monospace-font-size" ws

{- | Sets the default font size used to display monospace text.

     Allowed values: >= 5

     Default value: 10
-}
webSettingsSetDefaultMonospaceFontSize
    :: MonadIO m
    => WebSettings -- ^ settings
    -> Int         -- ^ the font size
    -> m ()
webSettingsSetDefaultMonospaceFontSize ws i = liftIO $
    objectSetPropertyInt
        "default-monospace-font-size" ws i

{- | Returns the 'EditingBehavior'. For more information have a look at
     'webSettingsSetEditingBehavior'.
-}
webSettingsGetEditingBehavior
    :: MonadIO m
    => WebSettings       -- ^ settings
    -> m EditingBehavior -- ^ the editing behavior
webSettingsGetEditingBehavior ws = liftIO $
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
    :: MonadIO m
    => WebSettings     -- ^ settings
    -> EditingBehavior -- ^ the editing behavior
    -> m ()
webSettingsSetEditingBehavior ws enum = liftIO $
    objectSetPropertyInt
        "editing-behavior" ws
            $ fromEnum enum

{- | Returns whether caret browsing mode is enabled.

     Default value: 'False'
-}
webSettingsGetEnableCaretBrowsing
    :: MonadIO m
    => WebSettings -- ^ settings
    -> m Bool      -- ^ 'True' if caret browsing mode is enabled
webSettingsGetEnableCaretBrowsing ws = liftIO $
    objectGetPropertyBool
        "enable-caret-browsing" ws

{- | Sets whether to enable caret browsing mode.

     Default value: 'False'
-}
webSettingsSetEnableCaretBrowsing
    :: MonadIO m
    => WebSettings -- ^ settings
    -> Bool        -- ^ 'True' to enable caret browsing mode
    -> m ()
webSettingsSetEnableCaretBrowsing ws b = liftIO $
    objectSetPropertyBool
        "enable-caret-browsing" ws b

{- | Returns whether context menu is enabled by default. For more information
     have a look at 'webSettingsSetEnableDefaultContextMenu'.
-}
webSettingsGetEnableDefaultContextMenu
    :: MonadIO m
    => WebSettings -- ^ settings
    -> m Bool      -- ^ 'False' if context menu is disabled by default
webSettingsGetEnableDefaultContextMenu ws = liftIO $
    objectGetPropertyBool
        "enable-default-context-menu" ws

{- | Whether right-clicks should be handled automatically to create, and display
     the context menu. Turning this off will make WebKitGTK+ not emit the
     populate-popup signal. Notice that the default button press event handler
     may still handle right clicks for other reasons, such as in-page context
     menus, or right-clicks that are handled by the page itself.

     Default value: 'True'
-}
webSettingsSetEnableDefaultContextMenu
    :: MonadIO m
    => WebSettings -- ^ settings
    -> Bool        -- ^ 'False' to disable context menu by default
    -> m ()
webSettingsSetEnableDefaultContextMenu ws b = liftIO $
    objectSetPropertyBool
        "enable-default-context-menu" ws b

{- | Returns whether developer extensions are enabled. For more information have
     a look at 'webSettingsSetEnableDeveloperExtras'.
-}
webSettingsGetEnableDeveloperExtras
    :: MonadIO m
    => WebSettings -- ^ settings
    -> m Bool      -- ^ 'True' if developer extensions are enabled
webSettingsGetEnableDeveloperExtras ws = liftIO $
    objectGetPropertyBool
        "enable-developer-extras" ws

{- | Whether developer extensions should be enabled. This enables, for now, the
     Web Inspector, which can be controlled using the 'WebInspector' held by the
     'WebView' this setting is enabled for.

     Default value: 'False'
-}
webSettingsSetEnableDeveloperExtras
    :: MonadIO m
    => WebSettings -- ^ settings
    -> Bool        -- ^ 'True' to enable developer extensions
    -> m ()
webSettingsSetEnableDeveloperExtras ws b = liftIO $
    objectSetPropertyBool
        "enable-developer-extras" ws b

{- | Returns whether DOM paste is enabled. For further information have a look
     at 'webSettingsSetEnableDomPaste'.
-}
webSettingsGetEnableDomPaste
    :: MonadIO m
    => WebSettings -- ^ settings
    -> m Bool      -- ^ 'True' if DOM paste is enabled
webSettingsGetEnableDomPaste ws = liftIO $
    objectGetPropertyBool
        "enable-dom-paste" ws

{- | Sets whether to enable DOM paste. If set to 'True',
     document.execCommand(\"Paste\") will correctly execute and paste content
     of the clipboard.

     Default value: 'False'
-}
webSettingsSetEnableDomPaste
    :: MonadIO m
    => WebSettings -- ^ settings
    -> Bool        -- ^ 'True' to enable DOM paste
    -> m ()
webSettingsSetEnableDomPaste ws b = liftIO $
    objectSetPropertyBool
        "enable-dom-paste" ws b

{- | Returns whether enable-file-access-from-file-uris property is enabled. For
     more information look at 'webSettingsSetEnableFileAccessFromFileUris'.
-}
webSettingsGetEnableFileAccessFromFileUris
    :: MonadIO m
    => WebSettings -- ^ settings
    -> m Bool      -- ^ 'True' if enabled
webSettingsGetEnableFileAccessFromFileUris ws = liftIO $
    objectGetPropertyBool
        "enable-file-access-from-file-uris" ws

{- | Boolean property to control file access for file:// URIs. If this option is
     enabled every file:// will have its own security unique domain.

     Default value: 'False'
-}
webSettingsSetEnableFileAccessFromFileUris
    :: MonadIO m
    => WebSettings -- ^ settings
    -> Bool        -- ^ 'True' to enable
    -> m ()
webSettingsSetEnableFileAccessFromFileUris ws b = liftIO $
    objectSetPropertyBool
        "enable-file-access-from-file-uris" ws b

{- | Returns whether HTML5 client-side SQL database support is enabled. For
     further information look at 'webSettingsSetEnableHtml5Database'.
-}
webSettingsGetEnableHtml5Database
    :: MonadIO m
    => WebSettings -- ^ settings
    -> m Bool      -- ^ 'True' if client-side SQL database support is enabled
webSettingsGetEnableHtml5Database ws = liftIO $
    objectGetPropertyBool
        "enable-html5-database" ws

{- | Whether to enable HTML5 client-side SQL database support. Client-side SQL
     database allows web pages to store structured data and be able to use SQL
     to manipulate that data asynchronously.

     Default value: 'True'
-}
webSettingsSetEnableHtml5Database
    :: MonadIO m
    => WebSettings -- ^ settings
    -> Bool        -- ^ 'False' to disable client-side SQL database support
    -> m ()
webSettingsSetEnableHtml5Database ws b = liftIO $
    objectSetPropertyBool
        "enable-html5-database" ws b

{- | Returns whether HTML5 localStorage support is enabled. localStorage
     provides simple synchronous storage access.

     Default value: 'True'
-}
webSettingsGetEnableHtml5LocalStorage
    :: MonadIO m
    => WebSettings -- ^ settings
    -> m Bool      -- ^ 'True' if localStorage support is enabled
webSettingsGetEnableHtml5LocalStorage ws = liftIO $
    objectGetPropertyBool
        "enable-html5-local-storage" ws

{- | Sets whether to enable HTML5 localStorage support. localStorage
     provides simple synchronous storage access.

     Default value: 'True'
-}
webSettingsSetEnableHtml5LocalStorage
    :: MonadIO m
    => WebSettings -- ^ settings
    -> Bool        -- ^ 'False' to disable localStorage support
    -> m ()
webSettingsSetEnableHtml5LocalStorage ws b = liftIO $
    objectSetPropertyBool
        "enable-html5-local-storage" ws b

{- | Returns whether the Java \<applet\> tag is enabled. For further information
     have a look at 'webSettingsSetEnableJavaApplet'.
-}
webSettingsGetEnableJavaApplet
    :: MonadIO m
    => WebSettings -- ^ settings
    -> m Bool      -- ^ 'True' if \<applet\> tag is enabled
webSettingsGetEnableJavaApplet ws = liftIO $
    objectGetPropertyBool
        "enable-java-applet" ws

{- | Enable or disable support for the Java \<applet\> tag. Keep in mind that
     Java content can be still shown in the page through \<object\> or
     \<embed\>, which are the preferred tags for this task.

     Default value: 'True'
-}
webSettingsSetEnableJavaApplet
    :: MonadIO m
    => WebSettings -- ^ settings
    -> Bool        -- ^ 'False' to disable the Java \<applet\> tag
    -> m ()
webSettingsSetEnableJavaApplet ws b = liftIO $
    objectSetPropertyBool
        "enable-java-applet" ws b

{- | Returns wether offline web application cache support is enabled. For more
     information look at 'webSettingsSetEnableOfflineWebApplicationCache'.
-}
webSettingsGetEnableOfflineWebApplicationCache
    :: MonadIO m
    => WebSettings -- ^ settings
    -> m Bool     -- ^ 'True' if offline web application cache is enabled
webSettingsGetEnableOfflineWebApplicationCache ws = liftIO $
    objectGetPropertyBool
        "enable-offline-web-application-cache" ws

{- | Whether to enable HTML5 offline web application cache support. Offline Web
     Application Cache ensures web applications are available even when the user
     is not connected to the network.

     Default value: 'True'
-}
webSettingsSetEnableOfflineWebApplicationCache
    :: MonadIO m
    => WebSettings -- ^ settings
    -> Bool        -- ^ 'False' to disable offline web application cache support
    -> m ()
webSettingsSetEnableOfflineWebApplicationCache ws b = liftIO $
    objectSetPropertyBool
        "enable-offline-web-application-cache" ws b

{- | Returns whether page cache is enabled. For further information have a look
     at 'webSettingsSetEnablePageCache'.
-}
webSettingsGetEnablePageCache
    :: MonadIO m
    => WebSettings -- ^ settings
    -> m Bool      -- ^ 'True' if page cache is enabled
webSettingsGetEnablePageCache ws = liftIO $
    objectGetPropertyBool
        "enable-page-cache" ws

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
    :: MonadIO m
    => WebSettings -- ^ settings
    -> Bool        -- ^ 'True' to enable page cache
    -> m ()
webSettingsSetEnablePageCache ws b = liftIO $
    objectSetPropertyBool
        "enable-page-cache" ws b

{- | Returns whether embedded plugin objects are enabled.

     Default value: 'True'
-}
webSettingsGetEnablePlugins
    :: MonadIO m
    => WebSettings -- ^ settings
    -> m Bool      -- ^ 'True' if embedded plugin objects are enabled
webSettingsGetEnablePlugins ws = liftIO $
    objectGetPropertyBool
        "enable-plugins" ws

{- | Sets whether to enable embedded plugin objects.

     Default value: 'True'
-}
webSettingsSetEnablePlugins
    :: MonadIO m
    => WebSettings -- ^ settings
    -> Bool        -- ^ 'False' to disable embedded plugin objects
    -> m ()
webSettingsSetEnablePlugins ws b = liftIO $
    objectSetPropertyBool
        "enable-plugins" ws b

{- | Returns wheter private browsing mode is enabled. For further information
     have a look at 'webSettingsSetEnablePrivateBrowsing'.
-}
webSettingsGetEnablePrivateBrowsing
    :: MonadIO m
    => WebSettings -- ^ settings
    -> m Bool      -- ^ 'True' if private browsing mode is enabled
webSettingsGetEnablePrivateBrowsing ws = liftIO $
    objectGetPropertyBool
        "enable-private-browsing" ws

{- | Whether to enable private browsing mode. Private browsing mode prevents
     WebKit from updating the global history and storing any session information
     e.g., on-disk cache, as well as suppressing any messages from being printed
     into the (javascript) console.

     This is currently experimental for WebKitGtk.

     Default value: 'False'
-}
webSettingsSetEnablePrivateBrowsing
    :: MonadIO m
    => WebSettings -- ^ settings
    -> Bool        -- ^ 'True' to enable private browsing mode
    -> m ()
webSettingsSetEnablePrivateBrowsing ws b = liftIO $
    objectSetPropertyBool
        "enable-private-browsing" ws b

{- | Returns wheter embedded scripting languages are enabled.

     Default value: 'True'
-}
webSettingsGetEnableScripts
    :: MonadIO m
    => WebSettings -- ^ settings
    -> m Bool      -- ^ 'True' if embedded scripting languages are enabled
webSettingsGetEnableScripts ws = liftIO $
    objectGetPropertyBool
        "enable-scripts" ws

{- | Sets wether to enable embedded scripting languages.

     Default value: 'True'
-}
webSettingsSetEnableScripts
    :: MonadIO m
    => WebSettings -- ^ settings
    -> Bool        -- ^ 'False' to disable embedded scripting languages
    -> m ()
webSettingsSetEnableScripts ws b = liftIO $
    objectSetPropertyBool
        "enable-scripts" ws b

{- | Returns wheter the site-specific compatibility workarounds are enabled.

     Default value: 'False'
-}
webSettingsGetEnableSiteSpecificQuirks
    :: MonadIO m
    => WebSettings -- ^ settings
    -> m Bool      -- ^ 'True' if site-specific workarounds are enabled
webSettingsGetEnableSiteSpecificQuirks ws = liftIO $
    objectGetPropertyBool
        "enable-site-specific-quirks" ws

{- | Sets wheter to enable the site-specific compatibility workarounds.

     Default value: 'False'
-}
webSettingsSetEnableSiteSpecificQuirks
    :: MonadIO m
    => WebSettings -- ^ settings
    -> Bool        -- ^ 'True' to enable site-specific workarounds
    -> m ()
webSettingsSetEnableSiteSpecificQuirks ws b = liftIO $
    objectSetPropertyBool
        "enable-site-specific-quirks" ws b

{- | Sets wheter to enable spartial-navigation.

     Default value: 'False'
-}
webSettingsSetEnableSpartialNavigation
    :: MonadIO m
    => WebSettings -- ^ settings
    -> Bool        -- ^ 'True' to enable spartial-navigation
    -> m ()
webSettingsSetEnableSpartialNavigation ws b = liftIO $
    objectSetPropertyBool
        "enable-spatial-navigation" ws b

{- | Returns whether spartial-navigation is enabled.

     Default value: 'False'
-}
webSettingsGetEnableSpartialNavigation
    :: MonadIO m
    => WebSettings -- ^ settings
    -> m Bool      -- ^ 'True' if spartial-navigation is enabled
webSettingsGetEnableSpartialNavigation ws = liftIO $
    objectGetPropertyBool
        "enabled-spartial-navigation" ws

{- | Returns whether spell checking while typing is enabled.

     Default value: 'False'
-}
webSettingsGetEnableSpellChecking
    :: MonadIO m
    => WebSettings -- ^ settings
    -> m Bool      -- ^ 'True' if spell checking while typing is enabled
webSettingsGetEnableSpellChecking ws = liftIO $
    objectGetPropertyBool
        "enable-spell-checking" ws

{- | Sets whether to enable spell checking while typing.

     Default value: 'False'
-}
webSettingsSetEnableSpellChecking
    :: MonadIO m
    => WebSettings -- ^ settings
    -> Bool        -- ^ 'True' to enable spell checking while typing
    -> m ()
webSettingsSetEnableSpellChecking ws b = liftIO $
    objectSetPropertyBool
        "enable-spell-checking" ws b

{- | Returns whether it is allowed for files loaded through file:// URIs
     to have universal access to all pages.

     Default value: 'False'
-}
webSettingsGetEnableUniversalAccessFromFileUris
    :: MonadIO m
    => WebSettings -- ^ settings
    -> m Bool      -- ^ 'True' if universal access is granted
webSettingsGetEnableUniversalAccessFromFileUris ws = liftIO $
    objectGetPropertyBool
        "enable-universal-access-from-file-uris" ws

{- | Sets whether to allow files loaded through file:// URIs universal access
     to all pages.

     Default value: 'False'
-}
webSettingsSetEnableUniversalAccessFromFileUris
    :: MonadIO m
    => WebSettings -- ^ settings
    -> Bool        -- ^ 'True' to grant universal access
    -> m ()
webSettingsSetEnableUniversalAccessFromFileUris ws b = liftIO $
    objectSetPropertyBool
        "enable-universal-access-from-file-uris" ws b

{- | Returns whether the XSS Auditor is enabled. This feature filters some kinds
     of reflective XSS attacks on vulnerable web sites.

     Default value: 'True'
-}
webSettingsGetEnableXssAuditor
    :: MonadIO m
    => WebSettings -- ^ settings
    -> m Bool      -- ^ 'True' if XSS Auditor is enabled
webSettingsGetEnableXssAuditor ws = liftIO $
    objectGetPropertyBool
        "enable-xss-auditor" ws

{- | Sets whether to enable the XSS Auditor. This feature filters some kinds of
     reflective XSS attacks on vulnerable web sites.

     Default value: 'True'
-}
webSettingsSetEnableXssAuditor
    :: MonadIO m
    => WebSettings -- ^ settings
    -> Bool        -- ^ 'False' to disable XSS Auditor
    -> m ()
webSettingsSetEnableXssAuditor ws b = liftIO $
    objectSetPropertyBool
        "enable-xss-auditor" ws b

{- | Returns whether a resolution of 96 DPI is enforced. For further information
     have a look at 'webSettingsSetEnforce96Dpi'.
-}
webSettingsGetEnforce96Dpi
    :: MonadIO m
    => WebSettings -- ^ settings
    -> m Bool      -- ^ 'True' if resolution of 96 DPI is enforced
webSettingsGetEnforce96Dpi ws = liftIO $
    objectGetPropertyBool
        "enforce-96-dpi" ws

{- | Enforce a resolution of 96 DPI. This is meant for compatibility with web
     pages which cope badly with different screen resolutions and for automated
     testing. Web browsers and applications that typically display arbitrary
     content from the web should provide a preference for this.

     Default value: 'False'
-}
webSettingsSetEnforce96Dpi
    :: MonadIO m
    => WebSettings -- ^ settings
    -> Bool        -- ^ 'True' to enforce resolution of 96 DPI
    -> m ()
webSettingsSetEnforce96Dpi ws b = liftIO $
    objectSetPropertyBool
        "enforce-96-dpi" ws b

{- | Returns the default Fantasy font family used to display text.

     Default value: \"serif\"
-}
webSettingsGetFantasyFontFamily
    :: MonadIO m
    => WebSettings -- ^ settings
    -> m String    -- ^ the font family
webSettingsGetFantasyFontFamily ws = liftIO $
    objectGetPropertyString
        "fantasy-font-family" ws

{- | Sets the default Fantasy font family used to display text.

     Default value: \"serif\"
-}
webSettingsSetFantasyFontFamily
    :: MonadIO m
    => WebSettings -- ^ settings
    -> String      -- ^ the font family
    -> m ()
webSettingsSetFantasyFontFamily ws s = liftIO $
    objectSetPropertyString
        "fantasy-font-family" ws s

{- | Returns whether JavaScript can open popup windows automatically without
     user intervention.

     Default value: 'False'
-}
webSettingsGetJavascriptCanOpenWindowsAutomatically
    :: MonadIO m
    => WebSettings -- ^ settings
    -> m Bool      -- ^ 'True' if allowed
webSettingsGetJavascriptCanOpenWindowsAutomatically ws = liftIO $
    objectGetPropertyBool
        "javascript-can-open-windows-automatically" ws

{- | Sets whether JavaScript can open popup windows automatically without user
     intervention.

     Default value: 'False'
-}
webSettingsSetJavascriptCanOpenWindowsAutomatically
    :: MonadIO m
    => WebSettings -- ^ settings
    -> Bool        -- ^ 'True' to allow
    -> m ()
webSettingsSetJavascriptCanOpenWindowsAutomatically ws b = liftIO $
    objectSetPropertyBool
        "javascript-can-open-windows-automatically" ws b

{- | Returns the minimum font size used to display text.

     Allowed values: >= 1

     Default value: 5
-}
webSettingsGetMinimumFontSize
    :: MonadIO m
    => WebSettings -- ^ settings
    -> m Int       -- ^ minimum font size
webSettingsGetMinimumFontSize ws = liftIO $
    objectGetPropertyInt
        "minimum-font-size" ws

{- | Sets the minimum font size used to display text.

     Allowed values: >= 1

     Default value: 5
-}
webSettingsSetMinimumFontSize
    :: MonadIO m
    => WebSettings -- ^ settings
    -> Int         -- ^ minimum font size
    -> m ()
webSettingsSetMinimumFontSize ws i = liftIO $
    objectSetPropertyInt
        "minimum-font-size" ws i

{- | Returns the minimum logical font size used to display text.

     Allowed values: >= 1

     Default value: 5
-}
webSettingsGetMinimumLogicalFontSize
    :: MonadIO m
    => WebSettings -- ^ settings
    -> m Int       -- ^ minimum logical font size
webSettingsGetMinimumLogicalFontSize ws = liftIO $
    objectGetPropertyInt
        "minimum-logical-font-size" ws

{- | Sets the minimum logical font size used to display text.

     Allowed values: >= 1

     Default value: 5
-}
webSettingsSetMinimumLogicalFontSize
    :: MonadIO m
    => WebSettings -- ^ settings
    -> Int         -- ^ minimum logical font size
    -> m ()
webSettingsSetMinimumLogicalFontSize ws i = liftIO $
    objectSetPropertyInt
        "minimum-logical-font-size" ws i

{- | Returns the default font family used to display monospace text.

     Default value: \"monospace\"
-}
webSettingsGetMonospaceFontFamily
    :: MonadIO m
    => WebSettings -- ^ settings
    -> m String    -- ^ the font family
webSettingsGetMonospaceFontFamily ws = liftIO $
    objectGetPropertyString
        "monospace-font-family" ws

{- | Sets the default font family used to display monospace text.

     Default value: \"monospace\"
-}
webSettingsSetMonospaceFontFamily
    :: MonadIO m
    => WebSettings -- ^ settings
    -> String      -- ^ the font family
    -> m ()
webSettingsSetMonospaceFontFamily ws s = liftIO $
    objectSetPropertyString
        "monospace-font-family" ws s

{- | Returns whether background images should be printed.

     Default value: 'True'
-}
webSettingsGetPrintBackgrounds
    :: MonadIO m
    => WebSettings -- ^ settings
    -> m Bool      -- ^ 'True' if background images should be printed
webSettingsGetPrintBackgrounds ws = liftIO $
    objectGetPropertyBool
        "print-backgrounds" ws

{- | Sets whether background images should be printed.

     Default value: 'True'
-}
webSettingsSetPrintBackgrounds
    :: MonadIO m
    => WebSettings -- ^ settings
    -> Bool        -- ^ 'False' to prevent background images from being printed
    -> m ()
webSettingsSetPrintBackgrounds ws b = liftIO $
    objectSetPropertyBool
        "print-backgrounds" ws b

{- | Returns whether text areas are resizable.

     Default value: 'True'
-}
webSettingsGetResizableTextAreas
    :: MonadIO m
    => WebSettings -- ^ settings
    -> m Bool     -- ^ 'True' if text areas are resizable
webSettingsGetResizableTextAreas ws = liftIO $
    objectGetPropertyBool
        "resizable-text-areas" ws

{- | Sets whether text areas are resizable.

     Default value: 'True'
-}
webSettingsSetResizableTextAreas
    :: MonadIO m
    => WebSettings -- ^ settings
    -> Bool        -- ^ 'False' if text areas should not be resizable
    -> m ()
webSettingsSetResizableTextAreas ws b = liftIO $
    objectSetPropertyBool
        "resizable-text-areas" ws b

{- | Returns the default Sans Serif font family used to display text.

     Default value: \"sans-serif\"
-}
webSettingsGetSansSerifFontFamily
    :: MonadIO m
    => WebSettings -- ^ settings
    -> m String    -- ^ the font family
webSettingsGetSansSerifFontFamily ws = liftIO $
    objectGetPropertyString
        "sans-serif-font-family" ws

{- | Sets the default Sans Serif font family used to display text.

     Default value: \"sans-serif\"
-}
webSettingsSetSansSerifFontFamily
    :: MonadIO m
    => WebSettings -- ^ settings
    -> String      -- ^ the font family
    -> m ()
webSettingsSetSansSerifFontFamily ws s = liftIO $
    objectSetPropertyString
        "sans-serif-font-family" ws s

{- | Returns the default Serif font family used to display text.

     Default value: \"serif\"
-}
webSettingsGetSerifFontFamily
    :: MonadIO m
    => WebSettings -- ^ settings
    -> m String    -- ^ the font family
webSettingsGetSerifFontFamily ws = liftIO $
    objectGetPropertyString
        "serif-font-family" ws

{- | Sets the default Serif font family used to display text.

     Default value: \"serif\"
-}
webSettingsSetSerifFontFamily
    :: MonadIO m
    => WebSettings -- ^ settings
    -> String      -- ^ the font family
    -> m ()
webSettingsSetSerifFontFamily ws s = liftIO $
    objectSetPropertyString
        "serif-font-family" ws s

{- | Returns the languages to be used for spell checking. For further
     information have a look at 'webSettingsSetSpellCheckingLanguages'.
-}
webSettingsGetSpellCheckingLanguages
    :: MonadIO m
    => WebSettings      -- ^ settings
    -> m (Maybe String) -- ^ 'Just' the languages or 'Nothing'
webSettingsGetSpellCheckingLanguages ws = liftIO $
    objectGetPropertyMaybeString
        "spell-checking-languages" ws

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
    :: MonadIO m
    => WebSettings  -- ^ settings
    -> Maybe String -- ^ 'Just' the languages to use or 'Nothing'
    -> m ()
webSettingsSetSpellCheckingLanguages ws ms = liftIO $
    objectSetPropertyMaybeString
        "spell-checking-languages" ws ms

{- | Returns whether the tab key cycles through elements on the page. For
     further information look at 'webSettingsSetTabKeyCyclesThroughElements'.
-}
webSettingsGetTabKeyCyclesThroughElements
    :: MonadIO m
    => WebSettings -- ^ settings
    -> m Bool      -- ^ 'True' if tab key cycles through elements
webSettingsGetTabKeyCyclesThroughElements ws = liftIO $
    objectGetPropertyBool
        "tab-key-cycles-through-elements" ws

{- | Whether the tab key cycles through elements on the page.

     If flag is 'True', pressing the tab key will focus the next element in the
     'WebView'. If flag is 'False', the 'WebView' will interpret tab key presses
     as normal key presses. If the selected element is editable, the tab key
     will cause the insertion of a tab character.

     Default value: 'True'
-}
webSettingsSetTabKeyCyclesThroughElements
    :: MonadIO m
    => WebSettings -- ^ settings
    -> Bool        -- ^ 'False' if tab key should not cycle through elements
    -> m ()
webSettingsSetTabKeyCyclesThroughElements ws b = liftIO $
    objectSetPropertyBool
        "tab-key-cycles-through-elements" ws b

{- | Get the User-Agent string used by WebKitGtk. For further information have a
     look at 'webSettingsSetUserAgent'.
-}
webSettingsGetUserAgent
    :: MonadIO m
    => WebSettings -- ^ settings
    -> m String    -- ^ User-Agent in use
webSettingsGetUserAgent ws = liftIO $
    objectGetPropertyString
        "user-agent" ws

{- | Set the User-Agent string used by WebKitGtk.

     This will be a default User-Agent string if a custom string isn't provided
     by the application. Setting this property to a an empty string will result
     in the User-Agent string being reset to the default value.

     Default value: \"Mozilla\/5.0 (X11; U; Linux x86_64; c) AppleWebKit/531.2+
                      (KHTML, like Gecko) Safari\/531.2+\"
     (I guess it doesn't always look exactly like this.)
-}
webSettingsSetUserAgent
    :: MonadIO m
    => WebSettings -- ^ settings
    -> String      -- ^ User-Agent string to use
    -> m ()
webSettingsSetUserAgent ws s = liftIO $
    objectSetPropertyString
        "user-agent" ws s

{- | Returns 'Just' the URI of a stylesheet that is applied to every page or
     'Nothing' if none is set.

     Default value: 'Nothing'
-}
webSettingsGetUserStylesheetUri
    :: MonadIO m
    => WebSettings      -- ^ settings
    -> m (Maybe String) -- ^ 'Just' the URI of the stylesheet or 'Nothing'
webSettingsGetUserStylesheetUri ws = liftIO $
    objectGetPropertyMaybeString
        "user-stylesheet-uri" ws

{- | Set the URI of a stylesheet that is applied to every page. Use 'Nothing' to
     set none.

     Default value: 'Nothing'
-}
webSettingsSetUserStylesheetUri
    :: MonadIO m
    => WebSettings  -- ^ settings
    -> Maybe String -- ^ 'Just' the URI of the stylesheet or 'Nothing'
    -> m ()
webSettingsSetUserStylesheetUri ws ms = liftIO $
    objectSetPropertyMaybeString
        "user-stylesheet-uri" ws ms

{- | Returns the value by which the zoom level is changed when zooming in
     or out.

     Allowed values: >= 0

     Default value: 0.1
-}
webSettingsGetZoomStep
    :: MonadIO m
    => WebSettings -- ^ settings
    -> m Float     -- ^ the step
webSettingsGetZoomStep ws = liftIO $
    objectGetPropertyFloat
        "zoom-step" ws

{- | Sets the value by which the zoom level is changed when zooming in or out.

     Allowed values: >= 0

     Default value: 0.1
-}
webSettingsSetZoomStep
    :: MonadIO m
    => WebSettings -- ^ settings
    -> Float       -- ^ the step
    -> m ()
webSettingsSetZoomStep ws f = liftIO $
    objectSetPropertyFloat
        "zoom-step" ws f

