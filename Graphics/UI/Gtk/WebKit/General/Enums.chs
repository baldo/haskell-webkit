{-# LANGUAGE ForeignFunctionInterface #-}

{# context lib="libwebkit" prefix="webkit" #}

module Graphics.UI.Gtk.WebKit.General.Enums
    ( DownloadStatus (..)
    , DownloadError (..)
    , NetworkError (..)
    , PolicyError (..)
    , PluginError (..)
    , LoadStatus (..)
    , WebNavigationReason (..)
    , NavigationResponse (..)
    , WebViewTargetInfo (..)
    , EditingBehavior (..)
    , HitTestResultContext (..)
    -- , CacheModel (..) -- new in webkit 1.1.18
    ) where

#include <webkit/webkit.h>

-- Error.h --------------------------------------------------------------------
{#enum NetworkError {underscoreToCase} 
    with prefix = "WebKit_" deriving (Eq, Show)#}

{#enum PolicyError {underscoreToCase} 
    with prefix = "WebKit_" deriving (Eq, Show)#}

{#enum PluginError {underscoreToCase} 
    with prefix = "WebKit_" deriving (Eq, Show)#}

-- WebFrame.h -----------------------------------------------------------------

{#enum LoadStatus {underscoreToCase} 
    with prefix = "WebKit_Load_" deriving (Eq, Show)#}

-- WebView.h ------------------------------------------------------------------

{#enum NavigationResponse {underscoreToCase} 
    with prefix = "WebKit_Navigation_Response_" deriving (Eq, Show)#}

{#enum WebViewTargetInfo {underscoreToCase} 
    with prefix = "WebKit_Web_View_" deriving (Eq, Show)#}

-- WebSettings.h --------------------------------------------------------------

{#enum EditingBehavior {underscoreToCase}
     with prefix = "WebKit_" deriving (Eq,Show)#}

-- CacheModel.h ---------------------------------------------------------------

{- new in webkit 1.1.18
{#enum CacheModel {underscoreToCase}
    with prefix = "WebKit_" deriving (Eq,Show)#}
-}

-- WebNavigation.h --------------------------------------------------------------

{#enum WebNavigationReason {underscoreToCase}
    with prefix = "WebKit_Web_" deriving (Eq,Show) #}

-- HitTestResult,h --------------------------------------------------------------

{#enum HitTestResultContext {underscoreToCase}
    with prefix = "WebKit_" deriving (Eq,Show) #}

-- Download.h -------------------------------------------------------------------

{#enum DownloadStatus {underscoreToCase}
    with prefix = "WebKit_" deriving (Eq,Show) #}

{#enum DownloadError {underscoreToCase}
    with prefix = "WebKit_" deriving (Eq,Show) #}

