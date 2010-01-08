{-# LANGUAGE ForeignFunctionInterface #-}

{# context lib="libwebkit" prefix="webkit" #}

module Graphics.UI.Gtk.WebKit.General.Enums
    ( DownloadStatus (..)
    , DownloadError (..)
    , NetworkError (..)
    , PolicyError (..)
    , PluginError (..)
    , LoadStatus (..)
    , NavigationReason (..)
    , NavigationResponse (..)
    , WebViewTargetInfo (..)
    ) where

#include <webkit/webkit.h>

{#enum DownloadStatus {underscoreToCase} deriving (Eq, Show)#}

{#enum DownloadError {underscoreToCase} deriving (Eq, Show)#}

{#enum NetworkError {underscoreToCase} deriving (Eq, Show)#}

{#enum PolicyError {underscoreToCase} deriving (Eq, Show)#}

{#enum PluginError {underscoreToCase} deriving (Eq, Show)#}

{#enum LoadStatus {underscoreToCase} deriving (Eq, Show)#}

{#enum WebNavigationReason as NavigationReason {underscoreToCase}
    with prefix = "WebKit_Web" deriving (Eq, Show)#}

{#enum NavigationResponse {underscoreToCase} deriving (Eq, Show)#}

{#enum WebViewTargetInfo {underscoreToCase} deriving (Eq, Show)#}

