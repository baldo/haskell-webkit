{-# LANGUAGE ForeignFunctionInterface #-}

{# context lib="libwebkit" prefix="webkit" #}

module Graphics.UI.Gtk.WebKit.General.Enums
    ( DOMButton (..)
    , DownloadStatus (..)
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
    , CacheModel (..)

    , downloadStatusGetType
    , downloadErrorGetType
    , networkErrorGetType
    , policyErrorGetType
    , pluginErrorGetType
    , loadStatusGetType
    , webNavigationReasonGetType
    , navigationResponseGetType
    , webViewTargetInfoGetType
    , editingBehaviorGetType
    , hitTestResultContextGetType
    , cacheModelGetType
    ) where

#include <webkit/webkit.h>

import System.Glib.FFI
import System.Glib.GType

data DOMButton = ButtonLeft
               | ButtonMiddle
               | ButtonRight
               | ButtonNone

instance Enum DOMButton where
    toEnum   0  = ButtonLeft
    toEnum   1  = ButtonMiddle
    toEnum   2  = ButtonRight
    toEnum (-1) = ButtonNone
    toEnum unmatched = error ("DOMButton.toEnum: Cannot match "
                           ++ show unmatched)

    fromEnum ButtonLeft   =  0
    fromEnum ButtonMiddle =  1
    fromEnum ButtonRight  =  2
    fromEnum ButtonNone   = -1

-- Error.h --------------------------------------------------------------------

networkErrorGetType
    :: IO GType
networkErrorGetType =
    {#call network_error_get_type#}

{#enum NetworkError {underscoreToCase} 
    with prefix = "WebKit_" deriving (Eq, Show)#}

policyErrorGetType
    :: IO GType
policyErrorGetType =
    {#call policy_error_get_type#}

{#enum PolicyError {underscoreToCase} 
    with prefix = "WebKit_" deriving (Eq, Show)#}

pluginErrorGetType
    :: IO GType
pluginErrorGetType =
    {#call plugin_error_get_type#}

{#enum PluginError {underscoreToCase} 
    with prefix = "WebKit_" deriving (Eq, Show)#}

-- WebFrame.h -----------------------------------------------------------------

loadStatusGetType
    :: IO GType
loadStatusGetType =
    {#call load_status_get_type#}

{#enum LoadStatus {underscoreToCase} 
    with prefix = "WebKit_Load_" deriving (Eq, Show)#}

-- WebView.h ------------------------------------------------------------------

navigationResponseGetType
    :: IO GType
navigationResponseGetType =
    {#call navigation_response_get_type#}

{#enum NavigationResponse {underscoreToCase} 
    with prefix = "WebKit_Navigation_Response_" deriving (Eq, Show)#}

webViewTargetInfoGetType
    :: IO GType
webViewTargetInfoGetType =
    {#call web_view_target_info_get_type#}

{#enum WebViewTargetInfo {underscoreToCase} 
    with prefix = "WebKit_Web_View_" deriving (Eq, Show)#}

-- WebSettings.h --------------------------------------------------------------

editingBehaviorGetType
    :: IO GType
editingBehaviorGetType =
    {#call editing_behavior_get_type#}

{#enum EditingBehavior {underscoreToCase}
     with prefix = "WebKit_" deriving (Eq,Show)#}

-- CacheModel.h ---------------------------------------------------------------

cacheModelGetType
    :: IO GType
cacheModelGetType =
    {#call cache_model_get_type#}

{#enum CacheModel {underscoreToCase}
    with prefix = "WebKit_" deriving (Eq,Show)#}

-- WebNavigation.h --------------------------------------------------------------

webNavigationReasonGetType
    :: IO GType
webNavigationReasonGetType =
    {#call web_navigation_reason_get_type#}

{#enum WebNavigationReason {underscoreToCase}
    with prefix = "WebKit_Web_" deriving (Eq,Show) #}

-- HitTestResult,h --------------------------------------------------------------

hitTestResultContextGetType
    :: IO GType
hitTestResultContextGetType =
    {#call hit_test_result_context_get_type#}

{#enum HitTestResultContext {underscoreToCase}
    with prefix = "WebKit_" deriving (Eq,Show) #}

-- Download.h -------------------------------------------------------------------

downloadStatusGetType
    :: IO GType
downloadStatusGetType =
    {#call download_status_get_type#}

{#enum DownloadStatus {underscoreToCase}
    with prefix = "WebKit_" deriving (Eq,Show) #}

downloadErrorGetType
    :: IO GType
downloadErrorGetType =
    {#call download_error_get_type#}

{#enum DownloadError {underscoreToCase}
    with prefix = "WebKit_" deriving (Eq,Show) #}

