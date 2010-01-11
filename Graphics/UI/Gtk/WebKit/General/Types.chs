{-# LANGUAGE ForeignFunctionInterface #-}

{# context lib="libwebkit" prefix="webkit" #}

module Graphics.UI.Gtk.WebKit.General.Types
    ( NetworkRequest
    , withNetworkRequest
    , mkNetworkRequest
    , unNetworkRequest

    , NetworkResponse
    , withNetworkResponse
    , mkNetworkResponse
    , unNetworkResponse

    , WebFrame (..)
    , withWebFrame
    , mkWebFrame
    , unWebFrame

    , WebView (..)
    , withWebView
    , mkWebView
    , unWebView

    , WebSettings (..)
    , withWebSettings
    , mkWebSettings
    , unWebSettings

    , WebHistoryItem (..)
    , withWebHistoryItem
    , mkWebHistoryItem
    , unWebHistoryItem

    , WebBackForwardList
    , withWebBackForwardList
    , mkWebBackForwardList
    , unWebBackForwardList
    
    , WebNavigationAction
    , withWebNavigationAction
    , mkWebNavigationAction
    , unWebNavigationAction

    , WebPolicyDecision
    , withWebPolicyDecision
    , mkWebPolicyDecision
    , unWebPolicyDecision

    , HitTestResult
    , withHitTestResult
    , mkHitTestResult
    , unHitTestResult

    ) where

#include <webkit/webkit.h>

import System.Glib.FFI

import Graphics.UI.Gtk.Types
    ( WidgetClass
    , ObjectClass
    , GObjectClass (..)

    , mkGObject
    , unGObject
    )

-- NetworkRequest -------------------------------------------------------------

{#pointer *NetworkRequest foreign newtype#}

instance ObjectClass NetworkRequest
instance GObjectClass NetworkRequest where
  toGObject = mkGObject . castForeignPtr . unNetworkRequest
  unsafeCastGObject = mkNetworkRequest . castForeignPtr . unGObject

mkNetworkRequest = NetworkRequest
unNetworkRequest (NetworkRequest o) = o

-- NetworkResponse ------------------------------------------------------------

{#pointer *NetworkResponse foreign newtype#}

instance ObjectClass NetworkResponse
instance GObjectClass NetworkResponse where
  toGObject = mkGObject . castForeignPtr . unNetworkResponse
  unsafeCastGObject = mkNetworkResponse . castForeignPtr . unGObject

mkNetworkResponse = NetworkResponse
unNetworkResponse (NetworkResponse o) = o

-- WebFrame --------------------------------------------------------------------

{#pointer *WebFrame foreign newtype#}

instance WidgetClass WebFrame 
instance ObjectClass WebFrame 
instance GObjectClass WebFrame where
  toGObject = mkGObject . castForeignPtr . unWebFrame
  unsafeCastGObject = mkWebFrame . castForeignPtr . unGObject

mkWebFrame = WebFrame
unWebFrame (WebFrame o) = o

-- WebView --------------------------------------------------------------------

{#pointer *WebView foreign newtype#}

instance WidgetClass WebView 
instance ObjectClass WebView 
instance GObjectClass WebView where
  toGObject = mkGObject . castForeignPtr . unWebView
  unsafeCastGObject = mkWebView . castForeignPtr . unGObject

mkWebView = WebView
unWebView (WebView o) = o

-- WebSettings ----------------------------------------------------------------

{#pointer *WebSettings foreign newtype#}

instance ObjectClass WebSettings
instance GObjectClass WebSettings where
  toGObject = mkGObject . castForeignPtr . unWebSettings
  unsafeCastGObject = mkWebSettings . castForeignPtr . unGObject

mkWebSettings = WebSettings
unWebSettings (WebSettings o) = o

-- WebHistoryItem -------------------------------------------------------------

{#pointer *WebHistoryItem foreign newtype#}

instance ObjectClass WebHistoryItem
instance GObjectClass WebHistoryItem where
  toGObject = mkGObject . castForeignPtr . unWebHistoryItem
  unsafeCastGObject = mkWebHistoryItem . castForeignPtr . unGObject

mkWebHistoryItem = WebHistoryItem
unWebHistoryItem (WebHistoryItem o) = o

-- WebBackForwardList ---------------------------------------------------------

{#pointer *WebBackForwardList foreign newtype#}

instance ObjectClass WebBackForwardList
instance GObjectClass WebBackForwardList where
  toGObject = mkGObject . castForeignPtr . unWebBackForwardList
  unsafeCastGObject = mkWebBackForwardList . castForeignPtr . unGObject

mkWebBackForwardList = WebBackForwardList
unWebBackForwardList (WebBackForwardList o) = o

-- WebNavigationAction --------------------------------------------------------

{#pointer *WebNavigationAction foreign newtype#}

instance ObjectClass WebNavigationAction
instance GObjectClass WebNavigationAction where
  toGObject = mkGObject . castForeignPtr . unWebNavigationAction
  unsafeCastGObject = mkWebNavigationAction . castForeignPtr . unGObject

mkWebNavigationAction = WebNavigationAction
unWebNavigationAction (WebNavigationAction o) = o
 
-- WebPolicyDecision ----------------------------------------------------------

{#pointer *WebPolicyDecision foreign newtype#}

instance ObjectClass WebPolicyDecision
instance GObjectClass WebPolicyDecision where
  toGObject = mkGObject . castForeignPtr . unWebPolicyDecision
  unsafeCastGObject = mkWebPolicyDecision . castForeignPtr . unGObject

mkWebPolicyDecision = WebPolicyDecision
unWebPolicyDecision (WebPolicyDecision o) = o

-- HitTestResult ----------------------------------------------------------

{#pointer *HitTestResult foreign newtype#}

instance ObjectClass HitTestResult
instance GObjectClass HitTestResult where
  toGObject = mkGObject . castForeignPtr . unHitTestResult
  unsafeCastGObject = mkHitTestResult . castForeignPtr . unGObject

mkHitTestResult = HitTestResult
unHitTestResult (HitTestResult o) = o


