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

    , WebInspector
    , withWebInspector
    , mkWebInspector
    , unWebInspector

    , WebDatabase
    , withWebDatabase
    , mkWebDatabase
    , unWebDatabase

    , SecurityOrigin
    , withSecurityOrigin
    , mkSecurityOrigin
    , unSecurityOrigin

    , SoupAuthDialog
    , withSoupAuthDialog
    , mkSoupAuthDialog
    , unSoupAuthDialog

    , WebDataSource
    , withWebDataSource
    , mkWebDataSource
    , unWebDataSource

    , WebResource
    , withWebResource
    , mkWebResource
    , unWebResource

    , Download
    , withDownload
    , mkDownload
    , unDownload

    , unsafeCastGObject
    , toGObject
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

-- WebInspector ----------------------------------------------------------

{#pointer *WebInspector foreign newtype#}

instance ObjectClass WebInspector
instance GObjectClass WebInspector where
  toGObject = mkGObject . castForeignPtr . unWebInspector
  unsafeCastGObject = mkWebInspector . castForeignPtr . unGObject

mkWebInspector = WebInspector
unWebInspector (WebInspector o) = o

-- WebDatabase ----------------------------------------------------------

{#pointer *WebDatabase foreign newtype#}

instance ObjectClass WebDatabase
instance GObjectClass WebDatabase where
  toGObject = mkGObject . castForeignPtr . unWebDatabase
  unsafeCastGObject = mkWebDatabase . castForeignPtr . unGObject

mkWebDatabase = WebDatabase
unWebDatabase (WebDatabase o) = o

-- SecurityOrigin ----------------------------------------------------------

{#pointer *SecurityOrigin foreign newtype#}

instance ObjectClass SecurityOrigin
instance GObjectClass SecurityOrigin where
  toGObject = mkGObject . castForeignPtr . unSecurityOrigin
  unsafeCastGObject = mkSecurityOrigin . castForeignPtr . unGObject

mkSecurityOrigin = SecurityOrigin
unSecurityOrigin (SecurityOrigin o) = o

-- SoupAuthDialog ----------------------------------------------------------

{#pointer *SoupAuthDialog foreign newtype#}

instance ObjectClass SoupAuthDialog
instance GObjectClass SoupAuthDialog where
  toGObject = mkGObject . castForeignPtr . unSoupAuthDialog
  unsafeCastGObject = mkSoupAuthDialog . castForeignPtr . unGObject

mkSoupAuthDialog = SoupAuthDialog
unSoupAuthDialog (SoupAuthDialog o) = o

-- WebDataSource ----------------------------------------------------------

{#pointer *WebDataSource foreign newtype#}

instance ObjectClass WebDataSource
instance GObjectClass WebDataSource where
  toGObject = mkGObject . castForeignPtr . unWebDataSource
  unsafeCastGObject = mkWebDataSource . castForeignPtr . unGObject

mkWebDataSource = WebDataSource
unWebDataSource (WebDataSource o) = o

-- WebResource ----------------------------------------------------------

{#pointer *WebResource foreign newtype#}

instance ObjectClass WebResource
instance GObjectClass WebResource where
  toGObject = mkGObject . castForeignPtr . unWebResource
  unsafeCastGObject = mkWebResource . castForeignPtr . unGObject

mkWebResource = WebResource
unWebResource (WebResource o) = o

-- Download ----------------------------------------------------------

{#pointer *Download foreign newtype#}

instance ObjectClass Download
instance GObjectClass Download where
  toGObject = mkGObject . castForeignPtr . unDownload
  unsafeCastGObject = mkDownload . castForeignPtr . unGObject

mkDownload = Download
unDownload (Download o) = o
