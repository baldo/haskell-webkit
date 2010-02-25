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

    , WebWindowFeatures
    , withWebWindowFeatures
    , mkWebWindowFeatures
    , unWebWindowFeatures

    , unsafeCastGObject
    , toGObject
    ) where

#include <webkit/webkit.h>

import System.Glib.FFI

import Graphics.UI.Gtk.Types
    ( WidgetClass
    , ObjectClass
    , GObjectClass (..)
    , GObject (..)

    , unGObject
    , objectUnref
    )

-- NetworkRequest -------------------------------------------------------------

{#pointer *NetworkRequest foreign newtype#}

instance ObjectClass NetworkRequest
instance GObjectClass NetworkRequest where
  toGObject (NetworkRequest o) =  GObject (castForeignPtr o)
  unsafeCastGObject = NetworkRequest . castForeignPtr . unGObject

mkNetworkRequest = (NetworkRequest, objectUnref)
unNetworkRequest (NetworkRequest o) = o

-- NetworkResponse ------------------------------------------------------------

{#pointer *NetworkResponse foreign newtype#}

instance ObjectClass NetworkResponse
instance GObjectClass NetworkResponse where
  toGObject (NetworkResponse o) = GObject (castForeignPtr o)
  unsafeCastGObject = NetworkResponse . castForeignPtr . unGObject

mkNetworkResponse = (NetworkResponse, objectUnref)
unNetworkResponse (NetworkResponse o) = o

-- WebFrame --------------------------------------------------------------------

{#pointer *WebFrame foreign newtype#}

instance WidgetClass WebFrame 
instance ObjectClass WebFrame 
instance GObjectClass WebFrame where
  toGObject (WebFrame o) = GObject (castForeignPtr o)
  unsafeCastGObject = WebFrame . castForeignPtr . unGObject

mkWebFrame = (WebFrame, objectUnref)
unWebFrame (WebFrame o) = o

-- WebView --------------------------------------------------------------------

{#pointer *WebView foreign newtype#}

instance WidgetClass WebView 
instance ObjectClass WebView 
instance GObjectClass WebView where
  toGObject (WebView o) = GObject (castForeignPtr o)
  unsafeCastGObject = WebView . castForeignPtr . unGObject

mkWebView = (WebView, objectUnref)
unWebView (WebView o) = o

-- WebSettings ----------------------------------------------------------------

{#pointer *WebSettings foreign newtype#}

instance ObjectClass WebSettings
instance GObjectClass WebSettings where
  toGObject (WebSettings o)= GObject (castForeignPtr o)
  unsafeCastGObject = WebSettings . castForeignPtr . unGObject

mkWebSettings = (WebSettings, objectUnref)
unWebSettings (WebSettings o) = o

-- WebHistoryItem -------------------------------------------------------------

{#pointer *WebHistoryItem foreign newtype#}

instance ObjectClass WebHistoryItem
instance GObjectClass WebHistoryItem where
  toGObject (WebHistoryItem o) = GObject (castForeignPtr o)
  unsafeCastGObject = WebHistoryItem . castForeignPtr . unGObject

mkWebHistoryItem = (WebHistoryItem, objectUnref)
unWebHistoryItem (WebHistoryItem o) = o

-- WebBackForwardList ---------------------------------------------------------

{#pointer *WebBackForwardList foreign newtype#}

instance ObjectClass WebBackForwardList
instance GObjectClass WebBackForwardList where
  toGObject (WebBackForwardList o) = GObject (castForeignPtr o)
  unsafeCastGObject = WebBackForwardList . castForeignPtr . unGObject

mkWebBackForwardList = (WebBackForwardList, objectUnref)
unWebBackForwardList (WebBackForwardList o) = o

-- WebNavigationAction --------------------------------------------------------

{#pointer *WebNavigationAction foreign newtype#}

instance ObjectClass WebNavigationAction
instance GObjectClass WebNavigationAction where
  toGObject (WebNavigationAction o) = GObject (castForeignPtr o) 
  unsafeCastGObject = WebNavigationAction . castForeignPtr . unGObject

mkWebNavigationAction = (WebNavigationAction, objectUnref) 
unWebNavigationAction (WebNavigationAction o) = o
 
-- WebPolicyDecision ----------------------------------------------------------

{#pointer *WebPolicyDecision foreign newtype#}

instance ObjectClass WebPolicyDecision
instance GObjectClass WebPolicyDecision where
  toGObject (WebPolicyDecision o) = GObject (castForeignPtr o)
  unsafeCastGObject = WebPolicyDecision . castForeignPtr . unGObject

mkWebPolicyDecision = (WebPolicyDecision, objectUnref)
unWebPolicyDecision (WebPolicyDecision o) = o

-- HitTestResult ----------------------------------------------------------

{#pointer *HitTestResult foreign newtype#}

instance ObjectClass HitTestResult
instance GObjectClass HitTestResult where
  toGObject (HitTestResult o) = GObject (castForeignPtr o)
  unsafeCastGObject = HitTestResult . castForeignPtr . unGObject

mkHitTestResult = (HitTestResult, objectUnref)
unHitTestResult (HitTestResult o) = o

-- WebInspector ----------------------------------------------------------

{#pointer *WebInspector foreign newtype#}

instance ObjectClass WebInspector
instance GObjectClass WebInspector where
  toGObject (WebInspector o) = GObject (castForeignPtr o)
  unsafeCastGObject = WebInspector . castForeignPtr . unGObject

mkWebInspector = (WebInspector, objectUnref)
unWebInspector (WebInspector o) = o

-- WebDatabase ----------------------------------------------------------

{#pointer *WebDatabase foreign newtype#}

instance ObjectClass WebDatabase
instance GObjectClass WebDatabase where
  toGObject (WebDatabase o) = GObject (castForeignPtr o)
  unsafeCastGObject = WebDatabase . castForeignPtr . unGObject

mkWebDatabase = (WebDatabase, objectUnref)
unWebDatabase (WebDatabase o) = o

-- SecurityOrigin ----------------------------------------------------------

{#pointer *SecurityOrigin foreign newtype#}

instance ObjectClass SecurityOrigin
instance GObjectClass SecurityOrigin where
  toGObject (SecurityOrigin o) = GObject (castForeignPtr o)
  unsafeCastGObject = SecurityOrigin . castForeignPtr . unGObject

mkSecurityOrigin = (SecurityOrigin, objectUnref) 
unSecurityOrigin (SecurityOrigin o) = o

-- SoupAuthDialog ----------------------------------------------------------

{#pointer *SoupAuthDialog foreign newtype#}

instance ObjectClass SoupAuthDialog
instance GObjectClass SoupAuthDialog where
  toGObject (SoupAuthDialog o) = GObject (castForeignPtr o)
  unsafeCastGObject = SoupAuthDialog . castForeignPtr . unGObject

mkSoupAuthDialog = (SoupAuthDialog, objectUnref)
unSoupAuthDialog (SoupAuthDialog o) = o

-- WebDataSource ----------------------------------------------------------

{#pointer *WebDataSource foreign newtype#}

instance ObjectClass WebDataSource
instance GObjectClass WebDataSource where
  toGObject (WebDataSource o) = GObject (castForeignPtr o)
  unsafeCastGObject = WebDataSource . castForeignPtr . unGObject

mkWebDataSource = (WebDataSource, objectUnref)
unWebDataSource (WebDataSource o) = o

-- WebResource ----------------------------------------------------------

{#pointer *WebResource foreign newtype#}

instance ObjectClass WebResource
instance GObjectClass WebResource where
  toGObject (WebResource o) = GObject (castForeignPtr o)
  unsafeCastGObject = WebResource . castForeignPtr . unGObject

mkWebResource = (WebResource, objectUnref)
unWebResource (WebResource o) = o

-- Download ----------------------------------------------------------

{#pointer *Download foreign newtype#}

instance ObjectClass Download
instance GObjectClass Download where
  toGObject (Download o) = GObject (castForeignPtr o)
  unsafeCastGObject = Download . castForeignPtr . unGObject

mkDownload = (Download, objectUnref)
unDownload (Download o) = o

-- WebWindowFeatures ----------------------------------------------------------

{#pointer *WebWindowFeatures foreign newtype#}

instance ObjectClass WebWindowFeatures
instance GObjectClass WebWindowFeatures where
  toGObject (WebWindowFeatures o) = GObject (castForeignPtr o)
  unsafeCastGObject = WebWindowFeatures . castForeignPtr . unGObject

mkWebWindowFeatures = (WebWindowFeatures, objectUnref)
unWebWindowFeatures (WebWindowFeatures o) = o

