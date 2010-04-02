{-# LANGUAGE ForeignFunctionInterface #-}

{# context lib="libwebkit" prefix="webkit" #}

module Graphics.UI.Gtk.WebKit.General.Types
    ( NetworkRequest
    , withNetworkRequest
    , makeNetworkRequest
    , mkNetworkRequest
    , unNetworkRequest

    , NetworkResponse
    , withNetworkResponse
    , makeNetworkResponse
    , mkNetworkResponse
    , unNetworkResponse

    , WebFrame (..)
    , withWebFrame
    , makeWebFrame 
    , mkWebFrame
    , unWebFrame

    , WebView (..)
    , withWebView
    , makeWebView
    , mkWebView
    , unWebView

    , WebSettings (..)
    , withWebSettings
    , makeWebSettings 
    , mkWebSettings
    , unWebSettings

    , WebHistoryItem (..)
    , withWebHistoryItem
    , makeWebHistoryItem 
    , mkWebHistoryItem
    , unWebHistoryItem

    , WebBackForwardList
    , withWebBackForwardList
    , makeWebBackForwardList
    , mkWebBackForwardList
    , unWebBackForwardList
    
    , WebNavigationAction
    , withWebNavigationAction
    , makeWebNavigationAction
    , mkWebNavigationAction
    , unWebNavigationAction

    , WebPolicyDecision
    , withWebPolicyDecision
    , makeWebPolicyDecision
    , mkWebPolicyDecision
    , unWebPolicyDecision

    , HitTestResult
    , withHitTestResult
    , makeHitTestResult
    , mkHitTestResult
    , unHitTestResult
    , hitTestResultGetType

    , WebInspector
    , withWebInspector
    , makeWebInspector
    , mkWebInspector
    , unWebInspector

    , WebDatabase
    , withWebDatabase
    , makeWebDatabase
    , mkWebDatabase
    , unWebDatabase

    , SecurityOrigin
    , withSecurityOrigin
    , makeSecurityOrigin
    , mkSecurityOrigin
    , unSecurityOrigin

    , SoupAuthDialog
    , withSoupAuthDialog
    , makeSoupAuthDialog
    , mkSoupAuthDialog
    , unSoupAuthDialog

    , WebDataSource
    , withWebDataSource
    , makeWebDataSource
    , mkWebDataSource
    , unWebDataSource

    , WebResource
    , withWebResource
    , makeWebResource
    , mkWebResource
    , unWebResource

    , Download
    , withDownload
    , makeDownload
    , mkDownload
    , unDownload

    , WebWindowFeatures
    , withWebWindowFeatures
    , makeWebWindowFeatures
    , mkWebWindowFeatures
    , unWebWindowFeatures

    , unsafeCastGObject
    , toGObject
    ) where

#include <webkit/webkit.h>

import System.Glib.FFI
import System.Glib.GType

import Graphics.UI.Gtk.Abstract.Object
    ( makeNewObject
    )

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
makeNetworkRequest = makeNewObject mkNetworkRequest

-- NetworkResponse ------------------------------------------------------------

{#pointer *NetworkResponse foreign newtype#}

instance ObjectClass NetworkResponse
instance GObjectClass NetworkResponse where
  toGObject (NetworkResponse o) = GObject (castForeignPtr o)
  unsafeCastGObject = NetworkResponse . castForeignPtr . unGObject

mkNetworkResponse = (NetworkResponse, objectUnref)
unNetworkResponse (NetworkResponse o) = o
makeNetworkResponse = makeNewObject mkNetworkResponse

-- WebFrame --------------------------------------------------------------------

{#pointer *WebFrame foreign newtype#}

instance WidgetClass WebFrame 
instance ObjectClass WebFrame 
instance GObjectClass WebFrame where
  toGObject (WebFrame o) = GObject (castForeignPtr o)
  unsafeCastGObject = WebFrame . castForeignPtr . unGObject

mkWebFrame = (WebFrame, objectUnref)
unWebFrame (WebFrame o) = o
makeWebFrame = makeNewObject mkWebFrame

-- WebView --------------------------------------------------------------------

{#pointer *WebView foreign newtype#}

instance WidgetClass WebView 
instance ObjectClass WebView 
instance GObjectClass WebView where
  toGObject (WebView o) = GObject (castForeignPtr o)
  unsafeCastGObject = WebView . castForeignPtr . unGObject

mkWebView = (WebView, objectUnref)
unWebView (WebView o) = o
makeWebView = makeNewObject mkWebView 

-- WebSettings ----------------------------------------------------------------

{#pointer *WebSettings foreign newtype#}

instance ObjectClass WebSettings
instance GObjectClass WebSettings where
  toGObject (WebSettings o)= GObject (castForeignPtr o)
  unsafeCastGObject = WebSettings . castForeignPtr . unGObject

mkWebSettings = (WebSettings, objectUnref)
unWebSettings (WebSettings o) = o
makeWebSettings = makeNewObject mkWebSettings

-- WebHistoryItem -------------------------------------------------------------

{#pointer *WebHistoryItem foreign newtype#}

instance ObjectClass WebHistoryItem
instance GObjectClass WebHistoryItem where
  toGObject (WebHistoryItem o) = GObject (castForeignPtr o)
  unsafeCastGObject = WebHistoryItem . castForeignPtr . unGObject

mkWebHistoryItem = (WebHistoryItem, objectUnref)
unWebHistoryItem (WebHistoryItem o) = o
makeWebHistoryItem = makeNewObject mkWebHistoryItem

-- WebBackForwardList ---------------------------------------------------------

{#pointer *WebBackForwardList foreign newtype#}

instance ObjectClass WebBackForwardList
instance GObjectClass WebBackForwardList where
  toGObject (WebBackForwardList o) = GObject (castForeignPtr o)
  unsafeCastGObject = WebBackForwardList . castForeignPtr . unGObject

mkWebBackForwardList = (WebBackForwardList, objectUnref)
unWebBackForwardList (WebBackForwardList o) = o
makeWebBackForwardList = makeNewObject mkWebBackForwardList

-- WebNavigationAction --------------------------------------------------------

{#pointer *WebNavigationAction foreign newtype#}

instance ObjectClass WebNavigationAction
instance GObjectClass WebNavigationAction where
  toGObject (WebNavigationAction o) = GObject (castForeignPtr o) 
  unsafeCastGObject = WebNavigationAction . castForeignPtr . unGObject

mkWebNavigationAction = (WebNavigationAction, objectUnref) 
unWebNavigationAction (WebNavigationAction o) = o
makeWebNavigationAction = makeNewObject mkWebNavigationAction    

-- WebPolicyDecision ----------------------------------------------------------

{#pointer *WebPolicyDecision foreign newtype#}

instance ObjectClass WebPolicyDecision
instance GObjectClass WebPolicyDecision where
  toGObject (WebPolicyDecision o) = GObject (castForeignPtr o)
  unsafeCastGObject = WebPolicyDecision . castForeignPtr . unGObject

mkWebPolicyDecision = (WebPolicyDecision, objectUnref)
unWebPolicyDecision (WebPolicyDecision o) = o
makeWebPolicyDecision = makeNewObject mkWebPolicyDecision  

-- HitTestResult ----------------------------------------------------------

{#pointer *HitTestResult foreign newtype#}

instance ObjectClass HitTestResult
instance GObjectClass HitTestResult where
  toGObject (HitTestResult o) = GObject (castForeignPtr o)
  unsafeCastGObject = HitTestResult . castForeignPtr . unGObject

mkHitTestResult = (HitTestResult, objectUnref)
unHitTestResult (HitTestResult o) = o
makeHitTestResult = makeNewObject mkHitTestResult  

hitTestResultGetType :: IO GType
hitTestResultGetType =
    {#call hit_test_result_get_type#}

-- WebInspector ----------------------------------------------------------

{#pointer *WebInspector foreign newtype#}

instance ObjectClass WebInspector
instance GObjectClass WebInspector where
  toGObject (WebInspector o) = GObject (castForeignPtr o)
  unsafeCastGObject = WebInspector . castForeignPtr . unGObject

mkWebInspector = (WebInspector, objectUnref)
unWebInspector (WebInspector o) = o
makeWebInspector = makeNewObject mkWebInspector 

-- WebDatabase ----------------------------------------------------------

{#pointer *WebDatabase foreign newtype#}

instance ObjectClass WebDatabase
instance GObjectClass WebDatabase where
  toGObject (WebDatabase o) = GObject (castForeignPtr o)
  unsafeCastGObject = WebDatabase . castForeignPtr . unGObject

mkWebDatabase = (WebDatabase, objectUnref)
unWebDatabase (WebDatabase o) = o
makeWebDatabase= makeNewObject mkWebDatabase

-- SecurityOrigin ----------------------------------------------------------

{#pointer *SecurityOrigin foreign newtype#}

instance ObjectClass SecurityOrigin
instance GObjectClass SecurityOrigin where
  toGObject (SecurityOrigin o) = GObject (castForeignPtr o)
  unsafeCastGObject = SecurityOrigin . castForeignPtr . unGObject

mkSecurityOrigin = (SecurityOrigin, objectUnref) 
unSecurityOrigin (SecurityOrigin o) = o
makeSecurityOrigin = makeNewObject mkSecurityOrigin 

-- SoupAuthDialog ----------------------------------------------------------

{#pointer *SoupAuthDialog foreign newtype#}

instance ObjectClass SoupAuthDialog
instance GObjectClass SoupAuthDialog where
  toGObject (SoupAuthDialog o) = GObject (castForeignPtr o)
  unsafeCastGObject = SoupAuthDialog . castForeignPtr . unGObject

mkSoupAuthDialog = (SoupAuthDialog, objectUnref)
unSoupAuthDialog (SoupAuthDialog o) = o
makeSoupAuthDialog = makeNewObject mkSoupAuthDialog 

-- WebDataSource ----------------------------------------------------------

{#pointer *WebDataSource foreign newtype#}

instance ObjectClass WebDataSource
instance GObjectClass WebDataSource where
  toGObject (WebDataSource o) = GObject (castForeignPtr o)
  unsafeCastGObject = WebDataSource . castForeignPtr . unGObject

mkWebDataSource = (WebDataSource, objectUnref)
unWebDataSource (WebDataSource o) = o
makeWebDataSource = makeNewObject mkWebDataSource 

-- WebResource ----------------------------------------------------------

{#pointer *WebResource foreign newtype#}

instance ObjectClass WebResource
instance GObjectClass WebResource where
  toGObject (WebResource o) = GObject (castForeignPtr o)
  unsafeCastGObject = WebResource . castForeignPtr . unGObject

mkWebResource = (WebResource, objectUnref)
unWebResource (WebResource o) = o
makeWebResource = makeNewObject mkWebResource  

-- Download ----------------------------------------------------------

{#pointer *Download foreign newtype#}

instance ObjectClass Download
instance GObjectClass Download where
  toGObject (Download o) = GObject (castForeignPtr o)
  unsafeCastGObject = Download . castForeignPtr . unGObject

mkDownload = (Download, objectUnref)
unDownload (Download o) = o
makeDownload = makeNewObject mkDownload 

-- WebWindowFeatures ----------------------------------------------------------

{#pointer *WebWindowFeatures foreign newtype#}

instance ObjectClass WebWindowFeatures
instance GObjectClass WebWindowFeatures where
  toGObject (WebWindowFeatures o) = GObject (castForeignPtr o)
  unsafeCastGObject = WebWindowFeatures . castForeignPtr . unGObject

mkWebWindowFeatures = (WebWindowFeatures, objectUnref)
unWebWindowFeatures (WebWindowFeatures o) = o
makeWebWindowFeatures = makeNewObject mkWebWindowFeatures 
