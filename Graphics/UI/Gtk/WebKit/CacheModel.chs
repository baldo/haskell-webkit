{-# LANGUAGE ForeignFunctionInterface #-}

{# context lib="libwebkit" prefix="webkit_" #}

{-| Usage model for 'WebView's to determine the caching behavior.

All 'WebView's follow the 'CacheModel'. It determines the RAM and disk space to
use for caching previously viewed content.

Research indicates that users tend to browse within clusters of documents that
hold resources in common, and to revisit previously visited documents. WebKit
and the frameworks below it include built-in caches that take advantage of these
patterns, substantially improving document load speed in browsing situations.
The WebKit 'CacheModel' controls the behaviors of all of these caches, including
various WebCore caches.

Browsers can improve document load speed substantially by specifying
'CacheModelWebBrowser'. Applications without a browsing interface can
reduce memory usage substantially by specifying 'CacheModelDocumentViewer'.
Default value is 'CacheModelWebBrowser'.
-}

module Graphics.UI.Gtk.WebKit.CacheModel
    ( CacheModel
    
    , getCacheModel
    , setCacheModel
    ) where

#include <webkit/webkitwebview.h>

import Foreign.C
import GHC.Ptr
import System.Glib.FFI

import Control.Monad

import Graphics.UI.Gtk.Abstract.Object
    ( makeNewObject
    )

{#import Graphics.UI.Gtk.WebKit.General.Enums#}
    ( CacheModel (..)
    )

-- | Returns the current 'CacheModel'.
getCacheModel :: IO CacheModel -- ^ the current 'CacheModel'
getCacheModel =
    liftM (toEnum . fromIntegral) $
        {#call get_cache_model#}

-- | Sets the 'CacheModel' for WebKit.
setCacheModel :: CacheModel -- ^ 'CacheModel' to use
              -> IO ()
setCacheModel cacheModel =
    {#call set_cache_model#} $ (fromIntegral . fromEnum) cacheModel

