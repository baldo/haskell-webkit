{-# LANGUAGE ForeignFunctionInterface #-}

{# context lib="libwebkit" prefix="webkit_" #}

module Graphics.UI.Gtk.WebKit.CacheModel
    ( CacheModel
    
    , getCacheModel
    , setCacheModel
    ) where

#include <webkit/webkitcachemodel.h>

import Foreign.C
import GHC.Ptr
import System.Glib.FFI

import Control.Monad

import Graphics.UI.Gtk.Abstract.Object
    ( makeNewObject
    )

{#import Graphics.UI.Gtk.WebKit.General.Types#}
    ( CacheModel 
    )

getCacheModel :: IO CacheModel
getCacheModel =
    {#call get_cache_model#}

setCacheModel :: CacheModel -> IO ()
setCacheModel cacheModel =
    {#call set_cache_model#} cacheModel
