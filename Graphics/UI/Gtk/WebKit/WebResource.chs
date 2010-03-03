{-# LANGUAGE ForeignFunctionInterface #-}

{# context lib="libwebkit" prefix="webkit_" #}

module Graphics.UI.Gtk.WebKit.WebResource
    ( WebResource

    , webResourceGetNew
    , webResourceGetUri
    , webResourceGetType
    , webResourceGetFrameName
    , webResourceGetEncoding
    --, webResourceGetData -- TODO

    ) where

#include <webkit/webkitwebresource.h>

import Foreign.C
import GHC.Ptr
import System.Glib.FFI
import System.Glib.GType

import Control.Monad

import Graphics.UI.Gtk.Abstract.Object
    ( makeNewObject
    )

{#import Graphics.UI.Gtk.WebKit.General.Types#}
    ( WebResource

    , withWebResource 
    , mkWebResource
    )


{- TODO figure out wth GString comes from
GString *           webkit_web_resource_get_data        (WebKitWebResource *web_resource);
-}

webResourceGetEncoding :: WebResource -> IO String
webResourceGetEncoding resource =
    withWebResource resource $ \ptr ->
        {#call web_resource_get_encoding#} ptr >>= peekCString

webResourceGetFrameName :: WebResource -> IO String
webResourceGetFrameName resource = 
    withWebResource resource $ \ptr ->
        {#call web_resource_get_frame_name#} ptr >>= peekCString

webResourceGetMimeType :: WebResource -> IO String
webResourceGetMimeType resource =
    withWebResource resource $ \ptr ->
        {#call web_resource_get_mime_type#} ptr >>= peekCString

webResourceGetType :: IO GType 
webResourceGetType = 
    {#call webkit_web_resource_get_type#}

webResourceGetUri :: WebResource -> IO String
webResourceGetUri resource =
    withWebResource resource $ \ptr ->
        {#call web_resource_get_uri#} ptr >>= peekCString

webResourceGetNew :: String -> Int -> String -> String -> String -> String -> IO WebResource
webResourceGetNew dat size uri mimeType encoding frameName =
    withCString dat $ \pData ->
      withCString uri $ \pUri ->
        withCString mimeType $ \pMimeType ->
          withCString encoding $ \pEncoding ->
            withCString frameName $ \pFrameName ->
              makeNewObject mkWebResource $ 
                {#call web_resource_new#} pData (fromIntegral size) pUri pMimeType pEncoding pFrameName
