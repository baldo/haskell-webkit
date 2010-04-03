{-# LANGUAGE ForeignFunctionInterface #-}

{# context lib="libwebkit" prefix="webkit_" #}

module Graphics.UI.Gtk.WebKit.WebResource
    ( WebResource

    , webResourceGetNew
    , webResourceGetUri
    , webResourceGetType
    , webResourceGetFrameName
    , webResourceGetMimeType 
    , webResourceGetEncoding
    , webResourceGetData

    ) where

#include <webkit/webkitwebresource.h>

import Foreign.C
import System.Glib.FFI
import System.Glib.GType

{#import Graphics.UI.Gtk.WebKit.General.Types#}
    ( WebResource

    , withWebResource 
    , makeWebResource
    )

webResourceGetData :: WebResource -> IO String
webResourceGetData resource = 
    withWebResource resource $ \rptr ->
        {#call web_resource_get_data#} rptr >>=
            {#get GString->str#} >>= peekCString

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
              makeWebResource $ 
                {#call web_resource_new#} pData (fromIntegral size) pUri pMimeType pEncoding pFrameName
