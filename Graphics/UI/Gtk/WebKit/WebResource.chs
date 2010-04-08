{-# LANGUAGE ForeignFunctionInterface #-}

{# context lib="libwebkit" prefix="webkit_" #}

{- | Represents a downloaded URI.

A 'WebResource' encapsulates the data of the download as well as the URI,
MIME type and frame name of the resource.
-}

module Graphics.UI.Gtk.WebKit.WebResource
    ( WebResource

    , webResourceNew
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

-- | Returns the data of the 'WebResource'.
webResourceGetData
    :: WebResource -- ^ a resource
    -> IO String   -- ^ its character data
webResourceGetData resource = 
    withWebResource resource $ \rptr ->
        {#call web_resource_get_data#} rptr >>=
            {#get GString->str#} >>= peekCString

-- | Returns the encoding name of the 'WebResource'.
webResourceGetEncoding
    :: WebResource       -- ^ a resource
    -> IO (Maybe String) -- ^ 'Just' its encoding name or 'Nothing'
webResourceGetEncoding resource =
    withWebResource resource $ \ptr ->
        {#call web_resource_get_encoding#} ptr >>=
            maybePeek peekCString

-- | Returns the frame name of the 'WebResource'.
webResourceGetFrameName
    :: WebResource       -- ^ a resource
    -> IO (Maybe String) -- ^ 'Just' its frame name or 'Nothing'
webResourceGetFrameName resource = 
    withWebResource resource $ \ptr ->
        {#call web_resource_get_frame_name#} ptr >>=
            maybePeek peekCString

-- | Returns the MIME type of the 'WebResource'.
webResourceGetMimeType
    :: WebResource -- ^ a resource
    -> IO String   -- ^ the MIME type of the resource
webResourceGetMimeType resource =
    withWebResource resource $ \ptr ->
        {#call web_resource_get_mime_type#} ptr >>= peekCString

webResourceGetType :: IO GType 
webResourceGetType = 
    {#call webkit_web_resource_get_type#}

-- | Returns the URI of the 'WebResource'.
webResourceGetUri
    :: WebResource -- ^ a resource
    -> IO String   -- ^ its URI
webResourceGetUri resource =
    withWebResource resource $ \ptr ->
        {#call web_resource_get_uri#} ptr >>= peekCString

{- | Returns a new 'WebResource'. The encoding can be 'Nothing'. The frame name
     argument can be used if the resource represents contents of an entire HTML
     frame, otherwise pass 'Nothing'.
-}
webResourceNew
    :: String         -- ^ the data to initialize the resource
    -> Int            -- ^ the length of the data
    -> String         -- ^ the uri of the resource
    -> String         -- ^ the MIME type of the resource
    -> Maybe String   -- ^ 'Just' the text encoding name of the resource
                      --   or 'Nothing' if none
    -> Maybe String   -- ^ 'Just' the frame name of the resource or 'Nothing'
                      --   if none
    -> IO WebResource -- ^ the new resource
webResourceNew dat size uri mimeType encoding frameName =
    withCString dat $ \pData ->
    withCString uri $ \pUri ->
    withCString mimeType $ \pMimeType ->
    maybeWith withCString encoding $ \pEncoding ->
    maybeWith withCString frameName $ \pFrameName ->
        makeWebResource $ 
            {#call web_resource_new#}
                pData
                (fromIntegral size)
                pUri
                pMimeType
                pEncoding
                pFrameName

