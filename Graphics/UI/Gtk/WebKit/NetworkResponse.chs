{-# LANGUAGE ForeignFunctionInterface #-}

{# context lib="libwebkit" prefix="webkit_" #}

module Graphics.UI.Gtk.WebKit.NetworkResponse
    ( NetworkResponse

    --, networkResponseGetType

    , networkResponseNew

    , networkResponseSetUri
    , networkResponseGetUri

    --, networkResponseGetMessage
    ) where
 
#include <webkit/webkitnetworkresponse.h>

import Foreign.C
import GHC.Ptr
import System.Glib.FFI

import System.Glib.Properties

import Control.Monad

import Graphics.UI.Gtk
    ( makeNewObject
    )

import Graphics.UI.Gtk.Signals

{#import Graphics.UI.Gtk.WebKit.General.Types#}
    ( NetworkResponse

    , withNetworkResponse
    , mkNetworkResponse
    )

-- TODO: GType webkit_network_response_get_type (void);

networkResponseNew :: String -> IO NetworkResponse
networkResponseNew uri =
    withCString uri $ \c_uri -> do
        ptr <- {#call network_response_new#} c_uri
        let ptr' = castPtr ptr
        makeNewObject mkNetworkResponse (return ptr')

networkResponseGetUri :: NetworkResponse -> IO (Maybe String)
networkResponseGetUri response =
    withNetworkResponse response $ \ptr ->
        {#call network_response_get_uri#} ptr
            >>= maybePeek peekCString

networkResponseSetUri :: NetworkResponse -> String -> IO ()
networkResponseSetUri response uri = do
    withCString uri $ \c_uri ->
        withNetworkResponse response $ \ptr ->
            {#call network_response_set_uri#} ptr c_uri

-- TODO: SoupMessage * webkit_network_response_get_message(WebKitNetworkResponse* response);

