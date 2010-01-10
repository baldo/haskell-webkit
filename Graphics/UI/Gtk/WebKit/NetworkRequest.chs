{-# LANGUAGE ForeignFunctionInterface #-}

{# context lib="libwebkit" prefix="webkit_" #}

module Graphics.UI.Gtk.WebKit.NetworkRequest
    ( NetworkRequest

    --, networkRequestGetType

    , networkRequestNew

    , networkRequestSetUri
    , networkRequestGetUri

    --, networkRequestGetMessage

    -- Properties --------------------------------------------------------------

    --, networkRequestSetMessage
    ) where
 
#include <webkit/webkitnetworkrequest.h>

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
    ( NetworkRequest

    , withNetworkRequest
    , mkNetworkRequest
    )

-- TODO: GType webkit_network_request_get_type (void);

networkRequestNew :: String -> IO NetworkRequest
networkRequestNew uri =
    withCString uri $ \c_uri -> do
        ptr <- {#call network_request_new#} c_uri
        let ptr' = castPtr ptr
        makeNewObject mkNetworkRequest (return ptr')

networkRequestGetUri :: NetworkRequest -> IO (Maybe String)
networkRequestGetUri request =
    withNetworkRequest request $ \ptr ->
        {#call network_request_get_uri#} ptr
            >>= maybePeek peekCString

networkRequestSetUri :: NetworkRequest -> String -> IO ()
networkRequestSetUri request uri = do
    withCString uri $ \c_uri ->
        withNetworkRequest request $ \ptr ->
            {#call network_request_set_uri#} ptr c_uri

-- TODO: SoupMessage * webkit_network_request_get_message(WebKitNetworkRequest* request);

-- Properties ------------------------------------------------------------------

-- TODO: networkRequestSetMessage 
-- "message" SoupMessage* : Read / Write / Construct Only

