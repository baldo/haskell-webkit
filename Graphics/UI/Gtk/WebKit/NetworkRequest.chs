{-# LANGUAGE ForeignFunctionInterface #-}

{# context lib="libwebkit" prefix="webkit_" #}

module Graphics.UI.Gtk.WebKit.NetworkRequest
    ( NetworkRequest

    , networkRequestGetType

    , networkRequestNew

    , networkRequestSetUri
    , networkRequestGetUri

    , networkRequestGetMessage

    -- Properties --------------------------------------------------------------

    , networkRequestSetMessage
    ) where
 
#include <webkit/webkitnetworkrequest.h>

import Foreign.C
import GHC.Ptr
import System.Glib.FFI

import System.Glib.GType
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

{#import Network.Soup.General.Types#}
    ( Message
    
    , mkMessage
    )

{#import Network.Soup.Message#}
    ( messageGetType
    )

networkRequestGetType :: IO GType
networkRequestGetType =
    {#call network_request_get_type#}

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

networkRequestGetMessage :: NetworkRequest -> IO Message
networkRequestGetMessage request =
    withNetworkRequest request $ \ptr ->
        makeNewObject mkMessage $ {#call network_request_get_message#} ptr

-- Properties ------------------------------------------------------------------

networkRequestSetMessage :: NetworkRequest -> Message -> IO ()
networkRequestSetMessage request message = do
    mt <- messageGetType
    objectSetPropertyGObject mt "message" request message

