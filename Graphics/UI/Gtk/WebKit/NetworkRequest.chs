{-# LANGUAGE ForeignFunctionInterface #-}

{# context lib="libwebkit" prefix="webkit_" #}

{-| The target of a navigation request.

'NetworkRequest' represents the network related aspects of a navigation request.
It is used whenever WebKit wants to provide information about a request that
will be sent, or has been sent. With it you can get the URI of the request, and,
for valid URIs, a 'Message' object, which provides access to further information
such as headers.
-}

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
import System.Glib.FFI

import System.Glib.GType
import System.Glib.Properties

import Control.Monad

import Graphics.UI.Gtk.Signals

{#import Graphics.UI.Gtk.WebKit.General.Types#}
    ( NetworkRequest

    , withNetworkRequest
    , makeNetworkRequest
    )

{#import Network.Soup.General.Types#}
    ( Message
    
    , makeMessage
    )

{#import Network.Soup.Message#}
    ( messageGetType
    )

networkRequestGetType :: IO GType
networkRequestGetType =
    {#call network_request_get_type#}

-- | Creates a new 'NetworkRequest' initialized with an URI.
networkRequestNew
    :: String                    -- ^ an URI
    -> IO (Maybe NetworkRequest) -- ^ 'Just' a new 'NetworkRequest', or
                                 --   'Nothing' if the URI is invalid. 
networkRequestNew uri =
    withCString uri $ \c_uri -> do
        ptr <- {#call network_request_new#} c_uri
        let ptr' = castPtr ptr
        maybePeek (makeNetworkRequest . return) ptr'

-- | Returns the URI belonging to the given 'NetworkRequest'
networkRequestGetUri
    :: NetworkRequest -- ^ the 'NetworkRequest'
    -> IO String      -- ^ the URI
networkRequestGetUri request =
    withNetworkRequest request $ \ptr ->
        {#call network_request_get_uri#} ptr
            >>= peekCString

{- | Sets the URI held and used by the given 'NetworkRequest'. When the request
     has an associated 'Message', its URI will also be set by this call.
-}
networkRequestSetUri
    :: NetworkRequest -- ^ the 'NetworkRequest'
    -> String         -- ^ the URI
    -> IO ()
networkRequestSetUri request uri = do
    withCString uri $ \c_uri ->
        withNetworkRequest request $ \ptr ->
            {#call network_request_set_uri#} ptr c_uri

-- | Returns the to the 'NetworkRequest' associated 'Message'.
networkRequestGetMessage
    :: NetworkRequest -- ^ the 'NetworkRequest'
    -> IO Message     -- ^ the 'Message'
networkRequestGetMessage request =
    withNetworkRequest request $ \ptr ->
        makeMessage $ {#call network_request_get_message#} ptr

-- Properties ------------------------------------------------------------------

-- | Associate the given 'Message' to the given 'NetworkRequest'.
networkRequestSetMessage
    :: NetworkRequest -- ^ the 'NetworkRequest'
    -> Message        -- ^ the 'Message'
    -> IO ()
networkRequestSetMessage request message = do
    mt <- messageGetType
    objectSetPropertyGObject mt "message" request message

