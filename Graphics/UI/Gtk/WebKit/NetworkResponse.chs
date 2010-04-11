{-# LANGUAGE ForeignFunctionInterface #-}

{# context lib="libwebkit" prefix="webkit_" #}

{-| The response given to a 'NetworkRequest'.

'NetworkResponse' represents the network related aspects of a navigation
response.
-}

module Graphics.UI.Gtk.WebKit.NetworkResponse
    ( NetworkResponse

    , networkResponseGetType

    , networkResponseNew

    , networkResponseSetUri
    , networkResponseGetUri

    , networkResponseGetMessage
    ) where

#include <webkit/webkitnetworkresponse.h>

import System.Glib.FFI
import System.Glib.GType
    ( GType
    )

import Control.Monad.Trans
    ( MonadIO
    , liftIO
    )

{#import Graphics.UI.Gtk.WebKit.General.Types#}
    ( NetworkResponse

    , withNetworkResponse
    , makeNetworkResponse
    )

{#import Network.Soup.General.Types#}
    ( Message

    , makeMessage
    )

networkResponseGetType
    :: MonadIO m
    => m GType
networkResponseGetType = liftIO $
    {#call network_response_get_type#}

-- | Creates a new 'NetworkResponse' initialized with an URI.
networkResponseNew
    :: MonadIO m
    => String                    -- ^ an URI
    -> m (Maybe NetworkResponse) -- ^ 'Just' a new 'NetworkResponse', or
                                 --   'Nothing' if the URI is invalid.
networkResponseNew uri = liftIO $
    withCString uri $ \c_uri -> do
        ptr <- {#call network_response_new#} c_uri
        let ptr' = castPtr ptr
        maybePeek (makeNetworkResponse . return) ptr'

-- | Returns the URI belonging to the given 'NetworkResponse'
networkResponseGetUri
    :: MonadIO m
    => NetworkResponse -- ^ the 'NetworkResponse'
    -> m String        -- ^ the URI
networkResponseGetUri response = liftIO $
    withNetworkResponse response $ \ptr ->
        {#call network_response_get_uri#} ptr
            >>= peekCString

{- | Sets the URI held and used by the given 'NetworkResponse'. When the
     response has an associated 'Message', its URI will also be set by this
     call.
-}
networkResponseSetUri
    :: MonadIO m
    => NetworkResponse -- ^ the 'NetworkResponse'
    -> String          -- ^ the URI
    -> m ()
networkResponseSetUri response uri = liftIO $
    withCString uri $ \c_uri ->
        withNetworkResponse response $ \ptr ->
            {#call network_response_set_uri#} ptr c_uri

-- | Returns the to the 'NetworkResponse' associated 'Message'.
networkResponseGetMessage
    :: MonadIO m
    => NetworkResponse -- ^ the 'NetworkResponse'
    -> m Message       -- ^ the 'Message'
networkResponseGetMessage response = liftIO $
    withNetworkResponse response $ \ptr ->
        makeMessage $ {#call network_response_get_message#} ptr

