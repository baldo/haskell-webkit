{-# LANGUAGE ForeignFunctionInterface #-}

{# context lib="libwebkit" prefix="webkit_" #}

{-| A security boundary for web sites.

'SecurityOrigin' is a representation of a security domain defined by web
sites. An origin consists of a host name, a protocol, and a port number. Web
sites with the same security origin can access each other's resources for
client-side scripting or database access.

Use 'webFrameGetSecurityOrigin' to get the 'SecurityOrigin' of a 'WebFrame'.

Database quotas and usages are also defined per 'SecurityOrigin'. The cumulative
disk usage of an origin's databases may be retrieved with
'securityOriginGetWebDatabaseUsage'. An origin's quota can be adjusted
with 'securityOriginSetWebDatabaseQuota'.
-}

module Graphics.UI.Gtk.WebKit.SecurityOrigin
    ( SecurityOrigin

    , securityOriginGetAllWebDatabase

    , securityOriginGetHost
    , securityOriginGetPort
    , securityOriginGetProtocol
    , securityOriginGetType

    , securityOriginGetWebDatabaseQuote
    , securityOriginSetWebDatabaseQuota

    , securityOriginGetWebDatabaseUsage
    ) where

#include <webkit/webkitsecurityorigin.h>

import System.Glib.FFI
import System.Glib.GList
    ( fromGList
    )
import System.Glib.GType
    ( GType
    )

import Control.Monad.Trans
    ( MonadIO
    , liftIO
    )

{#import Graphics.UI.Gtk.WebKit.General.Types#}
    ( SecurityOrigin
    , WebDatabase

    , withSecurityOrigin
    , makeWebDatabase
    )

-- | Returns a list of all 'WebDatabase's in the 'SecurityOrigin'.
securityOriginGetAllWebDatabase
    :: MonadIO m
    => SecurityOrigin   -- ^ the 'SecurityOrigin'
    -> m [WebDatabase]  -- ^ the 'WebDatabase's
securityOriginGetAllWebDatabase origin = liftIO $
    withSecurityOrigin origin $ \ptr ->
        {#call security_origin_get_all_web_databases#} ptr
            >>= fromGList >>= mapM (makeWebDatabase . return)

-- | Returns the hostname for the 'SecurityOrigin'.
securityOriginGetHost
    :: MonadIO m
    => SecurityOrigin -- ^ the 'SecurityOrigin'
    -> m String       -- ^ the hostname
securityOriginGetHost origin = liftIO $
    withSecurityOrigin origin $ \ptr ->
        {#call security_origin_get_host#} ptr >>= peekCString

-- | Returns the port for the 'SecurityOrigin'.
securityOriginGetPort
    :: MonadIO m
    => SecurityOrigin -- ^ the 'SecurityOrigin'
    -> m Int          -- ^ the port
securityOriginGetPort origin = liftIO $
    withSecurityOrigin origin $ \ptr ->
        {#call security_origin_get_port#} ptr >>=
            return . fromIntegral

-- | Returns the protocol for the 'SecurityOrigin'.
securityOriginGetProtocol
    :: MonadIO m
    => SecurityOrigin -- ^ the 'SecurityOrigin'
    -> m String       -- ^ the protocol
securityOriginGetProtocol origin = liftIO $
    withSecurityOrigin origin $ \ptr ->
        {#call security_origin_get_protocol#} ptr >>= peekCString

securityOriginGetType
    :: MonadIO m
    => m GType
securityOriginGetType = liftIO $
    {#call security_origin_get_type#}

{- | Returns the quota for 'WebDatabase' storage of the 'SecurityOrigin' in
     bytes.
-}
securityOriginGetWebDatabaseQuote
    :: MonadIO m
    => SecurityOrigin -- ^ the 'SecurityOrigin'
    -> m Integer      -- ^ the quota in bytes
securityOriginGetWebDatabaseQuote origin = liftIO $
    withSecurityOrigin origin $ \ptr ->
        {#call security_origin_get_web_database_quota#} ptr >>=
            return . toInteger

-- | Adjust the quota for 'WebDatabase' storage of the 'SecurityOrigin'
securityOriginSetWebDatabaseQuota
    :: MonadIO m
    => SecurityOrigin -- ^ the 'SecurityOrigin'
    -> Integer        -- ^ a new 'WebDatabase' quota in bytes
    -> m ()
securityOriginSetWebDatabaseQuota origin quota = liftIO $
     withSecurityOrigin origin $ \ptr ->
        {#call security_origin_set_web_database_quota#} ptr (fromInteger quota)

securityOriginGetWebDatabaseUsage
    :: MonadIO m
    => SecurityOrigin
    -> m Int
securityOriginGetWebDatabaseUsage origin = liftIO $
    withSecurityOrigin origin $ \ptr ->
        {#call security_origin_get_web_database_usage#} ptr >>=
            return . fromIntegral
