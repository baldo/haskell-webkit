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
    ) where

#include <webkit/webkitsecurityorigin.h>

import Foreign.C
import GHC.Ptr
import System.Glib.FFI

import System.Glib.GList
import System.Glib.GType

import Control.Monad

import Graphics.UI.Gtk.Abstract.Object
    ( makeNewObject
    )

{#import Graphics.UI.Gtk.WebKit.General.Types#}
    ( SecurityOrigin
    , WebDatabase

    , mkSecurityOrigin
    , withSecurityOrigin
    , mkWebDatabase
    )

-- | Returns a list of all 'WebDatabase's in the 'SecurityOrigin'.
securityOriginGetAllWebDatabase
    :: SecurityOrigin   -- ^ the 'SecurityOrigin'
    -> IO [WebDatabase] -- ^ the 'WebDatabase's
securityOriginGetAllWebDatabase origin = 
    withSecurityOrigin origin $ \ptr -> 
        {#call security_origin_get_all_web_databases#} ptr
            >>= fromGList >>= mapM (makeNewObject mkWebDatabase . return)

-- | Returns the hostname for the 'SecurityOrigin'.
securityOriginGetHost
    :: SecurityOrigin -- ^ the 'SecurityOrigin'
    -> IO String      -- ^ the hostname
securityOriginGetHost origin =
    withSecurityOrigin origin $ \ptr ->
        {#call webkit_security_origin_get_host#} ptr >>= peekCString 

-- | Returns the port for the 'SecurityOrigin'.
securityOriginGetPort
    :: SecurityOrigin -- ^ the 'SecurityOrigin'
    -> IO Int         -- ^ the port
securityOriginGetPort origin =
    withSecurityOrigin origin $ \ptr ->
        liftM fromIntegral $ {#call security_origin_get_port#} ptr

-- | Returns the protocol for the 'SecurityOrigin'.
securityOriginGetProtocol
    :: SecurityOrigin -- ^ the 'SecurityOrigin'
    -> IO String      -- ^ the protocol
securityOriginGetProtocol origin =
    withSecurityOrigin origin $ \ptr ->
        {#call webkit_security_origin_get_protocol#} ptr >>= peekCString 

securityOriginGetType :: IO GType 
securityOriginGetType = 
    {#call security_origin_get_type#}

{- | Returns the quota for 'WebDatabase' storage of the 'SecurityOrigin' in
     bytes.
-}
securityOriginGetWebDatabaseQuote
    :: SecurityOrigin -- ^ the 'SecurityOrigin'
    -> IO Integer     -- ^ the quota in bytes
securityOriginGetWebDatabaseQuote origin =
    withSecurityOrigin origin $ \ptr ->
        liftM toInteger $ {#call security_origin_get_web_database_quota#} ptr

-- | Adjust the quota for 'WebDatabase' storage of the 'SecurityOrigin'
securityOriginSetWebDatabaseQuota
    :: SecurityOrigin -- ^ the 'SecurityOrigin'
    -> Integer        -- ^ a new 'WebDatabase' quota in bytes
    -> IO ()
securityOriginSetWebDatabaseQuota origin quota =
     withSecurityOrigin origin $ \ptr ->
        {#call security_origin_set_web_database_quota#} ptr (fromInteger quota)

-- TODO: guint64 webkit_security_origin_get_web_database_usage (WebKitSecurityOrigin *securityOrigin);
