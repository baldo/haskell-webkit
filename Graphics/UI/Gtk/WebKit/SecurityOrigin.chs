{-# LANGUAGE ForeignFunctionInterface #-}

{# context lib="libwebkit" prefix="webkit_" #}

module Graphics.UI.Gtk.WebKit.SecurityOrigin
    ( SecurityOrigin
    
    ) where

#include <webkit/webkitsecurityorigin.h>

import Foreign.C
import GHC.Ptr
import System.Glib.FFI

import System.Glib.GList
import System.Glib.GType

import Control.Monad

import Graphics.UI.Gtk
    ( makeNewObject
    )

{#import Graphics.UI.Gtk.WebKit.General.Types#}
    ( SecurityOrigin
    , WebDatabase

    , mkSecurityOrigin
    , withSecurityOrigin
    , mkWebDatabase
    )

-- TODO Make a list of WebDatabase from GList
securityOriginGetAllWebDatabase :: SecurityOrigin -> IO [WebDatabase]
securityOriginGetAllWebDatabase origin = 
    withSecurityOrigin origin $ \ptr -> 
        {#call security_origin_get_all_web_databases#} ptr >>= fromGList  >>= mapM (makeNewObject mkWebDatabase . return)

securityOriginGetHost :: SecurityOrigin -> IO String
securityOriginGetHost origin =
    withSecurityOrigin origin $ \ptr ->
        {#call webkit_security_origin_get_host#} ptr >>= peekCString 

securityOriginGetPort :: SecurityOrigin -> IO Int
securityOriginGetPort origin =
    withSecurityOrigin origin $ \ptr ->
        liftM fromIntegral $ {#call security_origin_get_port#} ptr

securityOriginGetProtocol :: SecurityOrigin -> IO String
securityOriginGetProtocol origin =
    withSecurityOrigin origin $ \ptr ->
        {#call webkit_security_origin_get_protocol#} ptr >>= peekCString 

securityOriginGetType :: IO GType 
securityOriginGetType = 
    {#call security_origin_get_type#}

securityOriginGetWebDatabaseQuote :: SecurityOrigin -> IO Integer
securityOriginGetWebDatabaseQuote origin =
    withSecurityOrigin origin $ \ptr ->
        liftM toInteger $ {#call security_origin_get_web_database_quota#} ptr

securityOriginSetWebDatabaseQuota :: SecurityOrigin -> Integer -> IO ()
securityOriginSetWebDatabaseQuota origin quota =
     withSecurityOrigin origin $ \ptr ->
        {#call security_origin_set_web_database_quota#} ptr (fromInteger quota)

