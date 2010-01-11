{-# LANGUAGE ForeignFunctionInterface #-}

{# context lib="libwebkit" prefix="webkit_" #}

module Graphics.UI.Gtk.WebKit.WebDatabase
    ( WebDatabase
    
    , webDatabaseGetSize
    , webDatabaseRemove
    , webDatabaseGetSecurityOrigin
    , webDatabaseGetName
    , webDatabaseGetFilename
    , webDatabaseGetExpectedSize
    , webDatabaseGetDisplayName
 
    ) where

#include <webkit/webkitwebdatabase.h>

import Foreign.C
import GHC.Ptr
import System.Glib.FFI

import Control.Monad

import Graphics.UI.Gtk
    ( makeNewObject
    )

{#import Graphics.UI.Gtk.WebKit.General.Types#}
    ( WebDatabase
    , SecurityOrigin

    , mkWebDatabase
    , withWebDatabase
    , mkSecurityOrigin
    )


webDatabaseGetDisplayName :: WebDatabase -> IO String
webDatabaseGetDisplayName database =
    withWebDatabase database $ \ptr ->
        {#call webkit_web_database_get_display_name#} ptr >>= peekCString

webDatabaseGetExpectedSize :: WebDatabase -> IO Integer
webDatabaseGetExpectedSize database =
    withWebDatabase database $ \ptr ->
        liftM toInteger $
            {#call webkit_web_database_get_expected_size#} ptr

webDatabaseGetFilename :: WebDatabase -> IO String
webDatabaseGetFilename database =
    withWebDatabase database $ \ptr ->
        {#call webkit_web_database_get_filename#} ptr >>= peekCString

webDatabaseGetName :: WebDatabase -> IO String
webDatabaseGetName database =
    withWebDatabase database $ \ptr ->
        {#call webkit_web_database_get_name#} ptr >>= peekCString

webDatabaseGetSecurityOrigin :: WebDatabase -> IO SecurityOrigin
webDatabaseGetSecurityOrigin database =
    withWebDatabase database $ \ptr ->
        makeNewObject mkSecurityOrigin $
            {#call webkit_web_database_get_security_origin#} ptr

webDatabaseGetSize :: WebDatabase -> IO Integer
webDatabaseGetSize database =
    withWebDatabase database $ \ptr ->
        liftM toInteger $ {#call webkit_web_database_get_size#} ptr

webDatabaseRemove :: WebDatabase -> IO ()
webDatabaseRemove database =
    withWebDatabase database $ \ptr ->
       {#call webkit_web_database_remove#} ptr
