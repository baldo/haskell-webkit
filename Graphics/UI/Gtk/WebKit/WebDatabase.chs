{-# LANGUAGE ForeignFunctionInterface #-}

{# context lib="libwebkit" prefix="webkit_" #}

{-| A WebKit web application database.

'WebDatabase' is a representation of a Web Database database. The proposed
Web Database standard introduces support for SQL databases that web sites can
create and access on a local computer through JavaScript.

To get access to all databases defined by a 'SecurityOrigin', use
'securityOriginGetDatabases'. Each database has a canonical name, as well as a
user-friendly display name.

WebKit uses SQLite to create and access the local SQL databases. The location of
a 'WebDatabase' can be accessed wth 'webDatabaseGetFilename'. You can configure
the location of all databases with webkit_set_database_directory_path. (???)

For each database the web site can define an estimated size which can be
accessed with 'webDatabaseGetExpectedSize'. The current size of the database in
bytes is returned by 'webDatabaseGetSize'.

For more information refer to the Web Database specification proposal at
<http://dev.w3.org/html5/webdatabase>.
-}

module Graphics.UI.Gtk.WebKit.WebDatabase
    ( WebDatabase
    
    , webDatabaseGetType
    , webDatabaseGetSize
    , webDatabaseRemove
    , webDatabaseRemoveAll 
    , webDatabaseGetSecurityOrigin
    , webDatabaseGetName
    , webDatabaseGetFilename
    , webDatabaseGetExpectedSize
    , webDatabaseGetDisplayName
    , webDatabaseGetDefaultQuota 
    , webDatabaseSetDefaultQuota 
    , webDatabaseGetDirectoryPath 
    , webDatabaseSetDirectoryPath 

    ) where

#include <webkit/webkitwebdatabase.h>

import Foreign.C
import GHC.Ptr
import System.Glib.FFI
import System.Glib.GType
import Control.Monad

import Graphics.UI.Gtk.Abstract.Object
    ( makeNewObject
    )

{#import Graphics.UI.Gtk.WebKit.General.Types#}
    ( WebDatabase
    , SecurityOrigin

    , mkWebDatabase
    , withWebDatabase
    , mkSecurityOrigin
    )

webDatabaseGetType
    :: IO GType
webDatabaseGetType =
    {#call web_database_get_type#}

-- | Returns the name of the 'WebDatabase' as seen by the user.
webDatabaseGetDisplayName
    :: WebDatabase -- ^ the database
    -> IO String   -- ^ the name of the database as seen by the user.
webDatabaseGetDisplayName database =
    withWebDatabase database $ \ptr ->
        {#call webkit_web_database_get_display_name#} ptr >>= peekCString

{- | Returns the expected size of the 'WebDatabase' in bytes as defined by the
     web author. The Web Database standard allows web authors to specify an
     expected size of the database to optimize the user experience.
-}
webDatabaseGetExpectedSize
    :: WebDatabase -- ^ the database
    -> IO Integer  -- ^ the expected size of the database in bytes 
webDatabaseGetExpectedSize database =
    withWebDatabase database $ \ptr ->
        liftM toInteger $
            {#call webkit_web_database_get_expected_size#} ptr

-- | Returns the absolute filename to the 'WebDatabase' file on disk.
webDatabaseGetFilename
    :: WebDatabase -- ^ the database
    -> IO String   -- ^ the absolute filename of the database
webDatabaseGetFilename database =
    withWebDatabase database $ \ptr ->
        {#call webkit_web_database_get_filename#} ptr >>= peekCString

-- | Returns the canonical name of the 'WebDatabase'.
webDatabaseGetName
    :: WebDatabase -- ^ the database
    -> IO String   -- ^ the name of the database
webDatabaseGetName database =
    withWebDatabase database $ \ptr ->
        {#call webkit_web_database_get_name#} ptr >>= peekCString

-- | Returns the 'SecurityOrigin' of the 'WebDatabase'.
webDatabaseGetSecurityOrigin
    :: WebDatabase       -- ^ the database
    -> IO SecurityOrigin -- ^ the security origin of the database 
webDatabaseGetSecurityOrigin database =
    withWebDatabase database $ \ptr ->
        makeNewObject mkSecurityOrigin $
            {#call webkit_web_database_get_security_origin#} ptr

-- | Returns the actual size of the 'WebDatabase' space on disk in bytes.
webDatabaseGetSize
    :: WebDatabase -- ^ the database
    -> IO Integer  -- ^ the actual size of the database in bytes
webDatabaseGetSize database =
    withWebDatabase database $ \ptr ->
        liftM toInteger $ {#call webkit_web_database_get_size#} ptr

{- | Removes the 'WebDatabase' from its 'SecurityOrigin' and destroys all data
     stored in the database.
-}
webDatabaseRemove
    :: WebDatabase -- ^ the database to remove
    -> IO ()
webDatabaseRemove database =
    withWebDatabase database $ \ptr ->
       {#call webkit_web_database_remove#} ptr

webDatabaseRemoveAll :: IO ()
webDatabaseRemoveAll =
    {#call remove_all_web_databases#}

webDatabaseGetDefaultQuota :: IO Int
webDatabaseGetDefaultQuota = 
    liftM fromIntegral $
        {#call get_default_web_database_quota#}

webDatabaseSetDefaultQuota :: Int -> IO ()
webDatabaseSetDefaultQuota quota =
    {#call set_default_web_database_quota#} (fromIntegral quota)

webDatabaseGetDirectoryPath :: IO String
webDatabaseGetDirectoryPath = 
        {#call get_web_database_directory_path#} >>= peekCString

webDatabaseSetDirectoryPath :: String -> IO ()
webDatabaseSetDirectoryPath directory =
    withCString directory $ \ dir ->
        {#call set_web_database_directory_path#} dir
