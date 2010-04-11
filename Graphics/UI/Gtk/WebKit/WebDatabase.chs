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

import System.Glib.FFI
import System.Glib.GType
    ( GType
    )

import Control.Monad.Trans
    ( MonadIO
    , liftIO
    )

{#import Graphics.UI.Gtk.WebKit.General.Types#}
    ( WebDatabase
    , SecurityOrigin

    , withWebDatabase
    , makeSecurityOrigin
    )

webDatabaseGetType
    :: MonadIO m
    => m GType
webDatabaseGetType = liftIO $
    {#call web_database_get_type#}

-- | Returns the name of the 'WebDatabase' as seen by the user.
webDatabaseGetDisplayName
    :: MonadIO m
    => WebDatabase -- ^ the database
    -> m String    -- ^ the name of the database as seen by the user.
webDatabaseGetDisplayName database = liftIO $
    withWebDatabase database $ \ptr ->
        {#call webkit_web_database_get_display_name#} ptr >>= peekCString

{- | Returns the expected size of the 'WebDatabase' in bytes as defined by the
     web author. The Web Database standard allows web authors to specify an
     expected size of the database to optimize the user experience.
-}
webDatabaseGetExpectedSize
    :: MonadIO m
    => WebDatabase -- ^ the database
    -> m Integer   -- ^ the expected size of the database in bytes
webDatabaseGetExpectedSize database = liftIO $
    withWebDatabase database $ \ptr ->
        {#call webkit_web_database_get_expected_size#} ptr >>=
            return . toInteger

-- | Returns the absolute filename to the 'WebDatabase' file on disk.
webDatabaseGetFilename
    :: MonadIO m
    => WebDatabase -- ^ the database
    -> m String    -- ^ the absolute filename of the database
webDatabaseGetFilename database = liftIO $
    withWebDatabase database $ \ptr ->
        {#call webkit_web_database_get_filename#} ptr >>= peekCString

-- | Returns the canonical name of the 'WebDatabase'.
webDatabaseGetName
    :: MonadIO m
    => WebDatabase -- ^ the database
    -> m String    -- ^ the name of the database
webDatabaseGetName database = liftIO $
    withWebDatabase database $ \ptr ->
        {#call webkit_web_database_get_name#} ptr >>= peekCString

-- | Returns the 'SecurityOrigin' of the 'WebDatabase'.
webDatabaseGetSecurityOrigin
    :: MonadIO m
    => WebDatabase       -- ^ the database
    -> m SecurityOrigin  -- ^ the security origin of the database
webDatabaseGetSecurityOrigin database = liftIO $
    withWebDatabase database $ \ptr ->
        makeSecurityOrigin $
            {#call webkit_web_database_get_security_origin#} ptr

-- | Returns the actual size of the 'WebDatabase' space on disk in bytes.
webDatabaseGetSize
    :: MonadIO m
    => WebDatabase -- ^ the database
    -> m Integer   -- ^ the actual size of the database in bytes
webDatabaseGetSize database = liftIO $
    withWebDatabase database $ \ptr ->
        {#call webkit_web_database_get_size#} ptr >>=
            return . toInteger

{- | Removes the 'WebDatabase' from its 'SecurityOrigin' and destroys all data
     stored in the database.
-}
webDatabaseRemove
    :: MonadIO m
    => WebDatabase -- ^ the database to remove
    -> m ()
webDatabaseRemove database = liftIO $
    withWebDatabase database $ \ptr ->
       {#call webkit_web_database_remove#} ptr

webDatabaseRemoveAll
    :: MonadIO m
    => m ()
webDatabaseRemoveAll = liftIO $
    {#call remove_all_web_databases#}

webDatabaseGetDefaultQuota
    :: MonadIO m
    => m Int
webDatabaseGetDefaultQuota = liftIO $
    {#call get_default_web_database_quota#} >>=
        return . fromIntegral

webDatabaseSetDefaultQuota
    :: MonadIO m
    => Int
    -> m ()
webDatabaseSetDefaultQuota quota = liftIO $
    {#call set_default_web_database_quota#} (fromIntegral quota)

webDatabaseGetDirectoryPath
    :: MonadIO m
    => m String
webDatabaseGetDirectoryPath = liftIO $
    {#call get_web_database_directory_path#} >>=
        peekCString

webDatabaseSetDirectoryPath
    :: MonadIO m
    => String
    -> m ()
webDatabaseSetDirectoryPath directory = liftIO $
    withCString directory $ \ dir ->
        {#call set_web_database_directory_path#} dir

