{-# LANGUAGE ForeignFunctionInterface #-}

{-| One item of the 'WebBackForwardList' and or global history.

A 'WebHistoryItem' consists out of a title and an uri. It can be part of the
'WebBackForwardList' and the global history. The global history is used for
coloring the links of visited sites. A 'WebHistoryItem' constructed by
'webHistoryItemNew' and 'webHistoryItemNewWithData' is automatically added to
the global history.
-}

{# context lib="libwebkit" prefix="webkit_" #}

module Graphics.UI.Gtk.WebKit.WebHistoryItem
    ( WebHistoryItem

    , webHistoryItemGetType
    , webHistoryItemCopy
    , webHistoryItemGetAlternateTitle
    , webHistoryItemSetAlternateTitle
    , webHistoryItemNewWithData
    , webHistoryItemNew
    , webHistoryItemGetUri
    , webHistoryItemGetTitle
    , webHistoryItemGetOrginalUri
    , webHistoryItemGetLastVisitedTime
    ) where

#include <webkit/webkitwebhistoryitem.h>

import System.Glib.FFI
import System.Glib.GType
    ( GType
    )

import Control.Monad.Trans
    ( MonadIO
    , liftIO
    )

{#import Graphics.UI.Gtk.WebKit.General.Types#}
    ( WebHistoryItem

    , makeWebHistoryItem
    , withWebHistoryItem
    )

webHistoryItemGetType
    :: MonadIO m
    => m GType
webHistoryItemGetType = liftIO $
    {#call web_history_item_get_type#}

-- Property get/set functions are aready provided by WebKit :)

-- | Makes a copy of the item for use with other 'WebView's.
webHistoryItemCopy
    :: MonadIO m
    => WebHistoryItem    -- ^ an item
    -> m WebHistoryItem  -- ^ the new item
webHistoryItemCopy historyItem = liftIO $
    withWebHistoryItem historyItem $ \ptr ->
        makeWebHistoryItem $ {#call web_history_item_copy#} ptr

-- | Returns the alternate title of the 'WebHistoryItem'.
webHistoryItemGetAlternateTitle
    :: MonadIO m
    => WebHistoryItem -- ^ an item
    -> m String       -- ^ its alternate title
webHistoryItemGetAlternateTitle  historyItem = liftIO $
    withWebHistoryItem historyItem $ \ptr ->
        {#call web_history_item_get_alternate_title#} ptr >>= peekCString

webHistoryItemGetLastVisitedTime
    :: MonadIO m
    => WebHistoryItem
    -> m Double
webHistoryItemGetLastVisitedTime historyItem = liftIO $
    withWebHistoryItem historyItem $ \ptr ->
        {#call web_history_item_get_last_visited_time#} ptr >>=
            return . realToFrac

-- | Returns the original URI of the 'WebHistoryItem'.
webHistoryItemGetOrginalUri
    :: MonadIO m
    => WebHistoryItem -- ^ an item
    -> m String       -- ^ its original URI
webHistoryItemGetOrginalUri historyItem = liftIO $
    withWebHistoryItem historyItem $ \ptr ->
        {#call web_history_item_get_original_uri#} ptr >>= peekCString

-- | Returns the title of the 'WebHistoryItem'.
webHistoryItemGetTitle
    :: MonadIO m
    => WebHistoryItem -- ^ an item
    -> m String       -- ^ its title
webHistoryItemGetTitle historyItem = liftIO $
    withWebHistoryItem historyItem $ \ptr ->
        {#call web_history_item_get_title#} ptr >>= peekCString

-- | Returns the URI of the 'WebHistoryItem'.
webHistoryItemGetUri
    :: MonadIO m
    => WebHistoryItem -- ^ an item
    -> m String       -- ^ its URI
webHistoryItemGetUri historyItem = liftIO $
    withWebHistoryItem historyItem $ \ptr ->
        {#call web_history_item_get_uri#} ptr >>= peekCString

-- | Creates a new 'WebHistoryItem'.
webHistoryItemNew
    :: MonadIO m
    => m WebHistoryItem -- ^ the new item
webHistoryItemNew = liftIO $
    makeWebHistoryItem $ {#call web_history_item_new#}

-- | Creates a new 'WebHistoryItem' with the given URI and title.
webHistoryItemNewWithData
    :: MonadIO m
    => String           -- ^ the URI of the page
    -> String           -- ^ the title of the page
    -> m WebHistoryItem -- ^ the new item
webHistoryItemNewWithData uri title = liftIO $
    withCString uri $ \uptr ->
        withCString title $ \tptr ->
            makeWebHistoryItem
            $ {#call web_history_item_new_with_data#} uptr tptr

-- | Sets an alternate title for the 'WebHistoryItem'.
webHistoryItemSetAlternateTitle
    :: MonadIO m
    => WebHistoryItem -- ^ an item
    -> String         -- ^ the alternate title
    -> m ()
webHistoryItemSetAlternateTitle historyItem title = liftIO $
    withCString title $ \cTitle ->
        withWebHistoryItem historyItem $ \ptr ->
            {#call web_history_item_set_alternate_title#} ptr cTitle

