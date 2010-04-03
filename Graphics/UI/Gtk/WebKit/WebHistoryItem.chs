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
    , webHistoryItemSetAlternateTitle
    , webHistoryItemNewWithData
    , webHistoryItemNew
    , webHistoryItemGetUri
    , webHistoryItemGetTitle
    , webHistoryItemGetOrginalUri
    , webHistoryItemGetLastVisitedTime
    ) where

#include <webkit/webkitwebhistoryitem.h>

import Foreign.C
import System.Glib.FFI
import System.Glib.GType

import Control.Monad

{#import Graphics.UI.Gtk.WebKit.General.Types#}
    ( WebHistoryItem

    , makeWebHistoryItem
    , withWebHistoryItem
    )

webHistoryItemGetType :: IO GType
webHistoryItemGetType =
    {#call web_history_item_get_type#}

-- Property get/set functions are aready provided by WebKit :)

-- | Makes a copy of the item for use with other 'WebView's.
webHistoryItemCopy
    :: WebHistoryItem    -- ^ an item
    -> IO WebHistoryItem -- ^ the new item
webHistoryItemCopy historyItem =
    withWebHistoryItem historyItem $ \ptr ->
        makeWebHistoryItem $ {#call web_history_item_copy#} ptr

-- | Returns the alternate title of the 'WebHistoryItem'.
webHistoryItemGetAlternateTitle
    :: WebHistoryItem -- ^ an item
    -> IO String      -- ^ its alternate title
webHistoryItemGetAlternateTitle  historyItem = 
    withWebHistoryItem historyItem $ \ptr ->
        {#call web_history_item_get_alternate_title#} ptr >>= peekCString

webHistoryItemGetLastVisitedTime
    :: WebHistoryItem
    -> IO Double
webHistoryItemGetLastVisitedTime historyItem =
    withWebHistoryItem historyItem $ \ptr ->
         liftM realToFrac $ {#call web_history_item_get_last_visited_time#} ptr

-- | Returns the original URI of the 'WebHistoryItem'.
webHistoryItemGetOrginalUri
    :: WebHistoryItem -- ^ an item
    -> IO String      -- ^ its original URI
webHistoryItemGetOrginalUri historyItem =
    withWebHistoryItem historyItem $ \ptr ->
        {#call web_history_item_get_original_uri#} ptr >>= peekCString

-- | Returns the title of the 'WebHistoryItem'.
webHistoryItemGetTitle
    :: WebHistoryItem -- ^ an item
    -> IO String      -- ^ its title
webHistoryItemGetTitle historyItem =
    withWebHistoryItem historyItem $ \ptr ->
        {#call web_history_item_get_title#} ptr >>= peekCString

-- | Returns the URI of the 'WebHistoryItem'.
webHistoryItemGetUri
    :: WebHistoryItem -- ^ an item
    -> IO String      -- ^ its URI
webHistoryItemGetUri historyItem =
    withWebHistoryItem historyItem $ \ptr ->
        {#call web_history_item_get_uri#} ptr >>= peekCString

-- | Creates a new 'WebHistoryItem'.
webHistoryItemNew
    :: IO WebHistoryItem -- ^ the new item
webHistoryItemNew = 
    makeWebHistoryItem $ {#call web_history_item_new#}

-- | Creates a new 'WebHistoryItem' with the given URI and title.
webHistoryItemNewWithData
    :: String            -- ^ the URI of the page
    -> String            -- ^ the title of the page
    -> IO WebHistoryItem -- ^ the new item
webHistoryItemNewWithData uri title =
    withCString uri $ \uptr ->
        withCString title $ \tptr ->
            makeWebHistoryItem 
            $ {#call web_history_item_new_with_data#} uptr tptr

-- | Sets an alternate title for the 'WebHistoryItem'.
webHistoryItemSetAlternateTitle
    :: WebHistoryItem -- ^ an item
    -> String         -- ^ the alternate title
    -> IO ()
webHistoryItemSetAlternateTitle historyItem title =
    withCString title $ \cTitle ->
        withWebHistoryItem historyItem $ \ptr ->
            {#call web_history_item_set_alternate_title#} ptr cTitle
    
