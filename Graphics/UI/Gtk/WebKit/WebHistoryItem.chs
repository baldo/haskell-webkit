{-# LANGUAGE ForeignFunctionInterface #-}

{# context lib="libwebkit" prefix="webkit_" #}

module Graphics.UI.Gtk.WebKit.WebHistoryItem
    ( WebHistoryItem
    ) where

#include <webkit/webkitwebhistoryitem.h>

import Foreign.C
import GHC.Ptr
import System.Glib.FFI

import Control.Monad

import Graphics.UI.Gtk
    ( makeNewObject
    )

{#import Graphics.UI.Gtk.WebKit.General.Types#}
    ( WebHistoryItem

    , mkWebHistoryItem
    , unWebHistoryItem
    )

-- Property get/set functions are aready provided by WebKit :)

-- New in WebKit 1.1.18
--webHistoryItemCopy :: WebHistoryItem -> IO WebHistoryItem 
--webHistoryItemCopy historyItem =
--    withForeignPtr (unHistoryItem historyItem) $ \ptr ->
--        makeNewObject mkHistoryItem $ {#call web_history_item_copy#} 

-- const gchar *       webkit_web_history_item_get_alternate_title (WebKitWebHistoryItem *web_history_item);
webHistoryItemGetAlternateTitle :: WebHistoryItem -> IO String
webHistoryItemGetAlternateTitle  historyItem = 
    withForeignPtr (unWebHistoryItem historyItem) $ \ptr ->
        {#call web_history_item_get_alternate_title#} ptr >>= peekCString

-- gdouble             webkit_web_history_item_get_last_visited_time  (WebKitWebHistoryItem *web_history_item);
webHistoryItemGetLastVisitedTime :: WebHistoryItem -> IO Double
webHistoryItemGetLastVisitedTime historyItem =
    withForeignPtr (unWebHistoryItem historyItem) $ \ptr ->
         liftM realToFrac $ {#call web_history_item_get_last_visited_time#} ptr

-- const gchar *       webkit_web_history_item_get_original_uri  (WebKitWebHistoryItem *web_history_item);

webHistoryItemGetOrginalUri :: WebHistoryItem -> IO String
webHistoryItemGetOrginalUri historyItem =
    withForeignPtr (unWebHistoryItem historyItem) $ \ptr ->
        {#call web_history_item_get_original_uri#} ptr >>= peekCString

-- const gchar *       webkit_web_history_item_get_title   (WebKitWebHistoryItem *web_history_item);

webHistoryItemGetTitle :: WebHistoryItem -> IO String
webHistoryItemGetTitle historyItem =
    withForeignPtr (unWebHistoryItem historyItem) $ \ptr ->
        {#call web_history_item_get_title#} ptr >>= peekCString

-- const gchar *       webkit_web_history_item_get_uri     (WebKitWebHistoryItem *web_history_item);

webHistoryItemGetUri :: WebHistoryItem -> IO String
webHistoryItemGetUri historyItem =
    withForeignPtr (unWebHistoryItem historyItem) $ \ptr ->
        {#call web_history_item_get_uri#} ptr >>= peekCString


-- WebKitWebHistoryItem * webkit_web_history_item_new      (void);

webHistoryItemNew :: IO WebHistoryItem 
webHistoryItemNew = 
    makeNewObject mkWebHistoryItem $ {#call web_history_item_new#}

-- WebKitWebHistoryItem * webkit_web_history_item_new_with_data (const gchar *uri, const gchar *title);

webHistoryItemNewWithData :: String -> String -> IO WebHistoryItem
webHistoryItemNewWithData uri title =
    withCString uri $ \uptr ->
        withCString title $ \tptr ->
            makeNewObject mkWebHistoryItem 
                $ {#call web_history_item_new_with_data#} uptr tptr

-- void                webkit_web_history_item_set_alternate_title (WebKitWebHistoryItem *web_history_item, const gchar *title);

webHistoryItemSetAlternateTitle :: WebHistoryItem -> String -> IO ()
webHistoryItemSetAlternateTitle historyItem title =
    withCString title $ \cTitle ->
        withForeignPtr (unWebHistoryItem historyItem) $ \ptr ->
            {#call web_history_item_set_alternate_title#} ptr cTitle
    
