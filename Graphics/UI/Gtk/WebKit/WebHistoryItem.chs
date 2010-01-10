{-# LANGUAGE ForeignFunctionInterface #-}

{# context lib="libwebkit" prefix="webkit_" #}

module Graphics.UI.Gtk.WebKit.WebHistoryItem
    ( WebHistoryItem

    --, webHistoryItemCopy
    , webHistoryItemSetAlternateTitle
    , webHistoryItemNewWithData
    , webHistoryItemNew
    , webHistoryItemGetUri
    , webHistoryItemGetTitle
    , webHistoryItemGetOrginalUri
    , webHistoryItemGetLastVisitedTime
    , webHistoryItemNew
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


-- New in WebKit 1.1.18
--webHistoryItemCopy :: WebHistoryItem -> IO WebHistoryItem 
--webHistoryItemCopy historyItem =
--    withForeignPtr (unHistoryItem historyItem) $ \ptr ->
--        makeNewObject mkHistoryItem $ {#call web_history_item_copy#} 


webHistoryItemGetAlternateTitle :: WebHistoryItem -> IO String
webHistoryItemGetAlternateTitle  historyItem = 
    withForeignPtr (unWebHistoryItem historyItem) $ \ptr ->
        {#call web_history_item_get_alternate_title#} ptr >>= peekCString

webHistoryItemGetLastVisitedTime :: WebHistoryItem -> IO Double
webHistoryItemGetLastVisitedTime historyItem =
    withForeignPtr (unWebHistoryItem historyItem) $ \ptr ->
         liftM realToFrac $ {#call web_history_item_get_last_visited_time#} ptr

webHistoryItemGetOrginalUri :: WebHistoryItem -> IO String
webHistoryItemGetOrginalUri historyItem =
    withForeignPtr (unWebHistoryItem historyItem) $ \ptr ->
        {#call web_history_item_get_original_uri#} ptr >>= peekCString

webHistoryItemGetTitle :: WebHistoryItem -> IO String
webHistoryItemGetTitle historyItem =
    withForeignPtr (unWebHistoryItem historyItem) $ \ptr ->
        {#call web_history_item_get_title#} ptr >>= peekCString

webHistoryItemGetUri :: WebHistoryItem -> IO String
webHistoryItemGetUri historyItem =
    withForeignPtr (unWebHistoryItem historyItem) $ \ptr ->
        {#call web_history_item_get_uri#} ptr >>= peekCString

webHistoryItemNew :: IO WebHistoryItem 
webHistoryItemNew = 
    makeNewObject mkWebHistoryItem $ {#call web_history_item_new#}

webHistoryItemNewWithData :: String -> String -> IO WebHistoryItem
webHistoryItemNewWithData uri title =
    withCString uri $ \uptr ->
        withCString title $ \tptr ->
            makeNewObject mkWebHistoryItem 
                $ {#call web_history_item_new_with_data#} uptr tptr

webHistoryItemSetAlternateTitle :: WebHistoryItem -> String -> IO ()
webHistoryItemSetAlternateTitle historyItem title =
    withCString title $ \cTitle ->
        withForeignPtr (unWebHistoryItem historyItem) $ \ptr ->
            {#call web_history_item_set_alternate_title#} ptr cTitle
    
