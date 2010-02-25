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
    ) where

#include <webkit/webkitwebhistoryitem.h>

import Foreign.C
import GHC.Ptr
import System.Glib.FFI

import Control.Monad

import Graphics.UI.Gtk.Abstract.Object
    ( makeNewObject
    )

{#import Graphics.UI.Gtk.WebKit.General.Types#}
    ( WebHistoryItem

    , mkWebHistoryItem
    , unWebHistoryItem

    , withWebHistoryItem
    )

-- Property get/set functions are aready provided by WebKit :)

-- New in WebKit 1.1.18
--webHistoryItemCopy :: WebHistoryItem -> IO WebHistoryItem 
--webHistoryItemCopy historyItem =
--    withHistoryItem historyItem $ \ptr ->
--        makeNewObject mkHistoryItem $ {#call web_history_item_copy#} 


webHistoryItemGetAlternateTitle :: WebHistoryItem -> IO String
webHistoryItemGetAlternateTitle  historyItem = 
    withWebHistoryItem historyItem $ \ptr ->
        {#call web_history_item_get_alternate_title#} ptr >>= peekCString

webHistoryItemGetLastVisitedTime :: WebHistoryItem -> IO Double
webHistoryItemGetLastVisitedTime historyItem =
    withWebHistoryItem historyItem $ \ptr ->
         liftM realToFrac $ {#call web_history_item_get_last_visited_time#} ptr

webHistoryItemGetOrginalUri :: WebHistoryItem -> IO String
webHistoryItemGetOrginalUri historyItem =
    withWebHistoryItem historyItem $ \ptr ->
        {#call web_history_item_get_original_uri#} ptr >>= peekCString

webHistoryItemGetTitle :: WebHistoryItem -> IO String
webHistoryItemGetTitle historyItem =
    withWebHistoryItem historyItem $ \ptr ->
        {#call web_history_item_get_title#} ptr >>= peekCString

webHistoryItemGetUri :: WebHistoryItem -> IO String
webHistoryItemGetUri historyItem =
    withWebHistoryItem historyItem $ \ptr ->
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
        withWebHistoryItem historyItem $ \ptr ->
            {#call web_history_item_set_alternate_title#} ptr cTitle
    
