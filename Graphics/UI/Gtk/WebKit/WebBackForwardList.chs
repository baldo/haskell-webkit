{-# LANGUAGE ForeignFunctionInterface #-}

{# context lib="libwebkit" prefix="webkit_" #}

{-| The history of a 'WebView'.
-}

module Graphics.UI.Gtk.WebKit.WebBackForwardList
    ( WebBackForwardList

    , webBackForwardListGetType

    , webBackForwardListAddItem
    , webBackForwardListContainsItem

    , webBackForwardListGetBackItem
    , webBackForwardListGetBackLength
    , webBackForwardListGetBackListWithLimit

    , webBackForwardListGetCurrentItem

    , webBackForwardListGetForwardItem
    , webBackForwardListGetForwardLength
    , webBackForwardListGetForwardListWithLimit

    , webBackForwardListGetLimit
    , webBackForwardListSetLimit

    , webBackForwardListGetNthItem

    , webBackForwardListGoBack
    , webBackForwardListGoForward
    , webBackForwardListGoToItem

    , webBackForwardListNewWithWebView
    ) where

#include <webkit/webkitwebbackforwardlist.h>

import Foreign.C
import GHC.Ptr
import System.Glib.FFI
import System.Glib.GType

import System.Glib.GList

import Control.Monad

import Graphics.UI.Gtk.Abstract.Object
    ( makeNewObject
    )

{#import Graphics.UI.Gtk.WebKit.General.Types#}
    ( WebBackForwardList
    , WebHistoryItem
    , WebView

    , mkWebBackForwardList
    , withWebBackForwardList
    , mkWebHistoryItem
    , withWebHistoryItem
    , withWebView
    )

webBackForwardListGetType
    :: IO GType
webBackForwardListGetType =
    {#call web_back_forward_list_get_type#}

{- | Adds the item to the 'WebBackForwardList'.

     The 'WebBackForwardList' will add a reference to the 'WebHistoryItem', so
     you don't need to keep a reference once you've added it to the list.
-}
webBackForwardListAddItem
    :: WebBackForwardList -- ^ the 'WebBackForwardList'
    -> WebHistoryItem     -- ^ the 'WebHistoryItem' to add
    -> IO ()
webBackForwardListAddItem list item = 
    withWebBackForwardList list $ \lptr ->
        withWebHistoryItem item $ \iptr ->
            {#call web_back_forward_list_add_item#} lptr iptr

-- | Checks if 'WebHistoryItem' is in the 'WebBackForwardList'.
webBackForwardListContainsItem
    :: WebBackForwardList -- ^ a 'WebBackForwardList'
    -> WebHistoryItem     -- ^ a 'WebHistoryItem'
    -> IO Bool            -- ^ 'True' if 'WebHistoryItem' is in
                          --   'WebBackForwardList', otherwise 'False'
webBackForwardListContainsItem list item =
    withWebBackForwardList list $ \lptr ->
        withWebHistoryItem item $ \iptr ->
            liftM toBool $  {#call web_back_forward_list_contains_item#} lptr iptr 

-- | Returns the item that precedes the current item.
webBackForwardListGetBackItem
    :: WebBackForwardList -- ^ a 'WebBackForwardList'
    -> IO WebHistoryItem  -- ^ the 'WebHistoryItem' preceding the current item
webBackForwardListGetBackItem list =
   withWebBackForwardList list $ \ptr ->
        makeNewObject mkWebHistoryItem $ {#call web_back_forward_list_get_back_item#} ptr 

-- | Returns the number of items that preced the current item.
webBackForwardListGetBackLength
    :: WebBackForwardList -- ^ 'WebBackForwardList'
    -> IO Int             -- ^ the number of items preceding the current item 
webBackForwardListGetBackLength list = 
    withWebBackForwardList list $ \ptr ->
        liftM fromIntegral $ {#call web_back_forward_list_get_back_length#} ptr

{- | Returns a list of items that precede the current item, limited by a given
     limit.
-}
webBackForwardListGetBackListWithLimit
    :: WebBackForwardList  -- ^ the list
    -> Int                 -- ^ the limit
    -> IO [WebHistoryItem] -- ^ the preceding items
webBackForwardListGetBackListWithLimit list limit =
    withWebBackForwardList list $ \ptr ->
        {#call web_back_forward_list_get_back_list_with_limit#} ptr (fromIntegral limit)
            >>= fromGList >>= mapM (makeNewObject mkWebHistoryItem . return) 

-- | Returns the current item.
webBackForwardListGetCurrentItem
    :: WebBackForwardList        -- ^ the list
    -> IO (Maybe WebHistoryItem) -- ^ 'Nothing' if the list is empty, 'Just'
                                 --   the item otherwise
webBackForwardListGetCurrentItem list =
    withWebBackForwardList list $ \ptr -> do
        item <- {#call web_back_forward_list_get_current_item#} ptr
        maybePeek ((makeNewObject mkWebHistoryItem) . return) item

-- | Returns the item that succeeds the current item.
webBackForwardListGetForwardItem
    :: WebBackForwardList         -- ^ the list
    -> IO (Maybe WebHistoryItem)  -- ^ 'Nothing' if there is no next item,
                                  --   'Just' the item otherwise
webBackForwardListGetForwardItem list = 
    withWebBackForwardList list $ \ptr -> do
        item <- {#call web_back_forward_list_get_forward_item#} ptr
        maybePeek ((makeNewObject mkWebHistoryItem) . return) item

-- | Returns the number of items that succeed the current item.
webBackForwardListGetForwardLength
    :: WebBackForwardList -- ^ the list
    -> IO Int             -- ^ number of items that succeed the current item
webBackForwardListGetForwardLength list = 
    withWebBackForwardList list $ \ptr ->
        liftM fromIntegral $
            {#call web_back_forward_list_get_forward_length#} ptr

{- | Returns a list of items that succeed the current item, limited by a given
     limit.
-}
webBackForwardListGetForwardListWithLimit
    :: WebBackForwardList  -- ^ the list
    -> Int                 -- ^ the limit
    -> IO [WebHistoryItem] -- ^ succeeding items
webBackForwardListGetForwardListWithLimit list limit =
    withWebBackForwardList list $ \ptr ->
        {#call web_back_forward_list_get_forward_list_with_limit#} ptr
            (fromIntegral limit)
                >>= fromGList
                    >>= mapM (makeNewObject mkWebHistoryItem . return) 

-- | Returns the maximum limit of the 'WebBackForwardList'.
webBackForwardListGetLimit
    :: WebBackForwardList -- ^ a 'WebBackForwardList'
    -> IO Int             -- ^ the number of 'WebHistoryItem's the
                          --   'WebBackForwardList' can hold
webBackForwardListGetLimit list = 
    withWebBackForwardList list $ \ptr ->
        liftM fromIntegral $ {#call web_back_forward_list_get_limit#} ptr

-- | Returns the item at a given index relative to the current item.
webBackForwardListGetNthItem
    :: WebBackForwardList        -- ^ the list
    -> Int                       -- ^ index of the item
    -> IO (Maybe WebHistoryItem) -- ^ 'Just' the corresponding item if existing,
                                 --   'Nothing' otherwise
webBackForwardListGetNthItem list index = 
    withWebBackForwardList list $ \ptr -> do
        item <- {#call web_back_forward_list_get_nth_item#} ptr
                    (fromIntegral index)
        maybePeek ((makeNewObject mkWebHistoryItem) . return) item

-- | Steps backward in the 'WebBackForwardList'.
webBackForwardListGoBack
    :: WebBackForwardList -- ^ the 'WebBackForwardList'
    -> IO ()
webBackForwardListGoBack list = 
    withWebBackForwardList list $ \ptr ->
        {#call web_back_forward_list_go_back#} ptr

-- | Steps forward in the 'WebBackForwardList'.
webBackForwardListGoForward
    :: WebBackForwardList -- ^ the 'WebBackForwardList'
    -> IO ()
webBackForwardListGoForward list = 
    withWebBackForwardList list $ \ptr ->
        {#call web_back_forward_list_go_forward#} ptr

-- | Go to the specified 'WebHistoryItem' in the 'WebBackForwardList'.
webBackForwardListGoToItem
    :: WebBackForwardList -- ^ the 'WebBackForwardList'
    -> WebHistoryItem     -- ^ the 'WebHistoryItem' to go to 
    -> IO ()
webBackForwardListGoToItem list item = 
    withWebBackForwardList list $ \lptr ->
        withWebHistoryItem item $ \iptr ->
            {#call web_back_forward_list_go_to_item#} lptr iptr

{- | Creates an instance of the 'WebBackForwardList' with a controlling
     'WebView'.
-}
webBackForwardListNewWithWebView
    :: WebView               -- ^ a 'WebView'
    -> IO WebBackForwardList -- ^ the list
webBackForwardListNewWithWebView view =
    withWebView view $ \ptr ->
       makeNewObject  mkWebBackForwardList $
            {#call web_back_forward_list_new_with_web_view#} ptr

{- | Sets the maximum limit of the 'WebBackForwardList'. If the
     'WebBackForwardList' exceeds its capacity, items will be removed everytime
     a new item has been added.
-}
webBackForwardListSetLimit
    :: WebBackForwardList -- ^ a 'WebBackForwardList'
    -> Int                -- ^ the new limit
    -> IO ()
webBackForwardListSetLimit list limit = 
    withWebBackForwardList list $ \ptr ->
            {#call web_back_forward_list_set_limit#} ptr (fromIntegral limit)

