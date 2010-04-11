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

import System.Glib.FFI
import System.Glib.GType
    ( GType
    )
import System.Glib.GList
    ( fromGList
    )

import Control.Monad.Trans
    ( MonadIO
    , liftIO
    )

{#import Graphics.UI.Gtk.WebKit.General.Types#}
    ( WebBackForwardList
    , WebHistoryItem
    , WebView

    , makeWebBackForwardList
    , withWebBackForwardList
    , makeWebHistoryItem
    , withWebHistoryItem
    , withWebView
    )

webBackForwardListGetType
    :: MonadIO m
    => m GType
webBackForwardListGetType = liftIO $
    {#call web_back_forward_list_get_type#}

{- | Adds the item to the 'WebBackForwardList'.

     The 'WebBackForwardList' will add a reference to the 'WebHistoryItem', so
     you don't need to keep a reference once you've added it to the list.
-}
webBackForwardListAddItem
    :: MonadIO m
    => WebBackForwardList -- ^ the 'WebBackForwardList'
    -> WebHistoryItem     -- ^ the 'WebHistoryItem' to add
    -> m ()
webBackForwardListAddItem list item = liftIO $
    withWebBackForwardList list $ \lptr ->
        withWebHistoryItem item $ \iptr ->
            {#call web_back_forward_list_add_item#} lptr iptr

-- | Checks if 'WebHistoryItem' is in the 'WebBackForwardList'.
webBackForwardListContainsItem
    :: MonadIO m
    => WebBackForwardList -- ^ a 'WebBackForwardList'
    -> WebHistoryItem     -- ^ a 'WebHistoryItem'
    -> m Bool             -- ^ 'True' if 'WebHistoryItem' is in
                          --   'WebBackForwardList', otherwise 'False'
webBackForwardListContainsItem list item = liftIO $
    withWebBackForwardList list $ \lptr ->
        withWebHistoryItem item $ \iptr ->
            {#call web_back_forward_list_contains_item#} lptr iptr >>=
                return . toBool

-- | Returns the item that precedes the current item.
webBackForwardListGetBackItem
    :: MonadIO m
    => WebBackForwardList -- ^ a 'WebBackForwardList'
    -> m WebHistoryItem   -- ^ the 'WebHistoryItem' preceding the current item
webBackForwardListGetBackItem list = liftIO $
   withWebBackForwardList list $ \ptr ->
        makeWebHistoryItem $ {#call web_back_forward_list_get_back_item#} ptr

-- | Returns the number of items that preced the current item.
webBackForwardListGetBackLength
    :: MonadIO m
    => WebBackForwardList -- ^ 'WebBackForwardList'
    -> m Int              -- ^ the number of items preceding the current item
webBackForwardListGetBackLength list = liftIO $
    withWebBackForwardList list $ \ptr ->
        {#call web_back_forward_list_get_back_length#} ptr >>=
            return . fromIntegral

{- | Returns a list of items that precede the current item, limited by a given
     limit.
-}
webBackForwardListGetBackListWithLimit
    :: MonadIO m
    => WebBackForwardList -- ^ the list
    -> Int                -- ^ the limit
    -> m [WebHistoryItem] -- ^ the preceding items
webBackForwardListGetBackListWithLimit list limit = liftIO $
    withWebBackForwardList list $ \ptr ->
        {#call web_back_forward_list_get_back_list_with_limit#} ptr (fromIntegral limit)
            >>= fromGList >>= mapM (makeWebHistoryItem . return)

-- | Returns the current item.
webBackForwardListGetCurrentItem
    :: MonadIO m
    => WebBackForwardList       -- ^ the list
    -> m (Maybe WebHistoryItem) -- ^ 'Nothing' if the list is empty, 'Just'
                                --   the item otherwise
webBackForwardListGetCurrentItem list = liftIO $
    withWebBackForwardList list $ \ptr -> do
        item <- {#call web_back_forward_list_get_current_item#} ptr
        maybePeek (makeWebHistoryItem . return) item

-- | Returns the item that succeeds the current item.
webBackForwardListGetForwardItem
    :: MonadIO m
    => WebBackForwardList       -- ^ the list
    -> m (Maybe WebHistoryItem) -- ^ 'Nothing' if there is no next item,
                                --   'Just' the item otherwise
webBackForwardListGetForwardItem list = liftIO $
    withWebBackForwardList list $ \ptr -> do
        item <- {#call web_back_forward_list_get_forward_item#} ptr
        maybePeek (makeWebHistoryItem . return) item

-- | Returns the number of items that succeed the current item.
webBackForwardListGetForwardLength
    :: MonadIO m
    => WebBackForwardList -- ^ the list
    -> m Int              -- ^ number of items that succeed the current item
webBackForwardListGetForwardLength list = liftIO $
    withWebBackForwardList list $ \ptr ->
        {#call web_back_forward_list_get_forward_length#} ptr >>=
            return . fromIntegral

{- | Returns a list of items that succeed the current item, limited by a given
     limit.
-}
webBackForwardListGetForwardListWithLimit
    :: MonadIO m
    => WebBackForwardList -- ^ the list
    -> Int                -- ^ the limit
    -> m [WebHistoryItem] -- ^ succeeding items
webBackForwardListGetForwardListWithLimit list limit = liftIO $
    withWebBackForwardList list $ \ptr ->
        {#call web_back_forward_list_get_forward_list_with_limit#} ptr
            (fromIntegral limit)
                >>= fromGList
                    >>= mapM (makeWebHistoryItem . return)

-- | Returns the maximum limit of the 'WebBackForwardList'.
webBackForwardListGetLimit
    :: MonadIO m
    => WebBackForwardList -- ^ a 'WebBackForwardList'
    -> m Int              -- ^ the number of 'WebHistoryItem's the
                          --   'WebBackForwardList' can hold
webBackForwardListGetLimit list = liftIO $
    withWebBackForwardList list $ \ptr ->
        {#call web_back_forward_list_get_limit#} ptr >>=
            return . fromIntegral

-- | Returns the item at a given index relative to the current item.
webBackForwardListGetNthItem
    :: MonadIO m
    => WebBackForwardList       -- ^ the list
    -> Int                      -- ^ index of the item
    -> m (Maybe WebHistoryItem) -- ^ 'Just' the corresponding item if existing,
                                --   'Nothing' otherwise
webBackForwardListGetNthItem list index = liftIO $
    withWebBackForwardList list $ \ptr -> do
        item <- {#call web_back_forward_list_get_nth_item#} ptr
                    (fromIntegral index)
        maybePeek (makeWebHistoryItem . return) item

-- | Steps backward in the 'WebBackForwardList'.
webBackForwardListGoBack
    :: MonadIO m
    => WebBackForwardList -- ^ the 'WebBackForwardList'
    -> m ()
webBackForwardListGoBack list = liftIO $
    withWebBackForwardList list $ \ptr ->
        {#call web_back_forward_list_go_back#} ptr

-- | Steps forward in the 'WebBackForwardList'.
webBackForwardListGoForward
    :: MonadIO m
    => WebBackForwardList -- ^ the 'WebBackForwardList'
    -> m ()
webBackForwardListGoForward list = liftIO $
    withWebBackForwardList list $ \ptr ->
        {#call web_back_forward_list_go_forward#} ptr

-- | Go to the specified 'WebHistoryItem' in the 'WebBackForwardList'.
webBackForwardListGoToItem
    :: MonadIO m
    => WebBackForwardList -- ^ the 'WebBackForwardList'
    -> WebHistoryItem     -- ^ the 'WebHistoryItem' to go to
    -> m ()
webBackForwardListGoToItem list item = liftIO $
    withWebBackForwardList list $ \lptr ->
        withWebHistoryItem item $ \iptr ->
            {#call web_back_forward_list_go_to_item#} lptr iptr

{- | Creates an instance of the 'WebBackForwardList' with a controlling
     'WebView'.
-}
webBackForwardListNewWithWebView
    :: MonadIO m
    => WebView              -- ^ a 'WebView'
    -> m WebBackForwardList -- ^ the list
webBackForwardListNewWithWebView view = liftIO $
    withWebView view $ \ptr ->
        makeWebBackForwardList $
            {#call web_back_forward_list_new_with_web_view#} ptr

{- | Sets the maximum limit of the 'WebBackForwardList'. If the
     'WebBackForwardList' exceeds its capacity, items will be removed everytime
     a new item has been added.
-}
webBackForwardListSetLimit
    :: MonadIO m
    => WebBackForwardList -- ^ a 'WebBackForwardList'
    -> Int                -- ^ the new limit
    -> m ()
webBackForwardListSetLimit list limit = liftIO $
    withWebBackForwardList list $ \ptr ->
            {#call web_back_forward_list_set_limit#} ptr (fromIntegral limit)

