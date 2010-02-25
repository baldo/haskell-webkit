{-# LANGUAGE ForeignFunctionInterface #-}

{# context lib="libwebkit" prefix="webkit_" #}

module Graphics.UI.Gtk.WebKit.WebBackForwardList
    ( WebBackForwardList

    ) where

#include <webkit/webkitwebbackforwardlist.h>

import Foreign.C
import GHC.Ptr
import System.Glib.FFI

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


webBackForwardListAddItem :: WebBackForwardList -> WebHistoryItem -> IO ()
webBackForwardListAddItem list item = 
    withWebBackForwardList list $ \lptr ->
        withWebHistoryItem item $ \iptr ->
            {#call web_back_forward_list_add_item#} lptr iptr

webBackForwardListContainsItem :: WebBackForwardList -> WebHistoryItem -> IO Bool
webBackForwardListContainsItem list item =
    withWebBackForwardList list $ \lptr ->
        withWebHistoryItem item $ \iptr ->
            liftM toBool $  {#call web_back_forward_list_contains_item#} lptr iptr 

webBackForwardListGetBackItem :: WebBackForwardList -> IO WebHistoryItem 
webBackForwardListGetBackItem list =
   withWebBackForwardList list $ \ptr ->
        makeNewObject mkWebHistoryItem $ {#call web_back_forward_list_get_back_item#} ptr 

webBackForwardListGetBackLength :: WebBackForwardList -> IO Int
webBackForwardListGetBackLength list = 
    withWebBackForwardList list $ \ptr ->
        liftM fromIntegral $ {#call web_back_forward_list_get_back_length#} ptr


webBackForwardListGetBackListWithLimit :: WebBackForwardList -> Int -> IO [WebHistoryItem]
webBackForwardListGetBackListWithLimit list limit =
    withWebBackForwardList list $ \ptr ->
        {#call web_back_forward_list_get_back_list_with_limit#} ptr (fromIntegral limit)
            >>= fromGList >>= mapM (makeNewObject mkWebHistoryItem . return) 


webBackForwardListGetCurrentItem :: WebBackForwardList -> IO WebHistoryItem 
webBackForwardListGetCurrentItem list =
    withWebBackForwardList list $ \ptr ->
        makeNewObject mkWebHistoryItem $ 
            {#call web_back_forward_list_get_current_item#} ptr

webBackForwardListGetForwardItem :: WebBackForwardList -> IO WebHistoryItem
webBackForwardListGetForwardItem list = 
    withWebBackForwardList list $ \ptr ->
         makeNewObject mkWebHistoryItem $ 
            {#call web_back_forward_list_get_forward_item#} ptr


webBackForwardListGetForwardLength :: WebBackForwardList -> IO Int
webBackForwardListGetForwardLength list = 
    withWebBackForwardList list $ \ptr ->
        liftM fromIntegral $ {#call web_back_forward_list_get_forward_length#} ptr


webBackForwardListGetForwardListWithLimit :: WebBackForwardList -> Int -> IO [WebHistoryItem]
webBackForwardListGetForwardListWithLimit list limit =
    withWebBackForwardList list $ \ptr ->
        {#call web_back_forward_list_get_forward_list_with_limit#} ptr (fromIntegral limit)
            >>= fromGList >>= mapM (makeNewObject mkWebHistoryItem . return) 

webBackForwardListGetLimit :: WebBackForwardList -> IO Int
webBackForwardListGetLimit list = 
    withWebBackForwardList list $ \ptr ->
        liftM fromIntegral $ {#call web_back_forward_list_get_limit#} ptr

webBackForwardListGetNthItem :: WebBackForwardList -> Int -> IO WebHistoryItem
webBackForwardListGetNthItem list index = 
    withWebBackForwardList list $ \ptr ->
         makeNewObject mkWebHistoryItem $ 
            {#call web_back_forward_list_get_nth_item#} ptr (fromIntegral index)


webBackForwardListGoBack :: WebBackForwardList -> IO ()
webBackForwardListGoBack list = 
    withWebBackForwardList list $ \ptr ->
        {#call web_back_forward_list_go_back#} ptr

webBackForwardListGoForward :: WebBackForwardList -> IO ()
webBackForwardListGoForward list = 
    withWebBackForwardList list $ \ptr ->
        {#call web_back_forward_list_go_forward#} ptr


webBackForwardListGoToItem ::  WebBackForwardList -> WebHistoryItem -> IO ()
webBackForwardListGoToItem list item = 
    withWebBackForwardList list $ \lptr ->
        withWebHistoryItem item $ \iptr ->
            {#call web_back_forward_list_go_to_item#} lptr iptr

webBackForwardListNewWithWebView :: WebView -> IO WebBackForwardList
webBackForwardListNewWithWebView view =
    withWebView view $ \ptr ->
       makeNewObject  mkWebBackForwardList $
            {#call web_back_forward_list_new_with_web_view#} ptr

webBackForwardListSetLimit :: WebBackForwardList -> Int -> IO ()
webBackForwardListSetLimit list limit = 
    withWebBackForwardList list $ \ptr ->
            {#call web_back_forward_list_set_limit#} ptr (fromIntegral limit)

