{-# LANGUAGE ForeignFunctionInterface #-}

{# context lib="libwebkit" prefix="webkit_" #}

module Graphics.UI.Gtk.WebKit.WebNavigationAction
    ( WebNavigationAction

    , webNavigationActionGetButton
    , webNavigationActionSetReason
    , webNavigationActionSetOrginalUri
    , webNavigationActionGetType
    , webNavigationActionGetTargetFrame
    , webNavigationActionGetOriginalUri
    , webNavigationActionGetModifierState
    ) where

#include <webkit/webkitwebnavigationaction.h>

import Foreign.C
import GHC.Ptr
import System.Glib.FFI
import System.Glib.GType

import Control.Monad

import Graphics.UI.Gtk
    ( makeNewObject
    )

{#import Graphics.UI.Gtk.WebKit.General.Types#}
    ( WebNavigationAction

    , mkWebNavigationAction
    , unWebNavigationAction

    , withWebNavigationAction
    )

{#import Graphics.UI.Gtk.WebKit.General.Enums#}
    ( WebNavigationReason (..)
    )

webNavigationActionGetButton :: WebNavigationAction -> IO Int
webNavigationActionGetButton action = 
    withWebNavigationAction action $ \ptr -> 
       liftM fromIntegral $ {#call webkit_web_navigation_action_get_button#} ptr 

webNavigationActionGetModifierState :: WebNavigationAction -> IO Int
webNavigationActionGetModifierState action = 
    withWebNavigationAction action $ \ptr -> 
       liftM fromIntegral $ {#call webkit_web_navigation_action_get_modifier_state#} ptr 

webNavigationActionGetOriginalUri :: WebNavigationAction -> IO String
webNavigationActionGetOriginalUri action = 
    withWebNavigationAction action $ \ptr -> 
       {#call webkit_web_navigation_action_get_original_uri#} ptr >>= peekCString

webNavigationActionGetReason :: WebNavigationAction -> IO WebNavigationReason
webNavigationActionGetReason action =
    withWebNavigationAction action $ \ptr -> 
        liftM (toEnum . fromIntegral) $
        {#call web_navigation_action_get_reason#} ptr 

webNavigationActionGetTargetFrame :: WebNavigationAction -> IO String
webNavigationActionGetTargetFrame action =
    withWebNavigationAction action $ \ptr ->
        {#call web_navigation_action_get_target_frame#} ptr >>= peekCString

webNavigationActionGetType :: IO GType 
webNavigationActionGetType =
    {#call web_navigation_action_get_type#}

webNavigationActionSetOrginalUri :: WebNavigationAction -> String -> IO ()
webNavigationActionSetOrginalUri action uri = 
    withWebNavigationAction action $ \ptr -> 
        newCString uri >>= {#call webkit_web_navigation_action_set_original_uri#} ptr

webNavigationActionSetReason :: WebNavigationAction -> WebNavigationReason -> IO ()
webNavigationActionSetReason action reason =
    withWebNavigationAction action $ \ptr -> 
        {#call web_navigation_action_set_reason#} ptr ((fromIntegral . fromEnum)  reason)
