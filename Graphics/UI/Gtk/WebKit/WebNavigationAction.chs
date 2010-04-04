{-# LANGUAGE ForeignFunctionInterface #-}

{# context lib="libwebkit" prefix="webkit_" #}

{-| Used to report details of navigation actions.

'WebNavigationAction' is used in signals to provide details about what led
the navigation to happen. This includes, for instance, if the user clicked a
link to start that navigation, and what mouse button was used.
-}

module Graphics.UI.Gtk.WebKit.WebNavigationAction
    ( WebNavigationAction

    , webNavigationActionGetButton
    , webNavigationActionSetReason
    , webNavigationActionSetOrginalUri
    , webNavigationActionGetType
    , webNavigationActionGetTargetFrame
    , webNavigationActionGetOriginalUri
    , webNavigationActionGetReason 
    , webNavigationActionGetModifierState
    ) where

#include <webkit/webkitwebnavigationaction.h>

import Foreign.C
import System.Glib.FFI
import System.Glib.GType

import Control.Monad

{#import Graphics.UI.Gtk.WebKit.General.Types#}
    ( WebNavigationAction

    , withWebNavigationAction
    )

{#import Graphics.UI.Gtk.WebKit.General.Enums#}
    ( DOMButton (..)
    , WebNavigationReason (..)
    )

{- | Returns the DOM identifier for the mouse button used to click. If the
     action was not initiated by a mouse click, returns 'ButtonNone'.
-}
webNavigationActionGetButton
    :: WebNavigationAction -- ^ a navigation action
    -> IO DOMButton        -- ^ mouse button that used to click
webNavigationActionGetButton action = 
    withWebNavigationAction action $ \ptr -> 
        liftM (toEnum . fromIntegral) $
            {#call webkit_web_navigation_action_get_button#} ptr 

-- | Returns a bitmask with the the state of the modifier keys.
webNavigationActionGetModifierState
    :: WebNavigationAction -- ^ a navigation action
    -> IO Int              -- ^ a bitmask with the state of the modifier keys
webNavigationActionGetModifierState action = 
    withWebNavigationAction action $ \ptr -> 
       liftM fromIntegral $ {#call webkit_web_navigation_action_get_modifier_state#} ptr 

{- | Returns the URI that was originally requested. This may differ from the
     navigation target, for instance because of a redirect.
-}
webNavigationActionGetOriginalUri
    :: WebNavigationAction -- ^ a navigation action
    -> IO String           -- ^ the URI
webNavigationActionGetOriginalUri action = 
    withWebNavigationAction action $ \ptr -> 
        {#call webkit_web_navigation_action_get_original_uri#} ptr >>=
            peekCString

-- | Returns the reason why WebKit is requesting a navigation.
webNavigationActionGetReason
    :: WebNavigationAction    -- ^ a navigation action
    -> IO WebNavigationReason -- ^ the reason for the navigation
webNavigationActionGetReason action =
    withWebNavigationAction action $ \ptr -> 
        liftM (toEnum . fromIntegral) $
        {#call web_navigation_action_get_reason#} ptr 

-- | Returns the target frame of the action.
webNavigationActionGetTargetFrame
    :: WebNavigationAction -- ^ a navigation action
    -> IO (Maybe String)   -- ^ 'Just' the target frame or 'Nothing' if there
                           --   is no target.
webNavigationActionGetTargetFrame action =
    withWebNavigationAction action $ \ptr ->
        {#call web_navigation_action_get_target_frame#} ptr >>=
            maybePeek peekCString

webNavigationActionGetType :: IO GType 
webNavigationActionGetType =
    {#call web_navigation_action_get_type#}

{- | Sets the URI that was originally requested. This may differ from the
     navigation target, for instance because of a redirect.
-}
webNavigationActionSetOrginalUri
    :: WebNavigationAction -- ^ a navigation action
    -> String              -- ^ a URI
    -> IO ()
webNavigationActionSetOrginalUri action uri = 
    withWebNavigationAction action $ \ptr -> 
        newCString uri >>=
            {#call webkit_web_navigation_action_set_original_uri#} ptr

-- | Sets the reason why WebKit is requesting a navigation.
webNavigationActionSetReason
    :: WebNavigationAction -- ^ a navigation action
    -> WebNavigationReason -- ^ the reason for the navigation
    -> IO ()
webNavigationActionSetReason action reason =
    withWebNavigationAction action $ \ptr -> 
        {#call web_navigation_action_set_reason#} ptr $
            (fromIntegral . fromEnum)  reason

