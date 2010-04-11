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

import System.Glib.FFI
import System.Glib.GType
    ( GType
    )

import Control.Monad.Trans
    ( MonadIO
    , liftIO
    )

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
    :: MonadIO m
    => WebNavigationAction -- ^ a navigation action
    -> m DOMButton         -- ^ mouse button that used to click
webNavigationActionGetButton action = liftIO $
    withWebNavigationAction action $ \ptr ->
        {#call webkit_web_navigation_action_get_button#} ptr >>=
            return . toEnum . fromIntegral

-- | Returns a bitmask with the the state of the modifier keys.
webNavigationActionGetModifierState
    :: MonadIO m
    => WebNavigationAction -- ^ a navigation action
    -> m Int               -- ^ a bitmask with the state of the modifier keys
webNavigationActionGetModifierState action = liftIO $
    withWebNavigationAction action $ \ptr ->
        {#call webkit_web_navigation_action_get_modifier_state#} ptr >>=
            return . fromIntegral

{- | Returns the URI that was originally requested. This may differ from the
     navigation target, for instance because of a redirect.
-}
webNavigationActionGetOriginalUri
    :: MonadIO m
    => WebNavigationAction -- ^ a navigation action
    -> m String            -- ^ the URI
webNavigationActionGetOriginalUri action = liftIO $
    withWebNavigationAction action $ \ptr ->
        {#call webkit_web_navigation_action_get_original_uri#} ptr >>=
            peekCString

-- | Returns the reason why WebKit is requesting a navigation.
webNavigationActionGetReason
    :: MonadIO m
    => WebNavigationAction   -- ^ a navigation action
    -> m WebNavigationReason -- ^ the reason for the navigation
webNavigationActionGetReason action = liftIO $
    withWebNavigationAction action $ \ptr ->
        {#call web_navigation_action_get_reason#} ptr >>=
            return . toEnum . fromIntegral

-- | Returns the target frame of the action.
webNavigationActionGetTargetFrame
    :: MonadIO m
    => WebNavigationAction -- ^ a navigation action
    -> m (Maybe String)    -- ^ 'Just' the target frame or 'Nothing' if there
                           --   is no target.
webNavigationActionGetTargetFrame action = liftIO $
    withWebNavigationAction action $ \ptr ->
        {#call web_navigation_action_get_target_frame#} ptr >>=
            maybePeek peekCString

webNavigationActionGetType
    :: MonadIO m
    => m GType
webNavigationActionGetType = liftIO $
    {#call web_navigation_action_get_type#}

{- | Sets the URI that was originally requested. This may differ from the
     navigation target, for instance because of a redirect.
-}
webNavigationActionSetOrginalUri
    :: MonadIO m
    => WebNavigationAction -- ^ a navigation action
    -> String              -- ^ a URI
    -> m ()
webNavigationActionSetOrginalUri action uri = liftIO $
    withWebNavigationAction action $ \ptr ->
        newCString uri >>=
            {#call webkit_web_navigation_action_set_original_uri#} ptr

-- | Sets the reason why WebKit is requesting a navigation.
webNavigationActionSetReason
    :: MonadIO m
    => WebNavigationAction -- ^ a navigation action
    -> WebNavigationReason -- ^ the reason for the navigation
    -> m ()
webNavigationActionSetReason action reason = liftIO $
    withWebNavigationAction action $ \ptr ->
        {#call web_navigation_action_set_reason#} ptr $
            (fromIntegral . fromEnum)  reason

