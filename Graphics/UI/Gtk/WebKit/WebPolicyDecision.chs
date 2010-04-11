{-# LANGUAGE ForeignFunctionInterface #-}

{# context lib="libwebkit" prefix="webkit_" #}

{-| Liason between WebKit and the application regarding asynchronous policy
    decisions.

'WebPolicyDecision's are given to the application on signal emissions that deal
with policy decisions, such as if a new window should be opened, or if a given
navigation should be allowed. The application uses it to tell the engine what to
do.
-}

module Graphics.UI.Gtk.WebKit.WebPolicyDecision
    ( WebPolicyDecision

    , webPolicyDecisionUse
    , webPolicyDecisionGetType
    , webPolicyDecisionDownload
    , webPolicyDecisionIgnore

    ) where

#include <webkit/webkitwebpolicydecision.h>

import System.Glib.FFI
import System.Glib.GType
    ( GType
    )

import Control.Monad.Trans
    ( MonadIO
    , liftIO
    )

{#import Graphics.UI.Gtk.WebKit.General.Types#}
    ( WebPolicyDecision

    , withWebPolicyDecision
    )

-- | Will send the DOWNLOAD decision to the policy implementer.
webPolicyDecisionDownload
    :: MonadIO m
    => WebPolicyDecision -- ^ a policy decision
    -> m ()
webPolicyDecisionDownload decision = liftIO $
    withWebPolicyDecision decision $ \ptr ->
        {#call web_policy_decision_download#} ptr

webPolicyDecisionGetType
    :: MonadIO m
    => m GType
webPolicyDecisionGetType = liftIO $
    {#call webkit_web_policy_decision_get_type#}

-- | Will send the IGNORE decision to the policy implementer.
webPolicyDecisionIgnore
    :: MonadIO m
    => WebPolicyDecision -- ^ a policy decision
    -> m ()
webPolicyDecisionIgnore decision = liftIO $
    withWebPolicyDecision decision $ \ptr ->
        {#call web_policy_decision_ignore#} ptr

-- | Will send the USE decision to the policy implementer.
webPolicyDecisionUse
    :: MonadIO m
    => WebPolicyDecision -- ^ a policy decision
    -> m ()
webPolicyDecisionUse decision = liftIO $
    withWebPolicyDecision decision $ \ptr ->
        {#call web_policy_decision_use#} ptr

