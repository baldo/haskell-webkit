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

import Foreign.C
import System.Glib.FFI
import System.Glib.GType

{#import Graphics.UI.Gtk.WebKit.General.Types#}
    ( WebPolicyDecision

    , withWebPolicyDecision
    )

-- | Will send the DOWNLOAD decision to the policy implementer.
webPolicyDecisionDownload
    :: WebPolicyDecision -- ^ a policy decision
    -> IO ()
webPolicyDecisionDownload decision =
    withWebPolicyDecision decision $ \ptr ->
        {#call web_policy_decision_download#} ptr

webPolicyDecisionGetType :: IO GType 
webPolicyDecisionGetType = 
    {#call webkit_web_policy_decision_get_type#}

-- | Will send the IGNORE decision to the policy implementer.
webPolicyDecisionIgnore
    :: WebPolicyDecision -- ^ a policy decision
    -> IO ()
webPolicyDecisionIgnore decision =
    withWebPolicyDecision decision $ \ptr ->
        {#call web_policy_decision_ignore#} ptr

-- | Will send the USE decision to the policy implementer.
webPolicyDecisionUse
    :: WebPolicyDecision -- ^ a policy decision
    -> IO ()
webPolicyDecisionUse decision =
    withWebPolicyDecision decision $ \ptr ->
        {#call web_policy_decision_use#} ptr 

