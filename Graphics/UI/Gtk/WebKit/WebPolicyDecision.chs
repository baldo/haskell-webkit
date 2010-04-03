{-# LANGUAGE ForeignFunctionInterface #-}

{# context lib="libwebkit" prefix="webkit_" #}

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

import Control.Monad

{#import Graphics.UI.Gtk.WebKit.General.Types#}
    ( WebPolicyDecision

    , withWebPolicyDecision
    )

webPolicyDecisionDownload :: WebPolicyDecision -> IO ()
webPolicyDecisionDownload decision =
    withWebPolicyDecision decision $ \ptr ->
        {#call web_policy_decision_download#} ptr

webPolicyDecisionGetType :: IO GType 
webPolicyDecisionGetType = 
    {#call webkit_web_policy_decision_get_type#}

webPolicyDecisionIgnore :: WebPolicyDecision -> IO ()
webPolicyDecisionIgnore decision =
    withWebPolicyDecision decision $ \ptr ->
        {#call web_policy_decision_ignore#} ptr
        
webPolicyDecisionUse :: WebPolicyDecision -> IO ()
webPolicyDecisionUse decision =
    withWebPolicyDecision decision $ \ptr ->
        {#call web_policy_decision_use#} ptr 

