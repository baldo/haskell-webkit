{-# LANGUAGE ForeignFunctionInterface #-}

{# context lib="libwebkit" prefix="webkit_" #}

module Network.Soup.Message
    ( Message

    -- Properties --------------------------------------------------------------

    -- Signals -----------------------------------------------------------------

    ) where

#include <libsoup/soup-message.h>

import Foreign.C
import GHC.Ptr
import System.Glib.FFI

import System.Glib.Properties

import Control.Monad

import Graphics.UI.Gtk
    ( makeNewObject
    )

import Graphics.UI.Gtk.Signals

{#import Network.Soup.General.Types#}
    ( Message 

    , withMessage
    , mkMessage
    )


