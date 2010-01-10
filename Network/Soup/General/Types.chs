{-# LANGUAGE ForeignFunctionInterface #-}

{# context lib="libsoup" prefix="soup" #}

module Network.Soup.General.Types
    ( Message
    , withMessage
    , mkMessage
    , unMessage
    ) where

#include <libsoup/soup.h>

import System.Glib.FFI

import Graphics.UI.Gtk.Types
    ( ObjectClass
    , GObjectClass (..)

    , mkGObject
    , unGObject
    )

-- Message --------------------------------------------------------------------

{#pointer *Message foreign newtype#}

instance ObjectClass Message
instance GObjectClass Message where
  toGObject = mkGObject . castForeignPtr . unMessage
  unsafeCastGObject = mkMessage . castForeignPtr . unGObject

mkMessage = Message
unMessage (Message o) = o

