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
    , GObject (..)

    , unGObject
    , objectUnref
    )

-- Message --------------------------------------------------------------------

{#pointer *Message foreign newtype#}

instance ObjectClass Message
instance GObjectClass Message where
  toGObject (Message o) = GObject (castForeignPtr o)
  unsafeCastGObject = Message . castForeignPtr . unGObject

mkMessage = (Message, objectUnref) 
unMessage (Message o) = o

