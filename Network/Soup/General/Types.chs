{-# LANGUAGE ForeignFunctionInterface #-}

{# context lib="libsoup" prefix="soup" #}

module Network.Soup.General.Types
    ( Message
    , withMessage
    , makeMessage
    , mkMessage
    , unMessage
    
    , SoupSession
    , withSoupSession
    , makeSoupSession
    , mkSoupSession
    , unSoupSession
    ) where

#include <libsoup/soup.h>

import System.Glib.FFI

import Graphics.UI.Gtk.Abstract.Object  
    ( makeNewObject )
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
makeMessage = makeNewObject mkMessage

-- SoupSession  ---------------------------------------------------------------

{#pointer *SoupSession foreign newtype#}

instance ObjectClass SoupSession
instance GObjectClass SoupSession where
  toGObject (SoupSession o) = GObject (castForeignPtr o)
  unsafeCastGObject = SoupSession . castForeignPtr . unGObject

mkSoupSession = (SoupSession, objectUnref) 
unSoupSession (SoupSession o) = o
makeSoupSession = makeNewObject mkSoupSession

