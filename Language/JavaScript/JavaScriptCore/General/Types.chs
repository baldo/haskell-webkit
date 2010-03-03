{-# LANGUAGE ForeignFunctionInterface #-}

{# context lib="webkit" prefix="JS" #}

module Language.JavaScript.JavaScriptCore.General.Types 
    ( ClassRef (..)
    , withClassRef 
    , makeClassRef 

    , ContextGroupRef (..)
    , withContextGroupRef 
    , makeContextGroupRef 
   
    , ContextRef (..)
    , withContextRef
    , makeContextRef

    , GlobalContextRef (..)
    , withGlobalContextRef
    , makeGlobalContextRef

    , ObjectRef (..)
    , withObjectRef
    , makeObjectRef

    , PropertyNameAccumulatorRef (..)
    , withPropertyNameAccumulatorRef
    , makePropertyNameAccumulatorRef

    , PropertyNameArrayRef (..)
    , withPropertyNameArrayRef
    , makePropertyNameArrayRef


    , StringRef (..)
    , withStringRef
    , makeStringRef

    , ValueRef (..)
    , withValueRef
    , makeValueRef
    ) where


import System.Glib.FFI
import Foreign.ForeignPtr

#include <JavaScriptCore/JavaScript.h>

-- JSBase.h data types --------------------------------------------------------

-- ClassRef -----------------------------------------------

{#pointer *ClassRef foreign newtype#}
makeClassRef = makeNewRef ClassRef

-- ContextGroupRef ----------------------------------------

{#pointer *ContextGroupRef foreign newtype#}
makeContextGroupRef = makeNewRef ContextGroupRef

-- ContextRef ---------------------------------------------

{#pointer *ContextRef foreign newtype#}
makeContextRef = makeNewRef ContextRef

-- GlobalContextRef ---------------------------------------

{#pointer *GlobalContextRef foreign newtype#}
makeGlobalContextRef = makeNewRef GlobalContextRef  

-- ObjectRef ----------------------------------------------

{#pointer *ObjectRef foreign newtype#}
makeObjectRef = makeNewRef ObjectRef

-- PropertyNameAccumulatorRef -----------------------------

{#pointer *PropertyNameAccumulatorRef foreign newtype#}
makePropertyNameAccumulatorRef = makeNewRef PropertyNameAccumulatorRef

-- PropertyNameArrayRef -----------------------------------

{#pointer *PropertyNameArrayRef foreign newtype#}
makePropertyNameArrayRef = makeNewRef PropertyNameArrayRef


-- StringRef ----------------------------------------------
{#pointer *StringRef foreign newtype#}
makeStringRef = makeNewRef StringRef

-- ValueRef -----------------------------------------------

{#pointer *ValueRef foreign newtype#}
makeValueRef = makeNewRef ValueRef

-- helper function for marshalling objects into the hs world
makeNewRef :: (ForeignPtr obj -> obj) -> IO (Ptr obj) -> IO obj
makeNewRef constr generator = do
    objPtr <- generator
    obj <- newForeignPtr_ objPtr 
    return $! constr obj
