{-# LANGUAGE ForeignFunctionInterface #-}

{# context lib="libwebkit" prefix="JS" #}

module Language.JavaScript.JavaScriptCore.Base 
    ( ContextRef
    , StringRef
    , ValueRef
    
    , checkScriptSyntax 
    ) where

import Foreign.C
import System.Glib.FFI

import Control.Monad

{#import Language.JavaScript.JavaScriptCore.General.Types#}
    ( ContextRef
    , withContextRef
    
    , StringRef
    , withStringRef

    , ValueRef 
    , withValueRef
    )

#include <JavaScriptCore/JavaScript.h>

{- 
JS_EXPORT bool JSCheckScriptSyntax(
    JSContextRef ctx,
    JSStringRef script,
    JSStringRef sourceURL,
    int startingLineNumber,
    JSValueRef *exception);  
-}
checkScriptSyntax :: 
    ContextRef -> StringRef -> StringRef -> Int -> ValueRef -> IO Bool
checkScriptSyntax context script sourceURL startingLineNr excp =
    withContextRef context $ \ctx ->
        withStringRef script $ \sptr -> 
            withStringRef sourceURL $ \url ->
                withValueRef excp $ \exception ->
                    liftM toBool $ 
                        jsCheckScriptSyntax
                          ctx sptr url (fromIntegral startingLineNr) exception

foreign import ccall safe "JSBase.h JSCheckScriptSyntax"
  jsCheckScriptSyntax :: 
   Ptr ContextRef -> Ptr StringRef -> Ptr StringRef -> CInt -> Ptr ValueRef -> IO CInt

{- 
JS_EXPORT JSValueRef JSEvaluateScript(
    JSContextRef ctx,
    JSStringRef script,
    JSObjectRef thisObject,
    JSStringRef sourceURL,
    int startingLineNumber,
    JSValueRef *exception);  
-}

{-
JS_EXPORT void JSGarbageCollect(
    JSContextRef ctx);  
-}
