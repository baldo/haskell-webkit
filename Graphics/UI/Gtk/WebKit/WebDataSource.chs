{-# LANGUAGE ForeignFunctionInterface #-}

{# context lib="libwebkit" prefix="webkit_" #}

module Graphics.UI.Gtk.WebKit.WebDataSource
    ( WebDataSource

    , webDataSourceNew
    , webDataSourceIsLoading
    , webDataSourceGetWebFrame
    , webDataSourceGetUnreachableUri
    , webDataSourceGetSubresources
    , webDataSourceGetRequest
    , webDataSourceGetMainResource
    , webDataSourceGetInitialRequest
    , webDataSourceGetEncoding
    , webDataSourceGetData
    ) where

#include <webkit/webkitwebdatasource.h>

import Foreign.C
import GHC.Ptr
import System.Glib.FFI
import System.Glib.GList 
    ( fromGList 
    )

import Control.Monad

import Graphics.UI.Gtk.Abstract.Object
    ( makeNewObject
    )

{#import Graphics.UI.Gtk.WebKit.General.Types#}
    ( WebDataSource
    , NetworkRequest
    , WebResource
    , WebFrame 
     
    , withWebDataSource
    , mkWebDataSource
    , unWebDataSource
    
    , mkNetworkRequest
    
    , mkWebResource
    
    , mkWebFrame
    )

webDataSourceGetData :: WebDataSource -> IO String
webDataSourceGetData source = 
    withWebDataSource source $ \ptr ->
        {#call web_data_source_get_data#} ptr >>=
            {#get GString->str#} >>= peekCString

webDataSourceGetEncoding :: WebDataSource -> IO String
webDataSourceGetEncoding source =
    withWebDataSource source $ \ptr ->
        {#call web_data_source_get_encoding#} ptr >>= peekCString

webDataSourceGetInitialRequest :: WebDataSource -> IO NetworkRequest 
webDataSourceGetInitialRequest source =
    withWebDataSource source $ \ptr ->
        makeNewObject mkNetworkRequest $
        {#call web_data_source_get_initial_request#} ptr 

webDataSourceGetMainResource :: WebDataSource -> IO WebResource 
webDataSourceGetMainResource source =
    withWebDataSource source $ \ptr ->
        makeNewObject mkWebResource $
        {#call web_data_source_get_main_resource#} ptr 

webDataSourceGetRequest :: WebDataSource -> IO NetworkRequest
webDataSourceGetRequest source =
    withWebDataSource source $ \ptr ->
        makeNewObject mkNetworkRequest $
        {#call web_data_source_get_request#} ptr 

webDataSourceGetSubresources :: WebDataSource -> IO [WebResource]
webDataSourceGetSubresources source =
    withWebDataSource source $ \ptr ->
        {#call web_data_source_get_subresources#} ptr >>= fromGList >>= mapM (makeNewObject mkWebResource . return) 

webDataSourceGetUnreachableUri :: WebDataSource -> IO String
webDataSourceGetUnreachableUri source =
    withWebDataSource source $ \ptr ->
        {#call web_data_source_get_unreachable_uri#} ptr >>= peekCString

webDataSourceGetWebFrame :: WebDataSource -> IO WebFrame
webDataSourceGetWebFrame source =
    withWebDataSource source $ \ptr ->
        makeNewObject mkWebFrame $
        {#call web_data_source_get_web_frame#} ptr 

webDataSourceIsLoading :: WebDataSource -> IO Bool
webDataSourceIsLoading source =
    withWebDataSource source $ \ptr ->
        liftM toBool $
            {#call web_data_source_is_loading#} ptr 

webDataSourceNew :: IO WebDataSource 
webDataSourceNew =
    makeNewObject mkWebDataSource $
        {#call web_data_source_new#} 
