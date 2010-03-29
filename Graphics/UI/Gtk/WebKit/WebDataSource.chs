{-# LANGUAGE ForeignFunctionInterface #-}

{# context lib="libwebkit" prefix="webkit_" #}

{-| Encapsulates the content to be displayed in a 'WebFrame'.

'WebDataSource' encapsulates the content of a 'WebFrame'. A 'WebFrame' has a
main resource and subresources and the data source provides access to these
resources. When a request gets loaded initially, it is set to a provisional
state. The application can request for the request that initiated the load by
asking for the provisional data source and invoking the
'webDataSourceGetInitialRequest' function. This data source may not have enough
data and some methods may return empty values. To get a \"full\" data source
with the data and resources loaded, you need to get the non-provisional data
source through 'webFrameGetDataSource' function. This data source will have the
data after everything was loaded. Make sure that the data source was finished
loading before using any of these functions. You can do this via
'webDataSourceIsLoading'.
-}

module Graphics.UI.Gtk.WebKit.WebDataSource
    ( WebDataSource

    , webDataSourceGetType
    , webDataSourceNew
    , webDataSourceNewWithRequest 
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
import System.Glib.GType
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
    , makeWebDataSource
    , mkWebDataSource
    , unWebDataSource
    
    , mkNetworkRequest
    , withNetworkRequest
    
    , mkWebResource
    
    , mkWebFrame
    )

webDataSourceGetType
    :: IO GType
webDataSourceGetType =
    {#call web_data_source_get_type#}

{- | Returns the raw data that represents the the frame's content. The data
     will be incomplete until the data has finished loading. Returns
     'Nothing' if the WebFrame hasn't loaded any data. Use
     'webDataSourceIsLoading' to test if data source is in the process of
     loading.
-}
webDataSourceGetData
    :: WebDataSource     -- ^ the data source
    -> IO (Maybe String) -- ^ 'Just' the data or 'Nothing' if no data has been
                         --   loaded
webDataSourceGetData source = 
    withWebDataSource source $ \ptr ->
        {#call web_data_source_get_data#} ptr >>=
            {#get GString->str#} >>= maybePeek peekCString

{- | Returns the text encoding name as set in the 'WebView', or if not, the
     text encoding of the response.
-}
webDataSourceGetEncoding
    :: WebDataSource -- ^ the data source
    -> IO String     -- ^ the encoding
webDataSourceGetEncoding source =
    withWebDataSource source $ \ptr ->
        {#call web_data_source_get_encoding#} ptr >>= peekCString

{- | Returns a reference to the original 'NetworkRequest' that was used to load
     the web content. The 'NetworkRequest' returned by this function is the
     request prior to the \"committed\" load state. See
     'webDataSourceGetRequest' for getting the \"committed\" request.
-}
webDataSourceGetInitialRequest
    :: WebDataSource     -- ^ the data source
    -> IO NetworkRequest -- ^ the request
webDataSourceGetInitialRequest source =
    withWebDataSource source $ \ptr ->
        makeNewObject mkNetworkRequest $
        {#call web_data_source_get_initial_request#} ptr 

-- | Returns the main 'WebResource' of the 'WebDataSource'.
webDataSourceGetMainResource
    :: WebDataSource  -- ^ the data source
    -> IO WebResource -- ^ a new 'WebResource' representing the main resource of
                      --   the data source
webDataSourceGetMainResource source =
    withWebDataSource source $ \ptr ->
        makeNewObject mkWebResource $
        {#call web_data_source_get_main_resource#} ptr 

{- | Returns a 'NetworkRequest' that was used to create this 'WebDataSource'.
     The 'NetworkRequest' returned by this function is the request that was
     \"committed\", and hence, different from the request you get from
     'webDataSourceGetInitialRequest'.
-}
webDataSourceGetRequest
    :: WebDataSource             -- ^ the data source
    -> IO (Maybe NetworkRequest) -- ^ 'Just' the request or 'Nothing' if the
                                 --   data source is not attached to a frame or
                                 --   the frame hasn't been loaded
webDataSourceGetRequest source =
    withWebDataSource source $ maybePeek $ \ptr ->
        makeNewObject mkNetworkRequest $
        {#call web_data_source_get_request#} ptr

{- | Gives you a list of 'WebResource's that compose the 'WebView' to which this
     'WebDataSource' is attached.
-}
webDataSourceGetSubresources
    :: WebDataSource    -- ^ the data source
    -> IO [WebResource] -- ^ the 'WebResource's
webDataSourceGetSubresources source =
    withWebDataSource source $ \ptr ->
        {#call web_data_source_get_subresources#} ptr >>=
            fromGList >>=
                mapM (makeNewObject mkWebResource . return) 

{- | Return the unreachable URI of the data source. The data source will have an
     unreachable URL if it was created using 'webFrameLoadAlternateHtmlString'.
-}
webDataSourceGetUnreachableUri
    :: WebDataSource     -- ^ the data source
    -> IO (Maybe String) -- ^ 'Just' the unreachable URL or 'Nothing' if there
                         --   is no unreachable URL. 
webDataSourceGetUnreachableUri source =
    withWebDataSource source $ \ptr ->
        {#call web_data_source_get_unreachable_uri#} ptr >>=
            maybePeek peekCString

-- | Returns the 'WebFrame' that represents this data source
webDataSourceGetWebFrame
    :: WebDataSource       -- ^ the data source
    -> IO (Maybe WebFrame) -- ^ 'Just' the 'WebFrame' that represents the data
                           --   source or 'Nothing' if the data source is not
                           --   attached to a frame. 
webDataSourceGetWebFrame source =
    withWebDataSource source $ maybePeek $ \ptr ->
        makeNewObject mkWebFrame $
        {#call web_data_source_get_web_frame#} ptr 

{- | Determines whether the data source is in the process of loading its
     content.
-}
webDataSourceIsLoading
    :: WebDataSource -- ^ the data source
    -> IO Bool       -- ^ 'True' if the data source is still loading, 'False'
                     --   otherwise
webDataSourceIsLoading source =
    withWebDataSource source $ \ptr ->
        liftM toBool $
            {#call web_data_source_is_loading#} ptr 

{- | Creates a new 'WebDataSource'. The URL of the 'WebDataSource' will be set
     to \"about:blank\". 
-}
webDataSourceNew
    :: IO WebDataSource -- ^ the new data source
webDataSourceNew =
    makeNewObject mkWebDataSource $
        {#call web_data_source_new#} 

webDataSourceNewWithRequest 
    :: NetworkRequest   -- ^ a network request
    -> IO WebDataSource -- ^ web data source for supplied request
webDataSourceNewWithRequest request =
    withNetworkRequest request $ \ptr ->
        makeWebDataSource $ 
            {#call web_data_source_new_with_request#} ptr

