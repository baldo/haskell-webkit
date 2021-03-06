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

import System.Glib.FFI
import System.Glib.GType
    ( GType
    )
import System.Glib.GList
    ( fromGList
    )

import Control.Monad.Trans
    ( MonadIO
    , liftIO
    )

{#import Graphics.UI.Gtk.WebKit.General.Types#}
    ( WebDataSource
    , NetworkRequest
    , WebResource
    , WebFrame

    , withWebDataSource
    , makeWebDataSource

    , makeNetworkRequest
    , withNetworkRequest

    , makeWebResource

    , makeWebFrame
    )

webDataSourceGetType
    :: MonadIO m
    => m GType
webDataSourceGetType = liftIO $
    {#call web_data_source_get_type#}

{- | Returns the raw data that represents the the frame's content. The data
     will be incomplete until the data has finished loading. Returns
     'Nothing' if the WebFrame hasn't loaded any data. Use
     'webDataSourceIsLoading' to test if data source is in the process of
     loading.
-}
webDataSourceGetData
    :: MonadIO m
    => WebDataSource    -- ^ the data source
    -> m (Maybe String) -- ^ 'Just' the data or 'Nothing' if no data has been
                        --   loaded
webDataSourceGetData source = liftIO $
    withWebDataSource source $ \sptr ->
        {#call web_data_source_get_data#} sptr >>=
            {#get GString->str#} >>= maybePeek peekCString

{- | Returns the text encoding name as set in the 'WebView', or if not, the
     text encoding of the response.
-}
webDataSourceGetEncoding
    :: MonadIO m
    => WebDataSource -- ^ the data source
    -> m String      -- ^ the encoding
webDataSourceGetEncoding source = liftIO $
    withWebDataSource source $ \ptr ->
        {#call web_data_source_get_encoding#} ptr >>= peekCString

{- | Returns a reference to the original 'NetworkRequest' that was used to load
     the web content. The 'NetworkRequest' returned by this function is the
     request prior to the \"committed\" load state. See
     'webDataSourceGetRequest' for getting the \"committed\" request.
-}
webDataSourceGetInitialRequest
    :: MonadIO m
    => WebDataSource    -- ^ the data source
    -> m NetworkRequest -- ^ the request
webDataSourceGetInitialRequest source = liftIO $
    withWebDataSource source $ \ptr ->
        makeNetworkRequest $
            {#call web_data_source_get_initial_request#} ptr

-- | Returns the main 'WebResource' of the 'WebDataSource'.
webDataSourceGetMainResource
    :: MonadIO m
    => WebDataSource -- ^ the data source
    -> m WebResource -- ^ a new 'WebResource' representing the main resource of
                     --   the data source
webDataSourceGetMainResource source = liftIO $
    withWebDataSource source $ \ptr ->
        makeWebResource $
            {#call web_data_source_get_main_resource#} ptr

{- | Returns a 'NetworkRequest' that was used to create this 'WebDataSource'.
     The 'NetworkRequest' returned by this function is the request that was
     \"committed\", and hence, different from the request you get from
     'webDataSourceGetInitialRequest'.
-}
webDataSourceGetRequest
    :: MonadIO m
    => WebDataSource            -- ^ the data source
    -> m (Maybe NetworkRequest) -- ^ 'Just' the request or 'Nothing' if the
                                --   data source is not attached to a frame or
                                --   the frame hasn't been loaded
webDataSourceGetRequest source = liftIO $
    withWebDataSource source $ maybePeek $ \ptr ->
        makeNetworkRequest $
            {#call web_data_source_get_request#} ptr

{- | Gives you a list of 'WebResource's that compose the 'WebView' to which this
     'WebDataSource' is attached.
-}
webDataSourceGetSubresources
    :: MonadIO m
    => WebDataSource   -- ^ the data source
    -> m [WebResource] -- ^ the 'WebResource's
webDataSourceGetSubresources source = liftIO $
    withWebDataSource source $ \ptr ->
        {#call web_data_source_get_subresources#} ptr >>=
            fromGList >>=
                mapM (makeWebResource . return)

{- | Return the unreachable URI of the data source. The data source will have an
     unreachable URL if it was created using 'webFrameLoadAlternateHtmlString'.
-}
webDataSourceGetUnreachableUri
    :: MonadIO m
    => WebDataSource    -- ^ the data source
    -> m (Maybe String) -- ^ 'Just' the unreachable URL or 'Nothing' if there
                        --   is no unreachable URL.
webDataSourceGetUnreachableUri source = liftIO $
    withWebDataSource source $ \ptr ->
        {#call web_data_source_get_unreachable_uri#} ptr >>=
            maybePeek peekCString

-- | Returns the 'WebFrame' that represents this data source
webDataSourceGetWebFrame
    :: MonadIO m
    => WebDataSource      -- ^ the data source
    -> m (Maybe WebFrame) -- ^ 'Just' the 'WebFrame' that represents the data
                          --   source or 'Nothing' if the data source is not
                          --   attached to a frame.
webDataSourceGetWebFrame source = liftIO $
    withWebDataSource source $ maybePeek $ \ptr ->
        makeWebFrame $
            {#call web_data_source_get_web_frame#} ptr

{- | Determines whether the data source is in the process of loading its
     content.
-}
webDataSourceIsLoading
    :: MonadIO m
    => WebDataSource -- ^ the data source
    -> m Bool        -- ^ 'True' if the data source is still loading, 'False'
                     --   otherwise
webDataSourceIsLoading source = liftIO $
    withWebDataSource source $ \ptr ->
        {#call web_data_source_is_loading#} ptr >>=
            return . toBool

{- | Creates a new 'WebDataSource'. The URL of the 'WebDataSource' will be set
     to \"about:blank\".
-}
webDataSourceNew
    :: MonadIO m
    => m WebDataSource -- ^ the new data source
webDataSourceNew = liftIO $
    makeWebDataSource $
        {#call web_data_source_new#}

webDataSourceNewWithRequest
    :: MonadIO m
    => NetworkRequest  -- ^ a network request
    -> m WebDataSource -- ^ web data source for supplied request
webDataSourceNewWithRequest request = liftIO $
    withNetworkRequest request $ \ptr ->
        makeWebDataSource $
            {#call web_data_source_new_with_request#} ptr

