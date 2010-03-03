{-# LANGUAGE ForeignFunctionInterface #-}

{# context lib="libsoup" prefix="Soup" #}

module Network.Soup.General.Enums
    ( AddressFamily (..)
    , DateFormat (..)
    , LogLevel (..)
    , MemoryUse (..)
    , HttpVersion (..)
    , MessageFlags (..)
    , MessageHeadersType (..)
    , Encoding (..)
    , Expectation (..)
    , SslError (..)
    , SocketIoStatus (..)
    , KnownStatusCode (..)
    , SoupXmlrpcError (..)
    ) where

#include <libsoup/soup.h>

-- Address.h ------------------------------------------------------------------

{#enum AddressFamily {underscoreToCase}
    deriving (Eq, Show)#}

-- Date.h ---------------------------------------------------------------------

{#enum DateFormat {underscoreToCase}
    deriving (Eq, Show)#}

-- Logger.h -------------------------------------------------------------------

{#enum LoggerLogLevel as LogLevel {underscoreToCase}
    with prefix="SOUP_LOGGER" deriving (Eq, Show)#}

-- MessageBody.h --------------------------------------------------------------

{#enum MemoryUse {underscoreToCase}
    deriving (Eq, Show)#}

-- Message.h ------------------------------------------------------------------

{#enum HTTPVersion as HttpVersion {underscoreToCase}
    deriving (Eq, Show)#}

{#enum MessageFlags {underscoreToCase}
    deriving (Eq, Show)#}

-- MessageHeaders.h -----------------------------------------------------------

{#enum MessageHeadersType {underscoreToCase}
    deriving (Eq, Show)#}

{#enum Encoding {underscoreToCase}
    deriving (Eq, Show)#}

{#enum Expectation {underscoreToCase}
    deriving (Eq, Show)#}

-- Misc.h ---------------------------------------------------------------------

{#enum SSLError as SslError {underscoreToCase}
    deriving (Eq, Show)#}

-- Socket.h -------------------------------------------------------------------

{#enum SocketIOStatus as SocketIoStatus {underscoreToCase}
    deriving (Eq, Show)#}

-- Status.h -------------------------------------------------------------------

{#enum KnownStatusCode {underscoreToCase}
    deriving (Eq, Show)#}

-- Xmlrpc.h -------------------------------------------------------------------

{#enum SoupXMLRPCError as SoupXmlrpcError {underscoreToCase}
    deriving (Eq, Show)#}

