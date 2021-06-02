{-# LANGUAGE DerivingVia #-}

module OpcXmlDaClient
  ( -- * Connection
    Connection,
    ConnectionParams,
    ConnectionError,
    OpError,
    connect,
    operate,
    disconnect,

    -- * Operations
    Op,
    getStatus,
    read,
    write,
    subscribe,
    subscriptionPolledRefresh,
    subscriptionCancel,
    browse,
    getProperties,

    -- * Data types
    module OpcXmlDaClient.Protocol,
  )
where

import OpcXmlDaClient.Prelude hiding (Read, read)
import OpcXmlDaClient.Protocol

-- |
-- Connection to the server.
data Connection

-- |
-- Connection parameters.
--
-- TODO: Blank until implemented.
data ConnectionParams

-- |
-- Error during the establishment of connection.
--
-- TODO: Blank until implemented.
data ConnectionError

-- |
-- Error during the execution of an operation.
--
-- TODO: Blank until implemented.
data OpError

-- |
-- Establish a connection given the params.
connect :: ConnectionParams -> IO (Either ConnectionError Connection)
connect =
  error "TODO"

-- |
-- Execute series of operations on a connection.
operate :: Connection -> Op a -> IO (Either OpError a)
operate =
  error "TODO"

-- |
-- Close a connection releasing all resources.
disconnect :: Connection -> IO ()
disconnect =
  error "TODO"

-- |
-- Composable series of interactions with the server.
newtype Op a = Op (Connection -> IO (Either OpError a))
  deriving
    (Functor, Applicative, Monad)
    via (ReaderT Connection (ExceptT OpError IO))

-- | @since 0.1
getStatus :: GetStatus -> Op GetStatusResponse
getStatus = error "TODO"

-- | @since 0.1
read :: Read -> Op ReadResponse
read = error "TODO"

-- | @since 0.1
write :: Write -> Op WriteResponse
write = error "TODO"

-- | @since 0.1
subscribe :: Subscribe -> Op SubscribeResponse
subscribe = error "TODO"

-- | @since 0.1
subscriptionPolledRefresh :: SubscriptionPolledRefresh -> Op SubscriptionPolledRefreshResponse
subscriptionPolledRefresh = error "TODO"

-- | @since 0.1
subscriptionCancel :: SubscriptionCancel -> Op SubscriptionCancelResponse
subscriptionCancel = error "TODO"

-- | @since 0.1
browse :: Browse -> Op BrowseResponse
browse = error "TODO"

-- | @since 0.1
getProperties :: GetProperties -> Op GetPropertiesResponse
getProperties = error "TODO"
