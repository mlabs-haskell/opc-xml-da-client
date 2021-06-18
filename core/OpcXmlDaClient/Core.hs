module OpcXmlDaClient.Core
  ( -- * Connection
    Connection,
    ConnectionParams,
    ConnectionError,
    OperationError,
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
    module OpcXmlDaClient.Core.Types,
  )
where

import OpcXmlDaClient.Base.Prelude hiding (Read, read)
import OpcXmlDaClient.Core.Types

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
data OperationError

-- |
-- Establish a connection given the params.
connect :: ConnectionParams -> IO (Either ConnectionError Connection)
connect =
  error "TODO"

-- |
-- Execute series of operations on a connection.
operate :: Connection -> Op a -> IO (Either OperationError a)
operate =
  error "TODO"

-- |
-- Close a connection releasing all resources.
disconnect :: Connection -> IO ()
disconnect =
  error "TODO"

-- |
-- Composable series of interactions with the server.
newtype Op a = Op (Connection -> IO (Either OperationError a))
  deriving
    (Functor, Applicative, Monad)
    via (ReaderT Connection (ExceptT OperationError IO))

getStatus :: GetStatus -> Op GetStatusResponse
getStatus = error "TODO"

read :: Read -> Op ReadResponse
read = error "TODO"

write :: Write -> Op WriteResponse
write = error "TODO"

subscribe :: Subscribe -> Op SubscribeResponse
subscribe = error "TODO"

subscriptionPolledRefresh :: SubscriptionPolledRefresh -> Op SubscriptionPolledRefreshResponse
subscriptionPolledRefresh = error "TODO"

subscriptionCancel :: SubscriptionCancel -> Op SubscriptionCancelResponse
subscriptionCancel = error "TODO"

browse :: Browse -> Op BrowseResponse
browse = error "TODO"

getProperties :: GetProperties -> Op GetPropertiesResponse
getProperties = error "TODO"
