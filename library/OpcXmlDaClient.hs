{-# LANGUAGE DerivingVia #-}

module OpcXmlDaClient
  ( -- * Operations
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

-- @since 0.1
newtype Op a = Op (Identity a)
  deriving
    ( -- | @since 0.1
      Functor,
      -- | @since 0.1
      Applicative,
      -- | @since 0.1
      Monad
    )
    via Identity

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
