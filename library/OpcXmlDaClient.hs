module OpcXmlDaClient
(
  -- * Operations
  Op,
  getStatus,
  -- * Data types
  module OpcXmlDaClient.Protocol,
)
where

import OpcXmlDaClient.Prelude
import OpcXmlDaClient.Protocol



data Op a

instance Functor Op

instance Applicative Op

instance Monad Op


getStatus :: GetStatus -> Op GetStatusResponse
getStatus = error "TODO"
