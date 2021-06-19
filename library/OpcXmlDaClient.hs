module OpcXmlDaClient where

import qualified Acc
import Control.Applicative.Free
import OpcXmlDaClient.Base.Prelude hiding (Ap)
import qualified OpcXmlDaClient.Core as Core

-- * Types

-- |
-- Validated server URI.
newtype ServerUri = ServerUri ByteString

-- |
-- Request timeout.
newtype RequestTimeout = RequestTimeout Int32

-- |
-- Timeout of 1 second.
instance Default RequestTimeout where
  def = RequestTimeout 1000

-- |
-- Locale to be used by the server in response.
newtype Locale = Locale ByteString

-- | \"en\" locale.
instance Default Locale where
  def = Locale "en"

-- |
-- Percentage of full engineering unit range of an item's value
-- needed to trigger a subscription handler.
newtype Deadband = Deadband Float

-- |
-- General information about the server to be acquired once and reused across operations.
--
-- E.g., it contains the time difference between the server's time and our local time
-- for the purpose of automatically setting options like \"deadline\" in server's time.
data ServerInfo
  = ServerInfo
      ServerUri
      -- ^ URI at which the server runs.
      DiffTime
      -- ^ Difference between server's and our time for synchronisation purposes.

data Error customValueParsingError
  = CustomValueParsingError customValueParsingError

data Subscription value
  = Subscription
      (Acc.Acc Core.SubscribeRequestItem)
      (Ap ValueDecoder value)

data ValueDecoder value

-- * Operations

-- |
subscribe ::
  -- |
  -- Request timeout.
  -- You can use 'def' here.
  RequestTimeout ->
  Locale ->
  Subscription value ->
  -- |
  --  Handler called on each received value,
  --  returning a boolean value,
  --  where 'False' signals to terminate the subscription.
  (value -> IO Bool) ->
  IO (Maybe (Error customValueParsingError))
subscribe (RequestTimeout requestTimeout) (Locale localeId) =
  error "TODO"
  where
    subscribe =
      Core.Subscribe
        (Just requestOptions)
        (Just itemList)
        returnValuesOnReply
        (Just subscriptionPingRate)
    requestOptions =
      Core.RequestOptions
        True
        True
        True
        True
        True
        deadline
        Nothing
        (Just localeId)
    deadline =
      error "TODO: derive it based on the synced server time"
    localeId =
      error "TODO"
    itemList =
      error "TODO"
    -- Tells the server to return the first batch of values in the response to our first request.
    returnValuesOnReply = True
    -- Specifies the timeout after which the server may consider us no longer interested
    -- and invalidate the subscription.
    -- We use our request timeout and extend it with 10 extra seconds to cover network delays.
    subscriptionPingRate = requestTimeout + 10000

-- * Subscriptions

-- |
-- Declares a single subscription item.
--
-- You can then compose it with other items into a single subscription
-- using the applicative interface of 'Subscription'.
subscriptionItem :: ValueDecoder value -> Subscription value
subscriptionItem =
  error "TODO"

floatSubscription :: Deadband -> Subscription Float
floatSubscription =
  error "TODO"

-- * Decoders

float :: ValueDecoder Float
float =
  error "TODO"
