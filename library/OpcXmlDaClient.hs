-- |
-- High-level OPC XML-DA client library.
--
-- The API comes free from all kinds of exceptions.
-- All error-reporting is explicit and is presented using the Either type.
module OpcXmlDaClient
  ( -- * Operations
    subscribe,

    -- * Subscription
    Subscription,
    subscription,

    -- * Value Decoding
    Decoder,
    intDecoder,
    floatDecoder,

    -- * Configuration

    -- | We use smart constructors to ensure their correctness.
    Uri,
    textUri,
    RequestTimeout,
    millisecondsRequestTimeout,
    Locale,
    textLocale,
    Deadband,
    fractionDeadband,
    SamplingRate,
    millisecondsSamplingRate,

    -- * Domain Values
    Core.OpcQuality (..),
    Core.QualityBits (..),
    Core.LimitBits (..),
    Core.OpcError (..),

    -- * Errors
    SubscriptionError (..),
    DecodingError (..),
  )
where

import qualified Data.Attoparsec.Text as Atto
import OpcXmlDaClient.Base.Prelude
import qualified OpcXmlDaClient.Core as Core
import qualified Text.XML as Xml

-- * Configuration Types

-- |
-- Validated URI.
newtype Uri = Uri Text

-- |
-- Construct a correct URI by validating a textual value.
textUri :: Text -> Maybe Uri
textUri = error "TODO"

-- |
-- Request timeout in milliseconds.
newtype RequestTimeout = RequestTimeout Int32

-- |
-- Timeout of 1 second.
instance Default RequestTimeout where
  def = RequestTimeout 1000

-- |
-- Construct a request timeout value,
-- ensuring that it's in the proper range.
millisecondsRequestTimeout :: Int -> Maybe RequestTimeout
millisecondsRequestTimeout =
  error "TODO"

-- |
-- Locale to be used by the server in response.
newtype Locale = Locale Text

-- | \"en\" locale.
instance Default Locale where
  def = Locale "en"

-- |
-- Construct a correct locale by validating a textual input.
textLocale :: Text -> Maybe Locale
textLocale =
  error "TODO"

-- |
-- Percentage of full engineering unit range of an item's value
-- needed to trigger a subscription handler.
newtype Deadband = Deadband Double

-- |
-- Deadband of 0.
instance Default Deadband where
  def = Deadband 0

-- |
-- Construct a deadband value from a floating point number by ensuring that it's in the @[0, 1]@ range,
-- where @1@ stands for @100%@.
fractionDeadband :: Double -> Maybe Deadband
fractionDeadband =
  error "TODO"

-- |
-- Rate in milliseconds at which the server should check for value changes.
--
-- 0 sampling rate instructs the server to attempt to poll underlying device at fastest practical rate and
-- return the most accurate data available.
newtype SamplingRate = SamplingRate Int

-- |
-- SamplingRate of 0.
instance Default SamplingRate where
  def = SamplingRate 0

-- |
-- Construct a sampling rate value from an amount of milliseconds,
-- ensuring that it's in the proper range.
millisecondsSamplingRate :: Int -> Maybe SamplingRate
millisecondsSamplingRate =
  error "TODO"

-- |
-- General information about the server to be acquired once and reused across operations.
--
-- E.g., it contains the time difference between the server's time and our local time
-- for the purpose of automatically setting options like \"deadline\" in server's time.
data ServerInfo
  = ServerInfo
      Uri
      -- ^ URI at which the server runs.
      DiffTime
      -- ^ Difference between server's and our time for synchronisation purposes.

data SubscriptionError
  = -- | Failure decoding a result item.
    DecodingSubscriptionError
      Int
      -- ^ Item index.
      DecodingError
      -- ^ Details of the decoding error.

-- |
-- Subscription to a single value.
data Subscription
  = Subscription
      Core.SubscribeRequestItem
      -- ^ Request parameters.
      (Decoder (IO Bool))
      -- ^ Value decoder mapped with handler.

-- * Operations

-- |
-- Establish and maintain a subscription communication channel.
--
-- Provides an abstraction over the series of @Subscription@, @SubscriptionPolledRefresh@ and @SubscriptionCancel@
-- requests as per the protocol spec.
--
-- Automatically handles the incidental subscription invalidation by the server
-- by processing the server-reported errors, reestablishing the subscription,
-- working with buffers, synchronising the clock and etc.
-- To the user it all looks as if the subscription is running smoothly with no interrupts.
subscribe ::
  -- |
  -- URI at which the server runs.
  Uri ->
  -- |
  -- Request timeout.
  -- Controls how long we wait for the server to produce a batch of new values
  -- before reissuing the request.
  -- It also affects how long it will take to detect server interrupts.
  -- So there is a tradeoff between the frequency of requests and
  -- the agility in handling server and network failures.
  -- 
  -- Unless you have a clear reason not to just use 'def' here.
  RequestTimeout ->
  -- |
  -- Locale for the server to use.
  -- 
  -- You can use 'def' here.
  Locale ->
  -- |
  -- Subscription list.
  --
  -- Allows to establish multiple subscriptions using just a single communication channel.
  --
  -- When you need to subscribe to multiple events,
  -- prefer providing a list of subscriptions instead of executing this function multiple times.
  -- This will reduce the amount of traffic.
  [Subscription] ->
  -- |
  -- IO action, which blocks for as long as the whole communication channel is active.
  --
  -- When a subscription error happens, it returns it in 'Just'.
  -- When the channel gets closed at the user's will, it produces 'Nothing'.
  IO (Maybe SubscriptionError)
subscribe (Uri uri) (RequestTimeout requestTimeout) (Locale localeId) =
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
subscription ::
  -- |
  -- Deadband.
  --
  -- Specifies the percentage of full engineering unit range of the value that must change to trigger the handler.
  -- Only applies to analog (integer or float) types and arrays of them.
  -- In case of arrays the entire array is returned if any array element exceeds the deadband threshold.
  -- 
  -- You can use 'def' here.
  Deadband ->
  -- |
  -- Rate at which the server should check for value changes.
  -- 
  -- You can use 'def' here.
  SamplingRate ->
  -- |
  -- List of alternative decoders of the value.
  --
  -- If the value produced is of a type that is not expected by any of them,
  -- an error will be raised and the whole running channel of communication will be closed.
  -- The same will happen when the server encodes the values in some unexpected syntax.
  [Decoder value] ->
  -- |
  -- Value handler.
  -- An action to be called on each received value with its metadata,
  -- producing a boolean value,
  -- where 'False' signals to close the channel of communication which this subscription is being executed on.
  --
  -- You can use this boolean value to cancel the subscriptions gracefully.
  -- Do keep in mind though that all the subscriptions executed in the same 'subscribe' operation will be cancelled as well.
  (Core.OpcQuality -> Maybe value -> IO Bool) ->
  Subscription
subscription =
  error "TODO"

-- * Decoders

-- |
-- Value decoding error.
data DecodingError
  = ContentDecodingError
      Text
      -- ^ Content that we've failed to parse.
      Text
      -- ^ Details of the error.
  | ElementDecodingError
      Xml.Element
      -- ^ Element that we've failed to parse.
      Text
      -- ^ Details of the error.

-- |
-- Decoder of a value produced by the server.
--
-- Used for subscriptions and reads.
data Decoder value
  = Decoder
      Text
      -- ^ Expected type namespace.
      Text
      -- ^ Expected type name.
      (Xml.Element -> Either DecodingError value)
      -- ^ Element parsing function.
  deriving (Functor)

-- | Decoder of the @int@ primitive XSD type.
intDecoder :: Decoder Int
intDecoder =
  contentDecoder xsdNamespace "int" Atto.decimal

-- | Decoder of the @float@ primitive XSD type.
floatDecoder :: Decoder Double
floatDecoder =
  contentDecoder xsdNamespace "float" Atto.double

opcQualityDecoder :: Decoder Core.OpcQuality
opcQualityDecoder =
  Decoder opcXmlDaNamespace "OPCQuality" (error "TODO")

-- |
-- Textual content decoder using attoparsec.
contentDecoder :: Text -> Text -> Atto.Parser value -> Decoder value
contentDecoder typeNamespace typeName contentParser =
  Decoder typeNamespace typeNamespace $ \(Xml.Element _ _ nodes) -> case nodes of
    [singleNode] -> case singleNode of
      Xml.NodeContent content -> case Atto.parseOnly (contentParser <* Atto.endOfInput) content of
        Right success -> Right success
        Left details -> Left (ContentDecodingError content (fromString details))
      _ -> Left $ error "TODO"
    _ -> error "TODO"

-- |
-- Decoder for vendor-specific values not covered by the OPC XML-DA standard.
--
-- As per the spec, all vendor types must be namespaced under vendor-specific URI.
customDecoder ::
  -- |
  -- Expected value type XML namespace.
  Uri ->
  -- |
  -- Expected value type name.
  Text ->
  -- |
  -- Parser of an XML element representing the value.
  (Xml.Element -> Either Text value) ->
  Decoder value
customDecoder =
  error "TODO"

-- * Namespaces

xsdNamespace = "http://www.w3.org/2001/XMLSchema"

xsiNamespace = "http://www.w3.org/2001/XMLSchema-instance"

opcXmlDaNamespace = "http://opcfoundation.org/webservices/XMLDA/1.0/"
