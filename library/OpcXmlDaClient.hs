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

    -- ** Operation parameter types
    Uri,
    textUri,
    RequestTimeout,
    millisecondsRequestTimeout,

    -- ** Operation errors
    Error (..),

    -- * Value types
    module OpcXmlDaClient.Protocol.Types,
    module OpcXmlDaClient.XmlSchemaValues.Types,
  )
where

import qualified Data.Text as Text
import qualified Network.HTTP.Client as Hc
import OpcXmlDaClient.Base.Prelude hiding (Read, read)
import OpcXmlDaClient.Protocol.Types
import qualified OpcXmlDaClient.Protocol.XmlConstruction as XmlConstruction
import qualified OpcXmlDaClient.Protocol.XmlParsing as XmlParsing
import OpcXmlDaClient.XmlSchemaValues.Types
import qualified XmlParser

-- * Operations

-- |
-- Alias to an HTTP request operation in the scope of
-- HTTP connection manager, timeout for the operation, URI of the server.
--
-- All errors are explicit and are wrapped by the 'Error' type.
type Op i o = Hc.Manager -> RequestTimeout -> Uri -> i -> IO (Either Error o)

getStatus :: Op GetStatus GetStatusResponse
getStatus = encDecOp XmlConstruction.getStatus XmlParsing.getStatusResponse

read :: Op Read ReadResponse
read = encDecOp XmlConstruction.read XmlParsing.readResponse

write :: Op Write WriteResponse
write = encDecOp XmlConstruction.write XmlParsing.writeResponse

subscribe :: Op Subscribe SubscribeResponse
subscribe = encDecOp XmlConstruction.subscribe XmlParsing.subscribeResponse

subscriptionPolledRefresh :: Op SubscriptionPolledRefresh SubscriptionPolledRefreshResponse
subscriptionPolledRefresh = encDecOp XmlConstruction.subscriptionPolledRefresh XmlParsing.subscriptionPolledRefreshResponse

subscriptionCancel :: Op SubscriptionCancel SubscriptionCancelResponse
subscriptionCancel = encDecOp XmlConstruction.subscriptionCancel XmlParsing.subscriptionCancelResponse

browse :: Op Browse BrowseResponse
browse = encDecOp XmlConstruction.browse XmlParsing.browseResponse

getProperties :: Op GetProperties GetPropertiesResponse
getProperties = encDecOp XmlConstruction.getProperties XmlParsing.getPropertiesResponse

encDecOp :: (i -> ByteString) -> XmlParser.Element (Either SoapFault o) -> Op i o
encDecOp encode decode manager (RequestTimeout timeout) (Uri request) input =
  request
    { Hc.method = "POST",
      Hc.requestBody = Hc.RequestBodyBS (encode input),
      Hc.responseTimeout = Hc.responseTimeoutMicro (timeout * 1000)
    }
    & \request -> do
      response <- try $ Hc.httpLbs request manager
      case response of
        Left exc
          | Just exc <- fromException @Hc.HttpException exc -> case exc of
            Hc.HttpExceptionRequest _ reason -> return $ Left $ HttpError reason
            Hc.InvalidUrlException uri reason -> error $ "Invalid URI: " <> uri <> ". " <> reason
          | Just exc <- fromException @IOException exc ->
            return $ Left $ IoError exc
          | otherwise -> throwIO exc
        Right response ->
          return $ case XmlParser.parseLazyByteString decode (Hc.responseBody response) of
            Right res -> case res of
              Right res -> Right res
              Left err -> Left $ SoapError err
            Left err -> Left $ ParsingError err

-- * Helper types

-- |
-- URI of the server.
newtype Uri = Uri Hc.Request

-- |
-- Construct a correct URI by validating a textual value.
textUri :: Text -> Maybe Uri
textUri = fmap Uri . Hc.parseRequest . Text.unpack

newtype RequestTimeout = RequestTimeout Int

-- |
-- RequestTimeout of 30 seconds.
instance Default RequestTimeout where
  def = RequestTimeout 30000

-- |
-- Construct a request timeout value,
-- ensuring that it's in the proper range.
millisecondsRequestTimeout :: Int -> Maybe RequestTimeout
millisecondsRequestTimeout x =
  if x >= 0
    then Just $ RequestTimeout x
    else Nothing

-- * Errors

-- |
-- Error during the execution of an operation.
data Error
  = HttpError Hc.HttpExceptionContent
  | IoError IOException
  | ParsingError Text
  | SoapError SoapFault

instance Show Error where
  show = \case
    HttpError a -> showString "HTTP error: " $ show a
    IoError a -> showString "IO error: " $ show a
    ParsingError a -> showString "Parsing error: " $ Text.unpack a
    SoapError a ->
      "SOAP fault response with code: " <> show (#code a) <> ". "
        <> "Reason: "
        <> Text.unpack (#reason a)
