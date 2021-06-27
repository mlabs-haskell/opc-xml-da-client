module OpcXmlDaClient.XmlParsing where

import qualified Attoparsec.Time.Text as AttoparsecTime
import qualified Data.Attoparsec.Text as Atto
import OpcXmlDaClient.Prelude hiding (Read)
import OpcXmlDaClient.Types
import qualified VectorBuilder.Alternative as Vb
import XmlUnscrambler

-- * Responses

-- |
-- General response SOAP envelope parser.
response :: Maybe Text -> Text -> Element a -> Element a
response ns name parser = do
  elementNameIs (Just soapEnvNs) "Envelope"
  childrenByName $ byName (Just soapEnvNs) "Body" $ childrenByName $ byName ns name $ parser

getStatusResponse :: Element GetStatusResponse
getStatusResponse = error "TODO"

readResponse :: Element ReadResponse
readResponse = error "TODO"

writeResponse :: Element WriteResponse
writeResponse = error "TODO"

subscribeResponse :: Element SubscribeResponse
subscribeResponse =
  response (Just opcNs) "SubscribeResponse" $
    join $
      attributesByName $ do
        _subHandle <- optional $ byName Nothing "ServerSubHandle" $ textContent
        return $
          childrenByName $ do
            _subscribeResult <- optional $ byName (Just opcNs) "SubscribeResult" $ replyBase
            _rItemList <- optional $ byName (Just opcNs) "RItemList" $ subscribeReplyItemList
            _errors <- Vb.many $ byName (Just opcNs) "OPCError" $ opcError
            return $ SubscribeResponse _subscribeResult _rItemList _errors _subHandle

subscriptionPolledRefreshResponse :: Element SubscriptionPolledRefreshResponse
subscriptionPolledRefreshResponse = error "TODO"

subscriptionCancelResponse :: Element SubscriptionCancelResponse
subscriptionCancelResponse = error "TODO"

browseResponse :: Element BrowseResponse
browseResponse = error "TODO"

getPropertiesResponse :: Element GetPropertiesResponse
getPropertiesResponse = error "TODO"

-- * Details

replyBase :: Element ReplyBase
replyBase =
  error "TODO"

subscribeReplyItemList :: Element SubscribeReplyItemList
subscribeReplyItemList =
  join $
    attributesByName $ do
      _revisedSamplingRate <- optional $ byName Nothing "RevisedSamplingRate" $ attoparsedContent Atto.decimal
      return $ do
        _items <- childrenByName $ Vb.many $ byName (Just opcNs) "Items" $ subscribeItemValue
        return (SubscribeReplyItemList _items _revisedSamplingRate)

subscribeItemValue :: Element SubscribeItemValue
subscribeItemValue =
  join $
    attributesByName $ do
      _revisedSamplingRate <- optional $ byName Nothing "RevisedSamplingRate" $ attoparsedContent Atto.decimal
      return $ do
        _itemValue <- childrenByName $ byName (Just opcNs) "ItemValue" $ itemValue
        return (SubscribeItemValue _itemValue _revisedSamplingRate)

opcError :: Element OpcError
opcError =
  join $
    attributesByName $ do
      _id <- byName Nothing "ID" $ adaptedQNameContent
      return $
        childrenByName $ do
          _text <- optional $ byName (Just opcNs) "Text" $ children $ contentNode $ textContent
          return $ OpcError _text _id

itemValue :: Element ItemValue
itemValue =
  join $
    attributesByName $ do
      _valueTypeQualifier <- optional $ byName Nothing "ValueTypeQualifier" $ adaptedQNameContent
      _itemPath <- optional $ byName Nothing "ItemPath" $ textContent
      _itemName <- optional $ byName Nothing "ItemName" $ textContent
      _clientItemHandle <- optional $ byName Nothing "ClientItemHandle" $ textContent
      _timestamp <- optional $ byName Nothing "Timestamp" $ dateTimeContent
      _resultId <- optional $ byName Nothing "ResultID" $ adaptedQNameContent
      return $ do
        childrenByName $ do
          _diagnosticInfo <- optional $ byName (Just opcNs) "DiagnosticInfo" $ children $ contentNode $ textContent
          _value <- optional $ byName (Just opcNs) "Value" $ astElement
          _opcQuality <- optional $ byName (Just opcNs) "Quality" $ opcQuality
          return (ItemValue _diagnosticInfo _value _opcQuality _valueTypeQualifier _itemPath _itemName _clientItemHandle _timestamp _resultId)

opcQuality :: Element OpcQuality
opcQuality =
  attributesByName $ do
    _qualityField <- byName Nothing "QualityField" qualityBitsContent <|> pure #good
    _limitField <- byName Nothing "LimitField" limitBitsContent <|> pure #none
    _vendorField <- byName Nothing "VendorField" unsignedByteContent <|> pure 0
    return (OpcQuality _qualityField _limitField _vendorField)

-- * Content

adaptedQNameContent :: Content QName
adaptedQNameContent =
  qNameContent <&> \(ns, name) -> case ns of
    Just ns -> NamespacedQName ns name
    Nothing -> UnnamespacedQName name

dateTimeContent :: Content UTCTime
dateTimeContent =
  attoparsedContent AttoparsecTime.utcTimeInISO8601

qualityBitsContent :: Content QualityBits
qualityBitsContent =
  error "TODO"

limitBitsContent :: Content LimitBits
limitBitsContent =
  error "TODO"

unsignedByteContent :: Content Word8
unsignedByteContent =
  error "TODO"

-- * Namespaces

soapEnvNs :: Text =
  "http://schemas.xmlsoap.org/soap/envelope/"

soapEncNs :: Text =
  "http://schemas.xmlsoap.org/soap/encoding/"

xsiNs :: Text =
  "http://www.w3.org/2001/XMLSchema-instance"

xsdNs :: Text =
  "http://www.w3.org/2001/XMLSchema"

opcNs :: Text =
  "http://opcfoundation.org/webservices/XMLDA/1.0/"
