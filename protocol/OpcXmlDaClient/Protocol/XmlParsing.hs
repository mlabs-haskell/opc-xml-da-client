module OpcXmlDaClient.Protocol.XmlParsing where

import qualified Attoparsec.Data as AttoparsecData
import qualified Data.Attoparsec.Text as Atto
import OpcXmlDaClient.Base.Prelude hiding (Read)
import OpcXmlDaClient.Protocol.Types
import qualified Text.XML as Xml
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
  attributesByName $ do
    _rcvTime <- byName Nothing "RcvTime" dateTimeContent
    _replyTime <- byName Nothing "ReplyTime" dateTimeContent
    _clientRequestHandle <- optional $ byName Nothing "ClientRequestHandle" textContent
    _revisedLocaleID <- optional $ byName Nothing "RevisedLocaleID" textContent
    _serverState <- byName Nothing "ServerState" serverStateContent
    return $ ReplyBase _rcvTime _replyTime _clientRequestHandle _revisedLocaleID _serverState

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
          _value <- optional $ byName (Just opcNs) "Value" $ value
          _opcQuality <- optional $ byName (Just opcNs) "Quality" $ opcQuality
          return (ItemValue _diagnosticInfo _value _opcQuality _valueTypeQualifier _itemPath _itemName _clientItemHandle _timestamp _resultId)

value :: Element Value
value =
  join $
    attributesByName $ do
      _type <- byName (Just xsiNs) "type" adaptedQNameContent
      return $ do
        Xml.Element _ _ _nodes <- astElement
        return $ Value _type _nodes

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
  attoparsedContent AttoparsecData.utcTimeInISO8601

qualityBitsContent :: Content QualityBits
qualityBitsContent =
  enumContent
    [ ("bad", #bad),
      ("badConfigurationError", #badConfigurationError),
      ("badNotConnected", #badNotConnected),
      ("badDeviceFailure", #badDeviceFailure),
      ("badSensorFailure", #badSensorFailure),
      ("badLastKnownValue", #badLastKnownValue),
      ("badCommFailure", #badCommFailure),
      ("badOutOfService", #badOutOfService),
      ("badWaitingForInitialData", #badWaitingForInitialData),
      ("uncertain", #uncertain),
      ("uncertainLastUsableValue", #uncertainLastUsableValue),
      ("uncertainSensorNotAccurate", #uncertainSensorNotAccurate),
      ("uncertainEUExceeded", #uncertainEUExceeded),
      ("uncertainSubNormal", #uncertainSubNormal),
      ("good", #good),
      ("goodLocalOverride", #goodLocalOverride)
    ]

limitBitsContent :: Content LimitBits
limitBitsContent =
  enumContent
    [ ("none", #none),
      ("low", #low),
      ("high", #high),
      ("constant", #constant)
    ]

unsignedByteContent :: Content Word8
unsignedByteContent =
  attoparsedContent AttoparsecData.lenientParser

serverStateContent :: Content ServerState
serverStateContent =
  enumContent
    [ ("running", #running),
      ("failed", #failed),
      ("noConfig", #noConfig),
      ("suspended", #suspended),
      ("test", #test),
      ("commFault", #commFault)
    ]

-- * Attributes

isNil :: ByName Content Bool
isNil =
  byName (Just xsiNs) "nil" (attoparsedContent AttoparsecData.bool) <|> pure False

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
