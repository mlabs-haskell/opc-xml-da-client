module OpcXmlDaClient.Protocol.XmlParsing where

import qualified Attoparsec.Data as AttoparsecData
import qualified Data.Attoparsec.Text as Atto
import qualified Data.ByteString.Base64 as Base64
import qualified Data.Text.Encoding as TextEncoding
import OpcXmlDaClient.Base.Prelude hiding (Read)
import qualified OpcXmlDaClient.Base.Vector as VectorUtil
import qualified OpcXmlDaClient.Protocol.Namespaces as Ns
import OpcXmlDaClient.Protocol.Types
import XmlParser

-- * Responses

-- |
-- General response SOAP envelope parser.
inSoapEnvelope :: Maybe Text -> Text -> Element a -> Element a
inSoapEnvelope ns name parser = do
  elementNameIs (Just Ns.soapEnv) "Envelope"
  childrenByName $ byName (Just Ns.soapEnv) "Body" $ childrenByName $ byName ns name $ parser

opcResponse :: Text -> Element a -> Element a
opcResponse = inSoapEnvelope (Just Ns.opc)

getStatusResponse :: Element GetStatusResponse
getStatusResponse =
  opcResponse "GetStatusResponse" $
    childrenByName $ do
      _getStatusResult <- optional $ byName (Just Ns.opc) "GetStatusResult" $ replyBase
      _status <- optional $ byName (Just Ns.opc) "Status" $ serverStatus
      return $ GetStatusResponse _getStatusResult _status

readResponse :: Element ReadResponse
readResponse =
  opcResponse "ReadResponse" $
    childrenByName $ do
      _readResult <- optional $ byName (Just Ns.opc) "ReadResult" $ replyBase
      _rItemList <- optional $ byName (Just Ns.opc) "RItemList" $ replyItemList
      _errors <- VectorUtil.many $ byName (Just Ns.opc) "Errors" $ opcError
      return $ ReadResponse _readResult _rItemList _errors

writeResponse :: Element WriteResponse
writeResponse =
  opcResponse "WriteResponse" $
    childrenByName $ do
      _writeResult <- optional $ byName (Just Ns.opc) "WriteResult" $ replyBase
      _rItemList <- optional $ byName (Just Ns.opc) "RItemList" $ replyItemList
      _errors <- VectorUtil.many $ byName (Just Ns.opc) "Errors" $ opcError
      return $ WriteResponse _writeResult _rItemList _errors

subscribeResponse :: Element SubscribeResponse
subscribeResponse =
  opcResponse "SubscribeResponse" $
    join $
      attributesByName $ do
        _subHandle <- optional $ byName Nothing "ServerSubHandle" $ textContent
        return $
          childrenByName $ do
            _subscribeResult <- optional $ byName (Just Ns.opc) "SubscribeResult" $ replyBase
            _rItemList <- optional $ byName (Just Ns.opc) "RItemList" $ subscribeReplyItemList
            _errors <- VectorUtil.many $ byName (Just Ns.opc) "OPCError" $ opcError
            return $ SubscribeResponse _subscribeResult _rItemList _errors _subHandle

subscriptionPolledRefreshResponse :: Element SubscriptionPolledRefreshResponse
subscriptionPolledRefreshResponse =
  opcResponse "SubscriptionPolledRefreshResponse" $
    join $
      childrenByName $ do
        _subscriptionPolledRefreshResult <- optional $ byName (Just Ns.opc) "SubscriptionPolledRefreshResult" $ replyBase
        _invalidServerSubHandles <- VectorUtil.many $ byName (Just Ns.opc) "InvalidServerSubHandles" $ children $ contentNode $ textContent
        _rItemList <- VectorUtil.many $ byName (Just Ns.opc) "RItemList" $ subscribePolledRefreshReplyItemList
        _errors <- VectorUtil.many $ byName (Just Ns.opc) "OPCError" $ opcError
        return $
          attributesByName $ do
            _dataBufferOverflow <- byName Nothing "DataBufferOverflow" booleanContent <|> pure False
            return $ SubscriptionPolledRefreshResponse _subscriptionPolledRefreshResult _invalidServerSubHandles _rItemList _errors _dataBufferOverflow

subscriptionCancelResponse :: Element SubscriptionCancelResponse
subscriptionCancelResponse =
  opcResponse "SubscriptionCancelResponse" $
    childrenByName $ do
      _clientRequestHandle <- optional $ byName (Just Ns.opc) "ClientRequestHandle" $ children $ contentNode $ textContent
      return $ SubscriptionCancelResponse _clientRequestHandle

browseResponse :: Element BrowseResponse
browseResponse =
  opcResponse "BrowseResponse" $
    join $
      childrenByName $ do
        _browseResult <- optional $ byName (Just Ns.opc) "BrowseResult" $ replyBase
        _elements <- VectorUtil.many $ byName (Just Ns.opc) "Elements" $ browseElement
        _errors <- VectorUtil.many $ byName (Just Ns.opc) "Errors" $ opcError
        return $
          attributesByName $ do
            _continuationPoint <- optional $ byName Nothing "ContinuationPoint" $ textContent
            _moreElements <- byName Nothing "MoreElements" booleanContent <|> pure False
            return $ BrowseResponse _browseResult _elements _errors _continuationPoint _moreElements

getPropertiesResponse :: Element GetPropertiesResponse
getPropertiesResponse =
  opcResponse "GetPropertiesResponse" $
    childrenByName $ do
      _getPropertiesResult <- optional $ byName (Just Ns.opc) "GetPropertiesResult" $ replyBase
      _propertiesList <- VectorUtil.many $ byName (Just Ns.opc) "PropertyLists" $ propertyReplyList
      _errors <- VectorUtil.many $ byName (Just Ns.opc) "Errors" $ opcError
      return $ GetPropertiesResponse _getPropertiesResult _propertiesList _errors

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
        _items <- childrenByName $ VectorUtil.many $ byName (Just Ns.opc) "Items" $ subscribeItemValue
        return (SubscribeReplyItemList _items _revisedSamplingRate)

subscribeItemValue :: Element SubscribeItemValue
subscribeItemValue =
  join $
    attributesByName $ do
      _revisedSamplingRate <- optional $ byName Nothing "RevisedSamplingRate" $ attoparsedContent Atto.decimal
      return $ do
        _itemValue <- childrenByName $ byName (Just Ns.opc) "ItemValue" $ itemValue
        return (SubscribeItemValue _itemValue _revisedSamplingRate)

opcError :: Element OpcError
opcError =
  join $
    attributesByName $ do
      _id <- byName Nothing "ID" $ adaptedQNameContent
      return $
        childrenByName $ do
          _text <- optional $ byName (Just Ns.opc) "Text" $ children $ contentNode $ textContent
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
          _diagnosticInfo <- optional $ byName (Just Ns.opc) "DiagnosticInfo" $ children $ contentNode $ textContent
          _value <- optional $ byName (Just Ns.opc) "Value" $ value
          _opcQuality <- optional $ byName (Just Ns.opc) "Quality" $ opcQuality
          return (ItemValue _diagnosticInfo _value _opcQuality _valueTypeQualifier _itemPath _itemName _clientItemHandle _timestamp _resultId)

value :: Element Value
value = error "TODO"

opcQuality :: Element OpcQuality
opcQuality =
  attributesByName $ do
    _qualityField <- byName Nothing "QualityField" qualityBitsContent <|> pure #good
    _limitField <- byName Nothing "LimitField" limitBitsContent <|> pure #none
    _vendorField <- byName Nothing "VendorField" unsignedByteContent <|> pure 0
    return (OpcQuality _qualityField _limitField _vendorField)

serverStatus :: Element ServerStatus
serverStatus =
  join $
    childrenByName $ do
      _statusInfo <- optional $ byName (Just Ns.opc) "StatusInfo" $ children $ contentNode $ textContent
      _vendorInfo <- optional $ byName (Just Ns.opc) "VendorInfo" $ children $ contentNode $ textContent
      _supportedLocaleIds <- VectorUtil.many $ byName (Just Ns.opc) "SupportedLocaleIDs" $ children $ contentNode $ textContent
      _supportedInterfaceVersions <- VectorUtil.many $ byName (Just Ns.opc) "SupportedInterfaceVersions" $ children $ contentNode $ textContent
      return $
        attributesByName $ do
          _startTime <- byName Nothing "StartTime" $ dateTimeContent
          _productVersion <- optional $ byName Nothing "ProductVersion" $ textContent
          return $ ServerStatus _statusInfo _vendorInfo _supportedLocaleIds _supportedInterfaceVersions _startTime _productVersion

replyItemList :: Element ReplyItemList
replyItemList =
  join $
    childrenByName $ do
      _items <- VectorUtil.many $ byName (Just Ns.opc) "Items" $ itemValue
      return $
        attributesByName $ do
          _reserved <- optional $ byName Nothing "Reserved" $ textContent
          return $ ReplyItemList _items _reserved

subscribePolledRefreshReplyItemList :: Element SubscribePolledRefreshReplyItemList
subscribePolledRefreshReplyItemList =
  join $
    childrenByName $ do
      _items <- VectorUtil.many $ byName (Just Ns.opc) "Items" $ itemValue
      return $
        attributesByName $ do
          _subscriptionHandle <- optional $ byName Nothing "SubscriptionHandle" $ textContent
          return $ SubscribePolledRefreshReplyItemList _items _subscriptionHandle

browseElement :: Element BrowseElement
browseElement =
  join $
    childrenByName $ do
      _properties <- VectorUtil.many $ byName (Just Ns.opc) "Properties" $ itemProperty
      return $
        attributesByName $ do
          _name <- optional $ byName Nothing "Name" textContent
          _itemPath <- optional $ byName Nothing "ItemPath" textContent
          _itemName <- optional $ byName Nothing "ItemName" textContent
          _isItem <- byName Nothing "IsItem" booleanContent
          _hasChildren <- byName Nothing "HasChildren" booleanContent
          return $ BrowseElement _properties _name _itemPath _itemName _isItem _hasChildren

itemProperty :: Element ItemProperty
itemProperty =
  join $
    childrenByName $ do
      _value <- optional $ byName (Just Ns.opc) "Value" $ value
      return $
        attributesByName $ do
          _name <- byName Nothing "Name" adaptedQNameContent
          _description <- optional $ byName Nothing "Description" textContent
          _itemPath <- optional $ byName Nothing "ItemPath" textContent
          _itemName <- optional $ byName Nothing "ItemName" textContent
          _resultId <- optional $ byName Nothing "ResultID" adaptedQNameContent
          return $ ItemProperty _value _name _description _itemPath _itemName _resultId

propertyReplyList :: Element PropertyReplyList
propertyReplyList = do
  join $
    childrenByName $ do
      _properties <- VectorUtil.many $ byName (Just Ns.opc) "Properties" $ itemProperty
      return $
        attributesByName $ do
          _itemPath <- optional $ byName Nothing "ItemPath" $ textContent
          _itemName <- optional $ byName Nothing "ItemName" $ textContent
          _resultId <- optional $ byName Nothing "ResultID" $ adaptedQNameContent
          return $ PropertyReplyList _properties _itemPath _itemName _resultId

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

booleanContent :: Content Bool
booleanContent = attoparsedContent AttoparsecData.bool

decimalContent :: Content Scientific
decimalContent = attoparsedContent AttoparsecData.lenientParser

base64BinaryContent :: Content ByteString
base64BinaryContent = refinedContent $ Base64.decodeBase64 . TextEncoding.encodeUtf8

-- * Attributes

isNil :: ByName Content Bool
isNil =
  byName (Just Ns.xsi) "nil" booleanContent <|> pure False

xsiType :: ByName Content QName
xsiType =
  byName (Just Ns.xsi) "type" adaptedQNameContent

-- * Value parsers

-- |
-- Parse array of any type by passing in a parser for elements
-- in the context of a QName of the element type.
arrayOfAnyType :: (QName -> ByName Element element) -> ByName Element (Vector (Maybe element))
arrayOfAnyType elementParser =
  VectorUtil.many $
    byName (Just Ns.opc) "anyType" $
      join $
        attributesByName $ do
          _isNil <- isNil
          if _isNil
            then return (return Nothing)
            else do
              _type <- xsiType
              return $ childrenByName $ fmap Just $ elementParser _type
