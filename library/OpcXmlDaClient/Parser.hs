{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module OpcXmlDaClient.Parser where

-- import qualified Data.XML.Types as Xml

import Attoparsec.Time.Text (utcTimeInISO8601)
import Control.Exception.Lens
import Control.Lens hiding (element)
import Data.Attoparsec.Text (parseOnly)
import Data.Constraint
import Data.Kind
import qualified Data.Map as Map
import qualified Data.Text as Text
import Data.Text.Lazy.Lens (packed)
import Data.Text.Lens hiding (text)
import OpcXmlDaClient.Prelude hiding (Read)
import OpcXmlDaClient.Types
import Shower
import qualified Text.Read as Text
import qualified Text.XML as XML
import Text.XML.Cursor
import qualified Text.XML.Cursor as Cursor
import Text.XML.Lens
import qualified Prelude

-- | Filter map by some predicate and safely extract the first occurrence
-- that maches the predicate applied to key
findByPartialKey :: (k -> Bool) -> Map k a -> Maybe a
findByPartialKey pred map = map ^.. ifolded . indices pred & mbSingleton

lookupAttribute :: Text -> Map Name a -> Maybe a
lookupAttribute attrName = findByPartialKey do (== attrName) . view _nameLocalName

lookupAttribute' :: Text -> Map Name a -> Either ParseError a
lookupAttribute' attrName attributes =
  lookupAttribute attrName attributes
    & maybe
      do Left . OtherParseError $ "\"" <> attrName <> "\" not found"
      pure

-- | Run specific parser and extract error it returns.
--
-- This functions helps to reduce
--
-- @
-- newtype T = T { t :: Maybe Int }
--
-- g :: Int -> Either String Int
-- g _ = Left "g"
--
-- h :: Maybe Int -> Either String T
-- h x = do
--   t <- maybe (pure Nothing) (sequence . Just . g) x
--   pure T { t }
-- @
--
-- pattern
tryParse :: Monad m => (a -> m b) -> Maybe a -> m (Maybe b)
tryParse parser = maybe (pure Nothing) (sequence . pure . parser)

withSOAPElement ::
  -- | element name to match
  Name ->
  -- | type of the element
  Maybe Text ->
  -- | error handling if the element is not found
  (XML.Element -> a) ->
  -- | callback that continues the execution if the name is found
  (Map Name Text -> [Node] -> Either a b) ->
  -- | element to look up children in
  XML.Element ->
  Either a b
withSOAPElement name mbGivenType errorHandler callback = \case
  Element
    { elementName,
      elementAttributes,
      elementNodes
    }
      | nameLocalName elementName == nameLocalName name,
        Just givenType <- mbGivenType,
        Just elementType <- lookupAttribute "type" elementAttributes,
        givenType == elementType ->
        callback elementAttributes elementNodes
  Element
    { elementName,
      elementAttributes,
      elementNodes
    }
      | nameLocalName elementName == nameLocalName name ->
        callback elementAttributes elementNodes
  el -> Left do errorHandler el

lookupElement :: Text -> [Node] -> Maybe XML.Element
lookupElement name nodes =
  (\(NodeElement e) -> e) <$> mbSingleton do
    filter
      \case
        NodeElement Element {elementName} -> nameLocalName elementName == name
        _ -> False
      nodes

lookupElement' :: Text -> [Node] -> Either ParseError XML.Element
lookupElement' name nodes =
  lookupElement name nodes
    & maybe
      do Left . OtherParseError $ "\"" <> name <> "\" node is not found"
      pure

-- | Construct a vector of successfully parsed elements
-- and fail if one of them fails
-- TODO: test it
tryParseMany ::
  (XML.Element -> Either ParseError b) ->
  [XML.Node] ->
  Either ParseError (Vector b)
tryParseMany parser =
  foldr' (curry matchParsed) (pure mempty)
  where
    matchParsed = \case
      (NodeElement e, Right acc) -> case parser e of
        Left err -> Left err
        Right e' -> Right do cons e' acc
      (_, acc) -> acc

--------------------------------------------------------------------------------
-- Parser
--------------------------------------------------------------------------------

type XMLAttrs = Map XML.Name Text

data ParseError
  = GetStatusResponseParseError XML.Element
  | -- | Attributes that are failed to parse
    ReplyBaseParseError XMLAttrs
  | ServerStateParseError Text
  | ServerStatusParseError XML.Element
  | InterfaceVersionParseError Text
  | ReadParseError XML.Element
  | RequestOptionsParseError XML.Element
  | ReadRequestItemListParseError XML.Element
  | ReadRequestItemParseError XML.Element
  | ReadResponseParseError XML.Element
  | ReplyItemListParseError XML.Element
  | ItemValueParseError XML.Element
  | OpcQualityParseError XML.Element
  | QualityBitsParseError Text
  | LimitBitsParseError Text
  | OpcErrorParseError XML.Element
  | ArrayOfFloatParseError XML.Element
  | ArrayOfIntParseError XML.Element
  | ArrayOfUnsignedIntParseError XML.Element
  | ArrayOfLongParseError XML.Element
  | ArrayOfUnsignedLongParseError XML.Element
  | ArrayOfDoubleParseError XML.Element
  | ArrayOfStringParseError XML.Element
  | ArrayOfDateTimeParseError XML.Element
  | ArrayOfAnyTypeParseError XML.Element
  | ArrayOfDecimalParseError XML.Element
  | ArrayOfByteParseError XML.Element
  | ArrayOfShortParseError XML.Element
  | WriteParseError XML.Element
  | WriteRequestItemListParseError XML.Element
  | WriteResponseParseError XML.Element
  | SubscribeParseError XML.Element
  | SubscribeRequestItemListParseError XML.Element
  | SubscribeRequestItemParseError XML.Element
  | SubscribeReplyItemListParseError XML.Element
  | SubscribeResponseParseError XML.Element
  | SubscribeItemValueParseError XML.Element
  | SubscriptionPolledRefreshParseError XML.Element
  | SubscribePolledRefreshReplyItemListParseError XML.Element
  | SubscriptionPolledRefreshResponseParseError XML.Element
  | SubscriptionCancelParseError XML.Element
  | SubscriptionCancelResponseParseError XML.Element
  | BrowseParseError XML.Element
  | BrowseFilterParseError XML.Element
  | BrowseElementParseError XML.Element
  | ItemPropertyParseError XML.Element
  | BrowseResponseParseError XML.Element
  | GetPropertiesParseError XML.Element
  | ItemIdentifierParseError XML.Element
  | PropertyReplyListParseError XML.Element
  | GetPropertiesResponseParseError XML.Element
  | DateTimeParseError Text
  | OtherParseError Text
  deriving stock (Eq, Show)

type Parser a = XML.Element -> Either ParseError a

parseGetStatusResponse :: Parser GetStatusResponse
parseGetStatusResponse = \case
  Element {elementName = s} ->
    error "TODO"

-- ReplyBase:
-- product:
--   rcvTime: DateTime
--   replyTime: DateTime
--   clientRequestHandle: Maybe Text
--   revisedLocaleId: Maybe Text
--   serverState: ServerState
parseReplyBase :: XMLAttrs -> Either ParseError ReplyBase
parseReplyBase attributes
  | Just rcvTime <- lookupAttribute "RcvTime" attributes,
    Just replyTime <- lookupAttribute "ReplyTime" attributes,
    _clientRequestHandle <- lookupAttribute "ClientRequestHandle" attributes,
    let _revisedLocaleId = lookupAttribute "RevisedLocaleId" attributes,
    Just serverState <- lookupAttribute "ServerState" attributes =
    do
      _serverState <- parseServerState serverState
      _rcvTime <- parseDateTime rcvTime
      _replyTime <- parseDateTime replyTime
      pure
        ReplyBase
          { _rcvTime,
            _replyTime,
            _clientRequestHandle,
            _revisedLocaleId,
            _serverState
          }
parseReplyBase attributes = Left $ ReplyBaseParseError attributes

parseServerState :: Text -> Either ParseError ServerState
parseServerState = \case
  "running" -> pure RunningServerState
  "failed" -> pure FailedServerState
  "noConfig" -> pure NoConfigServerState
  "suspended" -> pure SuspendedServerState
  "test" -> pure TestServerState
  "commFault" -> pure CommFaultServerState
  s -> Left $ ServerStateParseError s

parseServerStatus :: Parser ServerStatus
parseServerStatus = error "TODO"

-- |
-- "xxx_yyy_zzz_2_1_0" -> InterfaceVersion { _minor = 0, _major = 1 }
parseInterfaceVersion :: Text -> Either ParseError InterfaceVersion
parseInterfaceVersion v
  | let matchVersions =
          fmap do readMaybe @Int . Text.unpack
            >>> takeFromEnd 2
            >>> \case
              [Just _minor, Just _major] -> Just (_minor, _major)
              _ -> Nothing,
    Just (_major, _minor) <-
      v
        & Text.strip
        & Text.splitOn "_"
        & matchVersions =
    pure InterfaceVersion {_major, _minor}
parseInterfaceVersion v = Left do InterfaceVersionParseError v

parseRead :: Parser Read
parseRead = error "TODO"

parseRequestOptions :: Parser RequestOptions
parseRequestOptions = error "TODO"

parseReadRequestItemList :: Parser ReadRequestItemList
parseReadRequestItemList = error "TODO"

parseReadRequestItem :: Parser ReadRequestItem
parseReadRequestItem = error "TODO"

parseReadResponse :: Parser ReadResponse
parseReadResponse = error "TODO"

parseReplyItemList :: Parser ReplyItemList
parseReplyItemList = error "TODO"

-- ItemValue:
--   product:
--     diagnosticInfo: Maybe Text
--     value: Maybe Xml.Element
--     quality: Maybe OpcQuality
--     valueTypeQualifier: Maybe Xml.Name
--     itemPath: Maybe Text
--     itemName: Maybe Text
--     clientItemHandle: Maybe Text
--     timestamp: Maybe DateTime
--     resultId: Maybe Xml.Name
parseItemValue :: Parser ItemValue
parseItemValue =
  withSOAPElement
    "ItemValue"
    Nothing
    ItemValueParseError
    \attributes nodes -> do
      let _diagnosticInfo = lookupAttribute "DiagnosticInfo" attributes
          _value = lookupElement "Value" nodes
          _valueTypeQualifier = Nothing -- TODO
          _itemPath = lookupAttribute "ItemPath" attributes
          _itemName = lookupAttribute "ItemName" attributes
          _clientItemHandle = lookupAttribute "ClientItemHandle" attributes
          _resultId = Nothing -- TODO
          timestamp = lookupAttribute "Timestamp" attributes
          quality = lookupElement "OpcQuality" nodes
      _quality <- tryParse parseOpcQuality quality
      _timestamp <- tryParse parseDateTime timestamp
      pure
        ItemValue
          { _diagnosticInfo,
            _value,
            _quality,
            _valueTypeQualifier,
            _itemPath,
            _itemName,
            _clientItemHandle,
            _timestamp,
            _resultId
          }

-- OpcQuality:
--   product:
--     qualityField: QualityBits
--     limitField: LimitBits
--     vendorField: Word8
parseOpcQuality :: Parser OpcQuality
parseOpcQuality = withSOAPElement
  "Quality"
  (Just "OPCQuality")
  OpcQualityParseError
  \attributes nodes -> do
    _qualityField <-
      lookupAttribute' "QualityField" attributes >>= parseQualityBits
    _limitField <-
      lookupAttribute' "LimitField" attributes >>= parseLimitBits
    _vendorField <-
      lookupAttribute' "VendorField" attributes
        >>= maybe
          do Left $ OtherParseError "Cannot parse \"VendorField\" attribute value, not a `Word8`"
          pure
          . readMaybe @Word8
          . Text.unpack
    pure
      OpcQuality
        { _qualityField,
          _limitField,
          _vendorField
        }

-- QualityBits:
--   enum:
--     - bad
--     - badConfigurationError
--     - badNotConnected
--     - badDeviceFailure
--     - badSensorFailure
--     - badLastKnownValue
--     - badCommFailure
--     - badOutOfService
--     - badWaitingForInitialData
--     - uncertain
--     - uncertainLastUsableValue
--     - uncertainSensorNotAccurate
--     - uncertainEUExceeded
--     - uncertainSubNormal
--     - good
--     - goodLocalOverride
parseQualityBits :: Text -> Either ParseError QualityBits
parseQualityBits = \case
  "bad" -> pure BadQualityBits
  "badConfigurationError" -> pure BadConfigurationErrorQualityBits
  "badNotConnected" -> pure BadNotConnectedQualityBits
  "badDeviceFailure" -> pure BadDeviceFailureQualityBits
  "badSensorFailure" -> pure BadDeviceFailureQualityBits
  "badLastKnownValue" -> pure BadLastKnownValueQualityBits
  "badCommFailure" -> pure BadCommFailureQualityBits
  "badOutOfService" -> pure BadOutOfServiceQualityBits
  "uncertain" -> pure UncertainQualityBits
  "uncertainLastUsableValue" -> pure UncertainLastUsableValueQualityBits
  "uncertainSensorNotAccurate" -> pure UncertainSensorNotAccurateQualityBits
  "uncertainEUExceeded" -> pure UncertainEUExceededQualityBits
  "uncertainSubNormal" -> pure UncertainSubNormalQualityBits
  "good" -> pure GoodQualityBits
  "goodLocalOverride" -> pure GoodLocalOverrideQualityBits
  given -> Left do QualityBitsParseError given

-- LimitBits:
--   enum:
--     - none
--     - low
--     - high
--     - constant
parseLimitBits :: Text -> Either ParseError LimitBits
parseLimitBits = \case
  "none" -> pure NoneLimitBits
  "low" -> pure LowLimitBits
  "high" -> pure HighLimitBits
  "constant" -> pure ConstantLimitBits
  given -> Left do LimitBitsParseError given

parseOpcError :: Parser OpcError
parseOpcError = error "TODO"

parseArrayOfFloat :: Parser ArrayOfFloat
parseArrayOfFloat = error "TODO"

parseArrayOfInt :: Parser ArrayOfInt
parseArrayOfInt = error "TODO"

parseArrayOfUnsignedInt :: Parser ArrayOfUnsignedInt
parseArrayOfUnsignedInt = error "TODO"

parseArrayOfLong :: Parser ArrayOfLong
parseArrayOfLong = error "TODO"

parseArrayOfUnsignedLong :: Parser ArrayOfUnsignedLong
parseArrayOfUnsignedLong = error "TODO"

parseArrayOfDouble :: Parser ArrayOfDouble
parseArrayOfDouble = error "TODO"

parseArrayOfString :: Parser ArrayOfString
parseArrayOfString = error "TODO"

parseArrayOfDateTime :: Parser ArrayOfDateTime
parseArrayOfDateTime = error "TODO"

parseArrayOfAnyType :: Parser ArrayOfAnyType
parseArrayOfAnyType = error "TODO"

parseArrayOfDecimal :: Parser ArrayOfDecimal
parseArrayOfDecimal = error "TODO"

parseArrayOfByte :: Parser ArrayOfByte
parseArrayOfByte = error "TODO"

parseArrayOfShort :: Parser ArrayOfShort
parseArrayOfShort = error "TODO"

parseWrite :: Parser Write
parseWrite = error "TODO"

parseWriteRequestItemList :: Parser WriteRequestItemList
parseWriteRequestItemList = error "TODO"

parseWriteResponse :: Parser WriteResponse
parseWriteResponse = error "TODO"

parseSubscribe :: Parser Subscribe
parseSubscribe = error "TODO"

parseSubscribeRequestItemList :: Parser SubscribeRequestItemList
parseSubscribeRequestItemList = error "TODO"

parseSubscribeRequestItem :: Parser SubscribeRequestItem
parseSubscribeRequestItem = error "TODO"

-- SubscribeItemValue:
--   product:
--     itemValue: ItemValue
--     revisedSamplingRate: Maybe Int
parseSubscribeItemValue :: Parser SubscribeItemValue
parseSubscribeItemValue = withSOAPElement
  "Items"
  (Just "SubscribeItemValue")
  SubscribeItemValueParseError
  \attributes nodes -> do
    let itemValueNode = lookupElement "ItemValue" nodes
        _revisedSamplingRate =
          lookupAttribute "RevisedSamplingRate" attributes
            >>= readMaybe @Int . Text.unpack
    _itemValue <-
      tryParse parseItemValue itemValueNode
        >>= maybe
          do Left $ OtherParseError "required element \"ItemValue\" is not found"
          pure
    pure
      SubscribeItemValue
        { _itemValue,
          _revisedSamplingRate
        }

-- SubscribeReplyItemList:
--   product:
--     items: Vector SubscribeItemValue
--     revisedSamplingRate: Maybe Int32
-- TODO: guard on xsi:type="SubscribeReplyItemList"
parseSubscribeReplyItemList :: Parser SubscribeReplyItemList
parseSubscribeReplyItemList =
  withSOAPElement
    "RItemList"
    Nothing
    ReplyItemListParseError
    \attributes nodes -> do
      let _revisedSamplingRate =
            lookupAttribute "RevisedSamplingRate" attributes
              >>= readMaybe @Int32 . Text.unpack
      _items <- tryParseMany parseSubscribeItemValue nodes
      pure
        SubscribeReplyItemList
          { _items,
            _revisedSamplingRate
          }

-- SubscribeResponse:
--   product:
--     subscribeResult: Maybe ReplyBase
--     rItemList: Maybe SubscribeReplyItemList
--     errors: Vector OpcError
--     serverSubHandle: Maybe Text
parseSubscribeResponse :: Parser SubscribeResponse
parseSubscribeResponse =
  withSOAPElement
    "SubscribeResponse"
    Nothing
    SubscribeResponseParseError
    \attributes nodes -> do
      let _serverSubHandle = lookupAttribute "ServerSubHandle" attributes
          _errors = error "TODO"
      _subscribeResult <- sequence do
        Element {elementAttributes} <- lookupElement "SubscribeResult" nodes
        pure do parseReplyBase elementAttributes
      _rItemList <- sequence do
        element <- lookupElement "RItemList" nodes
        pure do parseSubscribeReplyItemList element
      pure
        SubscribeResponse
          { _serverSubHandle,
            _subscribeResult,
            _rItemList,
            _errors = mempty
          }

parseSubscriptionPolledRefresh :: Parser SubscriptionPolledRefresh
parseSubscriptionPolledRefresh = error "TODO"

-- Note, that it takes attributes only for one element for item list.
-- For an example of how it's called you can refer to @parseSubscriptionPolledRefreshResponse@
parseSubscribePolledRefreshReplyItemList :: XMLAttrs -> Either ParseError SubscribePolledRefreshReplyItemList
parseSubscribePolledRefreshReplyItemList = error "TODO"

parseSubscriptionPolledRefreshResponse :: Parser SubscriptionPolledRefreshResponse
parseSubscriptionPolledRefreshResponse =
  withSOAPElement
    "SubscriptionPolledRefreshResponse"
    Nothing
    SubscriptionPolledRefreshParseError
    \attributes nodes -> do
      let mbSubscriptionPolledRefreshResult = lookupElement "SubscriptionPolledRefreshResult" nodes
          _invalidServerSubHandles = error "TODO"
          _errors = error "TODO"
          dataBufferOverflow = lookupAttribute "DataBufferOverflow" attributes
      _dataBufferOverflow <- case dataBufferOverflow of
        Nothing -> Left do OtherParseError "Cannot find DataBufferOverflow attribute"
        Just (Text.unpack -> x : xs) -> case toUpper x : xs & readMaybe @Bool of
          Nothing -> Left do OtherParseError "Malformed value passed to DataBufferOverflow attribute"
          Just v -> pure v
      _rItemList <-
        tryParseMany
          ( withSOAPElement
              "SubscriptionPolledRefreshResult"
              Nothing
              (const do OtherParseError "SubscriptionPolledRefreshResult error")
              \attributes _ -> parseSubscribePolledRefreshReplyItemList attributes
          )
          nodes
      -- _subscriptionPolledRefreshResult <- tryParse parseReplyBase mbSubscriptionPolledRefreshResult
      _subscriptionPolledRefreshResult <- error "TODO"
      pure
        SubscriptionPolledRefreshResponse
          { _subscriptionPolledRefreshResult,
            _invalidServerSubHandles,
            _rItemList,
            _errors,
            _dataBufferOverflow
          }

--
-- SubscriptionCancel:
--   product:
--     serverSubHandle: Maybe Text
--     clientRequestHandle: Maybe Text
parseSubscriptionCancel :: Parser SubscriptionCancel
parseSubscriptionCancel = withSOAPElement
  "SubscriptionCancel"
  Nothing
  SubscriptionCancelParseError
  \attributes nodes -> do
    let _serverSubHandle = lookupAttribute "ServerSubHandle" attributes
        _clientRequestHandle = lookupAttribute "ClientRequestHandle" attributes
    pure
      SubscriptionCancel
        { _serverSubHandle,
          _clientRequestHandle
        }

parseSubscriptionCancelResponse :: Parser SubscriptionCancelResponse
parseSubscriptionCancelResponse = error "TODO"

parseBrowse :: Parser Browse
parseBrowse = error "TODO"

parseBrowseFilter :: Parser BrowseFilter
parseBrowseFilter = error "TODO"

parseBrowseElement :: Parser BrowseElement
parseBrowseElement = error "TODO"

parseItemProperty :: Parser ItemProperty
parseItemProperty = error "TODO"

parseBrowseResponse :: Parser BrowseResponse
parseBrowseResponse = error "TODO"

parseGetProperties :: Parser GetProperties
parseGetProperties = error "TODO"

parseItemIdentifier :: Parser ItemIdentifier
parseItemIdentifier = error "TODO"

parsePropertyReplyList :: Parser PropertyReplyList
parsePropertyReplyList = error "TODO"

-- GetPropertiesResponse:
-- product:
--   getPropertiesResult: Maybe ReplyBase
--   propertyLists: Vector PropertyReplyList
--   errors: Vector OpcError
parseGetPropertiesResponse :: Parser GetPropertiesResponse
parseGetPropertiesResponse = withSOAPElement
  "GetPropertiesResponse"
  Nothing
  GetPropertiesResponseParseError
  \attributes nodes -> do
    -- _getPropertiesResult <- tryParse parseReplyBase do lookupElement "GetPropertiesResult" nodes
    _getPropertiesResult <- error "TODO"
    _propertyLists <- tryParseMany parsePropertyReplyList nodes
    errors <- tryParseMany parseOpcError nodes
    pure
      GetPropertiesResponse
        { _getPropertiesResult,
          _propertyLists,
          _errors = errors
        }

parseDateTime :: Text -> Either ParseError DateTime
parseDateTime (parseOnly utcTimeInISO8601 -> parsed) = case parsed of
  Left (Text.pack -> err) -> Left do DateTimeParseError err
  Right _utcTime -> pure DateTime {_utcTime}

g :: IO XML.Document
g = XML.readFile def "/Users/awkure/work/mlabs/opc-xml-da-client/session/%2f(1)"