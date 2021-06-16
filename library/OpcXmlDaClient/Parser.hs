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
      (NodeElement e, Right acc) -> (`cons` acc) <$> parser e
      (_, acc) -> acc

tryParseManyValues ::
  (Text -> Either ParseError b) ->
  [XML.Node] ->
  Either ParseError (Vector b)
tryParseManyValues parser =
  foldr' (curry matchParsed) (pure mempty)
  where
    matchParsed = \case
      (NodeContent c, Right acc) -> (`cons` acc) <$> parser c
      (_, acc) -> acc

readAttribute' :: forall a. Prelude.Read a => Text -> XMLAttrs -> Either ParseError a
readAttribute' attr attrs = maybe
  (Left do OtherParseError $ "Cannot parse \"" <> attr <> "\" attribute")
  pure
  do lookupAttribute attr attrs >>= readMaybe @a . Text.unpack

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

-- GetStatusResponse:
--   product:
--     result: Maybe ReplyBase
--     status: Maybe ServerStatus
parseGetStatusResponse :: Parser GetStatusResponse
parseGetStatusResponse = withSOAPElement
  "GetStatusResponse"
  Nothing
  GetStatusResponseParseError
  \attributes nodes -> do
    _status <- sequence do
      r <- lookupElement "ServerStatus" nodes
      pure do parseServerStatus r
    _result <- sequence do
      Element {elementAttributes} <- lookupElement "SubscribeResult" nodes
      pure do parseReplyBase elementAttributes
    pure
      GetStatusResponse
        { _result,
          _status
        }

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

-- ServerState:
--   enum:
--     - running
--     - failed
--     - noConfig
--     - suspended
--     - test
--     - commFault
parseServerState :: Text -> Either ParseError ServerState
parseServerState = \case
  "running" -> pure RunningServerState
  "failed" -> pure FailedServerState
  "noConfig" -> pure NoConfigServerState
  "suspended" -> pure SuspendedServerState
  "test" -> pure TestServerState
  "commFault" -> pure CommFaultServerState
  s -> Left $ ServerStateParseError s

-- ServerStatus:
--   product:
--     statusInfo: Maybe Text
--     vendorInfo: Maybe Text
--     supportedLocaleIds: Vector Text
--     supportedInterfaceVersions: Vector InterfaceVersion
--     startTime: DateTime
--     productVersion: Maybe Text
parseServerStatus :: Parser ServerStatus
parseServerStatus = withSOAPElement
  "ServerStatus"
  Nothing
  ServerStatusParseError
  \attributes nodes -> do
    let _statusInfo = lookupAttribute "StatusInfo" attributes
        _vendorInfo = lookupAttribute "VendorInfo" attributes
        _productVersion = lookupAttribute "ProductVersion" attributes
        _supportedLocaleIds = error "TODO"
    -- _supportedLocaleIds <- tryParseMany
    --   ( withSOAPElement
    --     "SupportedLocaleIDs"
    --     Nothing
    --     (\el -> OtherParseError do "Cannot parse locale id element in server status: " <> Text.pack do show el )
    --     \attributes
    --   )
    _supportedInterfaceVersions <-
      sequence
        =<< tryParseMany
          ( withSOAPElement
              "SupportedInterfaceVersions"
              (Just "interfaceVersion")
              (\el -> OtherParseError do "Cannot parse interface version element: " <> Text.pack do { show el })
              \attributes _ -> do
                case lookupAttribute "Value" attributes of
                  Nothing -> Left do OtherParseError "Attribute \"Value\" in \"SupportedInterfaceVersions\" is absent"
                  Just attr -> pure do parseInterfaceVersion attr
          )
          nodes
    _startTime <- lookupAttribute' "StartTime" attributes >>= parseDateTime
    pure
      ServerStatus
        { _statusInfo,
          _vendorInfo,
          _supportedLocaleIds,
          _supportedInterfaceVersions,
          _startTime,
          _productVersion
        }

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

-- Read:
--   product:
--     options: Maybe RequestOptions
--     itemList: Maybe ReadRequestItemList
parseRead :: Parser Read
parseRead = withSOAPElement
  "Read"
  Nothing
  ReadParseError
  \attributes nodes -> do
    _options <- tryParse parseRequestOptions do lookupElement "Options" nodes
    _itemList <- tryParse parseReadRequestItemList do lookupElement "ItemList" nodes
    pure Read {_options, _itemList}

-- RequestOptions:
--   product:
--     returnErrorText: Bool
--     returnDiagnosticInfo: Bool
--     returnItemTime: Bool
--     returnItemPath: Bool
--     returnItemName: Bool
--     requestDeadline: Maybe DateTime
--     clientRequestHandle: Maybe Text
--     localeId: Maybe Text
parseRequestOptions :: Parser RequestOptions
parseRequestOptions = withSOAPElement
  "RequestOptions"
  Nothing
  RequestOptionsParseError
  \attributes nodes -> do
    _returnErrorText <- readAttribute' "ReturnErrorText" attributes
    _returnDiagnosticInfo <- readAttribute' "ReturnDiagnosticInfo" attributes
    _returnItemTime <- readAttribute' "ReturnItemTime" attributes
    _returnItemPath <- readAttribute' "ReturnItemPath" attributes
    _returnItemName <- readAttribute' "ReturnItemName" attributes
    _requestDeadline <- sequence do
      r <- lookupAttribute "RequestDeadline" attributes
      pure do parseDateTime r
    -- _returnErrorText <- maybe (Left $ OtherParseError "ss") pure mbReturnErrorText
    let _clientRequestHandle = lookupAttribute "ClientRequestHandle" attributes
        _localeId = lookupAttribute "LocaleId" attributes
    pure
      RequestOptions
        { _returnErrorText,
          _returnDiagnosticInfo,
          _returnItemTime,
          _returnItemPath,
          _returnItemName,
          _requestDeadline,
          _clientRequestHandle,
          _localeId
        }

-- ReadRequestItemList:
--   product:
--     items: Vector ReadRequestItem
--     itemPath: Maybe Text
--     reqType: Maybe Xml.Name
--     maxAge: Maybe Int32
parseReadRequestItemList :: Parser ReadRequestItemList
parseReadRequestItemList = withSOAPElement
  "ReadRequestItemList"
  Nothing
  ReadRequestItemListParseError
  \attributes nodes -> do
    _items <- tryParseMany parseReadRequestItem nodes
    let _itemPath = lookupAttribute "ItemPath" attributes
    -- _reqType <- lookupAttribute "ReqType" attributes
    _reqType <- error "TODO"
    _maxAge <- readAttribute' "MaxAge" attributes
    pure
      ReadRequestItemList
        { _items,
          _itemPath,
          _reqType,
          _maxAge
        }

-- ReadRequestItem:
--   product:
--     itemPath: Maybe Text
--     reqType: Maybe Xml.Name
--     itemName: Maybe Text
--     clientItemHandle: Maybe Text
--     maxAge: Maybe Int32
parseReadRequestItem :: Parser ReadRequestItem
parseReadRequestItem = withSOAPElement
  "Items"
  (Just "ReadRequestItem")
  ReadRequestItemParseError
  \attributes nodes -> do
    let _itemPath = lookupAttribute "ItemPath" attributes
        _reqType = error "TODO"
        _itemName = lookupAttribute "ItemName" attributes
        _clientItemHandle = lookupAttribute "ClientItemHandle" attributes
    _maxAge <- readAttribute' "MaxAge" attributes
    pure
      ReadRequestItem
        { _itemPath,
          _reqType,
          _itemName,
          _clientItemHandle,
          _maxAge
        }

-- ReadResponse:
--   product:
--     readResult: Maybe ReplyBase
--     rItemList: Maybe ReplyItemList
--     errors: Vector OpcError
parseReadResponse :: Parser ReadResponse
parseReadResponse = withSOAPElement
  "ReadResponse"
  Nothing
  ReadResponseParseError
  \attributes nodes -> do
    _readResult <- sequence do
      Element {elementAttributes} <- lookupElement "SubscribeResult" nodes
      pure do parseReplyBase elementAttributes
    _rItemList <- sequence do
      element <- lookupElement "RItemList" nodes
      pure do parseReplyItemList element
    _errors <- tryParseMany parseOpcError nodes
    pure
      ReadResponse
        { _readResult,
          _rItemList,
          _errors
        }

-- ReplyItemList:
--   product:
--     items: Vector ItemValue
--     reserved: Maybe Text
parseReplyItemList :: Parser ReplyItemList
parseReplyItemList = withSOAPElement
  "RItemList"
  (Just "ReplyItemList")
  ReplyItemListParseError
  \attributes nodes -> do
    let _reserved = lookupAttribute "Reserved" attributes
    _items <- tryParseMany parseItemValue nodes
    pure
      ReplyItemList
        { _items,
          _reserved
        }

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
          _valueTypeQualifier = error "TODO"
          _itemPath = lookupAttribute "ItemPath" attributes
          _itemName = lookupAttribute "ItemName" attributes
          _clientItemHandle = lookupAttribute "ClientItemHandle" attributes
          _resultId = error "TODO"
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

-- OpcError:
--   product:
--     text: Maybe Text
--     id: Xml.Name
parseOpcError :: Parser OpcError
parseOpcError = withSOAPElement
  "OpcError"
  Nothing
  OpcErrorParseError
  \attributes nodes -> do
    let _text = lookupAttribute "Text" attributes
        _id = error "TODO"
    pure
      OpcError
        { _text,
          _id
        }

-- ArrayOfFloat:
--   product:
--     float: Vector Float
parseArrayOfFloat :: Parser ArrayOfFloat
parseArrayOfFloat = withSOAPElement
  "ArrayOfFloat"
  Nothing
  ArrayOfFloatParseError
  \attributes nodes -> do
    _float <-
      tryParseManyValues
        do
          Text.unpack
            >>> readMaybe @Float
            >>> maybe
              do Left $ OtherParseError "Malformed value passed to the array of float"
              pure
        nodes
    pure ArrayOfFloat {_float}

parseArrayOfInt :: Parser ArrayOfInt
parseArrayOfInt = withSOAPElement
  "ArrayOfInt"
  Nothing
  ArrayOfIntParseError
  \attributes nodes -> do
    _int <-
      tryParseManyValues
        do
          Text.unpack
            >>> readMaybe @Int32
            >>> maybe
              do Left $ OtherParseError "Malformed value passed to the array of int"
              pure
        nodes
    pure ArrayOfInt {_int}

parseArrayOfUnsignedInt :: Parser ArrayOfUnsignedInt
parseArrayOfUnsignedInt = withSOAPElement
  "ArrayOfUnsignedInt"
  Nothing
  ArrayOfUnsignedIntParseError
  \attributes nodes -> do
    _unsignedInt <-
      tryParseManyValues
        do
          Text.unpack
            >>> readMaybe @Word32
            >>> maybe
              do Left $ OtherParseError "Malformed value passed to the array of unsigned int"
              pure
        nodes
    pure ArrayOfUnsignedInt {_unsignedInt}

parseArrayOfLong :: Parser ArrayOfLong
parseArrayOfLong = withSOAPElement
  "ArrayOfLong"
  Nothing
  ArrayOfLongParseError
  \attributes nodes -> do
    _long <-
      tryParseManyValues
        do
          Text.unpack
            >>> readMaybe @Int64
            >>> maybe
              do Left $ OtherParseError "Malformed value passed to the array of long"
              pure
        nodes
    pure ArrayOfLong {_long}

parseArrayOfUnsignedLong :: Parser ArrayOfUnsignedLong
parseArrayOfUnsignedLong = withSOAPElement
  "ArrayOfUnsignedLong"
  Nothing
  ArrayOfUnsignedLongParseError
  \attributes nodes -> do
    _unsignedLong <-
      tryParseManyValues
        do
          Text.unpack
            >>> readMaybe @Word64
            >>> maybe
              do Left $ OtherParseError "Malformed value passed to the array of unsigned long"
              pure
        nodes
    pure ArrayOfUnsignedLong {_unsignedLong}

parseArrayOfDouble :: Parser ArrayOfDouble
parseArrayOfDouble = withSOAPElement
  "ArrayOfDouble"
  Nothing
  ArrayOfDoubleParseError
  \attributes nodes -> do
    _double <-
      tryParseManyValues
        do
          Text.unpack
            >>> readMaybe @Double
            >>> maybe
              do Left $ OtherParseError "Malformed value passed to the array of double"
              pure
        nodes
    pure ArrayOfDouble {_double}

parseArrayOfDecimal :: Parser ArrayOfDecimal
parseArrayOfDecimal = withSOAPElement
  "ArrayOfDecimal"
  Nothing
  ArrayOfDecimalParseError
  \attributes nodes -> do
    _decimal <-
      tryParseManyValues
        do
          Text.unpack
            >>> readMaybe @Scientific
            >>> maybe
              do Left $ OtherParseError "Malformed value passed to the array of decimal"
              pure
        nodes
    pure ArrayOfDecimal {_decimal}

parseArrayOfShort :: Parser ArrayOfShort
parseArrayOfShort = withSOAPElement
  "ArrayOfShort"
  Nothing
  ArrayOfShortParseError
  \attributes nodes -> do
    _short <-
      tryParseManyValues
        do
          Text.unpack
            >>> readMaybe @Int16
            >>> maybe
              do Left $ OtherParseError "Malformed value passed to the array of short"
              pure
        nodes
    pure ArrayOfShort {_short}

parseArrayOfByte :: Parser ArrayOfByte
parseArrayOfByte = withSOAPElement
  "ArrayOfByte"
  Nothing
  ArrayOfByteParseError
  \attributes nodes -> do
    _byte <-
      tryParseManyValues
        do
          Text.unpack
            >>> readMaybe @Int8
            >>> maybe
              do Left $ OtherParseError "Malformed value passed to the array of byte"
              pure
        nodes
    pure ArrayOfByte {_byte}

parseArrayOfString :: Parser ArrayOfString
parseArrayOfString = withSOAPElement
  "ArrayOfString"
  Nothing
  ArrayOfStringParseError
  \attributes nodes -> do
    _string <-
      tryParseManyValues
        (pure . pure) -- TODO?
        nodes
    pure ArrayOfString {_string}

parseArrayOfDateTime :: Parser ArrayOfDateTime
parseArrayOfDateTime = withSOAPElement
  "ArrayOfDateTime"
  Nothing
  ArrayOfDateTimeParseError
  \attributes nodes -> do
    _dateTime <- tryParseManyValues parseDateTime nodes
    pure ArrayOfDateTime {_dateTime}

parseArrayOfAnyType :: Parser ArrayOfAnyType
parseArrayOfAnyType = withSOAPElement
  "ArrayOfAnyType"
  Nothing
  ArrayOfAnyTypeParseError
  \attributes nodes -> do
    _anyType <- tryParseMany (pure . pure) nodes -- TODO?
    pure ArrayOfAnyType {_anyType}

-- Write:
--   product:
--     options: Maybe RequestOptions
--     itemList: Maybe WriteRequestItemList
--     returnValuesOnReply: Bool
parseWrite :: Parser Write
parseWrite = withSOAPElement
  "Write"
  Nothing
  WriteParseError
  \attributes nodes -> do
    _options <- tryParse parseRequestOptions $ lookupElement "Options" nodes
    _itemList <- tryParse parseWriteRequestItemList $ lookupElement "ItemList" nodes
    _returnValuesOnReply <-
      lookupAttribute' "VendorField" attributes
        >>= maybe
          do Left $ OtherParseError "Cannot parse \"VendorField\" attribute value, not a `Word8`"
          pure
          . readMaybe @Bool
          . Text.unpack
    pure
      Write
        { _options,
          _itemList,
          _returnValuesOnReply
        }

-- WriteRequestItemList:
--   product:
--     items: Vector ItemValue
--     itemPath: Maybe Text
parseWriteRequestItemList :: Parser WriteRequestItemList
parseWriteRequestItemList = withSOAPElement
  "WriteRequestItemList"
  Nothing
  WriteRequestItemListParseError
  \attributes nodes -> do
    _items <- tryParseMany parseItemValue nodes
    let _itemPath = lookupAttribute "ItemPath" attributes
    pure
      WriteRequestItemList
        { _items,
          _itemPath
        }

parseWriteResponse :: Parser WriteResponse
parseWriteResponse = withSOAPElement
  "WriteResponse"
  Nothing
  WriteResponseParseError
  \attributes nodes -> do
    error "TODO"

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

-- SubscriptionPolledRefreshResponse:
--   product:
--     subscriptionPolledRefreshResult: Maybe ReplyBase
--     invalidServerSubHandles: Vector Text
--     rItemList: Vector SubscribePolledRefreshReplyItemList
--     errors: Vector OpcError
--     dataBufferOverflow: Bool
parseSubscriptionPolledRefreshResponse :: Parser SubscriptionPolledRefreshResponse
parseSubscriptionPolledRefreshResponse =
  withSOAPElement
    "SubscriptionPolledRefreshResponse"
    Nothing
    SubscriptionPolledRefreshParseError
    \attributes nodes -> do
      let mbSubscriptionPolledRefreshResult = lookupElement "SubscriptionPolledRefreshResult" nodes
          _invalidServerSubHandles = error "TODO"
          dataBufferOverflow = lookupAttribute "DataBufferOverflow" attributes
      _errors <- tryParseMany parseOpcError nodes
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
    _errors <- tryParseMany parseOpcError nodes
    pure
      GetPropertiesResponse
        { _getPropertiesResult,
          _propertyLists,
          _errors
        }

parseDateTime :: Text -> Either ParseError DateTime
parseDateTime (parseOnly utcTimeInISO8601 -> parsed) = case parsed of
  Left (Text.pack -> err) -> Left do DateTimeParseError err
  Right _utcTime -> pure DateTime {_utcTime}

g :: IO XML.Document
g = XML.readFile def "/Users/awkure/work/mlabs/opc-xml-da-client/session/%2f(1)"