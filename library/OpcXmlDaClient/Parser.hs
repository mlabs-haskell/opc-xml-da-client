{-# LANGUAGE DeriveAnyClass           #-}
{-# LANGUAGE KindSignatures           #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE QuantifiedConstraints    #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module OpcXmlDaClient.Parser where

import           Data.Constraint
import           Data.Kind

import           Control.Lens                  hiding (element)

-- import qualified Data.XML.Types as Xml
import qualified Text.XML                      as XML
import qualified Text.XML.Cursor               as Cursor
import           Text.XML.Lens

import           OpcXmlDaClient.Protocol.Types

import           Attoparsec.Time.Text          (utcTimeInISO8601)
import           Control.Exception.Lens
import           Data.Attoparsec.Text          (parseOnly)
import qualified Data.Map                      as Map
import qualified Data.Text                     as Text
import           Data.Text.Lazy.Lens           (packed)
import           Data.Text.Lens                hiding (text)
import           OpcXmlDaClient.Prelude        hiding (Read)
import qualified Prelude
import           Shower
import qualified Text.Read                     as Text
import           Text.XML.Cursor

-- | Filter map by some predicate and safely extract the first occurrence
-- that maches the predicate applied to key
findByPartialKey :: (k -> Bool) -> Map k a -> Maybe a
findByPartialKey pred map = map ^.. ifolded . indices pred & mbSingleton

lookupAttribute :: Text -> Map Name a -> Maybe a
lookupAttribute attrName = findByPartialKey do (== attrName) . view _nameLocalName

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

withSOAPElement
  :: Name
    -- ^ element name to match
  -> (XML.Element -> a)
    -- ^ error handling if the element is not found
  -> (Map Name Text -> [Node] -> Either a b)
    -- ^ callback that continues the execution if the name is found
  -> XML.Element
    -- ^ element to look up children in
  -> Either a b
withSOAPElement name errorHandler callback = \case
  Element
    { elementName
    , elementAttributes
    , elementNodes
    } | elementName == name
    -> callback elementAttributes elementNodes
  el -> Left do errorHandler el

lookupElement :: Text -> [Node] -> Maybe XML.Element
lookupElement name nodes = (\(NodeElement e) -> e) <$> mbSingleton do
  filter
    \case
      NodeElement Element { elementName } -> nameLocalName elementName == name
      _                                   -> False
    nodes

-- | Construct a vector of successfully parsed elements
-- and fail if one of them fails
-- TODO: test it
tryParseMany
  :: (XML.Element -> Either ParseError b)
  -> [XML.Node]
  -> Either ParseError (Vector b)
tryParseMany parser =
  foldr' (curry matchParsed) (Left do OtherParseError "Empty node list passed")
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
  | ReplyBaseParseError XMLAttrs
  -- ^ Attributes that are failed to parse
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
  | QualityBitsParseError XML.Element
  | LimitBitsParseError XML.Element
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
  | DateTimeParseError XML.Element
  | OtherParseError Text
  deriving stock (Eq, Show)

type Parser a = XML.Element -> Either ParseError a

parseGetStatusResponse :: Parser GetStatusResponse
parseGetStatusResponse = \case
  Element { elementName = s }
    -> error "TODO"

parseReplyBase :: XMLAttrs -> Either ParseError ReplyBase
parseReplyBase attributes
  | Just rcvTime <- lookupAttribute "RcvTime" attributes
  , Just replyTime <- lookupAttribute "ReplyTime" attributes
  , _clientRequestHandle <- lookupAttribute "ClientRequestHandle" attributes
  , _revisedLocaleId <- error "TODO"
  , Just serverState <- lookupAttribute "ServerState" attributes
  = do
    _serverState <- parseServerState serverState
    _rcvTime <- parseDateTime rcvTime
    _replyTime <- parseDateTime replyTime
    pure ReplyBase
      { _rcvTime
      , _replyTime
      , _clientRequestHandle
      , _revisedLocaleId
      , _serverState
      }
parseReplyBase attributes = Left $ ReplyBaseParseError attributes

parseServerState :: Text -> Either ParseError ServerState
parseServerState = \case
  "running"   -> pure RunningServerState
  "failed"    -> pure FailedServerState
  "noConfig"  -> pure NoConfigServerState
  "suspended" -> pure SuspendedServerState
  "test"      -> pure TestServerState
  "commFault" -> pure CommFaultServerState
  s           -> Left $ ServerStateParseError s

parseServerStatus :: Parser ServerStatus
parseServerStatus = error "TODO"

-- |
-- "xxx_yyy_zzz_2_1_0" -> InterfaceVersion { _minor = 0, _major = 1 }
parseInterfaceVersion :: Text -> Either ParseError InterfaceVersion
parseInterfaceVersion v | let
  matchVersions =
    fmap do readMaybe @Int . Text.unpack
    >>> takeFromEnd 2
    >>> \case
      [Just _minor, Just _major] -> Just (_minor, _major)
      _                          -> Nothing
  , Just (_major, _minor) <- v
      & Text.strip
      & Text.splitOn "_"
      & matchVersions
  = pure InterfaceVersion {_major, _minor}
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

parseItemValue :: Parser ItemValue
parseItemValue =
  withSOAPElement
    "ItemValue"
    ItemValueParseError
    \attributes nodes -> do
      let
        _diagnosticInfo = error "TODO"
        -- _value = lookupElement "Value" elementNodes
        _value = error "TODO"
        _quality = error "TODO"
        _valueTypeQualifier  = error "TODO"
        _itemPath = lookupAttribute "ItemPath" attributes
        _itemName = lookupAttribute "ItemName" attributes
        _clientItemHandle = lookupAttribute "ClientItemHandle" attributes
        _timestamp = error "TODO"
        _resultId = error "TODO"
      pure ItemValue
        { _diagnosticInfo
        , _value
        , _quality
        , _valueTypeQualifier
        , _itemPath
        , _itemName
        , _clientItemHandle
        , _timestamp
        , _resultId
        }

parseOpcQuality :: Parser OpcQuality
parseOpcQuality = error "TODO"

parseQualityBits :: Parser QualityBits
parseQualityBits = error "TODO"

parseLimitBits :: Parser LimitBits
parseLimitBits = error "TODO"

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

parseSubscribeReplyItemList :: XML.Element -> Either ParseError SubscribeReplyItemList
parseSubscribeReplyItemList = error "TODO"

parseSubscribeResponse :: Parser SubscribeResponse
parseSubscribeResponse =
  withSOAPElement
    "SubscribeResponse"
    SubscribeResponseParseError
    \attributes nodes -> do
  let
    _serverSubHandle = lookupAttribute "ServerSubHandle" attributes
    _subscribeResult = error "TODO"
    _errors = error "TODO"
    mbRItemList = lookupElement "RItemList" nodes
  _rItemList <- tryParse parseSubscribeReplyItemList mbRItemList
  pure SubscribeResponse
    { _serverSubHandle
    , _subscribeResult
    , _rItemList
    , _errors
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
    SubscriptionPolledRefreshParseError
    \attributes nodes -> do
  let
    mbSubscriptionPolledRefreshResult = lookupElement "SubscriptionPolledRefreshResult" nodes
    _invalidServerSubHandles = error "TODO"
    _errors = error "TODO"
    dataBufferOverflow = lookupAttribute "DataBufferOverflow" attributes
  _dataBufferOverflow <- case dataBufferOverflow of
    Nothing -> Left do OtherParseError "Cannot find DataBufferOverflow attribute"
    Just (Text.unpack -> x:xs) -> case toUpper x : xs & readMaybe @Bool of
      Nothing -> Left do OtherParseError "Malformed value passed to DataBufferOverflow attribute"
      Just v -> pure v
  _rItemList <-
    tryParseMany
      ( withSOAPElement
        "SubscriptionPolledRefreshResult"
        (const do OtherParseError "SubscriptionPolledRefreshResult error")
        \attributes _ -> parseSubscribePolledRefreshReplyItemList attributes
      )
      nodes
  -- _subscriptionPolledRefreshResult <- tryParse parseReplyBase mbSubscriptionPolledRefreshResult
  _subscriptionPolledRefreshResult <- error "TODO"
  pure SubscriptionPolledRefreshResponse
    { _subscriptionPolledRefreshResult
    , _invalidServerSubHandles
    , _rItemList
    , _errors
    , _dataBufferOverflow
    }

parseSubscriptionCancel :: Parser SubscriptionCancel
parseSubscriptionCancel = error "TODO"

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

parseGetPropertiesResponse :: Parser GetPropertiesResponse
parseGetPropertiesResponse = error "TODO"

parseDateTime :: Text -> Either ParseError DateTime
parseDateTime t |
  _timezone <- parseOnly utcTimeInISO8601 "2017-02-01T05:03:58+01:00"
  = error "TODO"

