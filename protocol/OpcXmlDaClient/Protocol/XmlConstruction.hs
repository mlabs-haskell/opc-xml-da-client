module OpcXmlDaClient.Protocol.XmlConstruction
  ( subscribe,
    getStatus,
    write,
    read,
    subscriptionPolledRefresh,
    subscriptionCancel,
    browse,
    getProperties,
  )
where

import qualified Data.ByteString.Base64 as Base64
import qualified Data.Vector.Generic as Gv
import OpcXmlDaClient.Base.Prelude hiding (Read, read)
import qualified OpcXmlDaClient.Protocol.Namespaces as Ns
import OpcXmlDaClient.Protocol.Types
import qualified OpcXmlDaClient.XmlBuilder as X
import qualified OpcXmlDaClient.XmlSchemaValues.Rendering as XmlSchemaValuesRendering
import OpcXmlDaClient.XmlSchemaValues.Types
import qualified Text.Builder as Tb

-- * Documents

subscribe :: Subscribe -> ByteString
subscribe = inSoapEnvelope . subscribeElement "Subscribe"

getStatus :: GetStatus -> ByteString
getStatus = inSoapEnvelope . getStatusElement "GetStatus"

write :: Write -> ByteString
write = inSoapEnvelope . writeElement "Write"

read :: Read -> ByteString
read = inSoapEnvelope . readElement "Read"

subscriptionPolledRefresh :: SubscriptionPolledRefresh -> ByteString
subscriptionPolledRefresh = inSoapEnvelope . subscriptionPolledRefreshElement "SubscriptionPolledRefresh"

subscriptionCancel :: SubscriptionCancel -> ByteString
subscriptionCancel = inSoapEnvelope . subscriptionCancelElement "SubscriptionCancel"

browse :: Browse -> ByteString
browse = inSoapEnvelope . browseElement "Browse"

getProperties :: GetProperties -> ByteString
getProperties = inSoapEnvelope . getPropertiesElement "GetProperties"

-- |
-- Wraps the element in the following snippet.
--
-- > <SOAP-ENV:Envelope
-- >   xmlns:SOAP-ENC="http://schemas.xmlsoap.org/soap/encoding/"
-- >   xmlns:SOAP-ENV="http://schemas.xmlsoap.org/soap/envelope/"
-- >   xmlns:xsd="http://www.w3.org/2001/XMLSchema"
-- >   xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
-- >   <SOAP-ENV:Header/>
-- >   <SOAP-ENV:Body xmlns:opc="http://opcfoundation.org/webservices/XMLDA/1.0/">
-- >     ...
-- >   </SOAP-ENV:Body>
-- > </SOAP-ENV:Envelope>
inSoapEnvelope :: X.Element -> ByteString
inSoapEnvelope element =
  X.elementXml
    ( X.element
        (soapEnvQName "Envelope")
        []
        [ X.elementNode
            ( X.element
                (soapEnvQName "Header")
                []
                []
            ),
          X.elementNode
            ( X.element
                (soapEnvQName "Body")
                []
                [X.elementNode element]
            )
        ]
    )

-- * Elements

-- Some OPC Servers don't like XML elements of the form
-- <foo attr="bar"/>
-- but prefer the the form
-- <foo attr="bar"></foo>
-- When passing an empty Node list to X.element,
-- the former is generated.
-- An empty Node coaxes xml-conduit to use the latter form.
noContent :: [X.Node]
noContent = [((X.contentNode . X.textContent) "")]

subscribeElement :: Text -> Subscribe -> X.Element
subscribeElement elementName x =
  X.element
    (opcQName elementName)
    ( catMaybes
        [ Just ("ReturnValuesOnReply", booleanContent (#returnValuesOnReply x)),
          ("SubscriptionPingRate",) . intContent <$> #subscriptionPingRate x
        ]
    )
    ( catMaybes
        [ fmap (X.elementNode . requestOptionsElement "Options") (#options x),
          fmap (X.elementNode . subscribeRequestItemListElement "ItemList") (#itemList x)
        ]
    )

requestOptionsElement :: Text -> RequestOptions -> X.Element
requestOptionsElement elementName x =
  X.element
    (opcQName elementName)
    ( catMaybes
        [ if #returnErrorText x then Just ("ReturnErrorText", "true") else Nothing,
          if #returnDiagnosticInfo x then Just ("ReturnDiagnosticInfo", "true") else Nothing,
          if #returnItemTime x then Just ("ReturnItemTime", "true") else Nothing,
          if #returnItemPath x then Just ("ReturnItemPath", "true") else Nothing,
          if #returnItemName x then Just ("ReturnItemName", "true") else Nothing,
          fmap (("RequestDeadline",) . dateTimeContent) (#requestDeadline x),
          fmap (("ClientRequestHandle",) . X.textContent) (#clientRequestHandle x),
          fmap (("LocaleID",) . X.textContent) (#localeId x)
        ]
    )
    noContent

subscribeRequestItemListElement :: Text -> SubscribeRequestItemList -> X.Element
subscribeRequestItemListElement elementName x =
  X.element
    (opcQName elementName)
    ( catMaybes
        [ fmap (("ItemPath",) . X.textContent) (#itemPath x),
          fmap (("ReqType",) . qNameContent) (#reqType x),
          fmap (("Deadband",) . floatContent) (#deadband x),
          fmap (("RequestedSamplingRate",) . intContent) (#requestedSamplingRate x),
          fmap (("EnableBuffering",) . booleanContent) (#enableBuffering x)
        ]
    )
    (fmap (X.elementNode . subscribeRequestItemElement "Items") (toList (#items x)))

subscribeRequestItemElement :: Text -> SubscribeRequestItem -> X.Element
subscribeRequestItemElement elementName x =
  X.element
    (opcQName elementName)
    ( catMaybes
        [ fmap (("ItemPath",) . X.textContent) (#itemPath x),
          fmap (("ReqType",) . qNameContent) (#reqType x),
          fmap (("ItemName",) . X.textContent) (#itemName x),
          fmap (("ClientItemHandle",) . X.textContent) (#clientItemHandle x),
          fmap (("Deadband",) . floatContent) (#deadband x),
          fmap (("RequestedSamplingRate",) . intContent) (#requestedSamplingRate x),
          fmap (("EnableBuffering",) . booleanContent) (#enableBuffering x)
        ]
    )
    noContent

getStatusElement :: Text -> GetStatus -> X.Element
getStatusElement elementName x =
  X.element
    (opcQName elementName)
    ( catMaybes
        [ ("LocaleID",) . X.textContent <$> #localeId x,
          ("ClientRequestHandle",) . X.textContent <$> #clientRequestHandle x
        ]
    )
    noContent

itemValueElement :: Text -> ItemValue -> X.Element
itemValueElement elementName x =
  X.element
    (opcQName elementName)
    ( catMaybes
        [ ("ValueTypeQualifier",) . qNameContent <$> #valueTypeQualifier x,
          ("ItemPath",) . X.textContent <$> #itemPath x,
          ("ItemName",) . X.textContent <$> #itemName x,
          ("ClientItemHandle",) . X.textContent <$> #clientItemHandle x,
          ("Timestamp",) . dateTimeContent <$> #timestamp x,
          ("ResultID",) . qNameContent <$> #resultId x
        ]
    )
    ( catMaybes
        [ X.elementNode . diagnosticInfoElement "DiagnosticInfo" <$> #diagnosticInfo x,
          X.elementNode . valueElement "Value" <$> #value x,
          X.elementNode . opcQualityElement "Quality" <$> #quality x
        ]
    )

valueElement :: Text -> Value -> X.Element
valueElement elementName x =
  case x of
    StringValue x -> primitive "string" $ stringContent x
    BooleanValue x -> primitive "boolean" $ booleanContent x
    FloatValue x -> primitive "float" $ floatContent x
    DoubleValue x -> primitive "double" $ doubleContent x
    DecimalValue x -> primitive "decimal" $ decimalContent x
    LongValue x -> primitive "long" $ longContent x
    IntValue x -> primitive "int" $ intContent x
    ShortValue x -> primitive "short" $ shortContent x
    ByteValue x -> primitive "byte" $ byteContent x
    UnsignedLongValue x -> primitive "unsignedLong" $ unsignedLongContent x
    UnsignedIntValue x -> primitive "unsignedInt" $ unsignedIntContent x
    UnsignedShortValue x -> primitive "unsignedShort" $ unsignedShortContent x
    UnsignedByteValue x -> primitive "unsignedByte" $ unsignedByteContent x
    Base64BinaryValue x -> primitive "base64Binary" $ base64BinaryContent x
    DateTimeValue x -> primitive "dateTime" $ dateTimeContent x
    TimeValue x -> primitive "time" $ timeContent x
    DateValue x -> primitive "date" $ dateContent x
    DurationValue x -> primitive "duration" $ durationContent x
    QNameValue x -> primitive "QName" $ qNameContent x
    ArrayOfByteValue x -> primitiveArray "ArrayOfByte" "byte" byteContent x
    ArrayOfShortValue x -> primitiveArray "ArrayOfShort" "short" shortContent x
    ArrayOfUnsignedShortValue x -> primitiveArray "ArrayOfUnsignedShort" "unsignedShort" unsignedShortContent x
    ArrayOfIntValue x -> primitiveArray "ArrayOfInt" "int" intContent x
    ArrayOfUnsignedIntValue x -> primitiveArray "ArrayOfUnsignedInt" "unsignedInt" unsignedIntContent x
    ArrayOfLongValue x -> primitiveArray "ArrayOfLong" "long" longContent x
    ArrayOfUnsignedLongValue x -> primitiveArray "ArrayOfUnsignedLong" "unsignedLong" unsignedLongContent x
    ArrayOfFloatValue x -> primitiveArray "ArrayOfFloat" "float" floatContent x
    ArrayOfDecimalValue x -> primitiveArray "ArrayOfDecimal" "decimal" decimalContent x
    ArrayOfDoubleValue x -> primitiveArray "ArrayOfDouble" "double" doubleContent x
    ArrayOfBooleanValue x -> primitiveArray "ArrayOfBoolean" "boolean" booleanContent x
    ArrayOfStringValue x -> primitiveArray "ArrayOfString" "string" stringContent x
    ArrayOfDateTimeValue x -> primitiveArray "ArrayOfDateTime" "dateTime" dateTimeContent x
    ArrayOfAnyTypeValue x ->
      element (X.namespacedQName Ns.opc "ArrayOfAnyType") $ fmap item $ toList x
      where
        item = \case
          Just x ->
            X.elementNode $ valueElement "anyType" x
          Nothing ->
            X.elementNode $ X.element (opcQName "anyType") [(xsiQName "isNil", "true")] []
    OpcQualityValue x ->
      X.element
        (opcQName elementName)
        ((xsiQName "type", X.qNameContent "OPCQuality") : opcQualityAttributes x)
        []
    NonStandardValue (ValueNonStandard a b) -> element (qNameQName a) (fmap X.astNode b)
  where
    element typeQName =
      X.element (opcQName elementName) [(xsiQName "type", X.qNameContent typeQName)]
    primitive typeName content =
      element (X.namespacedQName Ns.xsd typeName) [X.contentNode content]
    primitiveArray :: Gv.Vector v a => Text -> Text -> (a -> X.Content) -> v a -> X.Element
    primitiveArray arrayTypeName itemTagName itemContentRenderer array =
      element (X.namespacedQName Ns.opc arrayTypeName) $ fmap item $ Gv.toList array
      where
        item x =
          X.elementNode $ X.element (opcQName itemTagName) [] [X.contentNode (itemContentRenderer x)]

diagnosticInfoElement :: Text -> Text -> X.Element
diagnosticInfoElement elementName x =
  X.element (opcQName elementName) [] [X.contentNode (X.textContent x)]

opcQualityElement :: Text -> OpcQuality -> X.Element
opcQualityElement elementName x =
  X.element (opcQName elementName) (opcQualityAttributes x) []

writeRequestItemListElement :: Text -> WriteRequestItemList -> X.Element
writeRequestItemListElement elementName x =
  X.element
    (opcQName elementName)
    ( catMaybes
        [("ItemPath",) . X.textContent <$> #itemPath x]
    )
    (X.elementNode . itemValueElement "Items" <$> toList (#items x))

writeElement :: Text -> Write -> X.Element
writeElement elementName x =
  X.element
    (opcQName elementName)
    [("ReturnValuesOnReply", booleanContent (#returnValuesOnReply x))]
    ( catMaybes
        [ X.elementNode . writeRequestItemListElement "ItemList" <$> #itemList x,
          X.elementNode . requestOptionsElement "Options" <$> #options x
        ]
    )

readRequestItemElement :: Text -> ReadRequestItem -> X.Element
readRequestItemElement elementName x =
  X.element
    (opcQName elementName)
    ( catMaybes
        [ ("ItemPath",) . X.textContent <$> #itemPath x,
          ("ReqType",) . qNameContent <$> #reqType x,
          ("ItemName",) . X.textContent <$> #itemName x,
          ("ClientItemHandle",) . X.textContent <$> #clientItemHandle x,
          ("MaxAge",) . intContent <$> #maxAge x
        ]
    )
    noContent

readRequestItemListElement :: Text -> ReadRequestItemList -> X.Element
readRequestItemListElement elementName x =
  X.element
    (opcQName elementName)
    ( catMaybes
        [ ("ItemPath",) . X.textContent <$> #itemPath x,
          ("ReqType",) . qNameContent <$> #reqType x,
          ("MaxAge",) . intContent <$> #maxAge x
        ]
    )
    (X.elementNode . readRequestItemElement "Items" <$> toList (#items x))

readElement :: Text -> Read -> X.Element
readElement elementName x =
  X.element
    (opcQName elementName)
    []
    ( catMaybes
        [ X.elementNode . requestOptionsElement "Options" <$> #options x,
          X.elementNode . readRequestItemListElement "ItemList" <$> #itemList x
        ]
    )

subscriptionPolledRefreshElement :: Text -> SubscriptionPolledRefresh -> X.Element
subscriptionPolledRefreshElement elementName x =
  X.element
    (opcQName elementName)
    ( catMaybes
        [ ("HoldTime",) . dateTimeContent <$> #holdTime x,
          pure ("WaitTime", intContent (#waitTime x)),
          pure ("ReturnAllItems", booleanContent (#returnAllItems x))
        ]
    )
    ( catMaybes [X.elementNode . requestOptionsElement "Options" <$> #options x]
        <> fmap (X.elementNode . serverSubHandlesElement "ServerSubHandles") (toList (#serverSubHandles x))
    )

serverSubHandlesElement :: Text -> Text -> X.Element
serverSubHandlesElement elementName x =
  X.element (opcQName elementName) [] [X.contentNode (X.textContent x)]

subscriptionCancelElement :: Text -> SubscriptionCancel -> X.Element
subscriptionCancelElement elementName x =
  X.element
    (opcQName elementName)
    ( catMaybes
        [ ("ClientRequestHandle",) . X.textContent <$> #clientRequestHandle x,
          ("ServerSubHandle",) . X.textContent <$> #serverSubHandle x
        ]
    )
    noContent

browseElement :: Text -> Browse -> X.Element
browseElement elementName x =
  X.element
    (opcQName elementName)
    ( catMaybes
        [ ("LocaleID",) . X.textContent <$> #localeId x,
          ("ClientRequestHandle",) . X.textContent <$> #clientRequestHandle x,
          ("ItemPath",) . X.textContent <$> #itemPath x,
          ("ItemName",) . X.textContent <$> #itemName x,
          ("ContinuationPoint",) . X.textContent <$> #continuationPoint x,
          pure ("MaxElementsReturned", intContent (#maxElementsReturned x)),
          pure ("BrowseFilter", browseFilterContent (#browseFilter x)),
          ("ElementNameFilter",) . X.textContent <$> #elementNameFilter x,
          ("VendorFilter",) . X.textContent <$> #vendorFilter x,
          pure ("ReturnAllProperties", booleanContent (#returnAllProperties x)),
          pure ("ReturnPropertyValues", booleanContent (#returnPropertyValues x)),
          pure ("ReturnErrorText", booleanContent (#returnErrorText x))
        ]
    )
    (X.elementNode . propertyNameElement "PropertyNames" <$> toList (#propertyNames x))

propertyNameElement :: Text -> QName -> X.Element
propertyNameElement elementName x =
  X.element
    (opcQName elementName)
    []
    [X.contentNode (qNameContent x)]

getPropertiesElement :: Text -> GetProperties -> X.Element
getPropertiesElement elementName x =
  X.element
    (opcQName elementName)
    ( catMaybes
        [ ("LocaleID",) . X.textContent <$> #localeId x,
          ("ClientRequestHandle",) . X.textContent <$> #clientRequestHandle x,
          ("ItemPath",) . X.textContent <$> #itemPath x,
          Just ("ReturnAllProperties", booleanContent (#returnAllProperties x)),
          Just ("ReturnPropertyValues", booleanContent (#returnPropertyValues x)),
          Just ("ReturnErrorText", booleanContent (#returnErrorText x))
        ]
    )
    ( mconcat
        [ X.elementNode . propertyNameElement "PropertyNames" <$> toList (#propertyNames x),
          X.elementNode . itemIdentifierElement "ItemIDs" <$> toList (#itemIds x)
        ]
    )

itemIdentifierElement :: Text -> ItemIdentifier -> X.Element
itemIdentifierElement elementName x =
  X.element
    (opcQName elementName)
    ( catMaybes
        [ ("ItemPath",) . X.textContent <$> #itemPath x,
          ("ItemName",) . X.textContent <$> #itemName x
        ]
    )
    noContent

-- * Attributes

opcQualityAttributes :: OpcQuality -> [(X.QName, X.Content)]
opcQualityAttributes x =
  [ ("QualityField", qualityBitsContent (#qualityField x)),
    ("LimitField", limitBitsContent (#limitField x)),
    ("VendorField", shownContent (#vendorField x))
  ]

-- * Content

shownContent :: Show a => a -> X.Content
shownContent = X.textContent . fromString . show

stringContent :: Text -> X.Content
stringContent = X.textContent

booleanContent :: Bool -> X.Content
booleanContent = X.textContent . bool "false" "true"

floatContent :: Float -> X.Content
floatContent = shownContent

doubleContent :: Double -> X.Content
doubleContent = shownContent

decimalContent :: Scientific -> X.Content
decimalContent = shownContent

longContent :: Int64 -> X.Content
longContent = shownContent

intContent :: Int32 -> X.Content
intContent = shownContent

shortContent :: Int16 -> X.Content
shortContent = shownContent

byteContent :: Int8 -> X.Content
byteContent = shownContent

unsignedLongContent :: Word64 -> X.Content
unsignedLongContent = shownContent

unsignedIntContent :: Word32 -> X.Content
unsignedIntContent = shownContent

unsignedShortContent :: Word16 -> X.Content
unsignedShortContent = shownContent

unsignedByteContent :: Word8 -> X.Content
unsignedByteContent = shownContent

base64BinaryContent :: ByteString -> X.Content
base64BinaryContent = X.textContent . Base64.encodeBase64

dateTimeContent :: UTCTime -> X.Content
dateTimeContent = X.textContent . Tb.run . XmlSchemaValuesRendering.dateTime

timeContent :: Time -> X.Content
timeContent = X.textContent . Tb.run . XmlSchemaValuesRendering.time

dateContent :: Date -> X.Content
dateContent = X.textContent . Tb.run . XmlSchemaValuesRendering.date

durationContent :: Duration -> X.Content
durationContent = X.textContent . Tb.run . XmlSchemaValuesRendering.duration

qNameContent :: QName -> X.Content
qNameContent = X.qNameContent . qNameQName

browseFilterContent :: BrowseFilter -> X.Content
browseFilterContent = \case
  AllBrowseFilter -> "all"
  BranchBrowseFilter -> "branch"
  ItemBrowseFilter -> "item"

qualityBitsContent :: QualityBits -> X.Content
qualityBitsContent = \case
  BadQualityBits -> "bad"
  BadConfigurationErrorQualityBits -> "badConfigurationError"
  BadNotConnectedQualityBits -> "badNotConnected"
  BadDeviceFailureQualityBits -> "badDeviceFailure"
  BadSensorFailureQualityBits -> "badSensorFailure"
  BadLastKnownValueQualityBits -> "badLastKnownValue"
  BadCommFailureQualityBits -> "badCommFailure"
  BadOutOfServiceQualityBits -> "badOutOfService"
  BadWaitingForInitialDataQualityBits -> "badWaitingForInitialData"
  UncertainQualityBits -> "uncertain"
  UncertainLastUsableValueQualityBits -> "uncertainLastUsableValue"
  UncertainSensorNotAccurateQualityBits -> "uncertainSensorNotAccurate"
  UncertainEUExceededQualityBits -> "uncertainEUExceeded"
  UncertainSubNormalQualityBits -> "uncertainSubNormal"
  GoodQualityBits -> "good"
  GoodLocalOverrideQualityBits -> "goodLocalOverride"

limitBitsContent :: LimitBits -> X.Content
limitBitsContent = \case
  NoneLimitBits -> "none"
  LowLimitBits -> "low"
  HighLimitBits -> "high"
  ConstantLimitBits -> "constant"

-- * Names

soapEncQName :: Text -> X.QName
soapEncQName =
  X.namespacedQName Ns.soapEnc

soapEnvQName :: Text -> X.QName
soapEnvQName =
  X.namespacedQName Ns.soapEnv

xsdQName :: Text -> X.QName
xsdQName =
  X.namespacedQName Ns.xsd

xsiQName :: Text -> X.QName
xsiQName =
  X.namespacedQName Ns.xsi

opcQName :: Text -> X.QName
opcQName =
  X.namespacedQName Ns.opc

qNameQName :: QName -> X.QName
qNameQName = \case
  NamespacedQName ns name -> X.namespacedQName ns name
  UnnamespacedQName name -> X.unnamespacedQName name
