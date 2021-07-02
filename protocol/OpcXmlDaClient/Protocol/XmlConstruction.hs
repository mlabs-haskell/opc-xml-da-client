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

import qualified Data.Time.Format.ISO8601 as Time
import OpcXmlDaClient.Base.Prelude hiding (Read, read)
import qualified OpcXmlDaClient.Protocol.Namespaces as Ns
import OpcXmlDaClient.Protocol.Types
import qualified OpcXmlDaClient.XmlBuilder as X

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

subscribeElement :: Text -> Subscribe -> X.Element
subscribeElement elementName x =
  X.element
    (opcQName elementName)
    ( catMaybes
        [ Just ("ReturnValuesOnReply", booleanContent (#returnValuesOnReply x)),
          ("SubcriptionPingRate",) . int32Content <$> #subscriptionPingRate x
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
        [ if #returnErrorText x then Nothing else Just ("ReturnErrorText", "true"),
          if #returnDiagnosticInfo x then Just ("ReturnDiagnosticInfo", "true") else Nothing,
          if #returnItemTime x then Just ("ReturnItemTime", "true") else Nothing,
          if #returnItemPath x then Just ("ReturnItemPath", "true") else Nothing,
          if #returnItemName x then Just ("ReturnItemName", "true") else Nothing,
          fmap (("RequestDeadline",) . dateTimeContent) (#requestDeadline x),
          fmap (("ClientRequestHandle",) . X.textContent) (#clientRequestHandle x),
          fmap (("LocaleID",) . X.textContent) (#localeId x)
        ]
    )
    []

subscribeRequestItemListElement :: Text -> SubscribeRequestItemList -> X.Element
subscribeRequestItemListElement elementName x =
  X.element
    (opcQName elementName)
    ( catMaybes
        [ fmap (("ItemPath",) . X.textContent) (#itemPath x),
          fmap (("ReqType",) . qNameContent) (#reqType x),
          fmap (("Deadband",) . floatContent) (#deadband x),
          fmap (("RequestedSamplingRate",) . int32Content) (#requestedSamplingRate x),
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
          fmap (("RequestedSamplingRate",) . int32Content) (#requestedSamplingRate x),
          fmap (("EnableBuffering",) . booleanContent) (#enableBuffering x)
        ]
    )
    []

getStatusElement :: Text -> GetStatus -> X.Element
getStatusElement elementName x =
  X.element
    (opcQName elementName)
    ( catMaybes
        [ ("LocaleID",) . X.textContent <$> #localeId x,
          ("ClientRequestHandle",) . X.textContent <$> #clientRequestHandle x
        ]
    )
    []

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
  X.element
    (opcQName elementName)
    [ (xsiQName "type", qNameContent (#type x))
    ]
    (fmap X.astNode (#xml x))

diagnosticInfoElement :: Text -> Text -> X.Element
diagnosticInfoElement elementName x =
  X.element (opcQName elementName) [] [X.contentNode (X.textContent x)]

opcQualityElement :: Text -> OpcQuality -> X.Element
opcQualityElement elementName x =
  X.element
    (opcQName elementName)
    [ ("QualityField", qualityBitsContent (#qualityField x)),
      ("LimitField", limitBitsContent (#limitField x)),
      ("VendorField", showContent (#vendorField x))
    ]
    []

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
          ("MaxAge",) . int32Content <$> #maxAge x
        ]
    )
    []

readRequestItemListElement :: Text -> ReadRequestItemList -> X.Element
readRequestItemListElement elementName x =
  X.element
    (opcQName elementName)
    ( catMaybes
        [ ("ItemPath",) . X.textContent <$> #itemPath x,
          ("ReqType",) . qNameContent <$> #reqType x,
          ("MaxAge",) . int32Content <$> #maxAge x
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
          pure ("WaitTime", int32Content (#waitTime x)),
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
    []

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
          pure ("MaxElementsReturned", int32Content (#maxElementsReturned x)),
          pure ("BrowseFilter", browseFilterContent (#browseFilter x)),
          ("ElementNameFilter",) . X.textContent <$> #elementNameFilter x,
          ("VendorFilter",) . X.textContent <$> #vendorFilter x,
          pure ("ReturnAllProperties", booleanContent (#returnAllProperties x)),
          pure ("ReturnAllPropertyValues", booleanContent (#returnAllPropertyValues x)),
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
    []

-- * Content

showContent :: Show a => a -> X.Content
showContent = stringContent . show

stringContent :: String -> X.Content
stringContent = X.textContent . fromString

dateTimeContent :: UTCTime -> X.Content
dateTimeContent = stringContent . Time.iso8601Show

booleanContent :: Bool -> X.Content
booleanContent = X.textContent . bool "false" "true"

floatContent :: Float -> X.Content
floatContent = showContent

int32Content :: Int32 -> X.Content
int32Content = showContent

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
  UncertainEUExceededQualityBits -> "uncertainEUExceededQualityBits"
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
