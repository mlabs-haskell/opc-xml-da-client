{-# LANGUAGE NamedFieldPuns #-}

module OpcXmlDaClient.Protocol.XmlConstruction
  ( -- * Documents
    subscribeDocument,
    getStatusDocument,
    writeDocument,
    readDocument,
    subscriptionPolledRefreshDocument,
    subscriptionCancelDocument,
    browseDocument,
    getPropertiesDocument,
  )
where

import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Data.Time.Format.ISO8601 (iso8601Show)
import OpcXmlDaClient.Base.Prelude hiding (Read, bool)
import OpcXmlDaClient.Protocol.Types
import qualified Text.XML as Xml

----------------------------------------------------------------

-- * Names

----------------------------------------------------------------

soapEncodingName :: Text -> Xml.Name
soapEncodingName name =
  Xml.Name name (Just "http://schemas.xmlsoap.org/soap/encoding/") (Just "SOAP-ENC")

soapEnvelopeName :: Text -> Xml.Name
soapEnvelopeName name =
  Xml.Name name (Just "http://schemas.xmlsoap.org/soap/envelope/") (Just "SOAP-ENV")

xsdName :: Text -> Xml.Name
xsdName name =
  Xml.Name name (Just "http://www.w3.org/2001/XMLSchema") (Just "xsd")

xsiName :: Text -> Xml.Name
xsiName name =
  Xml.Name name (Just "http://www.w3.org/2001/XMLSchema-instance") (Just "xsi")

xmlDaName :: Text -> Xml.Name
xmlDaName name =
  Xml.Name name (Just "http://opcfoundation.org/webservices/XMLDA/1.0/") (Just "XMLDA")

-- * Documents

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
inSoapEnvelope :: Xml.Node -> Xml.Document
inSoapEnvelope bodyChild =
  Xml.Document
    (Xml.Prologue [] Nothing [])
    ( Xml.Element
        (soapEnvelopeName "Envelope")
        Map.empty
        [ Xml.NodeElement
            ( Xml.Element
                (soapEnvelopeName "Header")
                Map.empty
                []
            ),
          Xml.NodeElement
            ( Xml.Element
                (soapEnvelopeName "Body")
                Map.empty
                [bodyChild]
            )
        ]
    )
    []

----------------------------------------------------------------

-- * Helpers

----------------------------------------------------------------

constructElement :: Text -> [(Xml.Name, Text)] -> [Xml.Node] -> Xml.Element
constructElement name attributes = Xml.Element (xmlDaName name) (Map.fromList attributes)

----------------------------------------------------------------

-- * Elements

----------------------------------------------------------------

-- |
-- > <ns1:Subscribe ReturnValuesOnReply="true" SubscriptionPingRate="3000">
-- >   <ns1:Options ClientRequestHandle="Subscribe" ReturnErrorText="true" ReturnItemName="true" ReturnItemPath="true"/>
-- >   <ns1:ItemList>
-- >     <ns1:Items ClientItemHandle="SubscribeItem_0" ItemName="A"/>
-- >     <ns1:Items ClientItemHandle="SubscribeItem_1" ItemName="B"/>
-- >     <ns1:Items ClientItemHandle="SubscribeItem_2" ItemName="C"/>
-- >   </ns1:ItemList>
-- > </ns1:Subscribe>
subscribeElement :: Subscribe -> Xml.Element
subscribeElement x =
  constructElement
    "Subscribe"
    ( catMaybes
        [ Just ("ReturnValuesOnReply", bool (#returnValuesOnReply x)),
          ("SubcriptionPingRate",) . int <$> #subscriptionPingRate x
        ]
    )
    ( catMaybes
        [ fmap (Xml.NodeElement . requestOptionsElement) (#options x),
          fmap (Xml.NodeElement . subscribeRequestItemListElement "ItemList") (#itemList x)
        ]
    )

subscribeDocument :: Subscribe -> Xml.Document
subscribeDocument = inSoapEnvelope . Xml.NodeElement . subscribeElement

-- |
-- > RequestOptions:
-- >   product:
-- >    returnErrorText: Bool
-- >    returnDiagnosticInfo: Bool
-- >    returnItemTime: Bool
-- >    returnItemPath: Bool
-- >    returnItemName: Bool
-- >    requestDeadline: Maybe UTCTime
-- >    clientRequestHandle: Maybe Text
-- >    localeId: Maybe Text
--
-- > <ns1:Options
-- >   ClientRequestHandle="Subscribe"
-- >   ReturnErrorText="true"
-- >   ReturnItemName="true"
-- >   ReturnItemPath="true"/>
requestOptionsElement :: RequestOptions -> Xml.Element
requestOptionsElement x =
  constructElement
    "Options"
    ( catMaybes
        [ if #returnErrorText x then Nothing else Just ("ReturnErrorText", "true"),
          if #returnDiagnosticInfo x then Just ("ReturnDiagnosticInfo", "true") else Nothing,
          if #returnItemTime x then Just ("ReturnItemTime", "true") else Nothing,
          if #returnItemPath x then Just ("ReturnItemPath", "true") else Nothing,
          if #returnItemName x then Just ("ReturnItemName", "true") else Nothing,
          fmap (("RequestDeadline",) . dateTime) (#requestDeadline x),
          fmap ("ClientRequestHandle",) (#clientRequestHandle x),
          fmap ("LocaleID",) (#localeId x)
        ]
    )
    []

-- |
-- > SubscribeRequestItemList:
-- >   product:
-- >    items: Vector SubscribeRequestItem
-- >    itemPath: Maybe Text
-- >    reqType: Maybe Xml.Name
-- >    deadband: Maybe Float
-- >    requestedSamplingRate: Maybe Int32
-- >    enableBuffering: Maybe Bool
--
-- > <ns1:ItemList>
-- >   <ns1:Items ClientItemHandle="SubscribeItem_0" ItemName="A"/>
-- >   <ns1:Items ClientItemHandle="SubscribeItem_1" ItemName="B"/>
-- >   <ns1:Items ClientItemHandle="SubscribeItem_2" ItemName="C"/>
-- > </ns1:ItemList>
subscribeRequestItemListElement :: Text -> SubscribeRequestItemList -> Xml.Element
subscribeRequestItemListElement name x =
  Xml.Element
    (xmlDaName name)
    ( Map.fromList
        ( catMaybes
            [ fmap ("ItemPath",) (#itemPath x),
              fmap (("ReqType",) . qName) (#reqType x),
              fmap (("Deadband",) . float) (#deadband x),
              fmap (("RequestedSamplingRate",) . int) (#requestedSamplingRate x),
              fmap (("EnableBuffering",) . bool) (#enableBuffering x)
            ]
        )
    )
    (fmap (Xml.NodeElement . subscribeRequestItemElement "Items") (toList (#items x)))

-- |
-- > <ns1:Items ClientItemHandle="SubscribeItem_0" ItemName="A"/>
subscribeRequestItemElement :: Text -> SubscribeRequestItem -> Xml.Element
subscribeRequestItemElement name x =
  Xml.Element
    (xmlDaName name)
    ( Map.fromList
        ( catMaybes
            [ fmap ("ItemPath",) (#itemPath x),
              fmap (("ReqType",) . qName) (#reqType x),
              fmap ("ItemName",) (#itemName x),
              fmap ("ClientItemHandle",) (#clientItemHandle x),
              fmap (("Deadband",) . float) (#deadband x),
              fmap (("RequestedSamplingRate",) . int) (#requestedSamplingRate x),
              fmap (("EnableBuffering",) . bool) (#enableBuffering x)
            ]
        )
    )
    []

-- |
-- > GetStatus:
-- >  product:
-- >    localeId: Maybe Text
-- >    clientRequestHandle: Maybe Text
--
-- > <GetStatus ClientRequestHandle="qewr" LocaleID="test"></GetStatus>
getStatusElement :: GetStatus -> Xml.Element
getStatusElement gs =
  constructElement
    "GetStatus"
    ( catMaybes
        [ ("LocaleID",) <$> #localeId gs,
          ("ClientRequestHandle",) <$> #clientRequestHandle gs
        ]
    )
    []

getStatusDocument :: GetStatus -> Xml.Document
getStatusDocument = inSoapEnvelope . Xml.NodeElement . getStatusElement

-- |
-- > ItemValue:
-- >  product:
-- >    diagnosticInfoElement: Maybe Text
-- >    value: Maybe Xml.Element
-- >    quality: Maybe OpcQuality
-- >    valueTypeQualifier: Maybe Xml.Name
-- >    itemPath: Maybe Text
-- >    itemName: Maybe Text
-- >    clientItemHandle: Maybe Text
-- >    timestamp: Maybe UTCTime
-- >    resultId: Maybe Xml.Name
--
-- > <Items
-- >   ClientItemHandle="qwer_jklsd"
-- >   ItemName="item"
-- >   ItemPath="some/path">
-- >  <DiagnosticInfo>qwer</DiagnosticInfo>
-- >  <Quality
-- >     LimitField="high"
-- >     QualityField="badLastKnownValue"
-- >     VendorField="3"></Quality>
-- > </Items>
itemValueElement :: ItemValue -> Xml.Element
itemValueElement iv =
  constructElement
    "Items"
    ( catMaybes
        [ ("ValueTypeQualifier",) . qName <$> #valueTypeQualifier iv,
          ("ItemPath",) <$> #itemPath iv,
          ("ItemName",) <$> #itemName iv,
          ("ClientItemHandle",) <$> #clientItemHandle iv,
          ("Timestamp",) . dateTime <$> #timestamp iv,
          ("ResultID",) . qName <$> #resultId iv
        ]
    )
    ( catMaybes
        [ Xml.NodeElement . diagnosticInfoElement <$> #diagnosticInfo iv,
          Xml.NodeElement . valueElement <$> #value iv,
          Xml.NodeElement . opcQualityElement <$> #quality iv
        ]
    )

valueElement :: Value -> Xml.Element
valueElement =
  error "TODO"

-- |
-- > <DiagnosticInfo>sss</DiagnosticInfo>
diagnosticInfoElement :: Text -> Xml.Element
diagnosticInfoElement info =
  constructElement
    "DiagnosticInfo"
    []
    [Xml.NodeContent info]

-- |
-- > OpcQuality:
-- >  product:
-- >    qualityField: QualityBits
-- >    limitField: LimitBits
-- >    vendorField: Word8
--
-- > <Quality LimitField="high" QualityField="badLastKnownValue" VendorField="3"></Quality>
opcQualityElement :: OpcQuality -> Xml.Element
opcQualityElement opcq =
  constructElement
    "Quality"
    [ ("QualityField", qualityBitsText (#qualityField opcq)),
      ("LimitField", limitBitsText (#limitField opcq)),
      ("VendorField", packed (#vendorField opcq))
    ]
    []

-- > QualityBits:
-- >  enum:
-- >    - bad
-- >    - badConfigurationError
-- >    - badNotConnected
-- >    - badDeviceFailure
-- >    - badSensorFailure
-- >    - badLastKnownValue
-- >    - badCommFailure
-- >    - badOutOfService
-- >    - badWaitingForInitialData
-- >    - uncertain
-- >    - uncertainLastUsableValue
-- >    - uncertainSensorNotAccurate
-- >    - uncertainEUExceeded
-- >    - uncertainSubNormal
-- >    - good
-- >    - goodLocalOverride
qualityBitsText :: QualityBits -> Text
qualityBitsText = \case
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

-- |
-- > LimitBits:
-- >  enum:
-- >    - none
-- >    - low
-- >    - high
-- >    - constant
limitBitsText :: LimitBits -> Text
limitBitsText = \case
  NoneLimitBits -> "none"
  LowLimitBits -> "low"
  HighLimitBits -> "high"
  ConstantLimitBits -> "constant"

-- |
-- > WriteRequestItemList:
-- >  product:
-- >    items: Vector ItemValue
-- >    itemPath: Maybe Text
--
-- > <WriteRequestItemList ItemPath="ss">
-- >  <Items ClientItemHandle="qwer_jklsd" ItemName="item" ItemPath="some/path">
-- >    <DiagnosticInfo>qwer</DiagnosticInfo>
-- >    <Quality LimitField="high" QualityField="badLastKnownValue" VendorField="3"></Quality>
-- >  </Items>
-- > </WriteRequestItemList>
writeRequestItemListElement :: WriteRequestItemList -> Xml.Element
writeRequestItemListElement wril =
  constructElement
    "WriteRequestItemList"
    ( catMaybes
        [("ItemPath",) <$> #itemPath wril]
    )
    (Xml.NodeElement . itemValueElement <$> toList (#items wril))

-- |
-- > Write:
-- >  product:
-- >    options: Maybe RequestOptions
-- >    itemList: Maybe WriteRequestItemList
-- >    returnValuesOnReply: Bool
--
-- > <Write ReturnValuesOnReply="true">
-- >  <WriteRequestItemList ItemPath="ss">
-- >    <Items ClientItemHandle="qwer_jklsd" ItemName="item" ItemPath="some/path">
-- >      <DiagnosticInfo>qwer</DiagnosticInfo>
-- >      <Quality LimitField="high" QualityField="badLastKnownValue" VendorField="3"></Quality>
-- >    </Items>
-- >  </WriteRequestItemList>
-- > </Write>
writeElement :: Write -> Xml.Element
writeElement w =
  constructElement
    "Write"
    [("ReturnValuesOnReply", bool (#returnValuesOnReply w))]
    ( catMaybes
        [ Xml.NodeElement . writeRequestItemListElement <$> #itemList w,
          Xml.NodeElement . requestOptionsElement <$> #options w
        ]
    )

writeDocument :: Write -> Xml.Document
writeDocument = inSoapEnvelope . Xml.NodeElement . writeElement

-- |
-- > ReadRequestItem:
-- >  product:
-- >    itemPath: Maybe Text
-- >    reqType: Maybe Xml.Name
-- >    itemName: Maybe Text
-- >    clientItemHandle: Maybe Text
-- >    maxAge: Maybe Int32
--
-- > <Items
-- >   ClientItemHandle="test"
-- >   ItemName="qwer"
-- >   ItemPath="some/path"
-- >   MaxAge="234"
-- >   ReqType="ss"></Items>
readRequestItemElement :: ReadRequestItem -> Xml.Element
readRequestItemElement rri =
  constructElement
    "Items"
    ( catMaybes
        [ ("ItemPath",) <$> #itemPath rri,
          ("ReqType",) . qName <$> #reqType rri,
          ("ItemName",) <$> #itemName rri,
          ("ClientItemHandle",) <$> #clientItemHandle rri,
          ("MaxAge",) . int <$> #maxAge rri
        ]
    )
    []

-- |
-- > ReadRequestItemList:
-- >  product:
-- >    items: Vector ReadRequestItem
-- >    itemPath: Maybe Text
-- >    reqType: Maybe Xml.Name
-- >    maxAge: Maybe Int32
--
-- > <ItemList
-- >    ItemPath="some/path"
-- >    MaxAge="234"
-- >    ReqType="Q:String">
-- >   <Items ClientItemHandle="test" ItemName="qwer" ItemPath="some/path" MaxAge="234" ReqType="ss"></Items>
-- > </ItemList>
readRequestItemListElement :: ReadRequestItemList -> Xml.Element
readRequestItemListElement rril =
  constructElement
    "ItemList"
    ( catMaybes
        [ ("ItemPath",) <$> #itemPath rril,
          ("ReqType",) . qName <$> #reqType rril,
          ("MaxAge",) . int <$> #maxAge rril
        ]
    )
    (Xml.NodeElement . readRequestItemElement <$> toList (#items rril))

-- |
-- > Read:
-- >  product:
-- >    options: Maybe RequestOptions
-- >    itemList: Maybe ReadRequestItemList
--
-- > <Read>
-- >   <Options ClientRequestHandle="qwer" LocaleID="qwer" ReturnItemName="true" ReturnItemPath="true"></Options>
-- >   <ItemList ItemPath="some/path" MaxAge="234" ReqType="Q:String">
-- >     <Items ClientItemHandle="test" ItemName="qwer" ItemPath="some/path" MaxAge="234" ReqType="ss"></Items>
-- >   </ItemList>
-- > </Read>
readElement :: Read -> Xml.Element
readElement r =
  constructElement
    "Read"
    []
    ( catMaybes
        [ Xml.NodeElement . requestOptionsElement <$> #options r,
          Xml.NodeElement . readRequestItemListElement <$> #itemList r
        ]
    )

readDocument :: Read -> Xml.Document
readDocument = inSoapEnvelope . Xml.NodeElement . readElement

-- |
-- > SubscriptionPolledRefresh:
-- > product:
-- >   options: Maybe RequestOptions
-- >   serverSubHandles: Vector Text
-- >   holdTime: Maybe UTCTime
-- >   waitTime: Int32
-- >   returnAllItems: Bool
--
-- > <ns1:SubscriptionPolledRefresh HoldTime="2026-19-23T16:34:38.004Z" ReturnAllItems="false" WaitTime="1000">
-- >   <ns1:Options ClientRequestHandle="ajskdlf_SubscriptionPolledRefresh" ReturnErrorText="true" ReturnItemName="true" ReturnItemPath="true">
-- >   </ns1:Options>
-- >   <ns1:ServerSubHandles>1000222</ns1:ServerSubHandles>
-- >   <ns1:ServerSubHandles>1002222</ns1:ServerSubHandles>
-- > </ns1:SubscriptionPolledRefresh>>
subscriptionPolledRefreshElement :: SubscriptionPolledRefresh -> Xml.Element
subscriptionPolledRefreshElement spr =
  constructElement
    "SubscriptionPolledRefresh"
    ( catMaybes
        [ ("HoldTime",) . dateTime <$> #holdTime spr,
          pure ("WaitTime", int (#waitTime spr)),
          pure ("ReturnAllItems", bool (#returnAllItems spr))
        ]
    )
    ( catMaybes [Xml.NodeElement . requestOptionsElement <$> #options spr]
        ++ fmap (Xml.NodeElement . serverSubHandlesElement) (toList (#serverSubHandles spr))
    )

serverSubHandlesElement :: Text -> Xml.Element
serverSubHandlesElement name =
  constructElement
    "ServerSubHandles"
    []
    [Xml.NodeContent name]

subscriptionPolledRefreshDocument :: SubscriptionPolledRefresh -> Xml.Document
subscriptionPolledRefreshDocument = inSoapEnvelope . Xml.NodeElement . subscriptionPolledRefreshElement

-- |
-- > SubscriptionCancel:
-- >   product:
-- >     serverSubHandle: Maybe Text
-- >     clientRequestHandle: Maybe Text
--
-- > <ns1:SubscriptionCancel ClientRequestHandle="asdffhhkl_SubscriptionCancel" ServerSubHandle="1111"></ns1:SubscriptionCancel>
subscriptionCancelElement :: SubscriptionCancel -> Xml.Element
subscriptionCancelElement sc =
  constructElement
    "SubscriptionCancel"
    ( catMaybes
        [ ("ClientRequestHandle",) <$> #clientRequestHandle sc,
          ("ServerSubHandle",) <$> #serverSubHandle sc
        ]
    )
    []

subscriptionCancelDocument :: SubscriptionCancel -> Xml.Document
subscriptionCancelDocument = inSoapEnvelope . Xml.NodeElement . subscriptionCancelElement

browseFilterText :: BrowseFilter -> Text
browseFilterText = \case
  AllBrowseFilter -> "all"
  BranchBrowseFilter -> "branch"
  ItemBrowseFilter -> "item"

-- |
-- > Browse:
-- >   product:
-- >     propertyNames: Vector Xml.Name
-- >     localeId: Maybe Text
-- >     clientRequestHandle: Maybe Text
-- >     itemPath: Maybe Text
-- >     itemName: Maybe Text
-- >     continuationPoint: Maybe Text
-- >     maxElementsReturned: Int32
-- >     browseFilter: BrowseFilter
-- >     elementNameFilter: Maybe Text
-- >     vendorFilter: Maybe Text
-- >     returnAllProperties: Bool
-- >     returnAllPropertyValues: Bool
-- >     returnErrorText: Bool
--
-- > <Browse BrowseFilter="all" ClientRequestHandle="qwer_rew" ContinuationPoint="endpoint" ElementNameFilter="qq" ItemName="someName" ItemPath="some/path" LocaleID="en" MaxElementsReturned="23" ReturnAllProperties="true" ReturnAllPropertyValues="false" ReturnErrorText="true" VendorFilter="ww">
-- >   <PropertyNames>test</PropertyNames>
-- >   <PropertyNames>qewr</PropertyNames>
-- > </Browse>
browseElement :: Browse -> Xml.Element
browseElement b =
  constructElement
    "Browse"
    ( catMaybes
        [ ("LocaleID",) <$> #localeId b,
          ("ClientRequestHandle",) <$> #clientRequestHandle b,
          ("ItemPath",) <$> #itemPath b,
          ("ItemName",) <$> #itemName b,
          ("ContinuationPoint",) <$> #continuationPoint b,
          pure ("MaxElementsReturned", int (#maxElementsReturned b)),
          pure ("BrowseFilter", browseFilterText (#browseFilter b)),
          ("ElementNameFilter",) <$> #elementNameFilter b,
          ("VendorFilter",) <$> #vendorFilter b,
          pure ("ReturnAllProperties", bool (#returnAllProperties b)),
          pure ("ReturnAllPropertyValues", bool (#returnAllPropertyValues b)),
          pure ("ReturnErrorText", bool (#returnErrorText b))
        ]
    )
    (Xml.NodeElement . propertyNamesElement . qName <$> toList (#propertyNames b))

browseDocument :: Browse -> Xml.Document
browseDocument = inSoapEnvelope . Xml.NodeElement . browseElement

-- |
-- > <PropertyNames>ss</PropertyNames>
propertyNamesElement :: Text -> Xml.Element
propertyNamesElement name =
  constructElement
    "PropertyNames"
    []
    [Xml.NodeContent name]

-- |
-- > GetProperties:
-- >   product:
-- >     itemIds: Vector ItemIdentifier
-- >     propertyNames: Vector Xml.Name
-- >     localeId: Maybe Text
-- >     clientRequestHandle: Maybe Text
-- >     itemPath: Maybe Text
-- >     returnAllProperties: Bool
-- >     returnPropertyValues: Bool
-- >     returnErrorText: Bool
--
-- > <GetProperties ClientRequestHandle="test" ItemPath="some/path" LocaleID="ww" ReturnAllProperties="true" ReturnErrorText="false" ReturnPropertyValues="false">
-- >   <PropertyNames>name1</PropertyNames>
-- >   <PropertyNames>name2</PropertyNames>
-- >   <ItemIdentifier ItemName="name" ItemPath="some/path"></ItemIdentifier>
-- >   <ItemIdentifier ItemName="name" ItemPath="some/path"></ItemIdentifier>
-- > </GetProperties>
getPropertiesElement :: GetProperties -> Xml.Element
getPropertiesElement gp =
  constructElement
    "GetProperties"
    ( catMaybes
        [ ("LocaleID",) <$> #localeId gp,
          ("ClientRequestHandle",) <$> #clientRequestHandle gp,
          ("ItemPath",) <$> #itemPath gp,
          pure ("ReturnAllProperties", bool (#returnAllProperties gp)),
          pure ("ReturnPropertyValues", bool (#returnPropertyValues gp)),
          pure ("ReturnErrorText", bool (#returnErrorText gp))
        ]
    )
    ( mconcat
        [ Xml.NodeElement . propertyNamesElement . qName <$> toList (#propertyNames gp),
          Xml.NodeElement . itemIdentifierElement <$> toList (#itemIds gp)
        ]
    )

getPropertiesDocument :: GetProperties -> Xml.Document
getPropertiesDocument = inSoapEnvelope . Xml.NodeElement . getPropertiesElement

-- |
-- > ItemIdentifier:
-- >   product:
-- >     itemPath: Maybe Text
-- >     itemName: Maybe Text
--
-- > <ItemIdentifier ItemName="name" ItemPath="some/path"></ItemIdentifier>
itemIdentifierElement :: ItemIdentifier -> Xml.Element
itemIdentifierElement ii =
  constructElement
    "ItemIdentifier"
    ( catMaybes
        [ ("ItemPath",) <$> #itemPath ii,
          ("ItemName",) <$> #itemName ii
        ]
    )
    []

packed :: Show a => a -> Text
packed = Text.pack . show

dateTime :: UTCTime -> Text
dateTime = Text.pack . iso8601Show

bool :: Bool -> Text
bool = Text.toLower . packed

float :: Float -> Text
float = packed

int :: Int32 -> Text
int = packed

qName :: QName -> Text
qName = error "TODO"
