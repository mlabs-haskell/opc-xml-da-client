module OpcXmlDaClient.XmlConstruction where

import qualified Data.Map.Strict as Map
import OpcXmlDaClient.Prelude hiding (Read, bool)
import OpcXmlDaClient.Types
import qualified Text.XML as Xml

-- * Final documents

getStatusDocument :: GetStatus -> Xml.Document
getStatusDocument = inSoapEnvelope . Xml.NodeElement . error "TODO"

readDocument :: Read -> Xml.Document
readDocument = inSoapEnvelope . Xml.NodeElement . error "TODO"

writeDocument :: Write -> Xml.Document
writeDocument = inSoapEnvelope . Xml.NodeElement . error "TODO"

subscribeDocument :: Subscribe -> Xml.Document
subscribeDocument = inSoapEnvelope . Xml.NodeElement . subscribe "Subscribe"

subscriptionPolledRefreshDocument :: SubscriptionPolledRefresh -> Xml.Document
subscriptionPolledRefreshDocument = inSoapEnvelope . Xml.NodeElement . error "TODO"

subscriptionCancelDocument :: SubscriptionCancel -> Xml.Document
subscriptionCancelDocument = inSoapEnvelope . Xml.NodeElement . error "TODO"

browseDocument :: Browse -> Xml.Document
browseDocument = inSoapEnvelope . Xml.NodeElement . error "TODO"

getPropertiesDocument :: GetProperties -> Xml.Document
getPropertiesDocument = inSoapEnvelope . Xml.NodeElement . error "TODO"

-- * Names

unnamespacedName :: Text -> Xml.Name
unnamespacedName name =
  Xml.Name name Nothing Nothing

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

-- * Elements

-- |
-- > <ns1:Subscribe ReturnValuesOnReply="true" SubscriptionPingRate="3000">
-- >   <ns1:Options ClientRequestHandle="Subscribe" ReturnErrorText="true" ReturnItemName="true" ReturnItemPath="true"/>
-- >   <ns1:ItemList>
-- >     <ns1:Items ClientItemHandle="SubscribeItem_0" ItemName="A"/>
-- >     <ns1:Items ClientItemHandle="SubscribeItem_1" ItemName="B"/>
-- >     <ns1:Items ClientItemHandle="SubscribeItem_2" ItemName="C"/>
-- >   </ns1:ItemList>
-- > </ns1:Subscribe>
subscribe :: Text -> Subscribe -> Xml.Element
subscribe name x =
  Xml.Element
    (xmlDaName name)
    ( Map.fromList
        [ ( unnamespacedName "ReturnValuesOnReply",
            if #returnValuesOnReply x then "true" else "false"
          ),
          ( unnamespacedName "SubcriptionPingRate",
            showText (#subscriptionPingRate x)
          )
        ]
    )
    ( catMaybes
        [ fmap (Xml.NodeElement . requestOptions "Options") (#options x),
          fmap (Xml.NodeElement . subscribeRequestItemList "ItemList") (#itemList x)
        ]
    )

-- |
-- > <ns1:Options
-- >   ClientRequestHandle="Subscribe"
-- >   ReturnErrorText="true"
-- >   ReturnItemName="true"
-- >   ReturnItemPath="true"/>
requestOptions :: Text -> RequestOptions -> Xml.Element
requestOptions name x =
  Xml.Element
    (xmlDaName name)
    ( Map.fromList
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
    )
    []

-- |
-- > <ns1:ItemList>
-- >   <ns1:Items ClientItemHandle="SubscribeItem_0" ItemName="A"/>
-- >   <ns1:Items ClientItemHandle="SubscribeItem_1" ItemName="B"/>
-- >   <ns1:Items ClientItemHandle="SubscribeItem_2" ItemName="C"/>
subscribeRequestItemList :: Text -> SubscribeRequestItemList -> Xml.Element
subscribeRequestItemList name x =
  Xml.Element
    (xmlDaName name)
    ( Map.fromList
        ( catMaybes
            [ fmap ("ItemPath",) (#itemPath x),
              fmap (("ReqType",) . qName) (#reqType x),
              fmap (("Deadband",) . float) (#deadband x),
              fmap (("RequestedSamplingRate",) . int) (#requestedSamplingRate x),
              fmap (("EnableBuffering",) . boolean) (#enableBuffering x)
            ]
        )
    )
    (fmap (Xml.NodeElement . subscribeRequestItem "Items") (toList (#items x)))

-- |
-- > <ns1:Items ClientItemHandle="SubscribeItem_0" ItemName="A"/>
subscribeRequestItem :: Text -> SubscribeRequestItem -> Xml.Element
subscribeRequestItem name x =
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
              fmap (("EnableBuffering",) . boolean) (#enableBuffering x)
            ]
        )
    )
    []

dateTime :: DateTime -> Text
dateTime =
  error "TODO"

float :: Float -> Text
float =
  error "TODO"

int :: Int32 -> Text
int =
  error "TODO"

boolean :: Bool -> Text
boolean =
  error "TODO"

qName :: Xml.Name -> Text
qName =
  error "TODO"
