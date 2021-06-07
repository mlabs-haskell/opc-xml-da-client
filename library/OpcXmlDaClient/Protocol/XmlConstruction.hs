module OpcXmlDaClient.Protocol.XmlConstruction where

import qualified Data.Map.Strict as Map
import OpcXmlDaClient.Prelude hiding (Read, bool)
import OpcXmlDaClient.Protocol.Types
import qualified Text.XML as Xml

-- |
-- Wraps the element in the following snippet.
-- Passing to it the name of the OPC namespace.
--
-- > <SOAP-ENV:Envelope
-- >   xmlns:SOAP-ENC="http://schemas.xmlsoap.org/soap/encoding/"
-- >   xmlns:SOAP-ENV="http://schemas.xmlsoap.org/soap/envelope/"
-- >   xmlns:ZSI="http://www.zolera.com/schemas/ZSI/"
-- >   xmlns:xsd="http://www.w3.org/2001/XMLSchema"
-- >   xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
-- >   <SOAP-ENV:Header/>
-- >   <SOAP-ENV:Body xmlns:opc="http://opcfoundation.org/webservices/XMLDA/1.0/">
-- >     ...
-- >   </SOAP-ENV:Body>
-- > </SOAP-ENV:Envelope>
inSoapEnvelope :: (Text -> Xml.Node) -> Xml.Document
inSoapEnvelope elementByNamespace =
  Xml.Document
    (Xml.Prologue [] Nothing [])
    ( Xml.Element
        (Xml.Name "Envelope" (Just "SOAP-ENV") Nothing)
        ( Map.fromList
            [ ( Xml.Name "SOAP-ENC" (Just "xmlns") Nothing,
                "http://schemas.xmlsoap.org/soap/encoding/"
              ),
              ( Xml.Name "SOAP-ENV" (Just "xmlns") Nothing,
                "http://schemas.xmlsoap.org/soap/envelope/"
              ),
              ( Xml.Name "ZSI" (Just "xmlns") Nothing,
                "http://www.zolera.com/schemas/ZSI/"
              ),
              ( Xml.Name "xsd" (Just "xmlns") Nothing,
                "http://www.w3.org/2001/XMLSchema"
              ),
              ( Xml.Name "xsi" (Just "xmlns") Nothing,
                "http://www.w3.org/2001/XMLSchema-instance"
              )
            ]
        )
        [ Xml.NodeElement
            ( Xml.Element
                (Xml.Name "Header" (Just "SOAP-ENV") Nothing)
                (Map.empty)
                []
            ),
          Xml.NodeElement
            ( Xml.Element
                (Xml.Name "Body" (Just "SOAP-ENV") Nothing)
                ( Map.fromList
                    [ ( Xml.Name namespace (Just "xmlns") Nothing,
                        "http://opcfoundation.org/webservices/XMLDA/1.0/"
                      )
                    ]
                )
                [elementByNamespace namespace]
            )
        ]
    )
    []
  where
    namespace = "opc"

-- |
-- > <ns1:Subscribe ReturnValuesOnReply="true" SubscriptionPingRate="3000">
-- >   <ns1:Options ClientRequestHandle="ZSI_mc3RFGFSn7_Subscribe" ReturnErrorText="true" ReturnItemName="true" ReturnItemPath="true"/>
-- >   <ns1:ItemList>
-- >     <ns1:Items ClientItemHandle="ZSI_mc3RFGFSn7_SubscribeItem_0" ItemName="Loc/Wec/Plant3/Vwind"/>
-- >     <ns1:Items ClientItemHandle="ZSI_mc3RFGFSn7_SubscribeItem_1" ItemName="Loc/Wec/Plant3/P"/>
-- >     <ns1:Items ClientItemHandle="ZSI_mc3RFGFSn7_SubscribeItem_2" ItemName="Loc/Wec/Plant3/Status/St"/>
-- >   </ns1:ItemList>
-- > </ns1:Subscribe>
subscribe :: Maybe Text -> Text -> Subscribe -> Xml.Element
subscribe ns name x =
  Xml.Element
    (Xml.Name name ns Nothing)
    ( Map.fromList
        [ ( Xml.Name "ReturnValuesOnReply" Nothing Nothing,
            if #returnValuesOnReply x then "true" else "false"
          ),
          ( Xml.Name "SubcriptionPingRate" Nothing Nothing,
            showText (#subscriptionPingRate x)
          )
        ]
    )
    ( catMaybes
        [ fmap (Xml.NodeElement . requestOptions ns "Options") (#options x),
          fmap (Xml.NodeElement . subscribeRequestItemList ns "ItemList") (#itemList x)
        ]
    )

-- |
-- > <ns1:Options
-- >   ClientRequestHandle="ZSI_mc3RFGFSn7_Subscribe"
-- >   ReturnErrorText="true"
-- >   ReturnItemName="true"
-- >   ReturnItemPath="true"/>
requestOptions :: Maybe Text -> Text -> RequestOptions -> Xml.Element
requestOptions ns name x =
  Xml.Element
    (Xml.Name name ns Nothing)
    ( Map.fromList
        ( catMaybes
            [ if #returnErrorText x then Nothing else Just ("ReturnErrorText", "true"),
              if #returnDiagnosticInfo x then Just ("ReturnDiagnosticInfo", "true") else Nothing,
              if #returnItemTime x then Just ("ReturnItemTime", "true") else Nothing,
              if #returnItemPath x then Just ("ReturnItemPath", "true") else Nothing,
              if #returnItemName x then Just ("ReturnItemName", "true") else Nothing,
              fmap ((,) "RequestDeadline" . dateTime) (#requestDeadline x),
              fmap ((,) "ClientRequestHandle") (#clientRequestHandle x),
              fmap ((,) "LocaleID") (#localeId x)
            ]
        )
    )
    []

-- |
-- > <ns1:ItemList>
-- >   <ns1:Items ClientItemHandle="ZSI_mc3RFGFSn7_SubscribeItem_0" ItemName="Loc/Wec/Plant3/Vwind"/>
-- >   <ns1:Items ClientItemHandle="ZSI_mc3RFGFSn7_SubscribeItem_1" ItemName="Loc/Wec/Plant3/P"/>
-- >   <ns1:Items ClientItemHandle="ZSI_mc3RFGFSn7_SubscribeItem_2" ItemName="Loc/Wec/Plant3/Status/St"/>
subscribeRequestItemList :: Maybe Text -> Text -> SubscribeRequestItemList -> Xml.Element
subscribeRequestItemList ns name x =
  Xml.Element
    (Xml.Name name ns Nothing)
    ( Map.fromList
        ( catMaybes
            [ fmap (("ItemPath",)) (#itemPath x),
              fmap (("ReqType",) . qName) (#reqType x),
              fmap (("Deadband",) . float) (#deadband x),
              fmap (("RequestedSamplingRate",) . int) (#requestedSamplingRate x),
              fmap (("EnableBuffering",) . boolean) (#enableBuffering x)
            ]
        )
    )
    (fmap (Xml.NodeElement . subscribeRequestItem ns "Items") (toList (#items x)))

-- |
-- > <ns1:Items ClientItemHandle="ZSI_mc3RFGFSn7_SubscribeItem_0" ItemName="Loc/Wec/Plant3/Vwind"/>
subscribeRequestItem :: Maybe Text -> Text -> SubscribeRequestItem -> Xml.Element
subscribeRequestItem ns name x =
  Xml.Element
    (Xml.Name name ns Nothing)
    ( Map.fromList
        ( catMaybes
            [ fmap (("ItemPath",)) (#itemPath x),
              fmap (("ReqType",) . qName) (#reqType x),
              fmap (("ItemName",)) (#itemName x),
              fmap (("ClientItemHandle",)) (#clientItemHandle x),
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
