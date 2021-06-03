module OpcXmlDaClient.Protocol.XmlConstruction where

import qualified Data.Map.Strict as Map
import OpcXmlDaClient.Prelude hiding (Read)
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
