module OpcXmlDaClient.Protocol.Namespaces where

import OpcXmlDaClient.Base.Prelude

-- |
-- Namespace for SOAP v1.2.
soapEnv :: Text =
  "http://www.w3.org/2003/05/soap-envelope"

-- |
-- Namespace for SOAP v <1.2
soapEnv2 :: Text =
  "http://schemas.xmlsoap.org/soap/envelope/"

soapEnc :: Text =
  "http://schemas.xmlsoap.org/soap/encoding/"

xsi :: Text =
  "http://www.w3.org/2001/XMLSchema-instance"

xsd :: Text =
  "http://www.w3.org/2001/XMLSchema"

opc :: Text =
  "http://opcfoundation.org/webservices/XMLDA/1.0/"
