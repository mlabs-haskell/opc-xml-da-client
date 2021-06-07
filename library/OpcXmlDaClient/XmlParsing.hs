module OpcXmlDaClient.XmlParsing where

import OpcXmlDaClient.Prelude hiding (Read)
import OpcXmlDaClient.Types
import qualified Text.XML as Xml

-- |
-- Parsing error details and hopefully a location.
data Error

getStatusResponse :: Xml.Element -> Either Error GetStatusResponse
getStatusResponse = error "TODO"

readResponse :: Xml.Element -> Either Error ReadResponse
readResponse = error "TODO"

writeResponse :: Xml.Element -> Either Error WriteResponse
writeResponse = error "TODO"

subscribeResponse :: Xml.Element -> Either Error SubscribeResponse
subscribeResponse = error "TODO"

subscriptionPolledRefreshResponse :: Xml.Element -> Either Error SubscriptionPolledRefreshResponse
subscriptionPolledRefreshResponse = error "TODO"

subscriptionCancelResponse :: Xml.Element -> Either Error SubscriptionCancelResponse
subscriptionCancelResponse = error "TODO"

browseResponse :: Xml.Element -> Either Error BrowseResponse
browseResponse = error "TODO"

getPropertiesResponse :: Xml.Element -> Either Error GetPropertiesResponse
getPropertiesResponse = error "TODO"
