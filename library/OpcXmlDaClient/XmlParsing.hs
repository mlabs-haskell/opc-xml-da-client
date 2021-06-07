module OpcXmlDaClient.XmlParsing where

import OpcXmlDaClient.Prelude hiding (Read)
import OpcXmlDaClient.Types
import qualified Text.XML as Xml

-- |
-- Parsing error details and hopefully a location.
data Error

getStatusResponse :: Xml.Document -> Either Error GetStatusResponse
getStatusResponse = error "TODO"

readResponse :: Xml.Document -> Either Error ReadResponse
readResponse = error "TODO"

writeResponse :: Xml.Document -> Either Error WriteResponse
writeResponse = error "TODO"

subscribeResponse :: Xml.Document -> Either Error SubscribeResponse
subscribeResponse = error "TODO"

subscriptionPolledRefreshResponse :: Xml.Document -> Either Error SubscriptionPolledRefreshResponse
subscriptionPolledRefreshResponse = error "TODO"

subscriptionCancelResponse :: Xml.Document -> Either Error SubscriptionCancelResponse
subscriptionCancelResponse = error "TODO"

browseResponse :: Xml.Document -> Either Error BrowseResponse
browseResponse = error "TODO"

getPropertiesResponse :: Xml.Document -> Either Error GetPropertiesResponse
getPropertiesResponse = error "TODO"
