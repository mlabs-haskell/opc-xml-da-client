module OpcXmlDaClient.Protocol where

import OpcXmlDaClient.Prelude
import Domain
import DomainOptics


declare
  (Just (True, False))
  (mconcat [
    enumDeriver,
    boundedDeriver,
    showDeriver,
    eqDeriver,
    ordDeriver,
    genericDeriver,
    dataDeriver,
    typeableDeriver,
    hashableDeriver,
    hasFieldDeriver,
    constructorIsLabelDeriver,
    accessorIsLabelDeriver,
    mapperIsLabelDeriver,
    labelOpticDeriver
    ])
  =<< loadSchema "schemas/protocol.yaml"
