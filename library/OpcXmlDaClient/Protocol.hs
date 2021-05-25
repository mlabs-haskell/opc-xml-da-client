module OpcXmlDaClient.Protocol where

import OpcXmlDaClient.Prelude
import Domain


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
    mapperIsLabelDeriver
    ])
  =<< loadSchema "schemas/protocol.yaml"
