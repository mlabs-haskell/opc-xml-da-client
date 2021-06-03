module OpcXmlDaClient.Protocol.Types where

import qualified Data.XML.Types as Xml
import qualified Domain
import OpcXmlDaClient.Prelude hiding (Read)

Domain.declare
  (Just (True, False))
  ( mconcat
      [ Domain.enumDeriver,
        Domain.boundedDeriver,
        Domain.showDeriver,
        Domain.eqDeriver,
        Domain.ordDeriver,
        Domain.genericDeriver,
        Domain.dataDeriver,
        Domain.typeableDeriver,
        Domain.hasFieldDeriver,
        Domain.constructorIsLabelDeriver,
        Domain.accessorIsLabelDeriver
      ]
  )
  =<< Domain.loadSchema "schemas/protocol.yaml"
