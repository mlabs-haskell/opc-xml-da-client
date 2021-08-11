module OpcXmlDaClient.Protocol.Types where

import Data.Time.Clock
import qualified Data.Vector.Unboxed as Uv
import qualified Domain
import qualified DomainOptics
import OpcXmlDaClient.Base.Prelude hiding (Read)
import OpcXmlDaClient.XmlSchemaValues.Types
import qualified Text.XML as Xml

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
        Domain.accessorIsLabelDeriver,
        DomainOptics.labelOpticDeriver
      ]
  )
  =<< Domain.loadSchema "protocol/types.domain.yaml"
