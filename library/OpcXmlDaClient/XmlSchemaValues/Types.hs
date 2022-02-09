module OpcXmlDaClient.XmlSchemaValues.Types where

import qualified Domain
import qualified DomainOptics
import OpcXmlDaClient.Base.Prelude

Domain.declare
  (Just (True, False))
  ( mconcat
      [ Domain.enumDeriver,
        Domain.boundedDeriver,
        Domain.showDeriver,
        Domain.eqDeriver,
        Domain.genericDeriver,
        Domain.dataDeriver,
        Domain.typeableDeriver,
        Domain.hasFieldDeriver,
        Domain.constructorIsLabelDeriver,
        Domain.accessorIsLabelDeriver,
        DomainOptics.labelOpticDeriver
      ]
  )
  =<< Domain.loadSchema "domain/xml-schema-values.domain.yaml"

deriving instance Ord Date

deriving instance Ord Time

instance Ord Duration where
  compare = on compare (\(Duration a (CalendarDiffTime b c)) -> (a, b, c))
