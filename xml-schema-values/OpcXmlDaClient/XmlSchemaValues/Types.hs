module OpcXmlDaClient.XmlSchemaValues.Types where

import qualified Domain
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
        Domain.accessorIsLabelDeriver
      ]
  )
  =<< Domain.loadSchema "xml-schema-values/types.domain.yaml"

deriving instance Ord Date

instance Ord Duration where
  compare = on compare (\(Duration a (CalendarDiffTime b c)) -> (a, b, c))
