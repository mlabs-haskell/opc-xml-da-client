module OpcXmlDaClient.XmlSchemaValues.Util.TimeMath where

import OpcXmlDaClient.Base.Prelude

negateDay :: Day -> Day
negateDay =
  -- TODO: Test it
  coerce update
  where
    ModifiedJulianDay baseOffset = read "0000-01-01"
    update x =
      negate (x + baseOffset) - baseOffset
