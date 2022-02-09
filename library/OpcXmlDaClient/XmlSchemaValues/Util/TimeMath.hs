module OpcXmlDaClient.XmlSchemaValues.Util.TimeMath where

import Data.Time.Calendar.OrdinalDate
import OpcXmlDaClient.Base.Prelude

negateDay :: Day -> Day
negateDay x =
  case toOrdinalDate x of
    (year, dayOfYear) -> fromOrdinalDate (negate year) dayOfYear
