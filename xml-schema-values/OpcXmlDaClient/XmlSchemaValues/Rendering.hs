module OpcXmlDaClient.XmlSchemaValues.Rendering
  ( dateTime,
    date,
    duration,
  )
where

import qualified Data.Time.Format.ISO8601 as Time
import OpcXmlDaClient.Base.Prelude
import OpcXmlDaClient.XmlSchemaValues.Types
import Text.Builder

dateTime :: UTCTime -> Builder
dateTime = fromString . Time.iso8601Show

date :: Date -> Builder
date (Date a b) = fromString (Time.iso8601Show a) <> foldMap (fromString . Time.iso8601Show) b

duration :: Duration -> Builder
duration = error "TODO"
