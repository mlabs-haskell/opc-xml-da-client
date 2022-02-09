module OpcXmlDaClient.XmlSchemaValues.Rendering
  ( dateTime,
    time,
    date,
    duration,
  )
where

import qualified Data.Time.Format.ISO8601 as Iso8601
import OpcXmlDaClient.Base.Prelude
import OpcXmlDaClient.XmlSchemaValues.Types
import Text.Builder

dateTime :: UTCTime -> Builder
dateTime = iso8601Show

time :: Time -> Builder
time (Time a b) = iso8601Show a <> foldMap iso8601Show b

date :: Date -> Builder
date (Date a b) = iso8601Show a <> foldMap iso8601Show b

duration :: Duration -> Builder
duration (Duration a b) = bool (mappend "-") id a $ iso8601Show b

iso8601Show :: Iso8601.ISO8601 a => a -> Builder
iso8601Show = fromString . Iso8601.iso8601Show
