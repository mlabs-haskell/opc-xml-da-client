module OpcXmlDaClient.XmlSchemaValues.Attoparsec
  ( dateTime,
    date,
    duration,
  )
where

import qualified Attoparsec.Data as Ad
import Data.Attoparsec.Text
import qualified Data.Text as Text
import qualified Data.Time.Format.ISO8601 as Iso8601
import OpcXmlDaClient.Base.Prelude
import OpcXmlDaClient.XmlSchemaValues.Types
import qualified OpcXmlDaClient.XmlSchemaValues.Util.TimeMath as TimeMath

-- |
-- XML Schema dateTime.
--
-- https://www.w3.org/TR/xmlschema-2/#dateTime
dateTime :: Parser UTCTime
dateTime = Ad.utcTimeInISO8601

-- |
-- XML Schema date.
--
-- https://www.w3.org/TR/xmlschema-2/#date
date :: Parser Date
date = do
  _applyNeg <- fmap TimeMath.negateDay <$ char '-' <|> pure id
  _day <- _applyNeg Ad.dayInISO8601
  _tz <- optional Ad.timeZoneInISO8601
  return $ Date _day _tz

-- |
-- XML Schema duration.
--
-- https://www.w3.org/TR/xmlschema-2/#duration
duration :: Parser Duration
duration = do
  _pos <- False <$ char '-' <|> pure True
  _textRemainder <- takeText
  _diff <- Iso8601.iso8601ParseM $ Text.unpack _textRemainder
  return $ Duration _pos _diff
