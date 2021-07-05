module OpcXmlDaClient.XmlSchemaValues.Attoparsec
  ( dateTime,
    date,
    duration,
  )
where

import qualified Attoparsec.Data as Ad
import Data.Attoparsec.Text
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
  char 'P'
  _years <- optional $ decimal <* char 'Y'
  _months <- optional $ decimal <* char 'M'
  _days <- optional $ decimal <* char 'D'
  asum
    [ do
        char 'T'
        _hours <- optional $ decimal <* char 'H'
        _minutes <- optional $ decimal <* char 'M'
        _seconds <- optional $ scientific <* char 'S'
        return $ Duration _pos _years _months _days _hours _minutes _seconds,
      return $ Duration _pos _years _months _days Nothing Nothing Nothing
    ]
