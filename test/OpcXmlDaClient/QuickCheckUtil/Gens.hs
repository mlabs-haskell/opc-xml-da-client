module OpcXmlDaClient.QuickCheckUtil.Gens where

import qualified Data.Text as Text
import Data.Time
import qualified Data.Time.Calendar.OrdinalDate as Time
import Data.Time.Clock.System
import qualified Data.Time.LocalTime as Time
import OpcXmlDaClient.Base.Prelude hiding (choose, optional)
import Test.QuickCheck

-- * General

maybeOf :: Gen a -> Gen (Maybe a)
maybeOf gen =
  oneof [pure Nothing, Just <$> gen]

onceIn :: Int -> Gen Bool
onceIn n =
  (== 1) <$> chooseInt (1, n)

-- * Time

year :: Gen Integer
year = choose (-9999, 9999)

dayOfCommonYear :: Gen Int
dayOfCommonYear = choose (1, 365)

dayOfLeapYear :: Gen Int
dayOfLeapYear = choose (1, 366)

-- |
-- The arbitrary instance generates scarce values.
day :: Gen Day
day = do
  _year <- year
  _dayOfYear <- if Time.isLeapYear _year then dayOfLeapYear else dayOfCommonYear
  return $ Time.fromOrdinalDate _year _dayOfYear

-- |
-- The arbitrary instance generates broken values.
-- This is an alternative to it.
timeZone :: Gen TimeZone
timeZone =
  Time.hoursToTimeZone <$> choose (-24, 24)

utcTimeInRange :: UTCTime -> UTCTime -> Gen UTCTime
utcTimeInRange min max =
  systemToUTCTime . flip MkSystemTime 0 <$> choose (minSec, maxSec)
  where
    MkSystemTime minSec _ = utcToSystemTime min
    MkSystemTime maxSec _ = utcToSystemTime max

recentTime :: Gen UTCTime
recentTime =
  utcTimeInRange (read "2020-01-01 00:00:00Z") (unsafePerformIO (getCurrentTime))

-- * Text

text :: Gen Text
text =
  do
    firstSentence <- sentence
    otherSentences <- listOf $ do
      doParagraph <- onceIn 5
      let prefix = if doParagraph then "\n" else " "
      (prefix <>) <$> sentence
    return (mconcat (firstSentence : otherSentences))

sentence :: Gen Text
sentence =
  do
    firstWord <- Text.toTitle <$> word
    extraWordsAmount <- chooseInt (0, 20)
    extraWords <- replicateM extraWordsAmount $ do
      prefix <- do
        prependPunctuation <- (== 0) <$> chooseInt (0, 9)
        if prependPunctuation
          then elements [", ", ": ", " - "]
          else pure " "
      theWord <- do
        titleCase <- (== 0) <$> chooseInt (0, 9)
        (if titleCase then Text.toTitle else id) <$> word
      return (prefix <> theWord)
    return (firstWord <> mconcat extraWords)

word :: Gen Text
word =
  elements
    ["foo", "bar", "qux", "quux", "quuz", "corge", "grault", "garply", "waldo", "fred", "plugh", "xyzzy", "thud"]
