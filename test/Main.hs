module Main where

import qualified Data.ByteString.Lazy as LazyByteString
import qualified OpcXmlDaClient.XmlParsing as XmlParsing
import qualified Test.QuickCheck as QuickCheck
import OpcXmlDaClient.Types
import Test.QuickCheck.Instances
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Test.Tasty.Runners
import qualified XmlUnscrambler as Xu
import Prelude

main =
  defaultMain $
    testGroup
      "Parsers"
      [ testCase "SubscribeResponse" $ do
          Right x <- Xu.parseFile XmlParsing.subscribeResponse "samples/680.response.xml"
          assertEqual "" (Just "Handle1") (#serverSubHandle x)
          assertEqual "" (Just (read "2019-09-23 16:01:50.576+00:00")) (fmap #rcvTime (#subscribeResult x))
      ]
