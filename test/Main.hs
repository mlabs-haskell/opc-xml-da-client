{-# LANGUAGE OverloadedLists #-}

module Main where

import qualified Data.Vector as Vector
import OpcXmlDaClient.Protocol.Types
import qualified OpcXmlDaClient.Protocol.XmlParsing as XmlParsing
import Test.QuickCheck.Instances ()
import Test.Tasty
import Test.Tasty.HUnit
import qualified XmlParser as Xp
import Prelude

main =
  defaultMain $
    let parsingResult =
          unsafePerformIO $
            Xp.parseFile XmlParsing.subscribeResponse "samples/680.response.xml"
     in testGroup
          "Subscribe Response"
          [ testCase "Top level properties" $ do
              assertEqual
                ""
                (Right (Just "Handle1"))
                (fmap #serverSubHandle parsingResult),
            testCase "DateTime" $ do
              assertEqual
                ""
                (Right (Just (read "2019-09-23 16:01:50.576+00:00")))
                (fmap (fmap #rcvTime . #subscribeResult) parsingResult),
            testCase "Item value at offset 0" $ do
              assertEqual
                ""
                (Right (Just (FloatValue 4.5)))
                (fmap (join . fmap #value . fmap #itemValue . join . fmap (Vector.!? 0) . fmap #items . #rItemList) parsingResult),
            testCase "Item value at offset 1" $ do
              assertEqual
                ""
                (Right (Just (IntValue 1234)))
                (fmap (join . fmap #value . fmap #itemValue . join . fmap (Vector.!? 1) . fmap #items . #rItemList) parsingResult),
            testCase "Item value at offset 2" $ do
              assertEqual
                ""
                (Right (Just (ArrayOfUnsignedShortValue [0, 0, 3, 11, 0, 0])))
                (fmap (join . fmap #value . fmap #itemValue . join . fmap (Vector.!? 2) . fmap #items . #rItemList) parsingResult)
          ]
