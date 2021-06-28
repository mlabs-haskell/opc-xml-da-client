module Main where

import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Vector as Vector
import OpcXmlDaClient.Types
import qualified OpcXmlDaClient.XmlParsing as XmlParsing
import qualified Test.QuickCheck as QuickCheck
import Test.QuickCheck.Instances
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Test.Tasty.Runners
import qualified Text.XML as Xml
import qualified XmlUnscrambler as Xu
import Prelude

main =
  defaultMain $
    testGroup
      "Subscribe Response"
      [ testCase "Top level properties" $ do
          Right x <- Xu.parseFile XmlParsing.subscribeResponse "samples/680.response.xml"
          assertEqual "" (Just "Handle1") (#serverSubHandle x),
        testCase "DateTime" $ do
          Right x <- Xu.parseFile XmlParsing.subscribeResponse "samples/680.response.xml"
          assertEqual "" (Just (read "2019-09-23 16:01:50.576+00:00")) (fmap #rcvTime (#subscribeResult x)),
        testCase "Item value at offset 0" $ do
          Right x <- Xu.parseFile XmlParsing.subscribeResponse "samples/680.response.xml"
          assertEqual
            ""
            (Just (Value (NamespacedQName "http://www.w3.org/2001/XMLSchema" "float") [Xml.NodeContent "4.5"]))
            ((join . fmap #value . fmap #itemValue . join . fmap (Vector.!? 0) . fmap #items . #rItemList) x),
        testCase "Item value at offset 1" $ do
          Right x <- Xu.parseFile XmlParsing.subscribeResponse "samples/680.response.xml"
          assertEqual
            ""
            (Just (Value (NamespacedQName "http://www.w3.org/2001/XMLSchema" "int") [Xml.NodeContent "1234"]))
            ((join . fmap #value . fmap #itemValue . join . fmap (Vector.!? 1) . fmap #items . #rItemList) x),
        testCase "Item value at offset 2" $ do
          Right x <- Xu.parseFile XmlParsing.subscribeResponse "samples/680.response.xml"
          assertEqual
            ""
            ( Just
                ( Value
                    (NamespacedQName "http://opcfoundation.org/webservices/XMLDA/1.0/" "ArrayOfUnsignedShort")
                    ( fmap
                        (Xml.NodeElement . Xml.Element (Xml.Name "unsignedShort" (Just "http://opcfoundation.org/webservices/XMLDA/1.0/") Nothing) mempty . pure . Xml.NodeContent)
                        ["0", "0", "3", "11", "0", "0"]
                    )
                )
            )
            ((join . fmap #value . fmap #itemValue . join . fmap (Vector.!? 2) . fmap #items . #rItemList) x)
      ]
