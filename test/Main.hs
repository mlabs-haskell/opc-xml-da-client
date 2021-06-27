module Main where

import qualified Data.ByteString.Lazy as LazyByteString
import qualified OpcXmlDaClient.XmlParsing as XmlParsing
import qualified Test.QuickCheck as QuickCheck
import Test.QuickCheck.Instances
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Test.Tasty.Runners
import qualified XmlUnscrambler as Xu
import Prelude hiding (assert)

main =
  defaultMain $
    testGroup "All tests" $
      [ testCase "" $ do
          res <- Xu.parseFile XmlParsing.subscribeResponse "samples/680.response.xml"
          let expected = Left ""
           in assertEqual "" expected res
      ]
