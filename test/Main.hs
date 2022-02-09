module Main where

import OpcXmlDaClient.Base.Prelude
import qualified OpcXmlDaClient.TestSuites.Mocking as MockingSuite
import qualified OpcXmlDaClient.TestSuites.Protocol as ProtocolSuite
import qualified OpcXmlDaClient.TestSuites.XmlSchemaValues as XmlSchemaValuesSuite
import Test.QuickCheck.Instances ()
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main =
  defaultMain . testGroup ""
    =<< sequence
      [ testGroup "Protocol" <$> ProtocolSuite.initTests,
        pure $ testGroup "XML Schema" XmlSchemaValuesSuite.tests,
        testGroup "Mocking server interactions" <$> MockingSuite.initTests
      ]
