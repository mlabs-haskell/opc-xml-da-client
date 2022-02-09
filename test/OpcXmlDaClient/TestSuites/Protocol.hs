{-# LANGUAGE OverloadedLists #-}

module OpcXmlDaClient.TestSuites.Protocol where

import qualified Data.Vector as Vector
import OpcXmlDaClient.Base.Prelude
import OpcXmlDaClient.Protocol.Types
import qualified OpcXmlDaClient.Protocol.XmlParsing as XmlParsing
import qualified OpcXmlDaClient.TestSuites.Protocol.Pcap as Pcap
import Test.QuickCheck.Instances ()
import Test.Tasty
import Test.Tasty.HUnit
import qualified XmlParser as Xp

initTests :: IO [TestTree]
initTests = do
  pcapTests <- Pcap.makePcapTests
  return $
    [ pcapTests,
      testGroup "Subscribe Response" $
        let parsingResult =
              unsafePerformIO $
                Xp.parseFile XmlParsing.subscribeResponse "samples/680.response.xml"
         in [ testCase "Top level properties" $ do
                assertEqual
                  ""
                  (Right (Right (Just "Handle1")))
                  ((fmap . fmap) #serverSubHandle parsingResult),
              testCase "DateTime" $ do
                assertEqual
                  ""
                  (Right (Right (Just (read "2019-09-23 16:01:50.576+00:00"))))
                  ((fmap . fmap) (fmap #rcvTime . #subscribeResult) parsingResult),
              testCase "Item value at offset 0" $ do
                assertEqual
                  ""
                  (Right (Right (Just (FloatValue 4.5))))
                  ((fmap . fmap) (join . fmap #value . fmap #itemValue . join . fmap (Vector.!? 0) . fmap #items . #rItemList) parsingResult),
              testCase "Item value at offset 1" $ do
                assertEqual
                  ""
                  (Right (Right (Just (IntValue 1234))))
                  ((fmap . fmap) (join . fmap #value . fmap #itemValue . join . fmap (Vector.!? 1) . fmap #items . #rItemList) parsingResult),
              testCase "Item value at offset 2" $ do
                assertEqual
                  ""
                  (Right (Right (Just (ArrayOfUnsignedShortValue [0, 0, 3, 11, 0, 0]))))
                  ((fmap . fmap) (join . fmap #value . fmap #itemValue . join . fmap (Vector.!? 2) . fmap #items . #rItemList) parsingResult)
            ],
      testGroup "Fault Response" $
        let parsingResult =
              unsafePerformIO $
                Xp.parseFile XmlParsing.subscribeResponse "samples/fault.response.xml"
         in [ testCase "" $ do
                assertEqual
                  ""
                  (Right (Left (SoapFault (#std SenderStdSoapFaultCode) "Server was unable to read request. ---> There is an error in XML document (4, 32). ---> The string 'dateTime' is not a valid AllXsd value.")))
                  parsingResult
            ],
      testGroup "Fault on old SOAP Response" $
        let parsingResult =
              unsafePerformIO $
                Xp.parseFile XmlParsing.subscribeResponse "samples/fault-on-old-soap.response.xml"
         in [ testCase "" $ do
                assertEqual
                  ""
                  (Right (Left (SoapFault (#std SenderStdSoapFaultCode) "XML syntax error")))
                  parsingResult
            ],
      testGroup "Fault on old SOAP Response 2" $
        let parsingResult =
              unsafePerformIO $
                Xp.parseFile XmlParsing.subscribeResponse "samples/fault-on-old-soap-2.response.xml"
         in [ testCase "" $ do
                assertEqual
                  ""
                  (Right (Left (SoapFault (#custom (UnnamespacedQName "E_NOSUBSCRIPTION")) "E_NOSUBSCRIPTION")))
                  parsingResult
            ]
    ]
