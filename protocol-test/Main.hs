{-# LANGUAGE OverloadedLists #-}

module Main where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import Data.ByteString.Internal (c2w)
import qualified Data.Vector as Vector
import qualified Network.HTTP.Client as Hc
import Network.Pcap
import OpcXmlDaClient (Error (..))
import OpcXmlDaClient.Protocol.Types
import qualified OpcXmlDaClient.Protocol.XmlParsing as XmlParsing
import Test.QuickCheck.Instances ()
import Test.Tasty
import Test.Tasty.HUnit
import qualified XmlParser as Xp
import Prelude

data XmlResponseParser
  = forall a.
    XmlResponseParser
      ( Xp.Element (Either SoapFault a),
        Xp.Element (Either SoapFault a) -> ByteString -> Maybe Error
      )

responseParsers :: [XmlResponseParser]
responseParsers =
  [ xml (XmlParsing.getStatusResponse, checkError),
    xml (XmlParsing.readResponse, checkError),
    xml (XmlParsing.writeResponse, checkError),
    xml (XmlParsing.subscribeResponse, checkError),
    xml (XmlParsing.subscriptionPolledRefreshResponse, checkError),
    xml (XmlParsing.subscriptionCancelResponse, checkError),
    xml (XmlParsing.browseResponse, checkError),
    xml (XmlParsing.getPropertiesResponse, checkError)
  ]
  where
    xml = XmlResponseParser

checkError :: Xp.Element (Either SoapFault o) -> ByteString -> Maybe Error
checkError decode str = case Xp.parseByteString decode str of
  Right res -> case res of
    Right res -> Nothing
    Left err -> Just $ SoapError err
  Left err -> Just $ ParsingError err

tryToParse :: ByteString -> Maybe [Error]
tryToParse str = sequence $ responseParsers <&> \(XmlResponseParser (parser, check)) -> check parser str

regroup :: [ByteString] -> [ByteString]
regroup str =
  let (x, y) =
        foldr
          ( \s (k, ks) ->
              if B.length s <= 2
                then ("", ks <> [k | k /= ""])
                else (k <> s, ks)
          )
          ("", [])
          str
   in x : y

separate :: [ByteString] -> ([ByteString], [ByteString])
separate = foldr f ([], [])
  where
    f = \x (a, b) ->
      if
          | B8.take (B8.length prefResp) x == prefResp -> ((B8.drop (B8.length prefResp) x) : a, b)
          | B8.take (B8.length prefReq) x == prefReq -> (a, x : b)
          | otherwise -> (a, b)
    prefResp = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>" -- prefix for responses
    prefReq = "<SOAP-ENV:Envelope" -- prefix for requests

readPcap :: IO TestTree
readPcap = do
  h <- openOffline "/home/freak/Downloads/opcOperations.s0i0.pcap"
  (resps, _reqs) <- separate . regroup . B.split (c2w '\n') <$> run h ""
  pure $
    testGroup "PCAP tests" $
      (zip [1 ..] resps) <&> \(n, resp) ->
        testCase ("Response #" <> show n) $
          assertEqual "Parse corrent" Nothing $ tryToParse resp
  where
    getLength ph =
      let l = hdrCaptureLength ph
       in if l > 100
            then 54
            else fromIntegral l
    run h f = do
      (ph, bs) <- nextBS h
      if ph == PktHdr 0 0 0 0
        then pure f
        else do
          let dropped = B.drop (getLength ph) bs
              dropped' =
                if B8.take 4 dropped `elem` (["HTTP", "POST"] :: [ByteString])
                  then "\n\n" <> dropped
                  else dropped
          run h (f <> dropped')

main = do
  pcapTests <- readPcap
  defaultMain $
    testGroup "" $
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
                    (Right (Left (SoapFault SenderSoapFaultCode "Server was unable to read request. ---> There is an error in XML document (4, 32). ---> The string 'dateTime' is not a valid AllXsd value.")))
                    parsingResult
              ],
        testGroup "Fault on old SOAP Response" $
          let parsingResult =
                unsafePerformIO $
                  Xp.parseFile XmlParsing.subscribeResponse "samples/fault-on-old-soap.response.xml"
           in [ testCase "" $ do
                  assertEqual
                    ""
                    (Right (Left (SoapFault SenderSoapFaultCode "XML syntax error")))
                    parsingResult
              ]
      ]
