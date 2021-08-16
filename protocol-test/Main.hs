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
import Network.Pcap
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString as B
import Data.ByteString.Internal (c2w)

regroup :: [ByteString] -> [ByteString]
regroup str = 
  let (x, y) = foldr
        (\s (k,ks) -> if B.length s <= 2
                         then ("", ks <> [k | k /= ""])
                         else (k <> s, ks)
        )
        ("", []) 
        str
  in x:y

separate :: [ByteString] -> ([ByteString], [ByteString])
separate ss = go ss 1 ([], []) where
  go :: [ByteString] -> Int -> ([ByteString], [ByteString]) -> ([ByteString], [ByteString])
  go []     _ a     = a
  go (x:xs) c (a,b) = go xs (c + 1)  if
      | (c + 2) `mod` 4 == 0 -> (x:a ,  b  )
      | c       `mod` 4 == 0 -> (a   ,  x:b)
      | otherwise            -> (a   ,  b  )



readPcap :: IO ()
readPcap = do
  h <- openOffline "/home/freak/Downloads/opcOperations.s0i0.pcap" 
  (resps, reqs) <- separate . regroup . B.split (c2w '\n') <$> run h ""
  putStrLn "==="
  putStrLn "Responses"
  sequence_ $ fmap B8.putStrLn resps
  putStrLn "==="
  putStrLn "Requests"
  sequence_ $ fmap B8.putStrLn reqs
  putStrLn "==="
    where 
      getLength ph = let l = hdrCaptureLength ph
                      in if l > 100
                            then 54
                            else fromIntegral l
      run h f = do
        (ph, bs) <- nextBS h
        if ph == PktHdr 0 0 0 0
        then pure f
        else do
          let dropped = B.drop (getLength ph) bs
              dropped' = if B8.take 4 dropped `elem` (["HTTP", "POST"] :: [ByteString])
                            then "\n\n" <> dropped
                            else dropped
          run h (f <> dropped')

main = do
  readPcap
  defaultMain $
    testGroup "" $
      [ testGroup "Subscribe Response" $
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
