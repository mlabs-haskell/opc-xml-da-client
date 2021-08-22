module Pcap where

import Data.Binary.Bits.Get (block, runBitGet, word8)
import Data.Binary.Get (getWord32le, runGet)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import Data.ByteString.Internal (c2w)
import qualified Data.ByteString.Lazy as BL
import OpcXmlDaClient (Error (..))
import OpcXmlDaClient.Protocol.Types
import qualified OpcXmlDaClient.Protocol.XmlParsing as XmlParsing
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

-- * Make tests

makePcapTests :: IO TestTree
makePcapTests =
  lookupEnv "PCAP_TEST_FILE_PATH" >>= \case
    Nothing -> pure $ testCase "No .pcap file" $ assertBool "" True
    Just path -> do
      content <- B.readFile path <&> pcapToEther
      let (resps, _reqs) =
            separate $
              regroup $
                B.split (c2w '\n') $
                  mconcat $ addEmptyLine . unwrapTCP . unwrapIP . unwrapEther <$> content
      pure $
        testGroup "PCAP tests" $
          (zip [1 ..] resps) <&> \(n, resp) ->
            testCase ("Response #" <> show @Int n) $
              assertEqual "Parse corrent" Nothing $ tryToParse resp

-- * Response parsing

tryToParse :: ByteString -> Maybe (ByteString, [Error])
tryToParse str = ((str,) <$>) $ sequence $ responseParsers <&> \(XmlResponseParser (parser, check')) -> check' parser str
  where
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
    xml = XmlResponseParser
    checkError decode s =
      Xp.parseByteString decode s & either
        do Just . ParsingError
        do either (Just . SoapError) (const Nothing)

-- |
-- This function concatinates strings which don't have empty strings between them
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

-- |
-- This function sorts strings to requests and responses
separate :: [ByteString] -> ([ByteString], [ByteString])
separate = foldr f ([], [])
  where
    f = \x (a, b) ->
      if
          | x `startWith` prefResp -> ((B8.drop (B8.length prefResp) x) : a, b)
          | x `startWith` prefReq -> (a, x : b)
          | otherwise -> (a, b)
    prefResp = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
    prefReq = "<SOAP-ENV:Envelope"
    startWith a pref = B8.take (B8.length pref) a == pref

-- * .pcap parsing

pcapToEther :: ByteString -> [ByteString]
pcapToEther b' = go (B.drop 24 b') []
  where
    go b v = case B.length b of
      0 -> v
      _ ->
        let l = fromIntegral $ runGet getWord32le (BL.fromStrict $ B.drop 8 b)
            (payload, tail') = B.splitAt l (B.drop 16 b)
         in go tail' (v <> [payload])

unwrapEther :: ByteString -> ByteString
unwrapEther = B.reverse . B.dropWhile isPaddingByte . B.reverse . B.drop etherHeaderLength
  where
    etherHeaderLength = 14
    isPaddingByte = (== 0)

unwrapIP :: ByteString -> ByteString
unwrapIP b =
  let (_, headerLength32) =
        flip runGet (BL.fromStrict b) $
          runBitGet $ block $ (,) <$> word8 4 <*> word8 4
   in B.drop ((fromIntegral headerLength32) * 4) b

unwrapTCP :: ByteString -> ByteString
unwrapTCP b =
  let headerLength32 = flip runGet (BL.drop 12 $ BL.fromStrict b) $ runBitGet $ block $ word8 4
   in B.drop ((fromIntegral headerLength32) * 4) b

-- * Helpers

addEmptyLine :: ByteString -> ByteString
addEmptyLine x =
  if B8.take 4 x `elem` (["HTTP", "POST"] :: [ByteString])
    then "\n\n" <> x
    else x
