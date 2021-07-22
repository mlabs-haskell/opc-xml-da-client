module Main where

import Data.Default
import qualified Network.HTTP.Client as Hc
import qualified OpcXmlDaClient as Opc
import Test.QuickCheck.Instances ()
import Test.Tasty
import Test.Tasty.HUnit
import Prelude

main =
  defaultMain $
    testGroup "Mocking server interactions" $
      let uri =
            fromJust . Opc.textUri $
              "http://info.advosol.com/XMLDADemo/XML_Sim/OpcXmlDaServer.asmx"
          manager = unsafePerformIO $ Hc.newManager Hc.defaultManagerSettings
          op _op = _op manager def uri
       in [ testCase "GetStatus" $ do
              _res <- op Opc.getStatus $ Opc.GetStatus Nothing Nothing
              assertBool (show _res) $ isRight _res
          ]
