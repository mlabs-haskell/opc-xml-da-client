module Main where

import qualified Data.Attoparsec.Text as At
import qualified OpcXmlDaClient.QuickCheckUtil.Gens as Gens
import qualified OpcXmlDaClient.XmlSchemaValues.Attoparsec as Attoparsec
import qualified OpcXmlDaClient.XmlSchemaValues.Rendering as Rendering
import OpcXmlDaClient.XmlSchemaValues.Types
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Test.Tasty
import Test.Tasty.QuickCheck
import qualified Text.Builder as Tb
import Prelude hiding (choose)

main =
  defaultMain $
    testGroup
      ""
      [ testProperty "Rendered date parses into the same value" $ do
          _date <- Date <$> Gens.day <*> Gens.maybeOf Gens.timeZone
          let _rendering = Tb.run $ Rendering.date _date
              _parsingResult = At.parseOnly Attoparsec.date _rendering
           in return $ Right _date === _parsingResult
      ]
