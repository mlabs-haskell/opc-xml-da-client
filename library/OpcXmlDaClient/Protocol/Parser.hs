module OpcXmlDaClient.Protocol.Parser where

import Control.Lens hiding (element)
import Data.Kind
import Data.Text.Lazy.Lens (packed)
import OpcXmlDaClient.Prelude
import OpcXmlDaClient.Protocol.Types
import qualified Text.XML as Xml
import Text.XML.Cursor
import qualified Text.XML.Cursor as Cursor
import Text.XML.Lens
import qualified Prelude

--------------------------------------------------------------------------------
-- Parser
--------------------------------------------------------------------------------

-- TODO: We can actually think of making the most generic parser generator
-- designed for SOAP protocol that generates parsers out of the domain spec

data Some :: (Type -> Type) -> Type where
  -- TODO: dict witnesses if we want something more than `Show`
  Some :: Show a => f a -> Some f

deriving instance Show (Some ParsedValue)

-- Since we extend data with @.yaml@ spec we still
-- need to write parsers for it, this data considers
-- values that we can parse
-- TODO: generate it from declarations in `Types.hs` file
data ParsedValue :: Type -> Type where
  VGetStatus :: ParsedValue GetStatus
  VGetStatusResponse :: ParsedValue GetStatusResponse

deriving instance Show a => Show (ParsedValue a)

data ParseError
  = OtherParseError Text
  deriving stock (Eq, Show)

parse :: Xml.Document -> Either ParseError (Some ParsedValue)
parse document
  | let cursor = Cursor.fromDocument document =
    Left $ OtherParseError "TODO"

parseGetStatus :: Fold Xml.Element GetStatus
parseGetStatus = runFold do
  _localeId <- Fold $ error "TODO"
  _clientRequestHandle <- Fold $ error "TODO"
  pure GetStatus {_localeId, _clientRequestHandle}

parseSubscribeItemValue :: Fold Xml.Element SubscribeItemValue
parseSubscribeItemValue = runFold do
  _itemValue <- Fold $ error "TODO"
  _revisedSamplingRate <- Fold $ error "TODO"
  pure SubscribeItemValue {_itemValue, _revisedSamplingRate}

--------------------------------------------------------------------------------
-- Example stuff
--------------------------------------------------------------------------------

ex =
  mconcat
    [ "<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?>",
      "<books>",
      "<book category=\"Language and library definition\">",
      "<title>Haskell 98 language and libraries: the Revised Report</title>",
      "<author year=\"2003\">Simon Peyton Jones</author>",
      "<pages>272 pages</pages>",
      "<price>£45.00</price>",
      "</book>",
      "<book category=\"Textbooks\">",
      "<title>Learn You a Haskell for Great Good!</title>",
      "<author year=\"2011\">Miran Lipovaca</author>",
      "<pages>360 pages</pages>",
      "</book>",
      "<book category=\"Textbooks\">",
      "<title>Programming in Haskell</title>",
      "<author year=\"2007\">Graham Hutton</author>",
      "<pages>200 pages</pages>",
      "</book>",
      "</books>"
    ]

data Book = Book
  { bookTitle :: BookTitle,
    bookAuthor :: Text
  }
  deriving stock (Show, Eq)

data BookTitle = BookTitle Text deriving stock (Show, Eq)

-- TODO: prettify api, make aliases for readable errors, generate parsers from TH spec
-- type Parser = ReifiedFold
-- runParser = runFold
-- parseElement l = Fold $ runFold

parseBookAnonymous :: ReifiedFold Xml.Element Book
parseBookAnonymous = do
  bookTitle <- Fold $ runFold parseBookTitle
  pure do Book {bookTitle, bookAuthor = "anonymous"}

parseBookWithAuthor :: ReifiedFold Xml.Element Book
parseBookWithAuthor = do
  bookTitle <- Fold $ runFold parseBookTitle
  bookAuthor <- Fold $ named "author" . text
  pure do Book {bookTitle, bookAuthor}

parseBookTitle :: ReifiedFold Xml.Element BookTitle
parseBookTitle = do
  title <- Fold $ named "title" . text
  pure do BookTitle title

parseBook :: Fold Xml.Element Book
parseBook = runFold $ asum [parseBookWithAuthor, parseBookAnonymous]

pb = parseBook . to Right

-- failingOver [x] = failing x (to id)
-- failingOver (x:xs) = failing x (failingOver xs)

f :: IO ()
f
  | let doc = Xml.parseLBS def ex
        plist =
          doc
            ^? traversed
              . root
              . named "books"
            ... named "book"
            ... pb
            ^. non (Left "Cannot parse book") =
    print plist

f' :: IO ()
f'
  | let cursor =
          Cursor.fromDocument
            . either (error . Prelude.show) id
            $ Xml.parseLBS def ex
        parsed =
          cursor
            $.// element "books"
            &// element "book"
            &/ element "title"
            &/ content =
    print parsed
