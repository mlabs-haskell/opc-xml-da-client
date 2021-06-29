-- |
-- A utility library providing automatic namespacing,
-- which is required in handling of custom value types by the OPC protocol.
module OpcXmlDaClient.XmlBuilder
  ( -- * ByteString
    elementXml,

    -- * Element
    Element,
    element,

    -- * Node
    Node,
    elementNode,
    contentNode,
    astNode,

    -- * Content
    Content,
    textContent,
    qNameContent,

    -- * QName
    QName,
    namespacedQName,
    unnamespacedQName,
  )
where

import qualified Data.ByteString.Lazy as Lbs
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import OpcXmlDaClient.Base.Prelude
import qualified OpcXmlDaClient.XmlBuilder.Identified as Identified
import qualified Text.XML as Xml

-- *

elementXml :: Element -> ByteString
elementXml (Element namespacedElement) =
  case runNamespaced namespacedElement of
    (element, namespaces) ->
      let newElement =
            element
              { Xml.elementAttributes =
                  foldl'
                    (\map (uri, prefix) -> Map.insert (Xml.Name prefix Nothing (Just "xmlns")) uri map)
                    (Xml.elementAttributes element)
                    namespaces
              }
          document = Xml.Document (Xml.Prologue [] Nothing []) newElement []
       in Lbs.toStrict (Xml.renderLBS def document)

-- *

newtype Element = Element (Namespaced Xml.Element)

element :: QName -> [(QName, Content)] -> [Node] -> Element
element (QName qName) attrList nodeList =
  Element $
    Xml.Element
      <$> qName
      <*> fmap Map.fromList (traverse attr attrList)
      <*> traverse runNode nodeList
  where
    attr (QName name, Content content) =
      (,) <$> name <*> content

-- *

newtype Node = Node {runNode :: Namespaced Xml.Node}

elementNode :: Element -> Node
elementNode (Element a) = Node (fmap Xml.NodeElement a)

contentNode :: Content -> Node
contentNode (Content a) = Node (fmap Xml.NodeContent a)

astNode :: Xml.Node -> Node
astNode = Node . pure

-- *

newtype Content = Content (Namespaced Text)

instance IsString Content where
  fromString = textContent . fromString

textContent :: Text -> Content
textContent text =
  Content (pure text)

qNameContent :: QName -> Content
qNameContent (QName qName) =
  Content (fmap renderName qName)
  where
    renderName (Xml.Name name _ prefix) =
      maybe name (\prefix -> prefix <> ":" <> name) prefix

-- *

newtype QName = QName (Namespaced Xml.Name)

instance IsString QName where
  fromString = unnamespacedQName . fromString

-- |
-- Namespaced QName.
namespacedQName ::
  -- | Namespace URI.
  Text ->
  -- | Name.
  Text ->
  QName
namespacedQName uri name =
  QName (Identified.identifying uri (\prefix -> Xml.Name name (Just uri) (Just prefix)))

-- |
-- Unnamespaced QName.
unnamespacedQName :: Text -> QName
unnamespacedQName name =
  QName (pure (Xml.Name name Nothing Nothing))

-- *

type Namespaced = Identified.Identified Text Text

runNamespaced :: Namespaced a -> (a, [(Text, Text)])
runNamespaced identified =
  Identified.run identified alias
  where
    alias x = "ns" <> fromString (show (succ x))
