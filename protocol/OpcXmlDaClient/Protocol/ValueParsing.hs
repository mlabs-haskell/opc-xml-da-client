module OpcXmlDaClient.Protocol.ValueParsing where

import qualified Data.Vector.Generic as GenericVector
import OpcXmlDaClient.Base.Prelude
import qualified OpcXmlDaClient.Base.Vector as VectorUtil
import qualified OpcXmlDaClient.Protocol.Namespaces as Ns
import OpcXmlDaClient.Protocol.Types
import qualified OpcXmlDaClient.Protocol.XmlParsing as ProtocolXp
import qualified Text.XML as Xml
import qualified XmlParser as Xp

data Error
  = UnexpectedTypeNamespaceError
      Text
      -- ^ Expected.
      Text
      -- ^ Actual.
  | UnexpectedTypeNameError
      Text
      -- ^ Expected.
      Text
      -- ^ Actual.
  | UnnamespacedTypeNameError
      Text
      -- ^ Expected namespace.
      Text
      -- ^ Expected name.
      Text
      -- ^ Actual type name.
  | ArrayElementError
      Int
      -- ^ Offset in the array.
      Error
      -- ^ Reason.

data ValueParser a
  = ValueParser
      Text
      -- ^ Type name for the containing array.
      Text
      -- ^ Tag name for elements of the containing array.
      (Xp.Element (Either Error a))
      -- ^ Element parser extended with value parsing error capability.

elementBase :: Text -> Text -> Text -> Text -> Xp.Element (Either Error a) -> ValueParser a
elementBase typeNs typeName arrayTypeName arrayElementTagName contParser =
  ValueParser arrayTypeName arrayElementTagName $
    join $
      Xp.attributesByName $ do
        _type <- ProtocolXp.xsiType
        case _type of
          NamespacedQName _ns _name ->
            if _ns == typeNs
              then
                if _name == typeName
                  then return $ contParser
                  else return $ return $ Left $ UnexpectedTypeNameError typeName _name
              else return $ return $ Left $ UnexpectedTypeNamespaceError typeNs _ns
          UnnamespacedQName _name ->
            return $ return $ Left $ UnnamespacedTypeNameError typeNs typeName _name

contentBase :: Text -> Text -> Text -> Text -> Xp.Content a -> ValueParser a
contentBase typeNs typeName arrayTypeName arrayElementTagName contentParser =
  elementBase typeNs typeName arrayTypeName arrayElementTagName $
    fmap Right $ Xp.children $ Xp.contentNode $ contentParser

primitive :: Text -> Text -> Xp.Content a -> ValueParser a
primitive typeName arrayTypeName =
  contentBase Ns.xsd typeName arrayTypeName typeName

-- |
-- Homogenous array.
array :: GenericVector.Vector v a => ValueParser a -> ValueParser (v a)
array (ValueParser arrayTypeName arrayElementTagName elementParser) =
  elementBase Ns.opc arrayTypeName "ArrayOfAnyType" "anyType" $
    Xp.childrenByName $
      let build !list !offset =
            join $
              asum
                [ Xp.byName (Just Ns.opc) arrayElementTagName $ do
                    res <- elementParser
                    case res of
                      Right element -> return $ build (element : list) (succ offset)
                      Left err -> return $ return $ Left $ ArrayElementError offset err,
                  return $ return $ Right $ VectorUtil.fromReverseListN offset list
                ]
       in build [] 0

nilable :: ValueParser a -> ValueParser (Maybe a)
nilable (ValueParser arrayTypeName arrayElementTagName elementParser) =
  ValueParser arrayTypeName arrayElementTagName $ do
    _isNil <- Xp.attributesByName $ ProtocolXp.isNil
    if _isNil
      then return (Right Nothing)
      else fmap (fmap Just) elementParser

-- |
-- Parser of a vendor-specific non-standard value element node.
--
-- Expects a function on a resolved QName of the type and the content nodes.
vendor :: (QName -> [Xml.Node] -> Either Text a) -> ValueParser a
vendor =
  error "TODO"

decimal :: ValueParser Scientific
decimal = primitive "decimal" "ArrayOfDecimal" ProtocolXp.decimalContent
