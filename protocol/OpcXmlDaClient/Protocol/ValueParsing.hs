module OpcXmlDaClient.Protocol.ValueParsing where

import qualified Attoparsec.Data as AttoparsecData
import qualified Data.Vector.Generic as GenericVector
import OpcXmlDaClient.Base.Prelude
import qualified OpcXmlDaClient.Base.Vector as VectorUtil
import qualified OpcXmlDaClient.Protocol.Namespaces as Ns
import qualified OpcXmlDaClient.Protocol.Types as ProtocolTypes
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

data Value a
  = Value
      Text
      -- ^ Type name for the containing array.
      Text
      -- ^ Tag name for elements of the containing array.
      (Xp.Element (Either Error a))
      -- ^ Element parser extended with value parsing error capability.
  deriving (Functor)

elementBase :: Text -> Text -> Text -> Text -> Xp.Element (Either Error a) -> Value a
elementBase typeNs typeName arrayTypeName arrayElementTagName contParser =
  Value arrayTypeName arrayElementTagName $
    join $
      Xp.attributesByName $ do
        _type <- ProtocolXp.xsiType
        case _type of
          ProtocolTypes.NamespacedQName _ns _name ->
            if _ns == typeNs
              then
                if _name == typeName
                  then return $ contParser
                  else return $ return $ Left $ UnexpectedTypeNameError typeName _name
              else return $ return $ Left $ UnexpectedTypeNamespaceError typeNs _ns
          ProtocolTypes.UnnamespacedQName _name ->
            return $ return $ Left $ UnnamespacedTypeNameError typeNs typeName _name

contentBase :: Text -> Text -> Text -> Text -> Xp.Content a -> Value a
contentBase typeNs typeName arrayTypeName arrayElementTagName contentParser =
  elementBase typeNs typeName arrayTypeName arrayElementTagName $
    fmap Right $ Xp.children $ Xp.contentNode $ contentParser

primitiveBase :: Text -> Text -> Xp.Content a -> Value a
primitiveBase typeName arrayTypeName =
  contentBase Ns.xsd typeName arrayTypeName typeName

-- |
-- Homogenous array.
array :: GenericVector.Vector v a => Value a -> Value (v a)
array (Value arrayTypeName arrayElementTagName elementParser) =
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

nilable :: Value a -> Value (Maybe a)
nilable (Value arrayTypeName arrayElementTagName elementParser) =
  Value arrayTypeName arrayElementTagName $ do
    _isNil <- Xp.attributesByName $ ProtocolXp.isNil
    if _isNil
      then return (Right Nothing)
      else fmap (fmap Just) elementParser

-- |
-- Parser of a vendor-specific non-standard value element node.
--
-- Expects a function on a resolved QName of the type and the content nodes.
vendor :: (ProtocolTypes.QName -> Xml.Element -> Either Text a) -> Value a
vendor =
  error "TODO"

primitive :: Primitive a -> Value a
primitive (Primitive arrayTypeName typeName parser) =
  contentBase Ns.xsd typeName arrayTypeName typeName parser

-- *

arrayOfPrimitive :: GenericVector.Vector vector a => Primitive a -> Value (vector a)
arrayOfPrimitive =
  error "TODO"

arrayOfAnyType ::
  GenericVector.Vector vector a =>
  -- | Non-empty list of alternative value parsers for each element of the vector.
  NonEmpty (AnyType a) ->
  Value (vector (Maybe a))
arrayOfAnyType =
  error "TODO"

-- *

data AnyType a
  = AnyType
      (Maybe Text)
      Text
      (Xp.Element a)

-- *

data Primitive a
  = Primitive
      Text
      -- ^ Type name for the containing array.
      Text
      -- ^ Primitive name and name of the primitive array element tag at the same time.
      (Xp.Content a)
      -- ^ Element content parser.

deriving instance Functor Primitive

decimal :: Primitive Scientific
decimal = Primitive "ArrayOfDecimal" "decimal" $ Xp.attoparsedContent AttoparsecData.lenientParser

byte :: Primitive Word8
byte = Primitive "ArrayOfByte" "byte" $ Xp.attoparsedContent AttoparsecData.lenientParser

short :: Primitive Int16
short = Primitive "ArrayOfShort" "short" $ Xp.attoparsedContent AttoparsecData.lenientParser
