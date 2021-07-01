-- |
-- Constructor of value parsers.
module OpcXmlDaClient.Protocol.ValueParsing
  ( -- * Execution
    parseValue,

    -- * Values
    Value,
    primitive,
    arrayOfPrimitive,
    arrayOfAnyType,
    vendor,

    -- * Primitives
    Primitive,
    decimal,
    byte,
    short,

    -- * Errors
    Error (..),
    ValueError (..),
  )
where

import qualified Attoparsec.Data as AttoparsecData
import qualified Data.Map.Strict as Map
import qualified Data.Vector.Generic as GenericVector
import OpcXmlDaClient.Base.Prelude
import qualified OpcXmlDaClient.Base.Vector as VectorUtil
import qualified OpcXmlDaClient.Protocol.Namespaces as Ns
import qualified OpcXmlDaClient.Protocol.Types as ProtocolTypes
import qualified OpcXmlDaClient.Protocol.XmlParsing as ProtocolXp
import qualified Text.XML as Xml
import qualified XmlParser as Xp

-- * Errors

data Error
  = ValueError ValueError
  | XmlError Text

data ValueError
  = ArrayElementValueError
      Int
      -- ^ Offset in the array.
      ValueError
      -- ^ Reason.
  | ByTypeNameValueError
      ProtocolTypes.QName
      -- ^ Matching parsed type name.
      ValueError
      -- ^ Reason.
  | NoneOfTypesMatchValueError
      [ProtocolTypes.QName]
      -- ^ Expected type names.
      ProtocolTypes.QName
      -- ^ Actual type name.

-- * Execution

parseValue ::
  -- | Alternative value parsers to try before failing.
  NonEmpty (Value a) ->
  -- | XML element AST to parse.
  Xml.Element ->
  -- | Either a parsing error or a result.
  Either Error a
parseValue =
  error "TODO"

-- |
-- Squash the value parser into element, bundling in the checks for the expected type.
applyTypeExpectations :: [Value a] -> Xp.Element (Either ValueError a)
applyTypeExpectations parsers =
  join $
    Xp.attributesByName $ do
      _type <- ProtocolXp.xsiType
      case Map.lookup _type parserByNameMap of
        Just parser ->
          return $ fmap (first (ByTypeNameValueError _type)) parser
        Nothing ->
          return $ return $ Left $ NoneOfTypesMatchValueError expectedNameList _type
  where
    (expectedNameList, parserByNameMap) =
      parsers
        & fmap
          ( \(Value expectedTypeNamespace expectedTypeName elementParser) ->
              ( case expectedTypeNamespace of
                  Just expectedTypeNamespace ->
                    ProtocolTypes.NamespacedQName expectedTypeNamespace expectedTypeName
                  Nothing ->
                    ProtocolTypes.UnnamespacedQName expectedTypeName,
                elementParser
              )
          )
        & \list -> (fmap fst list, Map.fromList list)

-- *

-- |
-- Parser of any value.
data Value a
  = Value
      (Maybe Text)
      -- ^ Expected type namespace.
      Text
      -- ^ Expected type name.
      (Xp.Element (Either ValueError a))
      -- ^ Element parser.
  deriving (Functor)

-- |
-- Lift a standard primitive parser into parser of any value.
primitive :: Primitive a -> Value a
primitive (Primitive _ typeName contentParser) =
  Value (Just Ns.opc) typeName $ fmap Right $ Xp.children $ Xp.contentNode contentParser

-- |
-- Array over a primitive type.
--
-- Works with all the @ArrayOf...@ types, excluding @ArrayOfAnyType@,
-- for which we provide 'arrayOfAnyType'.
arrayOfPrimitive :: GenericVector.Vector vector a => Primitive a -> Value (vector a)
arrayOfPrimitive (Primitive arrayTypeName typeName contentParser) =
  Value (Just Ns.opc) arrayTypeName $
    fmap Right $
      Xp.childrenByName $
        VectorUtil.many $ Xp.byName (Just Ns.opc) typeName $ Xp.children $ Xp.contentNode contentParser

arrayOfAnyType ::
  GenericVector.Vector vector (Maybe a) =>
  -- | Alternative value parsers tried on each element of the array.
  NonEmpty (Value a) ->
  Value (vector (Maybe a))
arrayOfAnyType valueParsers =
  Value (Just Ns.opc) "ArrayOfAnyType" $
    Xp.childrenByName $
      VectorUtil.manyWithIndexTerminating $ \i ->
        Xp.byName (Just Ns.opc) "anyType" $
          fmap (first (ArrayElementValueError i)) $ do
            _isNil <- Xp.attributesByName $ ProtocolXp.isNil
            if _isNil
              then return (Right Nothing)
              else fmap (fmap Just) (applyTypeExpectations (toList valueParsers))

-- |
-- Parser of a vendor-specific non-standard value element node.
vendor ::
  -- | Type namespace.
  Maybe Text ->
  -- | Type name.
  Text ->
  -- | Custom value element parser.
  Xp.Element a ->
  Value a
vendor =
  error "TODO"

-- *

-- |
-- Parser of standard primitives listed in the spec of OPC XML DA.
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
