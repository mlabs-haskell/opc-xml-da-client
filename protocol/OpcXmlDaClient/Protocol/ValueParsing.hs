-- |
-- Constructor of value parsers.
module OpcXmlDaClient.Protocol.ValueParsing
  ( -- * Execution
    parseValue,

    -- * Values
    Value,

    -- ** Simple
    string,
    boolean,
    float,
    double,
    decimal,
    long,
    int,
    byte,
    unsignedLong,
    unsignedInt,
    unsignedShort,
    unsignedByte,
    base64Binary,
    dateTime,
    time,
    duration,
    qName,

    -- ** Array
    arrayOfPrimitive,
    arrayOfByte,
    arrayOfShort,
    arrayOfUnsignedShort,
    arrayOfInt,
    arrayOfUnsignedInt,
    arrayOfLong,
    arrayOfWord64,
    arrayOfFloat,
    arrayOfDecimal,
    arrayOfDouble,
    arrayOfBoolean,
    arrayOfString,
    arrayOfDateTime,
    arrayOfAnyType,

    -- ** Custom
    vendor,

    -- * Errors
    Error (..),
    ValueError (..),
  )
where

import qualified Attoparsec.Data as AttoparsecData
import qualified Data.Map.Strict as Map
import qualified Data.Vector.Generic as GenericVector
import qualified Data.Vector.Unboxed as Uv
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
      (NonEmpty ProtocolTypes.QName)
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
parseValue parsers xml =
  Xp.parseElementAst (applyTypeExpectations parsers) xml
    & either (Left . XmlError) (first ValueError)

-- |
-- Squash the value parser into element, bundling in the checks for the expected type.
applyTypeExpectations :: NonEmpty (Value a) -> Xp.Element (Either ValueError a)
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
        & \list -> (fmap fst list, Map.fromList (toList list))

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

-- ** Simple values

-- |
-- Lift a standard primitive parser into parser of any value.
primitive :: Text -> Xp.Content a -> Value a
primitive typeName contentParser =
  Value (Just Ns.xsd) typeName $ fmap Right $ Xp.children $ Xp.contentNode contentParser

-- |
-- A sequence of UNICODE characters.
string :: Value Text
string = primitive "string" $ Xp.textContent

-- |
-- A binary logic value (true or false).
boolean :: Value Bool
boolean = primitive "boolean" $ Xp.attoparsedContent $ AttoparsecData.lenientParser

-- |
-- An IEEE single-precision 32-bit floating point value.
float :: Value Float
float = primitive "float" $ Xp.attoparsedContent $ fmap realToFrac $ AttoparsecData.lenientParser @Double

-- |
-- An IEEE double-precision 64-bit floating point value.
double :: Value Double
double = primitive "double" $ Xp.attoparsedContent $ AttoparsecData.lenientParser

-- |
-- A fixed-point decimal value with arbitrary precision.
-- Application development environments impose practical limitations on the precision supported by this type. XML-DA compliant applications must support at least the range supported by the VT_CY type.
decimal :: Value Scientific
decimal = primitive "decimal" $ Xp.attoparsedContent $ AttoparsecData.lenientParser

-- |
-- A 64-bit signed integer value.
long :: Value Int64
long = primitive "long" $ Xp.attoparsedContent $ AttoparsecData.lenientParser

-- |
-- A 32-bit signed integer value.
int :: Value Int32
int = primitive "int" $ Xp.attoparsedContent $ AttoparsecData.lenientParser

-- |
-- A 16-bit signed integer value.
short :: Value Int16
short = primitive "short" $ Xp.attoparsedContent $ AttoparsecData.lenientParser

-- |
-- An 8-bit signed integer value.
-- Note this differs from the definition of ‘byte’ used in most programming laguages.
byte :: Value Int8
byte = primitive "byte" $ Xp.attoparsedContent $ AttoparsecData.lenientParser

-- |
-- A 64-bit unsigned integer value.
unsignedLong :: Value Word64
unsignedLong = primitive "unsignedLong" $ Xp.attoparsedContent $ AttoparsecData.lenientParser

-- |
-- A 32-bit unsigned integer value.
unsignedInt :: Value Word32
unsignedInt = primitive "unsignedInt" $ Xp.attoparsedContent $ AttoparsecData.lenientParser

-- |
-- A 16-bit unsigned integer value.
unsignedShort :: Value Word16
unsignedShort = primitive "unsignedShort" $ Xp.attoparsedContent $ AttoparsecData.lenientParser

-- |
-- An 8-bit unsigned integer value.
unsignedByte :: Value Word8
unsignedByte = primitive "unsignedByte" $ Xp.attoparsedContent $ AttoparsecData.lenientParser

-- |
-- A sequence of 8-bit values represented in XML with Base-64 Encoding.
base64Binary :: Value ByteString
base64Binary = primitive "base64Binary" ProtocolXp.base64BinaryContent

-- |
-- A specific instance in time.
dateTime :: Value UTCTime
dateTime = primitive "dateTime" $ Xp.attoparsedContent $ AttoparsecData.lenientParser

-- |
-- An instant of time that recurs every day.
time :: Value LocalTime
time = primitive "time" $ error "TODO"

-- |
-- A Gregorian calendar date.
date :: Value Day
date = primitive "date" $ Xp.attoparsedContent $ AttoparsecData.lenientParser

-- |
-- A duration of time as specified by Gregorian year, month, day, hour, minute, and second components.
duration :: Value DiffTime
duration = primitive "duration" $ Xp.attoparsedContent $ error "TODO"

-- |
-- An XML qualified name comprising of a name and a namespace.
-- The name must be a valid XML element name and the namespace must be a valid URI.
-- QNames are equal only if the name and the namespace are equal.
qName :: Value (Maybe Text, Text)
qName = error "TODO"

-- ** Arrays

-- |
-- Array over a primitive type.
--
-- Works with all the @ArrayOf...@ types, excluding @ArrayOfAnyType@,
-- for which we provide 'arrayOfAnyType'.
arrayOfPrimitive :: GenericVector.Vector vector a => Text -> Text -> Xp.Content a -> Value (vector a)
arrayOfPrimitive arrayTypeName typeName contentParser =
  Value (Just Ns.opc) arrayTypeName $
    fmap Right $
      Xp.childrenByName $
        VectorUtil.many $ Xp.byName (Just Ns.opc) typeName $ Xp.children $ Xp.contentNode contentParser

arrayOfByte :: Value (Uv.Vector Int8)
arrayOfByte = arrayOfPrimitive "ArrayOfByte" "byte" $ Xp.attoparsedContent AttoparsecData.lenientParser

arrayOfShort :: Value (Uv.Vector Int16)
arrayOfShort = arrayOfPrimitive "ArrayOfShort" "short" $ Xp.attoparsedContent AttoparsecData.lenientParser

arrayOfUnsignedShort :: Value (Uv.Vector Word16)
arrayOfUnsignedShort = arrayOfPrimitive "ArrayOfUnsignedShort" "unsignedShort" $ Xp.attoparsedContent AttoparsecData.lenientParser

arrayOfInt :: Value (Uv.Vector Int32)
arrayOfInt = arrayOfPrimitive "ArrayOfInt" "int" $ Xp.attoparsedContent AttoparsecData.lenientParser

arrayOfUnsignedInt :: Value (Uv.Vector Word32)
arrayOfUnsignedInt = arrayOfPrimitive "ArrayOfUnsignedInt" "unsignedInt" $ Xp.attoparsedContent AttoparsecData.lenientParser

arrayOfLong :: Value (Uv.Vector Int64)
arrayOfLong = arrayOfPrimitive "ArrayOfLong" "long" $ Xp.attoparsedContent AttoparsecData.lenientParser

arrayOfWord64 :: Value (Uv.Vector Word64)
arrayOfWord64 = arrayOfPrimitive "ArrayOfWord64" "unsignedLong" $ Xp.attoparsedContent AttoparsecData.lenientParser

arrayOfFloat :: Value (Uv.Vector Float)
arrayOfFloat = arrayOfPrimitive "ArrayOfFloat" "float" $ Xp.attoparsedContent $ fmap realToFrac $ AttoparsecData.lenientParser @Double

arrayOfDecimal :: Value (Vector Scientific)
arrayOfDecimal = arrayOfPrimitive "ArrayOfDecimal" "decimal" $ Xp.attoparsedContent AttoparsecData.lenientParser

arrayOfDouble :: Value (Uv.Vector Double)
arrayOfDouble = arrayOfPrimitive "ArrayOfDouble" "double" $ Xp.attoparsedContent AttoparsecData.lenientParser

arrayOfBoolean :: Value (Uv.Vector Bool)
arrayOfBoolean = arrayOfPrimitive "ArrayOfBoolean" "boolean" $ Xp.attoparsedContent AttoparsecData.lenientParser

arrayOfString :: Value (Vector Text)
arrayOfString = arrayOfPrimitive "ArrayOfString" "string" $ Xp.textContent

arrayOfDateTime :: Value (Vector UTCTime)
arrayOfDateTime = arrayOfPrimitive "ArrayOfDateTime" "dateTime" $ Xp.attoparsedContent AttoparsecData.lenientParser

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
              else fmap (fmap Just) (applyTypeExpectations valueParsers)

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
