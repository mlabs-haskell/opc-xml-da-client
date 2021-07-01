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
  = UnexpectedTypeNamespaceValueError
      Text
      -- ^ Expected.
      Text
      -- ^ Actual.
  | UnexpectedTypeNameValueError
      Text
      -- ^ Expected.
      Text
      -- ^ Actual.
  | UnnamespacedTypeNameValueError
      Text
      -- ^ Expected namespace.
      Text
      -- ^ Expected name.
      Text
      -- ^ Actual type name.
  | ArrayElementValueError
      Int
      -- ^ Offset in the array.
      ValueError
      -- ^ Reason.

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

-- *

data Value a
  = Value
      (Maybe Text)
      -- ^ Type namespace.
      Text
      -- ^ Type name.
      (Xp.Element (Either ValueError a))
      -- ^ Element parser.
  deriving (Functor)

primitive :: Primitive a -> Value a
primitive (Primitive arrayTypeName typeName parser) =
  error "TODO"

arrayOfPrimitive :: GenericVector.Vector vector a => Primitive a -> Value (vector a)
arrayOfPrimitive =
  error "TODO"

arrayOfAnyType ::
  GenericVector.Vector vector a =>
  -- | Alternative value parsers tried on each element of the array.
  NonEmpty (Value a) ->
  Value (vector (Maybe a))
arrayOfAnyType =
  error "TODO"

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
