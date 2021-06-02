{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}

module OpcXmlDaClient.XmlTypes
  ( XmlElement (..),
    XmlQName (..),
    XmlDateTime (..),
  )
where

import Data.Hashable (Hashable (hashWithSalt))
import Data.Hashable.Generic (genericHashWithSalt)
import Data.Hashable.Lifted (liftHashWithSalt)
import Data.XML.Types
  ( Content,
    Element (Element),
    Name,
    Node (NodeComment, NodeContent, NodeElement, NodeInstruction),
  )
import OpcXmlDaClient.Prelude

-- | An XML Schema @dateTime@.
--
-- /Reference:/ [XML Schema 2
-- @dateTime@](https://www.w3.org/TR/xmlschema-2/#dateTime)
--
-- @since 0.1
data XmlDateTime = XmlDateTime !LocalTime !(Maybe TimeZone)
  deriving stock
    ( -- | @since 0.1
      Show,
      -- | This is /not/ standard-conformant, as the equality relation is
      -- partial for @dateTime@.
      --
      -- @since 0.1
      Eq,
      -- | This is /not/ standard-conformant, as the ordering relation is
      -- partial for @dateTime@.
      --
      -- @since 0.1
      Ord,
      -- | @since 0.1
      Data
    )

-- | @since 0.1
instance Hashable XmlDateTime where
  {-# INLINEABLE hashWithSalt #-}
  hashWithSalt salt (XmlDateTime lt mtz) = salt `hashWithSalt` lt `hashWithSalt` mtz

-- | An XML qualified name.
--
-- /References:/
--    * [Wikipedia overview](https://en.wikipedia.org/wiki/QName)
--    * [Standard text](https://www.w3.org/TR/REC-xml-names/#NT-QName)
--
-- @since 0.1
newtype XmlQName = XmlQName Name
  deriving stock
    ( -- | @since 0.1
      Show,
      -- | @since 0.1
      Data
    )
  deriving
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Ord
    )
    via Name

-- | @since 0.1
instance Hashable XmlQName where
  {-# INLINEABLE hashWithSalt #-}
  hashWithSalt salt (XmlQName n) = genericHashWithSalt salt n

-- | An XML fragment that does not necessarily constitute a full document.
--
-- @since 0.1
newtype XmlElement = XmlElement Element
  deriving stock
    ( -- | @since 0.1
      Show,
      -- | @since 0.1
      Data
    )
  deriving
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Ord
    )
    via Element

-- | @since 0.1
instance Hashable XmlElement where
  {-# INLINEABLE hashWithSalt #-}
  hashWithSalt salt (XmlElement el) = hashElement salt el
    where
      hashElement :: Int -> Element -> Int
      hashElement salt' (Element n as ns) =
        let s1 = genericHashWithSalt salt' n
            s2 = liftHashWithSalt hashContents s2 as
         in liftHashWithSalt hashNode s2 ns
      hashContents :: Int -> (Name, [Content]) -> Int
      hashContents salt' (n, cs) =
        let s1 = genericHashWithSalt salt' n
         in liftHashWithSalt genericHashWithSalt s1 cs
      hashNode :: Int -> Node -> Int
      hashNode salt' = \case
        NodeElement elt -> salt' `hashElement` elt `hashWithSalt` (0 :: Int)
        NodeInstruction inst -> salt' `genericHashWithSalt` inst `hashWithSalt` (1 :: Int)
        NodeContent cont -> salt' `genericHashWithSalt` cont `hashWithSalt` (2 :: Int)
        NodeComment comm -> salt `hashWithSalt` comm `hashWithSalt` (3 :: Int)
