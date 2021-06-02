{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}

module OpcXmlDaClient.XmlTypes where

import Data.XML.Types
  ( Content,
    Element (Element),
    Name,
    Node (NodeComment, NodeContent, NodeElement, NodeInstruction),
  )
import OpcXmlDaClient.Prelude

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
