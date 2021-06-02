{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module OpcXmlDaClient.Protocol where

import qualified Data.XML.Types as Xml
import qualified Domain
import OpcXmlDaClient.Prelude hiding (Read)

Domain.declare
  (Just (True, False))
  ( mconcat
      [ Domain.enumDeriver,
        Domain.boundedDeriver,
        Domain.showDeriver,
        Domain.eqDeriver,
        Domain.ordDeriver,
        Domain.genericDeriver,
        Domain.dataDeriver,
        Domain.typeableDeriver,
        Domain.hashableDeriver,
        Domain.hasFieldDeriver,
        Domain.constructorIsLabelDeriver,
        Domain.accessorIsLabelDeriver,
        Domain.mapperIsLabelDeriver
      ]
  )
  =<< Domain.loadSchema "schemas/protocol.yaml"

-- | An XML qualified name.
--
-- /References:/
--    * [Wikipedia overview](https://en.wikipedia.org/wiki/QName)
--    * [Standard text](https://www.w3.org/TR/REC-xml-names/#NT-QName)
--
-- @since 0.1
newtype XmlQName = XmlQName Xml.Name
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
    via Xml.Name

-- | @since 0.1
instance Hashable XmlQName where
  {-# INLINEABLE hashWithSalt #-}
  hashWithSalt salt (XmlQName n) = genericHashWithSalt salt n

-- | An XML fragment that does not necessarily constitute a full document.
--
-- @since 0.1
newtype XmlElement = XmlElement Xml.Element
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
    via Xml.Element

-- | @since 0.1
instance Hashable XmlElement where
  {-# INLINEABLE hashWithSalt #-}
  hashWithSalt salt (XmlElement el) = hashElement salt el
    where
      hashElement :: Int -> Xml.Element -> Int
      hashElement salt' (Xml.Element n as ns) =
        let s1 = genericHashWithSalt salt' n
            s2 = liftHashWithSalt hashContents s2 as
         in liftHashWithSalt hashNode s2 ns
      hashContents :: Int -> (Xml.Name, [Xml.Content]) -> Int
      hashContents salt' (n, cs) =
        let s1 = genericHashWithSalt salt' n
         in liftHashWithSalt genericHashWithSalt s1 cs
      hashNode :: Int -> Xml.Node -> Int
      hashNode salt' = \case
        Xml.NodeElement elt -> salt' `hashElement` elt `hashWithSalt` (0 :: Int)
        Xml.NodeInstruction inst -> salt' `genericHashWithSalt` inst `hashWithSalt` (1 :: Int)
        Xml.NodeContent cont -> salt' `genericHashWithSalt` cont `hashWithSalt` (2 :: Int)
        Xml.NodeComment comm -> salt `hashWithSalt` comm `hashWithSalt` (3 :: Int)
