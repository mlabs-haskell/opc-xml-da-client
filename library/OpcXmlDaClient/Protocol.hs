{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module OpcXmlDaClient.Protocol where

import Domain
  ( accessorIsLabelDeriver,
    boundedDeriver,
    constructorIsLabelDeriver,
    dataDeriver,
    declare,
    enumDeriver,
    eqDeriver,
    genericDeriver,
    hasFieldDeriver,
    hashableDeriver,
    loadSchema,
    mapperIsLabelDeriver,
    ordDeriver,
    showDeriver,
    typeableDeriver,
  )
import OpcXmlDaClient.Prelude hiding (Read)
import OpcXmlDaClient.XmlTypes (XmlElement, XmlQName)

declare
  (Just (True, False))
  ( mconcat
      [ enumDeriver,
        boundedDeriver,
        showDeriver,
        eqDeriver,
        ordDeriver,
        genericDeriver,
        dataDeriver,
        typeableDeriver,
        hashableDeriver,
        hasFieldDeriver,
        constructorIsLabelDeriver,
        accessorIsLabelDeriver,
        mapperIsLabelDeriver
      ]
  )
  =<< loadSchema "schemas/protocol.yaml"
