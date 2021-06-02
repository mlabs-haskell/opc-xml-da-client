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

import qualified Domain
import OpcXmlDaClient.Prelude hiding (Read)
import OpcXmlDaClient.XmlTypes (XmlElement, XmlQName)

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
