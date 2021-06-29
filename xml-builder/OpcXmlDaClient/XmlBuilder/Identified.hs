module OpcXmlDaClient.XmlBuilder.Identified where

import qualified Acc
import qualified Data.HashMap.Strict as HashMap
import OpcXmlDaClient.Base.Prelude
import qualified OpcXmlDaClient.XmlBuilder.HashMap as HashMap

-- |
-- Comonad automating incremental indexed value generation.
--
-- Useful for such things as generating aliases.
data Identified k v r
  = Identified (Acc.Acc k) ((k -> v) -> r)

deriving instance Functor (Identified k v)

instance Applicative (Identified k v) where
  pure x = Identified mempty (const x)
  Identified keysL buildL <*> Identified keysR buildR =
    Identified (keysL <> keysR) (\map -> buildL map (buildR map))

run :: (Hashable k, Eq k) => Identified k v a -> (Int -> v) -> (a, [(k, v)])
run (Identified uriAcc build) proj =
  HashMap.autoincrementedFoldable uriAcc proj
    & \map -> (build (\k -> HashMap.lookupDefault (error "Bug") k map), HashMap.toList map)

-- |
-- Register a key if it hasn't been registered already,
-- and build a result in the scope of the incremental value associated with the key.
identifying :: k -> (v -> a) -> Identified k v a
identifying uri byAlias =
  Identified (pure uri) (\map -> byAlias (map uri))
