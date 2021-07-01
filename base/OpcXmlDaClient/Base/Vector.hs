-- |
-- General utilities for immutable vectors.
module OpcXmlDaClient.Base.Vector where

import Data.Vector.Generic
import qualified Data.Vector.Generic.Mutable as M
import qualified OpcXmlDaClient.Base.MVector as M
import OpcXmlDaClient.Base.Prelude hiding (Vector)

-- |
-- >>> fromReverseListN 3 [1,2,3] :: Data.Vector.Vector Int
-- [3,2,1]
{-# INLINE fromReverseListN #-}
fromReverseListN :: Vector v a => Int -> [a] -> v a
fromReverseListN size list =
  initialized size $ \mv -> M.writeListInReverseOrderStartingFrom mv (pred size) list

{-# INLINE initialized #-}
initialized :: Vector v a => Int -> (forall s. Mutable v s a -> ST s ()) -> v a
initialized size initialize = runST $ do
  mv <- M.unsafeNew size
  initialize mv
  unsafeFreeze mv
