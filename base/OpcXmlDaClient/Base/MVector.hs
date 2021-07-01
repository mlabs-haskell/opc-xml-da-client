module OpcXmlDaClient.Base.MVector where

import Data.Vector.Generic.Mutable
import OpcXmlDaClient.Base.Prelude

{-# INLINE writeListInReverseOrderStartingFrom #-}
writeListInReverseOrderStartingFrom :: MVector v a => v s a -> Int -> [a] -> ST s ()
writeListInReverseOrderStartingFrom v =
  let loop !index list = case list of
        h : t -> do
          unsafeWrite v index h
          loop (pred index) t
        _ -> return ()
   in loop
