{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies    #-}
module Data.Vector.Persistent (
  I.Vector,
  I.index,
  I.unsafeIndex,
  (I.!),
  (I.!?),
  I.update1,
  I.empty,
  I.singleton,
  I.snoc,
  (I.<|),
  I.unsnoc,
  I.concat,
  I.concatMap,
  I.map,
  I.transient,
  I.persist
) where
import qualified Data.Vector.Internal.Persistent as I

