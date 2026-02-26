-- | RRB-tree persistent vectors with O(log n) concat\/split.
--
-- This module re-exports the main 'Data.PVector.Back.Vector'.
-- For other variants, import directly:
--
-- * "Data.PVector.Back"  — RRB vector with efficient prepend and append
-- * "Data.PVector.Front" — reversed internal ordering (cons-efficient wrapper)
-- * "Data.PVector.Deque" — double-ended deque
module Data.PVector
  ( module Data.PVector.Back
  ) where

import Data.PVector.Back hiding
  ( vSize, vShift, vRoot, vTail, vPrefix, forEach_
  , New(..), new, clone, fill, updateNew, mapNew, transformNew, inplace_map
  )
