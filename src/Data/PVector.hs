-- | Persistent vectors based on bit-partitioned array mapped tries.
--
-- This module re-exports the snoc-efficient 'Data.PVector.Back.Vector'.
-- For other variants, import directly:
--
-- * "Data.PVector.Back"  — efficient append (snoc)
-- * "Data.PVector.Front" — efficient prepend (cons)
-- * "Data.PVector.Deque" — efficient at both ends
module Data.PVector
  ( module Data.PVector.Back
  ) where

import Data.PVector.Back
