{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Data.PVector.Internal
  ( -- * Constants
    bfBits, bf, bfMask

    -- * Index arithmetic
  , tailOffset
  , indexAtLevel

    -- * Persistent node
  , Node(..)

    -- * Mutable node
  , MNode(..)
  , freezeNode
  , thawNode
  , editableInternal
  , editableLeaf

    -- * Array helpers
  , emptyRoot
  , emptyTail
  , snocArray
  , unsnocArray
  , cloneAndSet
  , rnfArray
  ) where

import Control.DeepSeq (NFData(..))
import Control.Monad.Primitive
import Control.Monad.ST
import Data.Bits
import Data.Primitive.SmallArray

-- | Bits per level of the trie. 5 bits = branching factor of 32.
bfBits :: Int
bfBits = 5
{-# INLINE bfBits #-}

-- | Branching factor (32).
bf :: Int
bf = 32
{-# INLINE bf #-}

-- | Mask for extracting the index within a level.
bfMask :: Int
bfMask = 31
{-# INLINE bfMask #-}

-- | Compute the index of the first element in the tail.
tailOffset :: Int -> Int
tailOffset n
  | n < bf    = 0
  | otherwise = unsafeShiftL (unsafeShiftR (n - 1) bfBits) bfBits
{-# INLINE tailOffset #-}

-- | Extract the sub-index at a given bit level.
indexAtLevel :: Int -> Int -> Int
indexAtLevel i level = unsafeShiftR i level .&. bfMask
{-# INLINE indexAtLevel #-}

------------------------------------------------------------------------
-- Persistent node
------------------------------------------------------------------------

data Node a
  = Internal {-# UNPACK #-} !(SmallArray (Node a))
  | Leaf     {-# UNPACK #-} !(SmallArray a)
  | Empty

instance NFData a => NFData (Node a) where
  rnf Empty = ()
  rnf (Leaf arr) = rnfArray arr
  rnf (Internal arr) = go 0
    where
      !n = sizeofSmallArray arr
      go i
        | i >= n    = ()
        | otherwise = rnf (indexSmallArray arr i) `seq` go (i + 1)

------------------------------------------------------------------------
-- Mutable (transient) node
------------------------------------------------------------------------

-- | A mutable trie node. 'Frozen' wraps a persistent node that has not
-- yet been cloned; mutation will clone it on demand (copy-on-write).
data MNode s a
  = MInternal {-# UNPACK #-} !(SmallMutableArray s (MNode s a))
  | MLeaf     {-# UNPACK #-} !(SmallMutableArray s a)
  | Frozen    !(Node a)

-- | Convert a persistent node to a mutable wrapper without copying.
thawNode :: Node a -> MNode s a
thawNode = Frozen
{-# INLINE thawNode #-}

-- | Recursively freeze a mutable node tree into a persistent tree.
freezeNode :: PrimMonad m => MNode (PrimState m) a -> m (Node a)
freezeNode (Frozen node) = pure node
freezeNode (MLeaf marr)  = Leaf <$> unsafeFreezeSmallArray marr
freezeNode (MInternal marr) = do
  n <- getSizeofSmallMutableArray marr
  let _ = n
  buf <- newSmallArray n Empty
  let go i
        | i >= n    = pure ()
        | otherwise = do
            child <- readSmallArray marr i
            p <- freezeNode child
            writeSmallArray buf i p
            go (i + 1)
  go 0
  Internal <$> unsafeFreezeSmallArray buf
{-# INLINABLE freezeNode #-}

-- | Obtain a mutable internal array, cloning a frozen one if necessary.
editableInternal
  :: PrimMonad m
  => MNode (PrimState m) a
  -> m (SmallMutableArray (PrimState m) (MNode (PrimState m) a))
editableInternal (MInternal arr) = pure arr
editableInternal (Frozen (Internal arr)) = do
  let !n = sizeofSmallArray arr
  marr <- newSmallArray n (Frozen Empty)
  let go i
        | i >= n    = pure ()
        | otherwise = do
            writeSmallArray marr i (Frozen (indexSmallArray arr i))
            go (i + 1)
  go 0
  pure marr
editableInternal (Frozen Empty) = newSmallArray bf (Frozen Empty)
editableInternal _ = error "pvector: editableInternal on leaf node"
{-# INLINABLE editableInternal #-}

-- | Obtain a mutable leaf array, cloning a frozen one if necessary.
editableLeaf
  :: PrimMonad m
  => MNode (PrimState m) a
  -> m (SmallMutableArray (PrimState m) a)
editableLeaf (MLeaf arr) = pure arr
editableLeaf (Frozen (Leaf arr)) = thawSmallArray arr 0 (sizeofSmallArray arr)
editableLeaf _ = error "pvector: editableLeaf on non-leaf node"
{-# INLINABLE editableLeaf #-}

------------------------------------------------------------------------
-- Array helpers
------------------------------------------------------------------------

-- | An empty root node (Internal with bf Empty children).
emptyRoot :: Node a
emptyRoot = Internal $ runST $ do
  arr <- newSmallArray bf Empty
  unsafeFreezeSmallArray arr
{-# NOINLINE emptyRoot #-}

-- | An empty, zero-length SmallArray.
emptyTail :: SmallArray a
emptyTail = runSmallArray (newSmallArray 0 undefinedElem)
{-# NOINLINE emptyTail #-}

-- | Append an element to the end of an immutable array.
snocArray :: SmallArray a -> a -> SmallArray a
snocArray arr x = runST $ do
  let !n = sizeofSmallArray arr
  marr <- newSmallArray (n + 1) x
  copySmallArray marr 0 arr 0 n
  unsafeFreezeSmallArray marr
{-# INLINE snocArray #-}

-- | Remove the last element, returning the shortened array and the element.
unsnocArray :: SmallArray a -> (SmallArray a, a)
unsnocArray arr =
  let !n  = sizeofSmallArray arr
      !x  = indexSmallArray arr (n - 1)
      !a' = cloneSmallArray arr 0 (n - 1)
  in (a', x)
{-# INLINE unsnocArray #-}

-- | Clone an immutable array and set one slot.
cloneAndSet :: SmallArray a -> Int -> a -> SmallArray a
cloneAndSet arr i x = runST $ do
  marr <- thawSmallArray arr 0 (sizeofSmallArray arr)
  writeSmallArray marr i x
  unsafeFreezeSmallArray marr
{-# INLINE cloneAndSet #-}

-- | Deeply evaluate every element of a SmallArray.
rnfArray :: NFData a => SmallArray a -> ()
rnfArray arr = go 0
  where
    !n = sizeofSmallArray arr
    go i
      | i >= n    = ()
      | otherwise = rnf (indexSmallArray arr i) `seq` go (i + 1)

undefinedElem :: a
undefinedElem = error "pvector: undefined element"
{-# NOINLINE undefinedElem #-}
