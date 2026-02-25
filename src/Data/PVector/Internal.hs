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

    -- * In-place mutable trie operations
  , mMapNodeInPlace
  , mapMutableArr

    -- * Array helpers
  , emptyRoot
  , emptyTail
  , snocArray
  , consArray
  , unsnocArray
  , cloneAndSet
  , rnfArray
  , mapArray'
  ) where

import Control.DeepSeq (NFData(..))
import Control.Monad.Primitive
import Control.Monad.ST
import Data.Bits
import Data.Primitive.SmallArray

bfBits :: Int
bfBits = 5
{-# INLINE bfBits #-}

bf :: Int
bf = 32
{-# INLINE bf #-}

bfMask :: Int
bfMask = 31
{-# INLINE bfMask #-}

tailOffset :: Int -> Int
tailOffset n
  | n < bf    = 0
  | otherwise = unsafeShiftL (unsafeShiftR (n - 1) bfBits) bfBits
{-# INLINE tailOffset #-}

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

data MNode s a
  = MInternal {-# UNPACK #-} !(SmallMutableArray s (MNode s a))
  | MLeaf     {-# UNPACK #-} !(SmallMutableArray s a)
  | Frozen    !(Node a)

thawNode :: Node a -> MNode s a
thawNode = Frozen
{-# INLINE thawNode #-}

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

editableLeaf
  :: PrimMonad m
  => MNode (PrimState m) a
  -> m (SmallMutableArray (PrimState m) a)
editableLeaf (MLeaf arr) = pure arr
editableLeaf (Frozen (Leaf arr)) = thawSmallArray arr 0 (sizeofSmallArray arr)
editableLeaf _ = error "pvector: editableLeaf on non-leaf node"
{-# INLINABLE editableLeaf #-}

------------------------------------------------------------------------
-- In-place mutable trie operations
------------------------------------------------------------------------

-- | Apply a function to every element of a mutable trie node in-place.
-- Frozen nodes are cloned-on-write. Returns the (possibly new) node.
mMapNodeInPlace :: PrimMonad m => (a -> a) -> Int -> MNode (PrimState m) a -> m (MNode (PrimState m) a)
mMapNodeInPlace _ _ n@(Frozen Empty) = pure n
mMapNodeInPlace f _ (MLeaf arr) = do
  n <- getSizeofSmallMutableArray arr
  mapMutableArr f arr 0 n
  pure (MLeaf arr)
mMapNodeInPlace f _ (Frozen (Leaf arr)) = do
  let !n = sizeofSmallArray arr
  marr <- thawSmallArray arr 0 n
  mapMutableArr f marr 0 n
  pure (MLeaf marr)
mMapNodeInPlace f shift (MInternal arr) = do
  n <- getSizeofSmallMutableArray arr
  let go i
        | i >= n    = pure ()
        | otherwise = do
            child <- readSmallArray arr i
            child' <- mMapNodeInPlace f (shift - bfBits) child
            writeSmallArray arr i child'
            go (i + 1)
  go 0
  pure (MInternal arr)
mMapNodeInPlace f shift (Frozen (Internal arr)) = do
  let !n = sizeofSmallArray arr
  marr <- newSmallArray n (Frozen Empty)
  let go i
        | i >= n    = pure ()
        | otherwise = do
            child' <- mMapNodeInPlace f (shift - bfBits) (Frozen (indexSmallArray arr i))
            writeSmallArray marr i child'
            go (i + 1)
  go 0
  pure (MInternal marr)
{-# INLINABLE mMapNodeInPlace #-}

-- | Apply a function to elements [i..n) of a mutable array in-place.
mapMutableArr :: PrimMonad m => (a -> a) -> SmallMutableArray (PrimState m) a -> Int -> Int -> m ()
mapMutableArr f arr = go
  where
    go !i !n
      | i >= n    = pure ()
      | otherwise = do
          x <- readSmallArray arr i
          writeSmallArray arr i (f x)
          go (i + 1) n
{-# INLINE mapMutableArr #-}

------------------------------------------------------------------------
-- Array helpers
------------------------------------------------------------------------

emptyRoot :: Node a
emptyRoot = Internal $ runST $ do
  arr <- newSmallArray bf Empty
  unsafeFreezeSmallArray arr
{-# NOINLINE emptyRoot #-}

emptyTail :: SmallArray a
emptyTail = runSmallArray (newSmallArray 0 undefinedElem)
{-# NOINLINE emptyTail #-}

snocArray :: SmallArray a -> a -> SmallArray a
snocArray arr x = runST $ do
  let !n = sizeofSmallArray arr
  marr <- newSmallArray (n + 1) x
  copySmallArray marr 0 arr 0 n
  unsafeFreezeSmallArray marr
{-# INLINE snocArray #-}

consArray :: a -> SmallArray a -> SmallArray a
consArray x arr = runST $ do
  let !n = sizeofSmallArray arr
  marr <- newSmallArray (n + 1) x
  copySmallArray marr 1 arr 0 n
  unsafeFreezeSmallArray marr
{-# INLINE consArray #-}

unsnocArray :: SmallArray a -> (SmallArray a, a)
unsnocArray arr =
  let !n  = sizeofSmallArray arr
      !x  = indexSmallArray arr (n - 1)
      !a' = cloneSmallArray arr 0 (n - 1)
  in (a', x)
{-# INLINE unsnocArray #-}

cloneAndSet :: SmallArray a -> Int -> a -> SmallArray a
cloneAndSet arr i x = runST $ do
  marr <- thawSmallArray arr 0 (sizeofSmallArray arr)
  writeSmallArray marr i x
  unsafeFreezeSmallArray marr
{-# INLINE cloneAndSet #-}

-- | Strict map over a SmallArray, producing a new array.
mapArray' :: (a -> b) -> SmallArray a -> SmallArray b
mapArray' f arr = runST $ do
  let !n = sizeofSmallArray arr
  marr <- newSmallArray n undefinedElem
  let go i
        | i >= n = pure ()
        | otherwise = do
            let !x = f (indexSmallArray arr i)
            writeSmallArray marr i x
            go (i + 1)
  go 0
  unsafeFreezeSmallArray marr
{-# INLINE mapArray' #-}

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
