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

    -- * Unrolled chunk operations
  , foldlChunk32
  , foldrChunk32
  , mapChunk32

    -- * Fused clone-and-set
  , cloneAndSet32

    -- * Unsafe node accessors (depth-invariant, skip tag check)
  , unsafeNodeChildren
  , unsafeLeafArray

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
import Unsafe.Coerce (unsafeCoerce)

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
      go i
        | i >= bf   = ()
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
  buf <- newSmallArray bf Empty
  let go i
        | i >= bf   = pure ()
        | otherwise = do
            child <- readSmallArray marr i
            p <- freezeNode child
            writeSmallArray buf i p
            go (i + 1)
  go 0
  Internal <$> unsafeFreezeSmallArray buf
{-# INLINEABLE freezeNode #-}

editableInternal
  :: PrimMonad m
  => MNode (PrimState m) a
  -> m (SmallMutableArray (PrimState m) (MNode (PrimState m) a))
editableInternal (MInternal arr) = pure arr
editableInternal (Frozen (Internal arr)) = do
  marr <- newSmallArray bf (Frozen Empty)
  let go i
        | i >= bf   = pure ()
        | otherwise = do
            writeSmallArray marr i (Frozen (indexSmallArray arr i))
            go (i + 1)
  go 0
  pure marr
editableInternal (Frozen Empty) = newSmallArray bf (Frozen Empty)
editableInternal _ = error "pvector: editableInternal on leaf node"
{-# INLINEABLE editableInternal #-}

editableLeaf
  :: PrimMonad m
  => MNode (PrimState m) a
  -> m (SmallMutableArray (PrimState m) a)
editableLeaf (MLeaf arr) = pure arr
editableLeaf (Frozen (Leaf arr)) = thawSmallArray arr 0 bf
editableLeaf _ = error "pvector: editableLeaf on non-leaf node"
{-# INLINEABLE editableLeaf #-}

------------------------------------------------------------------------
-- In-place mutable trie operations
------------------------------------------------------------------------

mMapNodeInPlace :: PrimMonad m => (a -> a) -> Int -> MNode (PrimState m) a -> m (MNode (PrimState m) a)
mMapNodeInPlace _ _ n@(Frozen Empty) = pure n
mMapNodeInPlace f _ (MLeaf arr) = do
  mapMutableArr f arr 0 bf
  pure (MLeaf arr)
mMapNodeInPlace f _ (Frozen (Leaf arr)) = do
  marr <- thawSmallArray arr 0 bf
  mapMutableArr f marr 0 bf
  pure (MLeaf marr)
mMapNodeInPlace f shift (MInternal arr) = do
  let go i
        | i >= bf   = pure ()
        | otherwise = do
            child <- readSmallArray arr i
            child' <- mMapNodeInPlace f (shift - bfBits) child
            writeSmallArray arr i child'
            go (i + 1)
  go 0
  pure (MInternal arr)
mMapNodeInPlace f shift (Frozen (Internal arr)) = do
  marr <- newSmallArray bf (Frozen Empty)
  let go i
        | i >= bf   = pure ()
        | otherwise = do
            child' <- mMapNodeInPlace f (shift - bfBits) (Frozen (indexSmallArray arr i))
            writeSmallArray marr i child'
            go (i + 1)
  go 0
  pure (MInternal marr)
{-# INLINEABLE mMapNodeInPlace #-}

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
-- Unrolled chunk operations
------------------------------------------------------------------------

-- | Strict left fold over a full 32-element SmallArray.
-- Uses manual 4x unrolling so GHC generates a tight loop
-- without per-element bounds checks.
foldlChunk32 :: (b -> a -> b) -> b -> SmallArray a -> b
foldlChunk32 f !z0 arr = go z0 0
  where
    go !z !i
      | i >= 32   = z
      | i + 3 < 32 =
          let !z1 = f z  (indexSmallArray arr i)
              !z2 = f z1 (indexSmallArray arr (i+1))
              !z3 = f z2 (indexSmallArray arr (i+2))
              !z4 = f z3 (indexSmallArray arr (i+3))
          in go z4 (i + 4)
      | otherwise =
          go (f z (indexSmallArray arr i)) (i + 1)
{-# INLINE foldlChunk32 #-}

-- | Right fold over a full 32-element SmallArray.
foldrChunk32 :: (a -> b -> b) -> SmallArray a -> b -> b
foldrChunk32 f arr z0 = go 0
  where
    go !i
      | i >= 32   = z0
      | otherwise = f (indexSmallArray arr i) (go (i + 1))
{-# INLINE foldrChunk32 #-}

-- | Strict map over a full 32-element SmallArray.
mapChunk32 :: (a -> b) -> SmallArray a -> SmallArray b
mapChunk32 f arr = runST $ do
  marr <- newSmallArray 32 undefinedElem
  let go !i
        | i >= 32 = pure ()
        | i + 3 < 32 = do
            writeSmallArray marr i     $! f (indexSmallArray arr i)
            writeSmallArray marr (i+1) $! f (indexSmallArray arr (i+1))
            writeSmallArray marr (i+2) $! f (indexSmallArray arr (i+2))
            writeSmallArray marr (i+3) $! f (indexSmallArray arr (i+3))
            go (i + 4)
        | otherwise = do
            writeSmallArray marr i $! f (indexSmallArray arr i)
            go (i + 1)
  go 0
  unsafeFreezeSmallArray marr
{-# INLINE mapChunk32 #-}

------------------------------------------------------------------------
-- Unsafe node accessors
------------------------------------------------------------------------

-- | Extract the child array from a Node that is known to be Internal.
-- Uses a single-alternative case to hint to GHC that only Internal
-- is possible, which can eliminate the tag check in optimized code.
unsafeNodeChildren :: Node a -> SmallArray (Node a)
unsafeNodeChildren n = case n of
  Internal arr -> arr
  _ -> undefinedElem  -- unreachable; marked as bottom so GHC eliminates it
{-# INLINE unsafeNodeChildren #-}

-- | Extract the element array from a Node that is known to be a Leaf.
unsafeLeafArray :: Node a -> SmallArray a
unsafeLeafArray n = case n of
  Leaf arr -> arr
  _ -> undefinedElem
{-# INLINE unsafeLeafArray #-}

------------------------------------------------------------------------
-- Fused clone-and-set
------------------------------------------------------------------------

-- | Clone a known-32-element SmallArray and set one slot.
-- Uses thawSmallArray with the constant size bf, then write+freeze.
-- The INLINE ensures this fuses into a single allocation at call sites.
cloneAndSet32 :: SmallArray a -> Int -> a -> SmallArray a
cloneAndSet32 arr i x = runST $ do
  marr <- thawSmallArray arr 0 bf
  writeSmallArray marr i x
  unsafeFreezeSmallArray marr
{-# INLINE cloneAndSet32 #-}

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
