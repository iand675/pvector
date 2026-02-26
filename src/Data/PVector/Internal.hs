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

    -- * RRB node operations
  , nodeSize
  , nodeSizes
  , mkBalanced
  , mkRelaxed
  , isBalancedAtShift
  , radixIndex
  , relaxedIndex
  , treeIndex
  , treeLeafFor
  , computeSizeTable

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
  , foldlChunk
  , foldrChunk
  , mapChunk

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
  , unconsArray
  , cloneAndSet
  , rnfArray
  , mapArray'

    -- * PrimArray re-exports and helpers
  , PrimArray
  , sizeofPrimArray
  , indexPrimArray
  , primArrayFromList
  , newPrimArray
  , writePrimArray
  , unsafeFreezePrimArray
  ) where

import Control.DeepSeq (NFData(..))
import Control.Monad.Primitive
import Control.Monad.ST
import Data.Bits
import Data.Primitive.SmallArray
import qualified Data.Primitive.PrimArray as PA
import Unsafe.Coerce (unsafeCoerce)

type PrimArray = PA.PrimArray

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
  | Relaxed  {-# UNPACK #-} !(SmallArray (Node a)) {-# UNPACK #-} !(PA.PrimArray Int)
  | Leaf     {-# UNPACK #-} !(SmallArray a)
  | Empty

instance NFData a => NFData (Node a) where
  rnf Empty = ()
  rnf (Leaf arr) = rnfArray arr
  rnf (Internal arr) = rnfNodeArr arr
  rnf (Relaxed arr _) = rnfNodeArr arr

rnfNodeArr :: NFData a => SmallArray (Node a) -> ()
rnfNodeArr arr = go 0
  where
    !n = sizeofSmallArray arr
    go i
      | i >= n    = ()
      | otherwise = rnf (indexSmallArray arr i) `seq` go (i + 1)

------------------------------------------------------------------------
-- RRB node operations
------------------------------------------------------------------------

-- | Compute the total number of elements under a node at the given shift level.
-- For Leaf: the array size.
-- For Internal (balanced): (nChildren - 1) * fullChildSize + lastChildSize.
-- For Relaxed: the last entry in the cumulative size table.
nodeSize :: Int -> Node a -> Int
nodeSize _ Empty = 0
nodeSize _ (Leaf arr) = sizeofSmallArray arr
nodeSize _ (Relaxed _ sizes) = PA.indexPrimArray sizes (PA.sizeofPrimArray sizes - 1)
nodeSize shift (Internal arr) =
  let !nch = sizeofSmallArray arr
  in case nch of
    0 -> 0
    1 -> nodeSize (shift - bfBits) (indexSmallArray arr 0)
    _ -> let !fullChildSz = unsafeShiftL 1 shift
             !lastSz = nodeSize (shift - bfBits) (indexSmallArray arr (nch - 1))
         in (nch - 1) * fullChildSz + lastSz
{-# INLINEABLE nodeSize #-}

-- | Build the cumulative size table for a SmallArray of children at the given shift.
-- Used when creating Relaxed nodes during concat/split.
computeSizeTable :: Int -> SmallArray (Node a) -> PA.PrimArray Int
computeSizeTable shift children = runST $ do
  let !n = sizeofSmallArray children
  marr <- PA.newPrimArray n
  let go !i !cumul
        | i >= n = pure ()
        | otherwise = do
            let !sz = nodeSize (shift - bfBits) (indexSmallArray children i)
                !cumul' = cumul + sz
            PA.writePrimArray marr i cumul'
            go (i + 1) cumul'
  go 0 0
  PA.unsafeFreezePrimArray marr
{-# INLINEABLE computeSizeTable #-}

-- | Build the cumulative size table for children, given a list of sizes.
nodeSizes :: [Int] -> PA.PrimArray Int
nodeSizes szs = primArrayFromList (scanl1' (+) szs)
  where
    scanl1' _ [] = []
    scanl1' f (x:xs) = go x xs
      where
        go !acc [] = [acc]
        go !acc (y:ys) = acc : go (f acc y) ys

-- | Create a balanced internal node from an array of children.
-- Only valid when all children (except possibly the last) are full at the given shift.
mkBalanced :: SmallArray (Node a) -> Node a
mkBalanced children
  | sizeofSmallArray children == 0 = Empty
  | otherwise = Internal children
{-# INLINE mkBalanced #-}

-- | Create a relaxed node with explicit size table.
mkRelaxed :: SmallArray (Node a) -> PA.PrimArray Int -> Node a
mkRelaxed children sizes
  | sizeofSmallArray children == 0 = Empty
  | otherwise = Relaxed children sizes
{-# INLINE mkRelaxed #-}

-- | Check if a node at the given shift is strictly radix-balanced
-- (all children except possibly the last have exactly 2^shift elements).
isBalancedAtShift :: Int -> SmallArray (Node a) -> Bool
isBalancedAtShift shift children =
  let !nch = sizeofSmallArray children
      !fullSz = unsafeShiftL 1 shift
      go !i
        | i >= nch - 1 = True
        | nodeSize (shift - bfBits) (indexSmallArray children i) == fullSz = go (i + 1)
        | otherwise = False
  in nch <= 1 || go 0

-- | Index into a tree. Handles both balanced (radix) and relaxed (size-table) nodes.
-- @shift@ is the shift level of the root, @i@ is the local index within the tree.
treeIndex :: Int -> Int -> Node a -> a
treeIndex !shift !i node = case node of
  Leaf arr -> indexSmallArray arr (i .&. bfMask)
  Internal arr ->
    let !idx = indexAtLevel i shift
    in treeIndex (shift - bfBits) i (indexSmallArray arr idx)
  Relaxed arr sizes ->
    let !idx = relaxedIndex sizes i
        !off = if idx == 0 then 0 else indexPrimArray sizes (idx - 1)
    in treeIndex (shift - bfBits) (i - off) (indexSmallArray arr idx)
  Empty -> error "pvector: treeIndex hit Empty"
{-# INLINEABLE treeIndex #-}

-- | Navigate to the leaf array containing element at local index @i@.
treeLeafFor :: Int -> Int -> Node a -> SmallArray a
treeLeafFor !shift !i node = case node of
  Leaf arr -> arr
  Internal arr ->
    let !idx = indexAtLevel i shift
    in treeLeafFor (shift - bfBits) i (indexSmallArray arr idx)
  Relaxed arr sizes ->
    let !idx = relaxedIndex sizes i
        !off = if idx == 0 then 0 else indexPrimArray sizes (idx - 1)
    in treeLeafFor (shift - bfBits) (i - off) (indexSmallArray arr idx)
  Empty -> error "pvector: treeLeafFor hit Empty"
{-# INLINEABLE treeLeafFor #-}

-- | Pure radix index: extract the slot index from an element index at the given level.
radixIndex :: Int -> Int -> Int
radixIndex i level = unsafeShiftR i level .&. bfMask
{-# INLINE radixIndex #-}

-- | Find the child index in a relaxed node's cumulative size table.
-- Uses linear scan (m <= 32, fits in a cache line).
relaxedIndex :: PA.PrimArray Int -> Int -> Int
relaxedIndex sizes !i = go 0
  where
    !n = PA.sizeofPrimArray sizes
    go !k
      | k >= n = error "pvector: relaxedIndex out of bounds"
      | PA.indexPrimArray sizes k > i = k
      | otherwise = go (k + 1)
{-# INLINE relaxedIndex #-}

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
  let !n = sizeofSmallMutableArray marr
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
{-# INLINEABLE freezeNode #-}

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
editableInternal (Frozen (Relaxed arr _)) = do
  let !n = sizeofSmallArray arr
  marr <- newSmallArray n (Frozen Empty)
  let go i
        | i >= n    = pure ()
        | otherwise = do
            writeSmallArray marr i (Frozen (indexSmallArray arr i))
            go (i + 1)
  go 0
  pure marr
editableInternal (Frozen Empty) = newSmallArray 0 (Frozen Empty)
editableInternal _ = error "pvector: editableInternal on leaf node"
{-# INLINEABLE editableInternal #-}

editableLeaf
  :: PrimMonad m
  => MNode (PrimState m) a
  -> m (SmallMutableArray (PrimState m) a)
editableLeaf (MLeaf arr) = pure arr
editableLeaf (Frozen (Leaf arr)) = thawSmallArray arr 0 (sizeofSmallArray arr)
editableLeaf _ = error "pvector: editableLeaf on non-leaf node"
{-# INLINEABLE editableLeaf #-}

------------------------------------------------------------------------
-- In-place mutable trie operations
------------------------------------------------------------------------

mMapNodeInPlace :: PrimMonad m => (a -> a) -> Int -> MNode (PrimState m) a -> m (MNode (PrimState m) a)
mMapNodeInPlace _ _ n@(Frozen Empty) = pure n
mMapNodeInPlace f _ (MLeaf arr) = do
  let !n = sizeofSmallMutableArray arr
  mapMutableArr f arr 0 n
  pure (MLeaf arr)
mMapNodeInPlace f _ (Frozen (Leaf arr)) = do
  let !n = sizeofSmallArray arr
  marr <- thawSmallArray arr 0 n
  mapMutableArr f marr 0 n
  pure (MLeaf marr)
mMapNodeInPlace f shift (MInternal arr) = do
  let !n = sizeofSmallMutableArray arr
      go i
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
mMapNodeInPlace f shift (Frozen (Relaxed arr _)) = do
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

-- | Strict left fold over a SmallArray. Uses 4x unrolling for full 32-element arrays.
foldlChunk :: (b -> a -> b) -> b -> SmallArray a -> b
foldlChunk f !z0 arr
  | n == bf   = foldlChunk32 f z0 arr
  | otherwise = foldlChunkN f z0 arr n
  where !n = sizeofSmallArray arr
{-# INLINE foldlChunk #-}

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

foldlChunkN :: (b -> a -> b) -> b -> SmallArray a -> Int -> b
foldlChunkN f !z0 arr !n = go z0 0
  where
    go !z !i
      | i >= n    = z
      | otherwise = go (f z (indexSmallArray arr i)) (i + 1)
{-# INLINE foldlChunkN #-}

-- | Right fold over a SmallArray.
foldrChunk :: (a -> b -> b) -> SmallArray a -> b -> b
foldrChunk f arr z0
  | n == bf   = foldrChunk32 f arr z0
  | otherwise = foldrChunkN f arr n z0
  where !n = sizeofSmallArray arr
{-# INLINE foldrChunk #-}

foldrChunk32 :: (a -> b -> b) -> SmallArray a -> b -> b
foldrChunk32 f arr z0 = go 0
  where
    go !i
      | i >= 32   = z0
      | otherwise = f (indexSmallArray arr i) (go (i + 1))
{-# INLINE foldrChunk32 #-}

foldrChunkN :: (a -> b -> b) -> SmallArray a -> Int -> b -> b
foldrChunkN f arr !n z0 = go 0
  where
    go !i
      | i >= n    = z0
      | otherwise = f (indexSmallArray arr i) (go (i + 1))
{-# INLINE foldrChunkN #-}

-- | Strict map over a SmallArray.
mapChunk :: (a -> b) -> SmallArray a -> SmallArray b
mapChunk f arr
  | n == bf   = mapChunk32 f arr
  | otherwise = mapArray' f arr
  where !n = sizeofSmallArray arr
{-# INLINE mapChunk #-}

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

unsafeNodeChildren :: Node a -> SmallArray (Node a)
unsafeNodeChildren n = case n of
  Internal arr -> arr
  Relaxed arr _ -> arr
  _ -> undefinedElem
{-# INLINE unsafeNodeChildren #-}

unsafeLeafArray :: Node a -> SmallArray a
unsafeLeafArray n = case n of
  Leaf arr -> arr
  _ -> undefinedElem
{-# INLINE unsafeLeafArray #-}

------------------------------------------------------------------------
-- Fused clone-and-set
------------------------------------------------------------------------

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
emptyRoot = Empty
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

unconsArray :: SmallArray a -> (a, SmallArray a)
unconsArray arr =
  let !x  = indexSmallArray arr 0
      !n  = sizeofSmallArray arr
      !a' = cloneSmallArray arr 1 (n - 1)
  in (x, a')
{-# INLINE unconsArray #-}

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

------------------------------------------------------------------------
-- PrimArray helpers
------------------------------------------------------------------------

sizeofPrimArray :: PA.PrimArray Int -> Int
sizeofPrimArray = PA.sizeofPrimArray
{-# INLINE sizeofPrimArray #-}

indexPrimArray :: PA.PrimArray Int -> Int -> Int
indexPrimArray = PA.indexPrimArray
{-# INLINE indexPrimArray #-}

newPrimArray :: PrimMonad m => Int -> m (PA.MutablePrimArray (PrimState m) Int)
newPrimArray = PA.newPrimArray
{-# INLINE newPrimArray #-}

writePrimArray :: PrimMonad m => PA.MutablePrimArray (PrimState m) Int -> Int -> Int -> m ()
writePrimArray = PA.writePrimArray
{-# INLINE writePrimArray #-}

unsafeFreezePrimArray :: PrimMonad m => PA.MutablePrimArray (PrimState m) Int -> m (PA.PrimArray Int)
unsafeFreezePrimArray = PA.unsafeFreezePrimArray
{-# INLINE unsafeFreezePrimArray #-}

primArrayFromList :: [Int] -> PA.PrimArray Int
primArrayFromList xs = runST $ do
  let !n = Prelude.length xs
  marr <- PA.newPrimArray n
  let go !_ [] = pure ()
      go !i (v:vs) = PA.writePrimArray marr i v >> go (i + 1) vs
  go 0 xs
  PA.unsafeFreezePrimArray marr
