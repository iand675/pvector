{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UnboxedTuples #-}

-- | Chunk-level fusion framework for RRB-tree persistent vectors.
--
-- Standard stream fusion (as in @vector@) decomposes everything to individual
-- elements, destroying cache locality and preventing chunk-level optimizations.
-- This module streams at the 'SmallArray' chunk level, only dropping to
-- element-level processing within tight inner loops over contiguous memory.
--
-- Based on ideas from:
--   * Acar, Charguéraud, Rainey — /Chunked Sequences/ (ESA 2014, ML Workshop 2017)
--   * Scala 2.13 collections (array-segment level operations)
module Data.PVector.Internal.ChunkedBundle
  ( -- * Core types
    Chunk(..)
  , CStep(..)
  , ChunkedBundle(..)
  , CSize(..)

    -- * Chunk operations
  , fullChunk
  , indexChunk
  , chunkToList
  , mapChunk
  , imapChunk
  , foldChunk
  , filterChunk

    -- * Construction
  , cbEmpty
  , cbSingleton
  , cbFromChunks

    -- * Chunk-preserving transformers
  , cbMap
  , cbImap
  , cbFilter
  , cbTake
  , cbDrop

    -- * Consumers
  , cbFoldl'
  , cbFoldr
  , cbFoldlChunks'
  , cbLength
  , cbAll
  , cbAny
  , cbEq

    -- * Materialization
  , cbToChunkList
  ) where

import Data.Primitive.SmallArray
import Control.Monad.ST (runST)
import GHC.Exts (reallyUnsafePtrEquality#, isTrue#)

------------------------------------------------------------------------
-- Chunk: a contiguous array segment
------------------------------------------------------------------------

-- | A chunk is a view into a 'SmallArray' with offset and length.
-- 'cbTake'/'cbDrop' produce sub-chunks without copying by adjusting
-- offset/length. Only 'cbFilter' and materialization create new arrays.
data Chunk a = Chunk
  { chunkArray  :: {-# UNPACK #-} !(SmallArray a)
  , chunkOffset :: {-# UNPACK #-} !Int
  , chunkLength :: {-# UNPACK #-} !Int
  }

fullChunk :: SmallArray a -> Chunk a
fullChunk arr = Chunk arr 0 (sizeofSmallArray arr)
{-# INLINE fullChunk #-}

indexChunk :: Chunk a -> Int -> a
indexChunk (Chunk arr off _) i = indexSmallArray arr (off + i)
{-# INLINE indexChunk #-}

chunkToList :: Chunk a -> [a]
chunkToList (Chunk arr off len) = go 0
  where
    go !i
      | i >= len  = []
      | otherwise = indexSmallArray arr (off + i) : go (i + 1)

mapChunk :: (a -> b) -> Chunk a -> Chunk b
mapChunk f (Chunk arr off len) = Chunk arr' 0 len
  where
    !arr' = runST $ do
      marr <- newSmallArray len uninit
      let go !i
            | i >= len  = pure ()
            | otherwise = do
                writeSmallArray marr i $! f (indexSmallArray arr (off + i))
                go (i + 1)
      go 0
      unsafeFreezeSmallArray marr
    uninit = error "pvector: mapChunk"
{-# INLINE mapChunk #-}

imapChunk :: Int -> (Int -> a -> b) -> Chunk a -> Chunk b
imapChunk baseIdx f (Chunk arr off len) = Chunk arr' 0 len
  where
    !arr' = runST $ do
      marr <- newSmallArray len uninit
      let go !i
            | i >= len  = pure ()
            | otherwise = do
                writeSmallArray marr i $! f (baseIdx + i) (indexSmallArray arr (off + i))
                go (i + 1)
      go 0
      unsafeFreezeSmallArray marr
    uninit = error "pvector: imapChunk"
{-# INLINE imapChunk #-}

foldChunk :: (b -> a -> b) -> b -> Chunk a -> b
foldChunk f !z (Chunk arr off len) = go 0 z
  where
    go !i !acc
      | i >= len  = acc
      | otherwise = go (i + 1) (f acc (indexSmallArray arr (off + i)))
{-# INLINE foldChunk #-}

filterChunk :: (a -> Bool) -> Chunk a -> Chunk a
filterChunk p (Chunk arr off len) = runST $ do
  marr <- newSmallArray len uninit
  let go !iSrc !iDst
        | iSrc >= len = pure iDst
        | otherwise =
            let !x = indexSmallArray arr (off + iSrc)
            in if p x
               then writeSmallArray marr iDst x >> go (iSrc + 1) (iDst + 1)
               else go (iSrc + 1) iDst
  finalLen <- go 0 0
  frozen <- unsafeFreezeSmallArray marr
  pure $! Chunk frozen 0 finalLen
  where uninit = error "pvector: filterChunk"
{-# INLINE filterChunk #-}

-- | Pointer equality on chunks. When two chunks share the same underlying
-- SmallArray (from structural sharing after take/append/etc.), element
-- comparison is unnecessary.
chunkPtrEq :: Chunk a -> Chunk a -> Bool
chunkPtrEq (Chunk a1 off1 len1) (Chunk a2 off2 len2) =
  len1 == len2 && off1 == off2 && isTrue# (reallyUnsafePtrEquality# a1 a2)
{-# INLINE chunkPtrEq #-}

eqChunk :: Eq a => Chunk a -> Chunk a -> Bool
eqChunk c1 c2
  | chunkLength c1 /= chunkLength c2 = False
  | chunkPtrEq c1 c2 = True
  | otherwise = go 0
  where
    !n = chunkLength c1
    go !i
      | i >= n = True
      | indexChunk c1 i /= indexChunk c2 i = False
      | otherwise = go (i + 1)
{-# INLINE eqChunk #-}

------------------------------------------------------------------------
-- ChunkedBundle: chunk-level stream
------------------------------------------------------------------------

data CSize
  = CExact {-# UNPACK #-} !Int
  | CMax   {-# UNPACK #-} !Int
  | CUnknown

data CStep s a
  = CYield !(Chunk a) s
  | CSkip s
  | CDone

data ChunkedBundle a = forall s. ChunkedBundle
  { cbStep  :: s -> CStep s a
  , cbState :: s
  , cbSize  :: !CSize
  }

------------------------------------------------------------------------
-- Construction
------------------------------------------------------------------------

cbEmpty :: ChunkedBundle a
cbEmpty = ChunkedBundle (\() -> CDone) () (CExact 0)
{-# INLINE cbEmpty #-}

cbSingleton :: SmallArray a -> ChunkedBundle a
cbSingleton arr = ChunkedBundle step True (CExact (sizeofSmallArray arr))
  where
    step True  = CYield (fullChunk arr) False
    step False = CDone
{-# INLINE cbSingleton #-}

cbFromChunks :: [Chunk a] -> ChunkedBundle a
cbFromChunks cs = ChunkedBundle step cs CUnknown
  where
    step []     = CDone
    step (c:rest)
      | chunkLength c == 0 = CSkip rest
      | otherwise = CYield c rest
{-# INLINE [1] cbFromChunks #-}

------------------------------------------------------------------------
-- Chunk-preserving transformers
------------------------------------------------------------------------

cbMap :: (a -> b) -> ChunkedBundle a -> ChunkedBundle b
cbMap f (ChunkedBundle step s0 sz) = ChunkedBundle step' s0 sz
  where
    step' s = case step s of
      CYield chunk s' -> CYield (mapChunk f chunk) s'
      CSkip s'        -> CSkip s'
      CDone           -> CDone
{-# INLINE [1] cbMap #-}

cbImap :: (Int -> a -> b) -> ChunkedBundle a -> ChunkedBundle b
cbImap f (ChunkedBundle step s0 sz) = ChunkedBundle step' (s0, 0 :: Int) sz
  where
    step' (s, !idx) = case step s of
      CYield chunk s' ->
        let !chunk' = imapChunk idx f chunk
        in CYield chunk' (s', idx + chunkLength chunk)
      CSkip s' -> CSkip (s', idx)
      CDone    -> CDone
{-# INLINE [1] cbImap #-}

cbFilter :: (a -> Bool) -> ChunkedBundle a -> ChunkedBundle a
cbFilter p (ChunkedBundle step s0 sz) = ChunkedBundle step' s0 sz'
  where
    sz' = case sz of { CExact m -> CMax m; CMax m -> CMax m; CUnknown -> CUnknown }
    step' s = case step s of
      CYield chunk s' ->
        let !chunk' = filterChunk p chunk
        in if chunkLength chunk' == 0
           then CSkip s'
           else CYield chunk' s'
      CSkip s' -> CSkip s'
      CDone    -> CDone
{-# INLINE [1] cbFilter #-}

cbTake :: Int -> ChunkedBundle a -> ChunkedBundle a
cbTake n (ChunkedBundle step s0 _) = ChunkedBundle step' (s0, n) (CMax n)
  where
    step' (_, !remaining) | remaining <= 0 = CDone
    step' (s, !remaining) = case step s of
      CYield chunk s'
        | chunkLength chunk <= remaining ->
            CYield chunk (s', remaining - chunkLength chunk)
        | otherwise ->
            CYield (chunk { chunkLength = remaining }) (s', 0)
      CSkip s' -> CSkip (s', remaining)
      CDone    -> CDone
{-# INLINE [1] cbTake #-}

cbDrop :: Int -> ChunkedBundle a -> ChunkedBundle a
cbDrop n (ChunkedBundle step s0 sz) = ChunkedBundle step' (s0, n) sz'
  where
    sz' = case sz of
      CExact m -> CExact (max 0 (m - n))
      CMax   m -> CMax   (max 0 (m - n))
      CUnknown -> CUnknown
    step' (s, !remaining) = case step s of
      CYield chunk s'
        | remaining <= 0 -> CYield chunk (s', 0)
        | chunkLength chunk <= remaining ->
            CSkip (s', remaining - chunkLength chunk)
        | otherwise ->
            let !off' = chunkOffset chunk + remaining
                !len' = chunkLength chunk - remaining
            in CYield (Chunk (chunkArray chunk) off' len') (s', 0)
      CSkip s' -> CSkip (s', remaining)
      CDone    -> CDone
{-# INLINE [1] cbDrop #-}

------------------------------------------------------------------------
-- Consumers
------------------------------------------------------------------------

-- | Element-level strict left fold, iterating by chunks internally.
-- The tight inner loop over each chunk benefits from cache locality.
cbFoldl' :: (b -> a -> b) -> b -> ChunkedBundle a -> b
cbFoldl' f z0 (ChunkedBundle step s0 _) = goOuter s0 z0
  where
    goOuter s !acc = case step s of
      CYield chunk s' -> goOuter s' (foldChunk f acc chunk)
      CSkip s'        -> goOuter s' acc
      CDone           -> acc
{-# INLINE [1] cbFoldl' #-}

-- | Lazy right fold, iterating by chunks.
cbFoldr :: (a -> b -> b) -> b -> ChunkedBundle a -> b
cbFoldr f z0 (ChunkedBundle step s0 _) = go s0
  where
    go s = case step s of
      CYield (Chunk arr off len) s' ->
        let goArr !i rest
              | i >= off + len = rest
              | otherwise = f (indexSmallArray arr i) (goArr (i + 1) rest)
        in goArr off (go s')
      CSkip s' -> go s'
      CDone    -> z0
{-# INLINE [1] cbFoldr #-}

-- | Fold at the chunk level. O(n/chunkSize) iterations for operations
-- like 'cbLength' that only need chunk metadata.
cbFoldlChunks' :: (b -> Chunk a -> b) -> b -> ChunkedBundle a -> b
cbFoldlChunks' f z0 (ChunkedBundle step s0 _) = go s0 z0
  where
    go s !acc = case step s of
      CYield chunk s' -> go s' (f acc chunk)
      CSkip s'        -> go s' acc
      CDone           -> acc
{-# INLINE [1] cbFoldlChunks' #-}

-- | O(n/chunkSize) — just sums chunk lengths, never inspects elements.
cbLength :: ChunkedBundle a -> Int
cbLength = cbFoldlChunks' (\acc c -> acc + chunkLength c) 0
{-# INLINE [1] cbLength #-}

cbAll :: (a -> Bool) -> ChunkedBundle a -> Bool
cbAll p (ChunkedBundle step s0 _) = go s0
  where
    go s = case step s of
      CYield chunk s' -> allInChunk chunk && go s'
      CSkip s'        -> go s'
      CDone           -> True
    allInChunk (Chunk arr off len) = goArr 0
      where
        goArr !i
          | i >= len  = True
          | otherwise = p (indexSmallArray arr (off + i)) && goArr (i + 1)
{-# INLINE [1] cbAll #-}

cbAny :: (a -> Bool) -> ChunkedBundle a -> Bool
cbAny p (ChunkedBundle step s0 _) = go s0
  where
    go s = case step s of
      CYield chunk s' -> anyInChunk chunk || go s'
      CSkip s'        -> go s'
      CDone           -> False
    anyInChunk (Chunk arr off len) = goArr 0
      where
        goArr !i
          | i >= len  = False
          | otherwise = p (indexSmallArray arr (off + i)) || goArr (i + 1)
{-# INLINE [1] cbAny #-}

-- | Structural equality with pointer-equality short-circuit.
-- After take/append that preserves structural sharing, many chunks are
-- pointer-identical. We check that first — O(1) per shared chunk.
cbEq :: Eq a => ChunkedBundle a -> ChunkedBundle a -> Bool
cbEq (ChunkedBundle stepA sA0 _) (ChunkedBundle stepB sB0 _) = go sA0 sB0
  where
    go sA sB = case (stepA sA, stepB sB) of
      (CYield cA sA', CYield cB sB')
        | chunkPtrEq cA cB -> go sA' sB'
        | otherwise        -> eqChunk cA cB && go sA' sB'
      (CSkip sA', _) -> go sA' sB
      (_, CSkip sB') -> go sA sB'
      (CDone, CDone) -> True
      _              -> False
{-# INLINE [1] cbEq #-}

------------------------------------------------------------------------
-- Materialization
------------------------------------------------------------------------

cbToChunkList :: ChunkedBundle a -> [Chunk a]
cbToChunkList (ChunkedBundle step s0 _) = go s0
  where
    go s = case step s of
      CYield chunk s' -> chunk : go s'
      CSkip s'        -> go s'
      CDone           -> []

------------------------------------------------------------------------
-- Rewrite rules: chunk-level fusion
------------------------------------------------------------------------

{-# RULES

-- Map composition: two chunk-level maps fuse into one
"cbMap/cbMap" forall f g b.
  cbMap f (cbMap g b) = cbMap (f . g) b

-- Filter composition: two filters fuse
"cbFilter/cbFilter" forall p q b.
  cbFilter p (cbFilter q b) = cbFilter (\x -> q x && p x) b

-- Fold/map fusion: avoid allocating intermediate chunk arrays
"cbFoldl'/cbMap" forall f g z b.
  cbFoldl' f z (cbMap g b) = cbFoldl' (\acc x -> f acc (g x)) z b

-- Fold/filter fusion: filter predicate inlined into fold
"cbFoldl'/cbFilter" forall f z p b.
  cbFoldl' f z (cbFilter p b) = cbFoldl' (\acc x -> if p x then f acc x else acc) z b

-- All/any through map: push predicate composition
"cbAll/cbMap" forall p f b. cbAll p (cbMap f b) = cbAll (p . f) b
"cbAny/cbMap" forall p f b. cbAny p (cbMap f b) = cbAny (p . f) b

-- Length after filter: counting fold (no chunk allocation)
"cbLength/cbFilter" forall p b.
  cbLength (cbFilter p b) = cbFoldl' (\acc x -> if p x then acc + 1 else acc) 0 b

-- Slice algebra
"cbTake/cbTake" forall n m b. cbTake n (cbTake m b) = cbTake (min n m) b
"cbDrop/cbDrop" forall n m b. cbDrop n (cbDrop m b) = cbDrop (n + m) b

-- Map through take/drop: push map inside (cheaper to map fewer elements)
"cbMap/cbTake" forall f n b. cbMap f (cbTake n b) = cbTake n (cbMap f b)
"cbMap/cbDrop" forall f n b. cbMap f (cbDrop n b) = cbDrop n (cbMap f b)

  #-}
