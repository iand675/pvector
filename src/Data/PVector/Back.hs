{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.PVector.Back
  ( -- * Vector type
    Vector(..)

    -- * Construction
  , empty
  , singleton
  , fromList
  , fromListN
  , replicate
  , generate
  , iterateN
  , unfoldr
  , unfoldrN
  , cons
  , snoc
  , (|>)
  , (<|)
  , (++)
  , concat
  , force

    -- * Query
  , null
  , length

    -- * Indexing
  , index
  , (!)
  , (!?)
  , unsafeIndex
  , head
  , last
  , indexM
  , headM
  , lastM

    -- * Slicing
  , slice
  , init
  , tail
  , take
  , drop
  , splitAt
  , uncons
  , unsnoc

    -- * Update
  , update
  , (//)
  , adjust
  , adjust'

    -- * Mapping
  , map
  , imap
  , concatMap
  , mapMaybe
  , imapMaybe

    -- * Monadic mapping
  , mapM
  , mapM_
  , forM
  , forM_
  , imapM
  , imapM_

    -- * Zipping
  , zip
  , zip3
  , zipWith
  , zipWith3
  , izipWith
  , unzip
  , unzip3

    -- * Filtering
  , filter
  , ifilter
  , takeWhile
  , dropWhile
  , partition
  , span
  , break

    -- * Searching
  , elem
  , notElem
  , find
  , findIndex
  , findIndices
  , elemIndex
  , elemIndices

    -- * Folding
  , foldl
  , foldl'
  , foldl1
  , foldl1'
  , foldr
  , foldr'
  , foldr1
  , foldr1'
  , ifoldl'
  , ifoldr
  , foldMap
  , foldMap'
  , all
  , any
  , and
  , or
  , sum
  , product
  , maximum
  , minimum
  , maximumBy
  , minimumBy

    -- * Scans
  , prescanl'
  , scanl
  , scanl'
  , scanl1
  , scanl1'
  , scanr
  , scanr'
  , scanr1
  , scanr1'

    -- * Enumeration
  , enumFromN
  , enumFromStepN
  , enumFromTo
  , enumFromThenTo

    -- * Conversions
  , toList
  , fromVector
  , reverse

    -- * Chunk-based operations
  , foldChunks
  , foldChunks'
  , mapChunks
  , forChunks_

    -- * Transient (mutable) operations
  , MVector
  , create
  , modify
  , thaw
  , freeze
  , unsafeFreeze
  , mNew
  , mLength
  , mRead
  , mWrite
  , mPush
  , mPushChunk
  , mPop

    -- * Fast bulk construction
  , generateN
  , unsafeFromChunks

    -- * Stream fusion
  , stream
  , unstream

    -- * Recycling framework (Leshchinskiy 2008)
  , New(..)
  , new
  , clone
  , fill
  , updateNew
  , mapNew
  , transformNew
  , inplace_map

    -- * Internal (used by Front/Deque)
  , rfoldl'
  , rfoldr
  , forEach_
  ) where

import Control.Applicative (Alternative)
import qualified Control.Applicative as A
import Control.DeepSeq (NFData(..))
import Control.Monad (MonadPlus(..), when, ap)
import Control.Monad.Primitive
import Control.Monad.ST
import Data.Bits
import Data.Primitive.MutVar
import Data.Primitive.SmallArray
-- PrimArray types and functions are re-exported from Internal
import qualified GHC.Exts as Exts
import GHC.Base (build)

import Data.PVector.Internal
import Data.PVector.Internal.Stream
  (Bundle(..), Step(..), Size(..), MStream(..), Id(..))
import qualified Data.PVector.Internal.Stream as S
import Data.PVector.Internal.ChunkedBundle (ChunkedBundle, Chunk(..), CStep(..), CSize(..))
import qualified Data.PVector.Internal.ChunkedBundle as CB
import GHC.Exts (reallyUnsafePtrEquality#, isTrue#)

import Prelude hiding
  ( null, length, head, last, map, reverse, filter, take, drop
  , foldMap, zip, zip3, zipWith, unzip
  , replicate, foldr, foldl, foldl1, foldr1
  , concat, concatMap, (++), init, tail, splitAt
  , mapM, mapM_, sequence, elem, notElem, all, any, and, or
  , sum, product, maximum, minimum
  , scanl, scanl1, scanr, scanr1
  , span, break, takeWhile, dropWhile
  , enumFromTo, enumFromThenTo
  , zip3, zipWith3, unzip3
  )
import qualified Prelude
import qualified Data.Foldable as F

------------------------------------------------------------------------
-- Vector type
------------------------------------------------------------------------

-- | RRB-tree based persistent vector with symmetric prefix/suffix buffers.
--
-- The elements are stored in three regions:
--   1. @vPrefix@ — leftmost partial leaf (prepend buffer, 0 to bf-1 elements)
--   2. @vRoot@   — the RRB tree (may contain Relaxed nodes with size tables)
--   3. @vTail@   — rightmost partial leaf (append buffer, 0 to bf-1 elements)
--
-- @vSize@ is the total number of elements across all three regions.
data Vector a = Vector
  { vSize   :: {-# UNPACK #-} !Int
  , vShift  :: {-# UNPACK #-} !Int
  , vPrefix :: {-# UNPACK #-} !(SmallArray a)
  , vRoot   :: !(Node a)
  , vTail   :: {-# UNPACK #-} !(SmallArray a)
  }

------------------------------------------------------------------------
-- Mutable (transient) vector
------------------------------------------------------------------------

data MVector s a = MVector {-# UNPACK #-} !(MutVar s (MVState s a))

data MVState s a = MVState
  { mvSize     :: {-# UNPACK #-} !Int
  , mvShift    :: {-# UNPACK #-} !Int
  , mvRoot     :: !(MNode s a)
  , mvTail     :: {-# UNPACK #-} !(SmallMutableArray s a)
  , mvTailSize :: {-# UNPACK #-} !Int
  }

getMV :: PrimMonad m => MVector (PrimState m) a -> m (MVState (PrimState m) a)
getMV (MVector ref) = readMutVar ref
{-# INLINE getMV #-}

putMV :: PrimMonad m => MVector (PrimState m) a -> MVState (PrimState m) a -> m ()
putMV (MVector ref) = writeMutVar ref
{-# INLINE putMV #-}

------------------------------------------------------------------------
-- Instances
------------------------------------------------------------------------

instance Show a => Show (Vector a) where
  showsPrec p v = showsPrec p (toList v)

instance Eq a => Eq (Vector a) where
  v1 == v2
    | length v1 /= length v2 = False
    | otherwise = CB.cbEq (cstream v1) (cstream v2)

instance Ord a => Ord (Vector a) where
  compare v1 v2 = go 0
    where
      !n1 = length v1
      !n2 = length v2
      !n  = min n1 n2
      go i | i >= n    = compare n1 n2
           | otherwise = case compare (unsafeIndex v1 i) (unsafeIndex v2 i) of
               EQ -> go (i + 1)
               x  -> x

instance Semigroup (Vector a) where
  (<>) = append
  {-# INLINE (<>) #-}

instance Monoid (Vector a) where
  mempty = empty
  {-# INLINE mempty #-}
  mconcat = concat
  {-# INLINE mconcat #-}

instance Functor Vector where
  fmap = map
  {-# INLINE fmap #-}

instance F.Foldable Vector where
  foldr   = Data.PVector.Back.foldr
  foldl'  = Data.PVector.Back.foldl'
  foldMap = Data.PVector.Back.foldMap
  null    = Data.PVector.Back.null
  length  = Data.PVector.Back.length
  toList  = Data.PVector.Back.toList
  elem    = Data.PVector.Back.elem
  maximum = Data.PVector.Back.maximum
  minimum = Data.PVector.Back.minimum
  sum     = Data.PVector.Back.sum
  product = Data.PVector.Back.product
  {-# INLINE foldr #-}
  {-# INLINE foldl' #-}
  {-# INLINE foldMap #-}
  {-# INLINE null #-}
  {-# INLINE length #-}
  {-# INLINE toList #-}

instance Traversable Vector where
  traverse f v = foldlDirect (\acc a -> snoc <$> acc <*> f a) (Prelude.pure empty) v
  {-# INLINE traverse #-}

instance Exts.IsList (Vector a) where
  type Item (Vector a) = a
  fromList  = Data.PVector.Back.fromList
  fromListN = Data.PVector.Back.fromListN
  toList    = Data.PVector.Back.toList
  {-# INLINE fromList #-}
  {-# INLINE toList #-}

instance NFData a => NFData (Vector a) where
  rnf (Vector _ _ prefix root tail_) = rnfArray prefix `seq` rnf root `seq` rnfArray tail_

instance Applicative Vector where
  pure = singleton
  (<*>) = ap
  {-# INLINE pure #-}
  {-# INLINE (<*>) #-}

instance Monad Vector where
  return = singleton
  v >>= f = concatMap f v
  {-# INLINE return #-}
  {-# INLINE (>>=) #-}

instance MonadPlus Vector where
  mzero = empty
  mplus = append

instance Alternative Vector where
  empty = Data.PVector.Back.empty
  (<|>) = append
  {-# INLINE (<|>) #-}

------------------------------------------------------------------------
-- Construction
------------------------------------------------------------------------

empty :: Vector a
empty = Vector 0 bfBits emptyTail Empty emptyTail
{-# INLINE empty #-}

singleton :: a -> Vector a
singleton x = Vector 1 bfBits emptyTail Empty tail_
  where
    !tail_ = runST $ do
      a <- newSmallArray 1 x
      unsafeFreezeSmallArray a
{-# INLINE singleton #-}

fromList :: [a] -> Vector a
fromList [] = empty
fromList xs = runST $ do
  mv <- mNew
  st0 <- getMV mv
  goFill mv (mvTail st0) 0 xs
  v <- unsafeFreeze mv
  pure v
  where
    goFill !mv !tail_ !ti [] = do
      st <- getMV mv
      putMV mv st { mvSize = mvSize st + ti - mvTailSize st
                   , mvTailSize = ti }
    goFill !mv !tail_ !ti (a:as)
      | ti < bf = do
          writeSmallArray tail_ ti a
          goFill mv tail_ (ti + 1) as
      | otherwise = do
          st <- getMV mv
          let !n = mvSize st + (bf - mvTailSize st)
          frozenTail <- unsafeFreezeSmallArray tail_
          let !willOverflow = n > 0 && unsafeShiftR n bfBits > unsafeShiftL 1 (mvShift st)
          (newRoot, newShift) <-
            if willOverflow
              then do
                arr <- newSmallArray 2 (Frozen Empty)
                writeSmallArray arr 0 (mvRoot st)
                let !path = thawNode (newPath (mvShift st) (Leaf frozenTail))
                writeSmallArray arr 1 path
                pure (MInternal arr, mvShift st + bfBits)
              else do
                r <- mPushTail n (mvShift st) (mvRoot st) frozenTail
                pure (r, mvShift st)
          newTail <- newSmallArray bf undefinedElem'
          writeSmallArray newTail 0 a
          putMV mv $! MVState (n + 1) newShift newRoot newTail 1
          goFill mv newTail 1 as
    undefinedElem' = error "pvector: uninitialised"
{-# INLINE [1] fromList #-}

fromListN :: Int -> [a] -> Vector a
fromListN n xs
  | n <= 0    = empty
  | otherwise = runST $ do
      mv <- mNew
      st0 <- getMV mv
      goFillN mv (mvTail st0) 0 n xs
      unsafeFreeze mv
  where
    goFillN !mv !_ !ti !_ [] = do
      st <- getMV mv
      putMV mv st { mvSize = mvSize st + ti - mvTailSize st
                   , mvTailSize = ti }
    goFillN !mv !_ !ti !remaining _
      | remaining <= 0 = do
          st <- getMV mv
          putMV mv st { mvSize = mvSize st + ti - mvTailSize st
                       , mvTailSize = ti }
    goFillN !mv !tail_ !ti !remaining (a:as)
      | ti < bf = do
          writeSmallArray tail_ ti a
          goFillN mv tail_ (ti + 1) (remaining - 1) as
      | otherwise = do
          st <- getMV mv
          let !n' = mvSize st + (bf - mvTailSize st)
          frozenTail <- unsafeFreezeSmallArray tail_
          let !willOverflow = n' > 0 && unsafeShiftR n' bfBits > unsafeShiftL 1 (mvShift st)
          (newRoot, newShift) <-
            if willOverflow
              then do
                arr <- newSmallArray 2 (Frozen Empty)
                writeSmallArray arr 0 (mvRoot st)
                let !path = thawNode (newPath (mvShift st) (Leaf frozenTail))
                writeSmallArray arr 1 path
                pure (MInternal arr, mvShift st + bfBits)
              else do
                r <- mPushTail n' (mvShift st) (mvRoot st) frozenTail
                pure (r, mvShift st)
          newTail <- newSmallArray bf undefinedElem'
          writeSmallArray newTail 0 a
          putMV mv $! MVState (n' + 1) newShift newRoot newTail 1
          goFillN mv newTail 1 (remaining - 1) as
    undefinedElem' = error "pvector: uninitialised"
{-# INLINE [1] fromListN #-}

replicate :: Int -> a -> Vector a
replicate n x
  | n <= 0    = empty
  | otherwise = create $ \mv -> do
      sharedChunk <- newSmallArray bf x >>= unsafeFreezeSmallArray
      let !fullChunks = unsafeShiftR n bfBits
          !remaining  = n .&. bfMask
          goChunks !i
            | i >= fullChunks = pure ()
            | otherwise = mPushChunk mv sharedChunk >> goChunks (i + 1)
          goRem !i
            | i >= remaining = pure ()
            | otherwise = mPush mv x >> goRem (i + 1)
      goChunks 0
      goRem 0
{-# INLINE replicate #-}

unsafeFromChunks :: [SmallArray a] -> SmallArray a -> Vector a
unsafeFromChunks chunks tail_ = create $ \mv -> do
  let go [] = pure ()
      go (c:cs) = mPushChunk mv c >> go cs
  go chunks
  let !ts = sizeofSmallArray tail_
      goTail !i
        | i >= ts   = pure ()
        | otherwise = mPush mv (indexSmallArray tail_ i) >> goTail (i + 1)
  goTail 0
{-# INLINE unsafeFromChunks #-}

generate :: Int -> (Int -> a) -> Vector a
generate n f
  | n <= 0    = empty
  | otherwise = create $ \mv ->
      let go i | i >= n = pure ()
               | otherwise = mPush mv (f i) >> go (i + 1)
      in go 0
{-# INLINE generate #-}

generateN :: Int -> (Int -> a) -> Vector a
generateN = generate
{-# INLINE generateN #-}

iterateN :: Int -> (a -> a) -> a -> Vector a
iterateN n f x0
  | n <= 0    = empty
  | otherwise = create $ \mv ->
      let go 0 _ = pure ()
          go i x = mPush mv x >> go (i - 1) (f x)
      in go n x0
{-# INLINE iterateN #-}

unfoldr :: (b -> Maybe (a, b)) -> b -> Vector a
unfoldr f = unstream . S.unfoldr f
{-# INLINE unfoldr #-}

unfoldrN :: Int -> (b -> Maybe (a, b)) -> b -> Vector a
unfoldrN n f s0 = create $ \mv ->
  let go 0 _ = pure ()
      go i s = case f s of
        Nothing      -> pure ()
        Just (a, s') -> mPush mv a >> go (i - 1) s'
  in go n s0
{-# INLINE unfoldrN #-}

-- | O(eC) amortized. Prepend an element using the prefix buffer.
-- When the prefix is full, the prefix leaf is pushed into the tree.
cons :: a -> Vector a -> Vector a
cons x v
  | n == 0    = singleton x
  | pLen < bf =
      v { vSize = n + 1
        , vPrefix = consArray x (vPrefix v)
        }
  | otherwise =
      let !prefixLeaf = Leaf (vPrefix v)
          (!newShift, !newRoot) = consLeafToTree (vShift v) prefixLeaf (vRoot v)
          !newPrefix = runST $ do
            a <- newSmallArray 1 x
            unsafeFreezeSmallArray a
      in Vector (n + 1) newShift newPrefix newRoot (vTail v)
  where
    !n    = vSize v
    !pLen = sizeofSmallArray (vPrefix v)
{-# INLINE cons #-}

snoc :: Vector a -> a -> Vector a
snoc v x
  | n == 0 = singleton x
  | tLen < bf =
      v { vSize = n + 1
        , vTail = snocArray (vTail v) x
        }
  | otherwise =
      let !newTreeN = treeN + tLen
          !willOverflow = unsafeShiftR newTreeN bfBits > unsafeShiftL 1 (vShift v)
          !newShift | willOverflow = vShift v + bfBits
                    | otherwise    = vShift v
          !newRoot
            | willOverflow =
                let arr = runST $ do
                      a <- newSmallArray 2 Empty
                      writeSmallArray a 0 (vRoot v)
                      writeSmallArray a 1 (newPath (vShift v) (Leaf (vTail v)))
                      unsafeFreezeSmallArray a
                in Internal arr
            | otherwise = pushTail newTreeN (vShift v) (vRoot v) (vTail v)
          !newTailArr = runST $ do
            a <- newSmallArray 1 x
            unsafeFreezeSmallArray a
      in Vector (n + 1) newShift (vPrefix v) newRoot newTailArr
  where
    !n = vSize v
    !tLen = sizeofSmallArray (vTail v)
    !treeN = n - sizeofSmallArray (vPrefix v) - tLen
{-# INLINE snoc #-}

(|>) :: Vector a -> a -> Vector a
(|>) = snoc
{-# INLINE (|>) #-}

(<|) :: a -> Vector a -> Vector a
(<|) = cons
{-# INLINE (<|) #-}

(++) :: Vector a -> Vector a -> Vector a
(++) = append
{-# INLINE (++) #-}

concat :: [Vector a] -> Vector a
concat [] = empty
concat [v] = v
concat vs = create $ \mv ->
  let go [] = pure ()
      go (v:vs')
        | vSize v == 0 = go vs'
        | otherwise = do
            forEach_ v $ \a -> mPush mv a
            go vs'
  in go vs
{-# INLINE concat #-}

force :: NFData a => Vector a -> Vector a
force v = rnf v `seq` v
{-# INLINE force #-}

------------------------------------------------------------------------
-- Query
------------------------------------------------------------------------

null :: Vector a -> Bool
null v = vSize v == 0
{-# INLINE null #-}

length :: Vector a -> Int
length = vSize
{-# INLINE length #-}

------------------------------------------------------------------------
-- Indexing
------------------------------------------------------------------------

index :: Vector a -> Int -> a
index v i
  | i < 0 || i >= vSize v = error $ "Data.PVector.Back.index: " Prelude.++ show i
                                  Prelude.++ " out of bounds [0," Prelude.++ show (vSize v) Prelude.++ ")"
  | otherwise = unsafeIndex v i
{-# INLINE [1] index #-}

(!) :: Vector a -> Int -> a
(!) = index
{-# INLINE [1] (!) #-}

(!?) :: Vector a -> Int -> Maybe a
(!?) v i
  | i < 0 || i >= vSize v = Nothing
  | otherwise = Just $! unsafeIndex v i
{-# INLINE [1] (!?) #-}

unsafeIndex :: Vector a -> Int -> a
unsafeIndex v i
  | i < pLen  = indexSmallArray (vPrefix v) i
  | i >= n - tLen = indexSmallArray (vTail v) (i - (n - tLen))
  | otherwise = treeIndex (vShift v) (i - pLen) (vRoot v)
  where
    !n    = vSize v
    !pLen = sizeofSmallArray (vPrefix v)
    !tLen = sizeofSmallArray (vTail v)
{-# INLINE [1] unsafeIndex #-}

head :: Vector a -> a
head v
  | vSize v == 0 = error "Data.PVector.Back.head: empty"
  | pLen > 0     = indexSmallArray (vPrefix v) 0
  | otherwise    = unsafeIndex v 0
  where !pLen = sizeofSmallArray (vPrefix v)
{-# INLINE [1] head #-}

last :: Vector a -> a
last v
  | vSize v == 0 = error "Data.PVector.Back.last: empty"
  | tLen > 0     = indexSmallArray (vTail v) (tLen - 1)
  | otherwise    = unsafeIndex v (vSize v - 1)
  where !tLen = sizeofSmallArray (vTail v)
{-# INLINE [1] last #-}

indexM :: Monad m => Vector a -> Int -> m a
indexM v i = index v i `seq` pure (index v i)
{-# INLINE indexM #-}

headM :: Monad m => Vector a -> m a
headM v = head v `seq` pure (head v)
{-# INLINE headM #-}

lastM :: Monad m => Vector a -> m a
lastM v = last v `seq` pure (last v)
{-# INLINE lastM #-}

------------------------------------------------------------------------
-- Slicing
------------------------------------------------------------------------

slice :: Int -> Int -> Vector a -> Vector a
slice i n v
  | i < 0 || n < 0 || i + n > vSize v = error "Data.PVector.Back.slice: out of bounds"
  | n == vSize v = v
  | n == 0       = empty
  | otherwise    = rrbSlice i n v
{-# INLINE slice #-}

init :: Vector a -> Vector a
init v = initDirect v
{-# NOINLINE [1] init #-}

initDirect :: Vector a -> Vector a
initDirect v = case unsnoc v of
  Nothing    -> error "Data.PVector.Back.init: empty"
  Just (v',_) -> v'
{-# INLINE initDirect #-}

tail :: Vector a -> Vector a
tail v = tailDirect v
{-# NOINLINE [1] tail #-}

tailDirect :: Vector a -> Vector a
tailDirect v
  | vSize v == 0 = error "Data.PVector.Back.tail: empty"
  | otherwise     = dropDirect 1 v
{-# INLINE tailDirect #-}

take :: Int -> Vector a -> Vector a
take n v = takeDirect n v
{-# NOINLINE [1] take #-}

takeDirect :: Int -> Vector a -> Vector a
takeDirect n v
  | n <= 0        = empty
  | n >= vSize v  = v
  | otherwise     = rrbSlice 0 n v
{-# INLINE takeDirect #-}

drop :: Int -> Vector a -> Vector a
drop n v = dropDirect n v
{-# NOINLINE [1] drop #-}

dropDirect :: Int -> Vector a -> Vector a
dropDirect n v
  | n <= 0        = v
  | n >= vSize v  = empty
  | otherwise     = rrbSlice n (vSize v - n) v
{-# INLINE dropDirect #-}

splitAt :: Int -> Vector a -> (Vector a, Vector a)
splitAt n v = (take n v, drop n v)
{-# INLINE splitAt #-}

uncons :: Vector a -> Maybe (a, Vector a)
uncons v
  | vSize v == 0 = Nothing
  | vSize v == 1 = Just (unsafeIndex v 0, empty)
  | pLen > 1 =
      let !x = indexSmallArray (vPrefix v) 0
          !newPrefix = cloneSmallArray (vPrefix v) 1 (pLen - 1)
      in Just (x, v { vSize = vSize v - 1, vPrefix = newPrefix })
  | pLen == 1 =
      let !x = indexSmallArray (vPrefix v) 0
      in Just (x, dropDirect 1 v)
  | otherwise =
      Just (unsafeIndex v 0, dropDirect 1 v)
  where !pLen = sizeofSmallArray (vPrefix v)
{-# INLINE uncons #-}

unsnoc :: Vector a -> Maybe (Vector a, a)
unsnoc v
  | n == 0 = Nothing
  | n == 1 = Just (empty, unsafeIndex v 0)
  | tLen > 1 =
      let (!newTail, !x) = unsnocArray (vTail v)
      in Just (v { vSize = n - 1, vTail = newTail }, x)
  | tLen == 1 =
      let !x = indexSmallArray (vTail v) 0
      in Just (takeDirect (n - 1) v, x)
  | otherwise =
      Just (takeDirect (n - 1) v, unsafeIndex v (n - 1))
  where
    !n = vSize v
    !tLen = sizeofSmallArray (vTail v)
{-# INLINE unsnoc #-}

------------------------------------------------------------------------
-- Update
------------------------------------------------------------------------

update :: Int -> a -> Vector a -> Vector a
update i x v
  | i < 0 || i >= vSize v = error "Data.PVector.Back.update: out of bounds"
  | i < pLen = v { vPrefix = cloneAndSet (vPrefix v) i x }
  | i >= n - tLen = v { vTail = cloneAndSet (vTail v) (i - (n - tLen)) x }
  | otherwise = v { vRoot = updateTree (vShift v) (i - pLen) x (vRoot v) }
  where
    !n    = vSize v
    !pLen = sizeofSmallArray (vPrefix v)
    !tLen = sizeofSmallArray (vTail v)
{-# INLINE update #-}

updateTree :: Int -> Int -> a -> Node a -> Node a
updateTree !shift !i x node = case node of
  Internal arr ->
    let !idx = indexAtLevel i shift
        !child = indexSmallArray arr idx
        !child' = updateTree (shift - bfBits) i x child
    in Internal (cloneAndSet arr idx child')
  Relaxed arr sizes ->
    let !idx = relaxedIndex sizes i
        !off = if idx == 0 then 0 else indexPrimArray sizes (idx - 1)
        !child = indexSmallArray arr idx
        !child' = updateTree (shift - bfBits) (i - off) x child
    in Relaxed (cloneAndSet arr idx child') sizes
  Leaf arr -> Leaf (cloneAndSet arr (i .&. bfMask) x)
  Empty -> error "pvector: updateTree hit Empty"
{-# INLINEABLE updateTree #-}

-- | Bulk update from a list of (index, value) pairs.
--
-- Sorts the updates by index so that multiple updates hitting the same
-- leaf are coalesced into a single path-copy.  For @k@ updates that all
-- land in distinct leaves this is @O(k log n)@; when several cluster in
-- the same leaf the shared path prefix is copied only once.
(//) :: Vector a -> [(Int, a)] -> Vector a
xs // [] = xs
xs // [(i, x)] = update i x xs
xs // ps = batchUpdate xs (sortByFst ps)
{-# INLINE [1] (//) #-}

-- | Apply a list of updates sorted by index.  Adjacent updates in the
-- same region (prefix / tree-leaf / tail) share the single clone.
batchUpdate :: Vector a -> [(Int, a)] -> Vector a
batchUpdate v [] = v
batchUpdate v ps = v { vPrefix = newPrefix, vRoot = newRoot, vTail = newTail }
  where
    !pLen = sizeofSmallArray (vPrefix v)
    !tLen = sizeofSmallArray (vTail v)
    !n    = vSize v
    !tStart = n - tLen

    (!prefixUpdates, rest1) = Prelude.span (\(i,_) -> i < pLen) ps
    (!treeUpdates,   rest2) = Prelude.span (\(i,_) -> i < tStart) rest1
    !tailUpdates = rest2

    !newPrefix
      | Prelude.null prefixUpdates = vPrefix v
      | otherwise = applyUpdates (vPrefix v) 0 prefixUpdates

    !newTail
      | Prelude.null tailUpdates = vTail v
      | otherwise = applyUpdates (vTail v) tStart tailUpdates

    !newRoot
      | Prelude.null treeUpdates = vRoot v
      | otherwise = batchUpdateTree (vShift v) pLen (vRoot v) treeUpdates

-- | Apply sorted updates to a SmallArray. @base@ is subtracted from each
-- index to get the local position in the array.
applyUpdates :: SmallArray a -> Int -> [(Int, a)] -> SmallArray a
applyUpdates arr base ups = runST $ do
  marr <- thawSmallArray arr 0 (sizeofSmallArray arr)
  let go [] = pure ()
      go ((i, x) : rest) = writeSmallArray marr (i - base) x >> go rest
  go ups
  unsafeFreezeSmallArray marr
{-# INLINE applyUpdates #-}

-- | Walk the tree, applying sorted updates.  At each internal node we
-- partition the remaining updates among children and only clone children
-- that actually receive updates.
batchUpdateTree :: Int -> Int -> Node a -> [(Int, a)] -> Node a
batchUpdateTree !_ !_ node [] = node
batchUpdateTree !shift !base node ups = case node of
  Leaf arr ->
    Leaf (applyUpdates arr base ups)
  Internal arr ->
    let !childSz = unsafeShiftL 1 shift
        go !i !remaining
          | i >= sizeofSmallArray arr || Prelude.null remaining = []
          | otherwise =
              let !childBase = base + i * childSz
                  !childEnd  = childBase + childSz
                  (!here, !rest) = Prelude.span (\(idx,_) -> idx < childEnd) remaining
              in (i, here) : go (i + 1) rest
        groups = go 0 ups
        applyGroup arr' (ci, cUps)
          | Prelude.null cUps = arr'
          | otherwise =
              let !child = indexSmallArray arr' ci
                  !child' = batchUpdateTree (shift - bfBits) (base + ci * childSz) child cUps
              in cloneAndSet arr' ci child'
    in Internal (Prelude.foldl applyGroup arr groups)
  Relaxed arr sizes ->
    let findChild !ci !remaining
          | ci >= sizeofSmallArray arr || Prelude.null remaining = []
          | otherwise =
              let !childEnd = base + indexPrimArray sizes ci
                  (!here, !rest) = Prelude.span (\(idx,_) -> idx < childEnd) remaining
                  !childBase = if ci == 0 then base else base + indexPrimArray sizes (ci - 1)
              in (ci, childBase, here) : findChild (ci + 1) rest
        groups = findChild 0 ups
        applyGroupR arr' (ci, cBase, cUps)
          | Prelude.null cUps = arr'
          | otherwise =
              let !child = indexSmallArray arr' ci
                  !child' = batchUpdateTree (shift - bfBits) cBase child cUps
              in cloneAndSet arr' ci child'
    in Relaxed (Prelude.foldl applyGroupR arr groups) sizes
  Empty -> error "pvector: batchUpdateTree hit Empty"

-- | Insertion sort by first element — fine for the typical small update lists.
sortByFst :: [(Int, a)] -> [(Int, a)]
sortByFst [] = []
sortByFst [x] = [x]
sortByFst xs = mergeSorted (sortByFst l) (sortByFst r)
  where
    !half = Prelude.length xs `div` 2
    (l, r) = Prelude.splitAt half xs
    mergeSorted [] bs = bs
    mergeSorted as [] = as
    mergeSorted (a@(ai,_):as) (b@(bi,_):bs)
      | ai <= bi  = a : mergeSorted as (b:bs)
      | otherwise = b : mergeSorted (a:as) bs

adjust :: (a -> a) -> Int -> Vector a -> Vector a
adjust f i v
  | i < 0 || i >= vSize v = error "Data.PVector.Back.adjust: out of bounds"
  | otherwise = update i (f (unsafeIndex v i)) v
{-# INLINE adjust #-}

adjust' :: (a -> a) -> Int -> Vector a -> Vector a
adjust' f i v
  | i < 0 || i >= vSize v = error "Data.PVector.Back.adjust': out of bounds"
  | otherwise = let !x = f (unsafeIndex v i) in update i x v
{-# INLINE adjust' #-}

------------------------------------------------------------------------
-- Mapping
------------------------------------------------------------------------

map :: (a -> b) -> Vector a -> Vector b
map f = mapDirect f
{-# NOINLINE [1] map #-}

mapDirect :: (a -> b) -> Vector a -> Vector b
mapDirect f v
  | n == 0 = empty
  | otherwise = Vector n (vShift v) newPrefix newRoot newTail
  where
    !n = vSize v
    !newPrefix = mapArray' f (vPrefix v)
    !newTail = mapArray' f (vTail v)
    !newRoot = mapNode f (vShift v) (vRoot v)
{-# INLINE mapDirect #-}

mapNode :: (a -> b) -> Int -> Node a -> Node b
mapNode _ !_ Empty = Empty
mapNode f !_ (Leaf arr) = Leaf (mapChunk f arr)
mapNode f !shift (Internal arr) = Internal $ runST $ do
  let !n = sizeofSmallArray arr
  marr <- newSmallArray n Empty
  let go i
        | i >= n = pure ()
        | otherwise = do
            writeSmallArray marr i (mapNode f (shift - bfBits) (indexSmallArray arr i))
            go (i + 1)
  go 0
  unsafeFreezeSmallArray marr
mapNode f !shift (Relaxed arr sizes) = Relaxed newArr sizes
  where
    !newArr = runST $ do
      let !n = sizeofSmallArray arr
      marr <- newSmallArray n Empty
      let go i
            | i >= n = pure ()
            | otherwise = do
                writeSmallArray marr i (mapNode f (shift - bfBits) (indexSmallArray arr i))
                go (i + 1)
      go 0
      unsafeFreezeSmallArray marr

imap :: (Int -> a -> b) -> Vector a -> Vector b
imap f v = generate (vSize v) (\i -> f i (unsafeIndex v i))
{-# INLINE imap #-}

concatMap :: (a -> Vector b) -> Vector a -> Vector b
concatMap f v = create $ \mv ->
  forEach_ v $ \a -> forEach_ (f a) $ \b -> mPush mv b
{-# INLINE concatMap #-}

mapMaybe :: (a -> Maybe b) -> Vector a -> Vector b
mapMaybe f v = mapMaybeDirect f v
{-# NOINLINE [1] mapMaybe #-}

mapMaybeDirect :: (a -> Maybe b) -> Vector a -> Vector b
mapMaybeDirect f v = create $ \mv ->
  forEach_ v $ \a -> case f a of
    Nothing -> pure ()
    Just b  -> mPush mv b
{-# INLINE mapMaybeDirect #-}

imapMaybe :: (Int -> a -> Maybe b) -> Vector a -> Vector b
imapMaybe f v = create $ \mv ->
  forI_ v $ \i a -> case f i a of
    Nothing -> pure ()
    Just b  -> mPush mv b
{-# INLINE imapMaybe #-}

------------------------------------------------------------------------
-- Monadic mapping
------------------------------------------------------------------------

mapM :: Monad m => (a -> m b) -> Vector a -> m (Vector b)
mapM f v = foldlDirect (\acc a -> acc >>= \v' -> f a >>= \b -> pure (snoc v' b)) (pure empty) v
{-# INLINE mapM #-}

mapM_ :: Monad m => (a -> m b) -> Vector a -> m ()
mapM_ f = foldlDirect (\m a -> m >> f a >> pure ()) (pure ())
{-# INLINE mapM_ #-}

forM :: Monad m => Vector a -> (a -> m b) -> m (Vector b)
forM = flip mapM
{-# INLINE forM #-}

forM_ :: Monad m => Vector a -> (a -> m b) -> m ()
forM_ = flip mapM_
{-# INLINE forM_ #-}

imapM :: Monad m => (Int -> a -> m b) -> Vector a -> m (Vector b)
imapM f v = ifoldl' (\acc i a -> acc >>= \v' -> f i a >>= \b -> pure (snoc v' b)) (Prelude.pure empty) v
{-# INLINE imapM #-}

imapM_ :: Monad m => (Int -> a -> m ()) -> Vector a -> m ()
imapM_ f = ifoldl' (\m i a -> m >> f i a) (Prelude.pure ())
{-# INLINE imapM_ #-}

------------------------------------------------------------------------
-- Zipping
------------------------------------------------------------------------

zip :: Vector a -> Vector b -> Vector (a, b)
zip = zipWith (,)
{-# INLINE zip #-}

zip3 :: Vector a -> Vector b -> Vector c -> Vector (a, b, c)
zip3 = zipWith3 (,,)
{-# INLINE zip3 #-}

zipWith :: (a -> b -> c) -> Vector a -> Vector b -> Vector c
zipWith f v1 v2 = zipWithDirect f v1 v2
{-# NOINLINE [1] zipWith #-}

zipWithDirect :: (a -> b -> c) -> Vector a -> Vector b -> Vector c
zipWithDirect f v1 v2 = generate (min (length v1) (length v2)) $ \i ->
  f (unsafeIndex v1 i) (unsafeIndex v2 i)
{-# INLINE zipWithDirect #-}

zipWith3 :: (a -> b -> c -> d) -> Vector a -> Vector b -> Vector c -> Vector d
zipWith3 f v1 v2 v3 = generate (min3 (length v1) (length v2) (length v3)) $ \i ->
  f (unsafeIndex v1 i) (unsafeIndex v2 i) (unsafeIndex v3 i)
  where min3 a b c = min a (min b c)
{-# INLINE zipWith3 #-}

izipWith :: (Int -> a -> b -> c) -> Vector a -> Vector b -> Vector c
izipWith f v1 v2 = generate (min (length v1) (length v2)) $ \i ->
  f i (unsafeIndex v1 i) (unsafeIndex v2 i)
{-# INLINE izipWith #-}

unzip :: Vector (a, b) -> (Vector a, Vector b)
unzip v = (map fst v, map snd v)
{-# INLINE unzip #-}

unzip3 :: Vector (a, b, c) -> (Vector a, Vector b, Vector c)
unzip3 v = (map (\(a,_,_) -> a) v, map (\(_,b,_) -> b) v, map (\(_,_,c) -> c) v)
{-# INLINE unzip3 #-}

------------------------------------------------------------------------
-- Filtering
------------------------------------------------------------------------

filter :: (a -> Bool) -> Vector a -> Vector a
filter p = filterDirect p
{-# NOINLINE [1] filter #-}

filterDirect :: forall a. (a -> Bool) -> Vector a -> Vector a
filterDirect p v
  | vSize v == 0 = empty
  | otherwise = create $ \mv -> do
      filterArr (vPrefix v) 0 (sizeofSmallArray (vPrefix v)) mv
      filterNode (vShift v) (vRoot v) mv
      filterArr (vTail v) 0 (sizeofSmallArray (vTail v)) mv
  where
    filterNode :: Int -> Node a -> MVector s a -> ST s ()
    filterNode !_ Empty !_ = pure ()
    filterNode !_ (Leaf arr) !mv = filterFullChunk arr mv
    filterNode !shift (Internal arr) !mv = do
      let !nc = sizeofSmallArray arr
          go !i | i >= nc   = pure ()
                | otherwise = filterNode (shift - bfBits) (indexSmallArray arr i) mv >> go (i + 1)
      go 0
    filterNode !shift (Relaxed arr _) !mv = do
      let !nc = sizeofSmallArray arr
          go !i | i >= nc   = pure ()
                | otherwise = filterNode (shift - bfBits) (indexSmallArray arr i) mv >> go (i + 1)
      go 0

    filterFullChunk :: SmallArray a -> MVector s a -> ST s ()
    filterFullChunk !arr !mv
      | allPass arr = do
          st <- getMV mv
          if mvTailSize st == 0 && sizeofSmallArray arr == bf
            then mPushChunk mv arr
            else pushAll arr 0 (sizeofSmallArray arr) mv
      | otherwise = filterArr arr 0 (sizeofSmallArray arr) mv

    allPass :: SmallArray a -> Bool
    allPass !arr = go 0
      where
        !n = sizeofSmallArray arr
        go !i | i >= n    = True
              | p (indexSmallArray arr i) = go (i + 1)
              | otherwise = False

    -- Push all elements unconditionally (we already know they all pass)
    pushAll :: SmallArray a -> Int -> Int -> MVector s a -> ST s ()
    pushAll !arr !i !limit !mv
      | i >= limit = pure ()
      | otherwise = mPush mv (indexSmallArray arr i) >> pushAll arr (i + 1) limit mv

    filterArr :: SmallArray a -> Int -> Int -> MVector s a -> ST s ()
    filterArr !arr !i !limit !mv
      | i >= limit = pure ()
      | otherwise = do
          let !a = indexSmallArray arr i
          when (p a) (mPush mv a)
          filterArr arr (i + 1) limit mv
{-# INLINE filterDirect #-}

ifilter :: (Int -> a -> Bool) -> Vector a -> Vector a
ifilter p v = create $ \mv ->
  forI_ v $ \i a -> when (p i a) (mPush mv a)
{-# INLINE ifilter #-}

takeWhile :: (a -> Bool) -> Vector a -> Vector a
takeWhile p v = takeWhileDirect p v
{-# NOINLINE [1] takeWhile #-}

takeWhileDirect :: (a -> Bool) -> Vector a -> Vector a
takeWhileDirect p v = case findIndex (not . p) v of
  Nothing -> v
  Just i  -> takeDirect i v
{-# INLINE takeWhileDirect #-}

dropWhile :: (a -> Bool) -> Vector a -> Vector a
dropWhile p v = dropWhileDirect p v
{-# NOINLINE [1] dropWhile #-}

dropWhileDirect :: (a -> Bool) -> Vector a -> Vector a
dropWhileDirect p v = case findIndex (not . p) v of
  Nothing -> empty
  Just i  -> dropDirect i v
{-# INLINE dropWhileDirect #-}

partition :: (a -> Bool) -> Vector a -> (Vector a, Vector a)
partition p v = (filter p v, filter (not . p) v)
{-# INLINE partition #-}

span :: (a -> Bool) -> Vector a -> (Vector a, Vector a)
span p v = case findIndex (not . p) v of
  Nothing -> (v, empty)
  Just i  -> (take i v, drop i v)
{-# INLINE span #-}

break :: (a -> Bool) -> Vector a -> (Vector a, Vector a)
break p = span (not . p)
{-# INLINE break #-}

------------------------------------------------------------------------
-- Searching
------------------------------------------------------------------------

elem :: Eq a => a -> Vector a -> Bool
elem x = any (== x)
{-# INLINE elem #-}

notElem :: Eq a => a -> Vector a -> Bool
notElem x = all (/= x)
{-# INLINE notElem #-}

find :: (a -> Bool) -> Vector a -> Maybe a
find p v = case findIndex p v of
  Nothing -> Nothing
  Just i  -> Just (unsafeIndex v i)
{-# INLINE find #-}

findIndex :: (a -> Bool) -> Vector a -> Maybe Int
findIndex p v = go 0
  where
    !n = vSize v
    go i
      | i >= n    = Nothing
      | p (unsafeIndex v i) = Just i
      | otherwise = go (i + 1)
{-# INLINE findIndex #-}

findIndices :: (a -> Bool) -> Vector a -> Vector Int
findIndices p v = create $ \mv ->
  forI_ v $ \i a -> when (p a) (mPush mv (i :: Int))
{-# INLINE findIndices #-}

elemIndex :: Eq a => a -> Vector a -> Maybe Int
elemIndex x = findIndex (== x)
{-# INLINE elemIndex #-}

elemIndices :: Eq a => a -> Vector a -> Vector Int
elemIndices x = findIndices (== x)
{-# INLINE elemIndices #-}

------------------------------------------------------------------------
-- Folding
------------------------------------------------------------------------

foldl :: (b -> a -> b) -> b -> Vector a -> b
foldl f z0 v = foldrDirect (\x k z -> k (f z x)) id v z0
{-# INLINE foldl #-}

foldl' :: (b -> a -> b) -> b -> Vector a -> b
foldl' = foldlDirect
{-# INLINE foldl' #-}

foldlDirect :: (b -> a -> b) -> b -> Vector a -> b
foldlDirect f = \ !z0 v ->
  if vSize v == 0
  then z0
  else
    let !z1 = goArr z0 (vPrefix v) 0 (sizeofSmallArray (vPrefix v))
        !z2 = goNode z1 (vShift v) (vRoot v)
        !z3 = goArr z2 (vTail v) 0 (sizeofSmallArray (vTail v))
    in z3
  where
    goNode !z !_ Empty = z
    goNode !z !_ (Leaf arr) = foldlChunk f z arr
    goNode !z !shift (Internal arr) = goChildren z shift arr
    goNode !z !shift (Relaxed arr _) = goChildren z shift arr

    goChildren !z !shift arr =
      let !nc = sizeofSmallArray arr
          go !z' !i
            | i >= nc   = z'
            | otherwise = go (goNode z' (shift - bfBits) (indexSmallArray arr i)) (i + 1)
      in go z 0

    goArr !z !arr !i !limit
      | i >= limit = z
      | otherwise  = goArr (f z (indexSmallArray arr i)) arr (i + 1) limit
{-# INLINE [1] foldlDirect #-}

foldl1 :: (a -> a -> a) -> Vector a -> a
foldl1 f v
  | vSize v == 0 = error "Data.PVector.Back.foldl1: empty"
  | otherwise     = foldlDirect f (unsafeIndex v 0) (dropDirect 1 v)

foldl1' :: (a -> a -> a) -> Vector a -> a
foldl1' f v
  | vSize v == 0 = error "Data.PVector.Back.foldl1': empty"
  | otherwise     = foldlDirect f (unsafeIndex v 0) (dropDirect 1 v)
{-# INLINE foldl1' #-}

foldr :: (a -> b -> b) -> b -> Vector a -> b
foldr = foldrDirect
{-# INLINE foldr #-}

foldrDirect :: (a -> b -> b) -> b -> Vector a -> b
foldrDirect f z0 v
  | vSize v == 0 = z0
  | otherwise =
      let !tailPart = goArr (vTail v) 0 (sizeofSmallArray (vTail v)) z0
          !treePart = goNode (vShift v) (vRoot v) tailPart
          !prefixPart = goArr (vPrefix v) 0 (sizeofSmallArray (vPrefix v)) treePart
      in prefixPart
  where
    goNode !_ Empty rest = rest
    goNode !_ (Leaf arr) rest = foldrChunk f arr rest
    goNode !shift (Internal arr) rest = goChildrenR shift arr rest
    goNode !shift (Relaxed arr _) rest = goChildrenR shift arr rest

    goChildrenR !shift arr rest =
      let !n = sizeofSmallArray arr
          go !i !r
            | i < 0     = r
            | otherwise = go (i - 1) (goNode (shift - bfBits) (indexSmallArray arr i) r)
      in go (n - 1) rest

    goArr !arr !i !limit rest
      | i >= limit = rest
      | otherwise  = f (indexSmallArray arr i) (goArr arr (i + 1) limit rest)
{-# INLINE [1] foldrDirect #-}

foldr' :: (a -> b -> b) -> b -> Vector a -> b
foldr' f z0 v = foldlDirect (\k x -> k . f x) id v z0
{-# INLINE foldr' #-}

foldr1 :: (a -> a -> a) -> Vector a -> a
foldr1 f v
  | vSize v == 0 = error "Data.PVector.Back.foldr1: empty"
  | otherwise     = foldr f (Data.PVector.Back.last v)
                           (Data.PVector.Back.take (vSize v - 1) v)
{-# INLINE foldr1 #-}

foldr1' :: (a -> a -> a) -> Vector a -> a
foldr1' f v
  | vSize v == 0 = error "Data.PVector.Back.foldr1': empty"
  | otherwise     = foldr' f (Data.PVector.Back.last v)
                            (Data.PVector.Back.take (vSize v - 1) v)

ifoldl' :: (b -> Int -> a -> b) -> b -> Vector a -> b
ifoldl' f z0 v
  | n == 0    = z0
  | otherwise = go z0 0
  where
    !n = vSize v
    go !z !i
      | i >= n    = z
      | otherwise = go (f z i (unsafeIndex v i)) (i + 1)
{-# INLINE ifoldl' #-}

ifoldr :: (Int -> a -> b -> b) -> b -> Vector a -> b
ifoldr f z0 v
  | n == 0    = z0
  | otherwise = go 0
  where
    !n = vSize v
    go !i
      | i >= n    = z0
      | otherwise = f i (unsafeIndex v i) (go (i + 1))
{-# INLINE ifoldr #-}

foldMap :: Monoid m => (a -> m) -> Vector a -> m
foldMap f = foldlDirect (\acc a -> acc <> f a) mempty
{-# INLINE foldMap #-}

foldMap' :: Monoid m => (a -> m) -> Vector a -> m
foldMap' = foldMap
{-# INLINE foldMap' #-}

all :: (a -> Bool) -> Vector a -> Bool
all p v = go 0
  where
    !n = vSize v
    go i | i >= n = True
         | otherwise = p (unsafeIndex v i) && go (i + 1)
{-# INLINE all #-}

any :: (a -> Bool) -> Vector a -> Bool
any p v = go 0
  where
    !n = vSize v
    go i | i >= n = False
         | otherwise = p (unsafeIndex v i) || go (i + 1)
{-# INLINE any #-}

and :: Vector Bool -> Bool
and = all id
{-# INLINE and #-}

or :: Vector Bool -> Bool
or = any id
{-# INLINE or #-}

sum :: Num a => Vector a -> a
sum = foldlDirect (+) 0
{-# INLINE sum #-}

product :: Num a => Vector a -> a
product = foldlDirect (*) 1
{-# INLINE product #-}

maximum :: Ord a => Vector a -> a
maximum = foldl1' max
{-# INLINE maximum #-}

minimum :: Ord a => Vector a -> a
minimum = foldl1' min
{-# INLINE minimum #-}

maximumBy :: (a -> a -> Ordering) -> Vector a -> a
maximumBy cmp = foldl1' (\x y -> if cmp x y == GT then x else y)
{-# INLINE maximumBy #-}

minimumBy :: (a -> a -> Ordering) -> Vector a -> a
minimumBy cmp = foldl1' (\x y -> if cmp x y == LT then x else y)
{-# INLINE minimumBy #-}

------------------------------------------------------------------------
-- Scans
------------------------------------------------------------------------

prescanl' :: (a -> b -> a) -> a -> Vector b -> Vector a
prescanl' f z v = create $ \mv -> do
  let !n = vSize v
      go !acc i
        | i >= n    = pure ()
        | otherwise = mPush mv acc >> go (f acc (unsafeIndex v i)) (i + 1)
  go z 0
{-# INLINE prescanl' #-}

scanl :: (a -> b -> a) -> a -> Vector b -> Vector a
scanl f z v = scanl' f z v

scanl' :: (a -> b -> a) -> a -> Vector b -> Vector a
scanl' f z v = create $ \mv -> do
  let !n = vSize v
      go !acc i
        | i > n     = pure ()
        | i == 0    = mPush mv acc >> go acc 1
        | otherwise = let !acc' = f acc (unsafeIndex v (i - 1))
                      in mPush mv acc' >> go acc' (i + 1)
  go z 0
{-# INLINE scanl' #-}

scanl1 :: (a -> a -> a) -> Vector a -> Vector a
scanl1 f v
  | vSize v == 0 = empty
  | otherwise     = scanl f (unsafeIndex v 0) (drop 1 v)

scanl1' :: (a -> a -> a) -> Vector a -> Vector a
scanl1' f v
  | vSize v == 0 = empty
  | otherwise     = scanl' f (unsafeIndex v 0) (drop 1 v)

scanr :: (a -> b -> b) -> b -> Vector a -> Vector b
scanr f z v = generate (vSize v + 1) step
  where
    !n = vSize v
    step i | i == n    = z
           | otherwise = f (unsafeIndex v i) (step (i + 1))
{-# INLINE scanr #-}

scanr' :: (a -> b -> b) -> b -> Vector a -> Vector b
scanr' f z v = create $ \mv -> do
  let !n = vSize v
      go !acc i
        | i < 0     = mPush mv acc
        | otherwise = mPush mv acc >> go (f (unsafeIndex v i) acc) (i - 1)
  go z (n - 1)
  reverseM mv
{-# INLINE scanr' #-}

reverseM :: MVector s a -> ST s ()
reverseM mv = do
  st <- getMV mv
  let !n = mvSize st
      go !lo !hi
        | lo >= hi = pure ()
        | otherwise = do
            a <- mRead mv lo
            b <- mRead mv hi
            mWrite mv lo b
            mWrite mv hi a
            go (lo + 1) (hi - 1)
  go 0 (n - 1)

scanr1 :: (a -> a -> a) -> Vector a -> Vector a
scanr1 f v
  | vSize v == 0 = empty
  | otherwise     = scanr f (Data.PVector.Back.last v) (Data.PVector.Back.init v)

scanr1' :: (a -> a -> a) -> Vector a -> Vector a
scanr1' f v
  | vSize v == 0 = empty
  | otherwise = reverse (scanl1' (flip f) (reverse v))

------------------------------------------------------------------------
-- Enumeration
------------------------------------------------------------------------

enumFromN :: Num a => a -> Int -> Vector a
enumFromN x n = generate n (\i -> x + Prelude.fromIntegral i)
{-# INLINE enumFromN #-}

enumFromStepN :: Num a => a -> a -> Int -> Vector a
enumFromStepN x step n = generate n (\i -> x + step * Prelude.fromIntegral i)
{-# INLINE enumFromStepN #-}

enumFromTo :: (Ord a, Num a) => a -> a -> Vector a
enumFromTo lo hi = unfoldr (\x -> if x > hi then Nothing else Just (x, x + 1)) lo
{-# INLINE enumFromTo #-}

enumFromThenTo :: (Ord a, Num a) => a -> a -> a -> Vector a
enumFromThenTo lo next hi
  | step > 0 = unfoldr (\x -> if x > hi then Nothing else Just (x, x + step)) lo
  | step < 0 = unfoldr (\x -> if x < hi then Nothing else Just (x, x + step)) lo
  | otherwise = empty
  where step = next - lo
{-# INLINE enumFromThenTo #-}

------------------------------------------------------------------------
-- Conversions
------------------------------------------------------------------------

toList :: Vector a -> [a]
toList v = build (\c n -> foldrDirect c n v)
{-# INLINE toList #-}

fromVector :: F.Foldable f => f a -> Vector a
fromVector = F.foldl' snoc empty
{-# INLINE fromVector #-}

-- | O(n). Reverse by mirroring the tree structure: swap prefix↔tail,
-- reverse child order at each internal node, reverse each leaf array.
-- Each array element is visited exactly once (no O(log n) per-element indexing).
reverse :: Vector a -> Vector a
reverse v
  | n <= 1    = v
  | otherwise = Vector n (vShift v) (reverseArr (vTail v)) (reverseNode (vShift v) (vRoot v)) (reverseArr (vPrefix v))
  where
    !n = vSize v
{-# INLINE reverse #-}

reverseArr :: SmallArray a -> SmallArray a
reverseArr arr
  | len <= 1 = arr
  | otherwise = runST $ do
      marr <- newSmallArray len uninit
      let go !i !j
            | i >= len  = pure ()
            | otherwise = writeSmallArray marr i (indexSmallArray arr j) >> go (i + 1) (j - 1)
      go 0 (len - 1)
      unsafeFreezeSmallArray marr
  where
    !len = sizeofSmallArray arr
    uninit = error "pvector: reverseArr"
{-# INLINE reverseArr #-}

reverseNode :: Int -> Node a -> Node a
reverseNode !_ Empty = Empty
reverseNode !_ (Leaf arr) = Leaf (reverseArr arr)
reverseNode !shift (Internal arr) = Internal (reverseChildren shift arr)
reverseNode !shift (Relaxed arr sizes) =
  let !newArr = reverseChildren shift arr
      !nc = sizeofSmallArray arr
      !newSizes = computeSizeTable shift newArr
  in Relaxed newArr newSizes

reverseChildren :: Int -> SmallArray (Node a) -> SmallArray (Node a)
reverseChildren shift arr = runST $ do
  let !nc = sizeofSmallArray arr
  marr <- newSmallArray nc Empty
  let go !i !j
        | i >= nc   = pure ()
        | otherwise = do
            writeSmallArray marr i (reverseNode (shift - bfBits) (indexSmallArray arr j))
            go (i + 1) (j - 1)
  go 0 (nc - 1)
  unsafeFreezeSmallArray marr
{-# INLINE reverseChildren #-}

------------------------------------------------------------------------
-- Chunk-based operations
------------------------------------------------------------------------

foldChunks :: (b -> Int -> SmallArray a -> b) -> b -> Vector a -> b
foldChunks f z0 v
  | n == 0    = z0
  | otherwise =
      let !z1 = if pLen > 0 then f z0 0 (vPrefix v) else z0
          !z2 = foldNodeChunks f z1 pLen (vShift v) (vRoot v)
          !z3 = if tLen > 0 then f z2 (n - tLen) (vTail v) else z2
      in z3
  where
    !n    = vSize v
    !pLen = sizeofSmallArray (vPrefix v)
    !tLen = sizeofSmallArray (vTail v)

    foldNodeChunks :: (b -> Int -> SmallArray a -> b) -> b -> Int -> Int -> Node a -> b
    foldNodeChunks _ z _ _ Empty = z
    foldNodeChunks g z off _ (Leaf arr) = g z off arr
    foldNodeChunks g z off shift (Internal arr) = goC z off 0
      where
        !nc = sizeofSmallArray arr
        goC !z' !o !i
          | i >= nc   = z'
          | otherwise =
              let !child = indexSmallArray arr i
                  !sz = nodeSize (shift - bfBits) child
                  !z'' = foldNodeChunks g z' o (shift - bfBits) child
              in goC z'' (o + sz) (i + 1)
    foldNodeChunks g z off shift (Relaxed arr _) = goC z off 0
      where
        !nc = sizeofSmallArray arr
        goC !z' !o !i
          | i >= nc   = z'
          | otherwise =
              let !child = indexSmallArray arr i
                  !sz = nodeSize (shift - bfBits) child
                  !z'' = foldNodeChunks g z' o (shift - bfBits) child
              in goC z'' (o + sz) (i + 1)
{-# INLINE foldChunks #-}

foldChunks' :: (b -> Int -> SmallArray a -> b) -> b -> Vector a -> b
foldChunks' = foldChunks
{-# INLINE foldChunks' #-}

mapChunks :: (Int -> SmallArray a -> SmallArray b) -> Vector a -> Vector b
mapChunks f v
  | n == 0    = empty
  | otherwise = Vector n (vShift v) newPrefix newRoot newTail
  where
    !n = vSize v
    !pLen = sizeofSmallArray (vPrefix v)
    !tLen = sizeofSmallArray (vTail v)
    !newPrefix = if pLen > 0 then f 0 (vPrefix v) else emptyTail
    !newTail = if tLen > 0 then f (n - tLen) (vTail v) else emptyTail
    !newRoot = mapChunksNode (vShift v) pLen f (vRoot v)

    mapChunksNode :: Int -> Int -> (Int -> SmallArray a -> SmallArray b) -> Node a -> Node b
    mapChunksNode _ _ _ Empty = Empty
    mapChunksNode _ off g (Leaf arr) = Leaf (g off arr)
    mapChunksNode shift off g (Internal arr) = Internal $ runST $ do
      let !nc = sizeofSmallArray arr
      marr <- newSmallArray nc Empty
      let go !i !o
            | i >= nc   = pure ()
            | otherwise = do
                let !child = indexSmallArray arr i
                    !sz = nodeSize (shift - bfBits) child
                writeSmallArray marr i (mapChunksNode (shift - bfBits) o g child)
                go (i + 1) (o + sz)
      go 0 off
      unsafeFreezeSmallArray marr
    mapChunksNode shift off g (Relaxed arr sizes) = Relaxed newArr sizes
      where
        !newArr = runST $ do
          let !nc = sizeofSmallArray arr
          marr <- newSmallArray nc Empty
          let go !i !o
                | i >= nc   = pure ()
                | otherwise = do
                    let !child = indexSmallArray arr i
                        !sz = nodeSize (shift - bfBits) child
                    writeSmallArray marr i (mapChunksNode (shift - bfBits) o g child)
                    go (i + 1) (o + sz)
          go 0 off
          unsafeFreezeSmallArray marr
{-# INLINE mapChunks #-}

forChunks_ :: Monad m => Vector a -> (Int -> SmallArray a -> m ()) -> m ()
forChunks_ v f
  | n == 0    = pure ()
  | otherwise = do
      when (pLen > 0) $ f 0 (vPrefix v)
      goNode pLen (vShift v) (vRoot v)
      when (tLen > 0) $ f (n - tLen) (vTail v)
  where
    !n    = vSize v
    !pLen = sizeofSmallArray (vPrefix v)
    !tLen = sizeofSmallArray (vTail v)
    goNode _ _ Empty = pure ()
    goNode off _ (Leaf arr) = f off arr
    goNode off shift (Internal arr) = goC off 0
      where
        !nc = sizeofSmallArray arr
        goC !o !i
          | i >= nc   = pure ()
          | otherwise = do
              let !child = indexSmallArray arr i
                  !sz = nodeSize (shift - bfBits) child
              goNode o (shift - bfBits) child
              goC (o + sz) (i + 1)
    goNode off shift (Relaxed arr _) = goC off 0
      where
        !nc = sizeofSmallArray arr
        goC !o !i
          | i >= nc   = pure ()
          | otherwise = do
              let !child = indexSmallArray arr i
                  !sz = nodeSize (shift - bfBits) child
              goNode o (shift - bfBits) child
              goC (o + sz) (i + 1)
{-# INLINE forChunks_ #-}

------------------------------------------------------------------------
-- Internal helpers
------------------------------------------------------------------------

forI_ :: Vector a -> (Int -> a -> ST s ()) -> ST s ()
forI_ v f
  | n == 0    = pure ()
  | otherwise = do
      let go !i
            | i >= n    = pure ()
            | otherwise = f i (unsafeIndex v i) >> go (i + 1)
      go 0
  where !n = vSize v
{-# INLINE forI_ #-}

forEach_ :: Vector a -> (a -> ST s ()) -> ST s ()
forEach_ v f
  | vSize v == 0 = pure ()
  | otherwise = do
      goArr (vPrefix v) 0 (sizeofSmallArray (vPrefix v))
      goNode (vShift v) (vRoot v)
      goArr (vTail v) 0 (sizeofSmallArray (vTail v))
  where
    goNode !_ Empty = pure ()
    goNode !_ (Leaf arr) = goArr arr 0 (sizeofSmallArray arr)
    goNode !shift (Internal arr) = goC shift arr 0 (sizeofSmallArray arr)
    goNode !shift (Relaxed arr _) = goC shift arr 0 (sizeofSmallArray arr)

    goC !shift !arr !i !nc
      | i >= nc   = pure ()
      | otherwise = goNode (shift - bfBits) (indexSmallArray arr i) >> goC shift arr (i + 1) nc

    goArr !arr !i !limit
      | i >= limit = pure ()
      | otherwise = f (indexSmallArray arr i) >> goArr arr (i + 1) limit
{-# INLINE forEach_ #-}

------------------------------------------------------------------------
-- RRB concatenation (O(log n) append)
------------------------------------------------------------------------

append :: Vector a -> Vector a -> Vector a
append v1 v2
  | null v1   = v2
  | null v2   = v1
  | otherwise = rrbConcat v1 v2
{-# INLINE append #-}

-- | RRB-tree concatenation. O(log n).
-- Flushes prefix/tail into their respective trees, concatenates the trees,
-- then wraps with the left's prefix and right's tail.
rrbConcat :: Vector a -> Vector a -> Vector a
rrbConcat v1 v2 =
  let !resultPrefix = vPrefix v1
      !resultTail   = vTail v2
      !resultSize   = vSize v1 + vSize v2
      -- Build left tree: v1's tree with v1's tail flushed in
      (!ls, !lr) = flushTailIntoTree (vShift v1) (vRoot v1) (vTail v1)
      -- Build right tree: v2's tree with v2's prefix flushed in
      (!rs, !rr) = flushPrefixIntoTree (vShift v2) (vRoot v2) (vPrefix v2)
      -- Concatenate the two trees
      (!cs, !cr) = concatTrees ls lr rs rr
  in Vector resultSize cs resultPrefix cr resultTail

-- | Flush a tail (suffix) into the tree as a new rightmost leaf.
flushTailIntoTree :: Int -> Node a -> SmallArray a -> (Int, Node a)
flushTailIntoTree shift root tailArr
  | sizeofSmallArray tailArr == 0 = (shift, root)
  | isEmpty root = (bfBits, Leaf tailArr)
  | otherwise =
      let !newTreeN = nodeSize shift root + sizeofSmallArray tailArr
          !willOverflow = unsafeShiftR newTreeN bfBits > unsafeShiftL 1 shift
      in if willOverflow
         then let !newArr = runST $ do
                    a <- newSmallArray 2 Empty
                    writeSmallArray a 0 root
                    writeSmallArray a 1 (newPath shift (Leaf tailArr))
                    unsafeFreezeSmallArray a
              in (shift + bfBits, Internal newArr)
         else (shift, pushTail newTreeN shift root tailArr)
  where
    isEmpty Empty = True
    isEmpty _ = False

-- | Flush a prefix into the tree as a new leftmost leaf.
flushPrefixIntoTree :: Int -> Node a -> SmallArray a -> (Int, Node a)
flushPrefixIntoTree shift root prefixArr
  | sizeofSmallArray prefixArr == 0 = (shift, root)
  | isEmpty root = (bfBits, Leaf prefixArr)
  | otherwise =
      let !prefixLeaf = Leaf prefixArr
      in consLeafToTree shift prefixLeaf root
  where
    isEmpty Empty = True
    isEmpty _ = False

-- | Concatenate two trees. Returns (shift, root).
concatTrees :: Int -> Node a -> Int -> Node a -> (Int, Node a)
concatTrees !s1 t1 !s2 t2
  | isEmpty t1 && isEmpty t2 = (bfBits, Empty)
  | isEmpty t1 = (s2, t2)
  | isEmpty t2 = (s1, t1)
  | otherwise  = rrbMerge s1 t1 s2 t2
  where
    isEmpty Empty = True
    isEmpty _ = False

-- | Core RRB merge algorithm.
-- Merges two trees by walking their spines and redistributing children.
rrbMerge :: Int -> Node a -> Int -> Node a -> (Int, Node a)
rrbMerge !s1 t1 !s2 t2
  -- Both are leaves: combine into a single internal node
  | s1 == bfBits && s2 == bfBits =
      case (t1, t2) of
        (Leaf _, Leaf _) ->
          let !arr = runST $ do
                a <- newSmallArray 2 Empty
                writeSmallArray a 0 t1
                writeSmallArray a 1 t2
                unsafeFreezeSmallArray a
          in (bfBits + bfBits, Internal arr)
        _ -> fallbackMerge s1 t1 s2 t2

  -- Equalize heights by lifting the shorter tree
  | s1 < s2 =
      let !lifted = liftToShift s2 s1 t1
      in rrbMerge s2 lifted s2 t2
  | s1 > s2 =
      let !lifted = liftToShift s1 s2 t2
      in rrbMerge s1 t1 s1 lifted

  -- Same height: merge the rightmost children of t1 with leftmost children of t2
  | otherwise = mergeAtLevel s1 t1 t2

-- | Merge two trees at the same shift level.
mergeAtLevel :: Int -> Node a -> Node a -> (Int, Node a)
mergeAtLevel !shift t1 t2 =
  let !c1 = getChildren t1
      !c2 = getChildren t2
      !nc1 = sizeofSmallArray c1
      !nc2 = sizeofSmallArray c2
  in if nc1 + nc2 <= bf
     then
       -- All children fit in a single node
       let !merged = runST $ do
             marr <- newSmallArray (nc1 + nc2) Empty
             copySmallArray marr 0 c1 0 nc1
             copySmallArray marr nc2 c2 0 nc2
             unsafeFreezeSmallArray marr
           -- Corrected: copy c2 starting at nc1, not nc2
           !merged' = runST $ do
             marr <- newSmallArray (nc1 + nc2) Empty
             copySmallArray marr 0 c1 0 nc1
             copySmallArray marr nc1 c2 0 nc2
             unsafeFreezeSmallArray marr
           !sizes = computeSizeTable shift merged'
       in (shift, mkRelaxed merged' sizes)
     else
       -- Need a new root level above
       let !newArr = runST $ do
             marr <- newSmallArray 2 Empty
             writeSmallArray marr 0 (makeRelaxedNode shift c1)
             writeSmallArray marr 1 (makeRelaxedNode shift c2)
             unsafeFreezeSmallArray marr
           !sizes = computeSizeTable (shift + bfBits) newArr
       in (shift + bfBits, mkRelaxed newArr sizes)

makeRelaxedNode :: Int -> SmallArray (Node a) -> Node a
makeRelaxedNode shift children =
  let !sizes = computeSizeTable shift children
  in mkRelaxed children sizes

getChildren :: Node a -> SmallArray (Node a)
getChildren (Internal arr) = arr
getChildren (Relaxed arr _) = arr
getChildren n = runST $ do
  marr <- newSmallArray 1 n
  unsafeFreezeSmallArray marr

fallbackMerge :: Int -> Node a -> Int -> Node a -> (Int, Node a)
fallbackMerge s1 t1 s2 t2 =
  let !maxShift = max s1 s2
      !arr = runST $ do
        a <- newSmallArray 2 Empty
        writeSmallArray a 0 (liftToShift maxShift s1 t1)
        writeSmallArray a 1 (liftToShift maxShift s2 t2)
        unsafeFreezeSmallArray a
      !sizes = computeSizeTable (maxShift + bfBits) arr
  in (maxShift + bfBits, mkRelaxed arr sizes)

------------------------------------------------------------------------
-- RRB split (O(log n) take/drop/slice)
------------------------------------------------------------------------

-- | O(log n) slice via RRB split. Extracts elements [start, start+len).
rrbSlice :: Int -> Int -> Vector a -> Vector a
rrbSlice start len v
  | len <= 0  = empty
  | len >= n  = v
  | len <= bf = -- Small result: just generate element by element
      generate len (\i -> unsafeIndex v (start + i))
  | otherwise =
      -- Split at the boundaries, collecting prefix, tree, and tail
      let !pLen = sizeofSmallArray (vPrefix v)
          !tLen = sizeofSmallArray (vTail v)
          !endExcl = start + len

          -- Compute what parts of prefix/tree/tail the slice covers
          !pEnd = pLen
          !tStart = n - tLen

          -- Get prefix portion
          !newPrefixStart = start
          !newPrefixEnd   = min endExcl pEnd
          !newPrefix
            | newPrefixStart < pEnd && newPrefixEnd > newPrefixStart =
                cloneSmallArray (vPrefix v) newPrefixStart (newPrefixEnd - newPrefixStart)
            | otherwise = emptyTail

          -- Get tail portion
          !newTailStart = max start tStart
          !newTailEnd   = endExcl
          !newTail
            | newTailEnd > tStart && newTailStart < newTailEnd =
                let !off = newTailStart - tStart
                    !cnt = min (newTailEnd - newTailStart) tLen
                in cloneSmallArray (vTail v) off cnt
            | otherwise = emptyTail

          -- Get tree portion
          !treeStart = max 0 (start - pLen)
          !treeEnd   = min (n - pLen - tLen) (endExcl - pLen)
      in if treeEnd <= treeStart
         then Vector len bfBits newPrefix Empty newTail
         else
           let (!newShift, !newRoot) = sliceTree (vShift v) treeStart treeEnd (vRoot v)
           in Vector len newShift newPrefix newRoot newTail
  where
    !n = vSize v

-- | Slice a tree to contain only elements in [start, end).
sliceTree :: Int -> Int -> Int -> Node a -> (Int, Node a)
sliceTree !shift !start !end node
  | start >= end = (bfBits, Empty)
  | start == 0 && end == nodeSize shift node = (shift, node)
  | otherwise = case node of
      Leaf arr ->
        (bfBits, Leaf (cloneSmallArray arr start (end - start)))
      Internal arr ->
        sliceInternal shift arr start end
      Relaxed arr sizes ->
        sliceRelaxed shift arr sizes start end
      Empty -> (bfBits, Empty)

sliceInternal :: Int -> SmallArray (Node a) -> Int -> Int -> (Int, Node a)
sliceInternal !shift arr !start !end =
  let !childSz = unsafeShiftL 1 shift
      !firstChild = start `div` childSz
      !lastChild = (end - 1) `div` childSz
  in if firstChild == lastChild
     then
       let !localStart = start - firstChild * childSz
           !localEnd   = end - firstChild * childSz
       in sliceTree (shift - bfBits) localStart localEnd (indexSmallArray arr firstChild)
     else
       let !nch = lastChild - firstChild + 1
           !newArr = runST $ do
             marr <- newSmallArray nch Empty
             let go !i
                   | i >= nch = pure ()
                   | otherwise = do
                       let !srcIdx = firstChild + i
                           !child = indexSmallArray arr srcIdx
                           !childStart = srcIdx * childSz
                           !localStart = max 0 (start - childStart)
                           !localEnd = min childSz (end - childStart)
                       if localStart == 0 && localEnd == childSz
                         then writeSmallArray marr i child
                         else do
                           let (_, sliced) = sliceTree (shift - bfBits) localStart localEnd child
                           writeSmallArray marr i sliced
                       go (i + 1)
             go 0
             unsafeFreezeSmallArray marr
           !sizes = computeSizeTable shift newArr
       in (shift, mkRelaxed newArr sizes)

sliceRelaxed :: Int -> SmallArray (Node a) -> PrimArray Int -> Int -> Int -> (Int, Node a)
sliceRelaxed !shift arr sizes !start !end =
  let !firstChild = relaxedIndex sizes start
      !lastChild  = relaxedIndex sizes (end - 1)
      firstOff = if firstChild == 0 then 0 else indexPrimArray sizes (firstChild - 1)
      lastOff  = if lastChild == 0 then 0 else indexPrimArray sizes (lastChild - 1)
  in if firstChild == lastChild
     then sliceTree (shift - bfBits) (start - firstOff) (end - firstOff) (indexSmallArray arr firstChild)
     else
       let !nch = lastChild - firstChild + 1
           !newArr = runST $ do
             marr <- newSmallArray nch Empty
             let go !i
                   | i >= nch = pure ()
                   | otherwise = do
                       let !srcIdx = firstChild + i
                           !child = indexSmallArray arr srcIdx
                           !childOff = if srcIdx == 0 then 0 else indexPrimArray sizes (srcIdx - 1)
                           !childSz = indexPrimArray sizes srcIdx - childOff
                           !localStart = max 0 (start - childOff)
                           !localEnd = min childSz (end - childOff)
                       if localStart == 0 && localEnd == childSz
                         then writeSmallArray marr i child
                         else do
                           let (_, sliced) = sliceTree (shift - bfBits) localStart localEnd child
                           writeSmallArray marr i sliced
                       go (i + 1)
             go 0
             unsafeFreezeSmallArray marr
           !sizes' = computeSizeTable shift newArr
       in (shift, mkRelaxed newArr sizes')

------------------------------------------------------------------------
-- Cons leaf into tree (for prefix flush)
------------------------------------------------------------------------

-- | Insert a leaf as the new leftmost child of the tree.
-- Returns (newShift, newRoot). May create relaxed nodes.
consLeafToTree :: Int -> Node a -> Node a -> (Int, Node a)
consLeafToTree !shift leaf root = case root of
  Empty -> (bfBits, leaf)
  Leaf _ ->
    -- Root is a single leaf; combine into an Internal node at shift bfBits
    let !arr = runST $ do
          a <- newSmallArray 2 Empty
          writeSmallArray a 0 leaf
          writeSmallArray a 1 root
          unsafeFreezeSmallArray a
        !sizes = computeSizeTable bfBits arr
    in (bfBits, mkRelaxed arr sizes)
  Internal arr ->
    let !nc = sizeofSmallArray arr
    in if shift == bfBits
       then -- At the bottom internal level: try to insert leaf here
         if nc < bf
           then let !newArr = consSmallArray leaf arr
                    !sizes = computeSizeTable shift newArr
                in (shift, mkRelaxed newArr sizes)
           else -- Full: create new root
             let !newArr = runST $ do
                   a <- newSmallArray 2 Empty
                   writeSmallArray a 0 (liftToShift shift bfBits leaf)
                   writeSmallArray a 1 root
                   unsafeFreezeSmallArray a
                 !sizes = computeSizeTable (shift + bfBits) newArr
             in (shift + bfBits, mkRelaxed newArr sizes)
       else -- Recurse into leftmost child
         let !leftChild = indexSmallArray arr 0
             (!childShift, !newChild) = consLeafToTree (shift - bfBits) leaf leftChild
         in if childShift > shift - bfBits
            then -- Child grew in height, need new root above
              let !liftedRoot = liftToShift (childShift + bfBits) shift root
                  !liftedChild = liftToShift (childShift + bfBits) childShift newChild
                  !newArr = runST $ do
                    a <- newSmallArray 2 Empty
                    writeSmallArray a 0 liftedChild
                    writeSmallArray a 1 liftedRoot
                    unsafeFreezeSmallArray a
                  !sizes = computeSizeTable (childShift + bfBits + bfBits) newArr
              in (childShift + bfBits + bfBits, mkRelaxed newArr sizes)
            else
              let !newArr = cloneAndSet arr 0 newChild
                  !sizes = computeSizeTable shift newArr
              in (shift, mkRelaxed newArr sizes)
  Relaxed arr _ ->
    let !nc = sizeofSmallArray arr
    in if shift == bfBits
       then if nc < bf
            then let !newArr = consSmallArray leaf arr
                     !sizes = computeSizeTable shift newArr
                 in (shift, mkRelaxed newArr sizes)
            else let !newArr = runST $ do
                       a <- newSmallArray 2 Empty
                       writeSmallArray a 0 (liftToShift shift bfBits leaf)
                       writeSmallArray a 1 root
                       unsafeFreezeSmallArray a
                     !sizes = computeSizeTable (shift + bfBits) newArr
                 in (shift + bfBits, mkRelaxed newArr sizes)
       else let !leftChild = indexSmallArray arr 0
                (!childShift, !newChild) = consLeafToTree (shift - bfBits) leaf leftChild
            in if childShift > shift - bfBits
               then let !liftedRoot = liftToShift (childShift + bfBits) shift root
                        !liftedChild = liftToShift (childShift + bfBits) childShift newChild
                        !newArr = runST $ do
                          a <- newSmallArray 2 Empty
                          writeSmallArray a 0 liftedChild
                          writeSmallArray a 1 liftedRoot
                          unsafeFreezeSmallArray a
                        !sizes = computeSizeTable (childShift + bfBits + bfBits) newArr
                    in (childShift + bfBits + bfBits, mkRelaxed newArr sizes)
               else let !newArr = cloneAndSet arr 0 newChild
                        !sizes = computeSizeTable shift newArr
                    in (shift, mkRelaxed newArr sizes)

consSmallArray :: a -> SmallArray a -> SmallArray a
consSmallArray x arr = runST $ do
  let !n = sizeofSmallArray arr
  marr <- newSmallArray (n + 1) x
  copySmallArray marr 1 arr 0 n
  unsafeFreezeSmallArray marr
{-# INLINE consSmallArray #-}

------------------------------------------------------------------------
-- Transient (mutable) operations
------------------------------------------------------------------------

create :: (forall s. MVector s a -> ST s ()) -> Vector a
create act = runST $ do
  mv <- mNew
  act mv
  unsafeFreeze mv
{-# INLINE create #-}

modify :: (forall s. MVector s a -> ST s ()) -> Vector a -> Vector a
modify act v = runST $ do
  mv <- thaw v
  act mv
  unsafeFreeze mv
{-# INLINE modify #-}

thaw :: PrimMonad m => Vector a -> m (MVector (PrimState m) a)
thaw v = do
  mtail <- newSmallArray bf undefinedElem'
  copySmallArray mtail 0 (vTail v) 0 tLen
  ref <- newMutVar $! MVState
    { mvSize     = vSize v - pLen
    , mvShift    = vShift v
    , mvRoot     = thawNode (vRoot v)
    , mvTail     = mtail
    , mvTailSize = tLen
    }
  pure (MVector ref)
  where
    !pLen = sizeofSmallArray (vPrefix v)
    !tLen = sizeofSmallArray (vTail v)
    undefinedElem' = error "pvector: uninitialised element"
{-# INLINE thaw #-}

freeze :: PrimMonad m => MVector (PrimState m) a -> m (Vector a)
freeze = unsafeFreeze
{-# INLINE freeze #-}

unsafeFreeze :: PrimMonad m => MVector (PrimState m) a -> m (Vector a)
unsafeFreeze mv = do
  st <- getMV mv
  frozenRoot0 <- freezeNode (mvRoot st)
  frozenTail <- unsafeFreezeSmallArray (mvTail st)
  let !tail_
        | sizeofSmallArray frozenTail > mvTailSize st
          = cloneSmallArray frozenTail 0 (mvTailSize st)
        | otherwise = frozenTail
      !frozenRoot = frozenRoot0
  pure $! Vector (mvSize st) (mvShift st) emptyTail frozenRoot tail_
{-# INLINE unsafeFreeze #-}

mNew :: PrimMonad m => m (MVector (PrimState m) a)
mNew = do
  mtail <- newSmallArray bf undefinedElem'
  ref <- newMutVar $! MVState 0 bfBits (Frozen Empty) mtail 0
  pure (MVector ref)
  where
    undefinedElem' = error "pvector: uninitialised element"
{-# INLINE mNew #-}

mLength :: PrimMonad m => MVector (PrimState m) a -> m Int
mLength mv = mvSize <$> getMV mv
{-# INLINE mLength #-}

mRead :: PrimMonad m => MVector (PrimState m) a -> Int -> m a
mRead mv i = do
  st <- getMV mv
  let !n = mvSize st
      !tLen = mvTailSize st
      !tailStart = n - tLen
  if i >= tailStart
    then readSmallArray (mvTail st) (i - tailStart)
    else mReadTree (mvShift st) i (mvRoot st)
{-# INLINE mRead #-}

mReadTree :: PrimMonad m => Int -> Int -> MNode (PrimState m) a -> m a
mReadTree shift i = go shift
  where
    go level node = case node of
      MInternal arr -> do
        let !n = sizeofSmallMutableArray arr
            !idx = indexAtLevel i level
        if idx < n
          then readSmallArray arr idx >>= go (level - bfBits)
          else error "pvector: mReadTree index out of bounds"
      MLeaf arr     -> readSmallArray arr (i .&. bfMask)
      Frozen n      -> pure $! readFrozen level n
    readFrozen level (Internal arr) =
      readFrozen (level - bfBits) (indexSmallArray arr (indexAtLevel i level))
    readFrozen level (Relaxed arr sizes) =
      let !idx = relaxedIndex sizes i
          !off = if idx == 0 then 0 else indexPrimArray sizes (idx - 1)
      in readFrozen (level - bfBits) (indexSmallArray arr idx)
    readFrozen _ (Leaf arr) = indexSmallArray arr (i .&. bfMask)
    readFrozen _ _ = error "pvector: mReadTree invariant violation"

mWrite :: PrimMonad m => MVector (PrimState m) a -> Int -> a -> m ()
mWrite mv i x = do
  st <- getMV mv
  let !n = mvSize st
      !tLen = mvTailSize st
      !tailStart = n - tLen
  if i >= tailStart
    then writeSmallArray (mvTail st) (i - tailStart) x
    else do
      newRoot <- mWriteTree (mvShift st) i x (mvRoot st)
      putMV mv st { mvRoot = newRoot }
{-# INLINE mWrite #-}

mWriteTree :: PrimMonad m => Int -> Int -> a -> MNode (PrimState m) a -> m (MNode (PrimState m) a)
mWriteTree shift i x = go shift
  where
    go level node
      | level == 0 = do
          arr <- editableLeaf node
          writeSmallArray arr (i .&. bfMask) x
          pure (MLeaf arr)
      | otherwise = do
          arr <- editableInternal node
          let !subIdx = indexAtLevel i level
          child <- readSmallArray arr subIdx
          child' <- go (level - bfBits) child
          writeSmallArray arr subIdx child'
          pure (MInternal arr)

mPush :: PrimMonad m => MVector (PrimState m) a -> a -> m ()
mPush mv x = do
  st <- getMV mv
  let !n  = mvSize st
      !ts = mvTailSize st
  if ts < bf
    then do
      writeSmallArray (mvTail st) ts x
      putMV mv st { mvSize = n + 1, mvTailSize = ts + 1 }
    else do
      frozenTail <- unsafeFreezeSmallArray (mvTail st)
      let !treeN = n - ts
          !willOverflow = treeN > 0 && unsafeShiftR (treeN + bf) bfBits > unsafeShiftL 1 (mvShift st)
      (newRoot, newShift) <-
        if willOverflow
          then do
            arr <- newSmallArray 2 (Frozen Empty)
            writeSmallArray arr 0 (mvRoot st)
            let !path = thawNode (newPath (mvShift st) (Leaf frozenTail))
            writeSmallArray arr 1 path
            pure (MInternal arr, mvShift st + bfBits)
          else do
            r <- mPushTail (treeN + bf) (mvShift st) (mvRoot st) frozenTail
            pure (r, mvShift st)
      newTail <- newSmallArray bf undefinedElem'
      writeSmallArray newTail 0 x
      putMV mv $! MVState (n + 1) newShift newRoot newTail 1
  where
    undefinedElem' = error "pvector: uninitialised element"
{-# INLINE mPush #-}

mPushChunk :: PrimMonad m => MVector (PrimState m) a -> SmallArray a -> m ()
mPushChunk mv chunk = do
  st <- getMV mv
  let !n  = mvSize st
      !ts = mvTailSize st
      !treeN = n - ts
      !n' = n + bf - ts
      !willOverflow = treeN > 0 && unsafeShiftR (treeN + bf) bfBits > unsafeShiftL 1 (mvShift st)
  -- First flush current tail if non-empty
  if ts > 0
    then do
      frozenTail <- unsafeFreezeSmallArray (mvTail st)
      let !actualTail = cloneSmallArray frozenTail 0 ts
      -- Push current tail as partial chunk, then push the new full chunk
      -- This is simpler: just push elements one by one for the tail
      -- Actually, let's just handle the common case: ts == 0
      -- For non-zero ts, flush element by element
      let pushRem !i
            | i >= ts = pure ()
            | otherwise = mPush mv (indexSmallArray frozenTail i) >> pushRem (i + 1)
      pushRem 0
      -- Now push the chunk
      let pushChunkElems !i
            | i >= bf = pure ()
            | otherwise = mPush mv (indexSmallArray chunk i) >> pushChunkElems (i + 1)
      pushChunkElems 0
    else do
      -- tail is empty, push chunk directly
      frozenTail <- unsafeFreezeSmallArray (mvTail st)
      let !willOF = treeN > 0 && unsafeShiftR (treeN + bf) bfBits > unsafeShiftL 1 (mvShift st)
      (newRoot, newShift) <-
        if willOF
          then do
            arr <- newSmallArray 2 (Frozen Empty)
            writeSmallArray arr 0 (mvRoot st)
            let !path = thawNode (newPath (mvShift st) (Leaf chunk))
            writeSmallArray arr 1 path
            pure (MInternal arr, mvShift st + bfBits)
          else if isEmpty (mvRoot st)
            then pure (Frozen (Leaf chunk), bfBits)
            else do
              r <- mPushTail (treeN + bf) (mvShift st) (mvRoot st) chunk
              pure (r, mvShift st)
      newTail <- newSmallArray bf undefinedElem'
      putMV mv $! MVState (n + bf) newShift newRoot newTail 0
  where
    undefinedElem' = error "pvector: uninitialised element"
    isEmpty (Frozen Empty) = True
    isEmpty _ = False
{-# INLINE mPushChunk #-}

mPushTail
  :: PrimMonad m
  => Int -> Int -> MNode (PrimState m) a -> SmallArray a
  -> m (MNode (PrimState m) a)
mPushTail size shift root tailArr = case root of
  Frozen (Leaf _) -> do
    -- Root is a single leaf; wrap it into an Internal node with the new tail
    arr <- newSmallArray 2 (Frozen Empty)
    writeSmallArray arr 0 root
    writeSmallArray arr 1 (Frozen (Leaf tailArr))
    pure (MInternal arr)
  _ -> go shift root
  where
    go level node = do
      arr <- editableInternal node
      let !n = sizeofSmallMutableArray arr
          !subIdx = indexAtLevel (size - 1) level
      if level == bfBits
        then do
          if subIdx < n
            then do
              writeSmallArray arr subIdx (Frozen (Leaf tailArr))
              pure (MInternal arr)
            else do
              newArr <- newSmallArray (subIdx + 1) (Frozen Empty)
              copySmallMutableArray newArr 0 arr 0 n
              writeSmallArray newArr subIdx (Frozen (Leaf tailArr))
              pure (MInternal newArr)
        else do
          if subIdx < n
            then do
              child <- readSmallArray arr subIdx
              case child of
                Frozen Empty -> do
                  let !path = thawNode (newPath (level - bfBits) (Leaf tailArr))
                  writeSmallArray arr subIdx path
                  pure (MInternal arr)
                _ -> do
                  child' <- go (level - bfBits) child
                  writeSmallArray arr subIdx child'
                  pure (MInternal arr)
            else do
              newArr <- newSmallArray (subIdx + 1) (Frozen Empty)
              copySmallMutableArray newArr 0 arr 0 n
              let !path = thawNode (newPath (level - bfBits) (Leaf tailArr))
              writeSmallArray newArr subIdx path
              pure (MInternal newArr)

mPop :: PrimMonad m => MVector (PrimState m) a -> m (Maybe a)
mPop mv = do
  st <- getMV mv
  let !n = mvSize st
      !ts = mvTailSize st
  if n == 0
    then pure Nothing
    else if ts > 1
      then do
        let !newTs = ts - 1
        x <- readSmallArray (mvTail st) newTs
        putMV mv st { mvSize = n - 1, mvTailSize = newTs }
        pure (Just x)
      else do
        x <- readSmallArray (mvTail st) 0
        v <- unsafeFreeze mv
        case unsnoc v of
          Nothing -> pure Nothing
          Just (v', _) -> do
            mv' <- thaw v'
            st' <- getMV mv'
            putMV mv st'
            pure (Just x)
{-# INLINE mPop #-}

------------------------------------------------------------------------
-- Stream fusion
------------------------------------------------------------------------

stream :: Vector a -> Bundle a
stream v = Bundle (MStream step (SInit 0)) (Exact n)
  where
    !n = vSize v

    step (SInit i)
      | i >= n    = Id Done
      | otherwise = Id (Yield (unsafeIndex v 0) (SSeq 1))
    step (SSeq i)
      | i >= n    = Id Done
      | otherwise = Id (Yield (unsafeIndex v i) (SSeq (i + 1)))
{-# INLINE [0] stream #-}

data SState
  = SInit  {-# UNPACK #-} !Int
  | SSeq   {-# UNPACK #-} !Int

unstream :: Bundle a -> Vector a
unstream = new . fill
{-# INLINE [0] unstream #-}

------------------------------------------------------------------------
-- Chunk-level streaming (ChunkedBundle integration)
------------------------------------------------------------------------

-- | Convert a Vector to a ChunkedBundle, yielding prefix, tree leaves,
-- and tail as contiguous chunks. Preserves chunk boundaries for
-- cache-friendly iteration and pointer-equality-based comparison.
cstream :: Vector a -> ChunkedBundle a
cstream v = CB.cbFromChunks (collectChunks v)
{-# INLINE [0] cstream #-}

-- | Collect all chunks from a Vector in order: prefix, tree leaves, tail.
collectChunks :: Vector a -> [Chunk a]
collectChunks v = prefix (treeChunks (vShift v) (vRoot v) (suffix []))
  where
    !pArr = vPrefix v
    !tArr = vTail v
    prefix rest
      | sizeofSmallArray pArr > 0 = Chunk pArr 0 (sizeofSmallArray pArr) : rest
      | otherwise = rest
    suffix rest
      | sizeofSmallArray tArr > 0 = Chunk tArr 0 (sizeofSmallArray tArr) : rest
      | otherwise = rest

    treeChunks :: Int -> Node a -> [Chunk a] -> [Chunk a]
    treeChunks _ Empty rest = rest
    treeChunks _ (Leaf arr) rest = Chunk arr 0 (sizeofSmallArray arr) : rest
    treeChunks sh (Internal arr) rest = childChunks sh arr 0 rest
    treeChunks sh (Relaxed arr _) rest = childChunks sh arr 0 rest

    childChunks :: Int -> SmallArray (Node a) -> Int -> [Chunk a] -> [Chunk a]
    childChunks sh arr i rest
      | i >= sizeofSmallArray arr = rest
      | otherwise = treeChunks (sh - bfBits) (indexSmallArray arr i)
                                (childChunks sh arr (i + 1) rest)

-- | Rebuild a Vector from a ChunkedBundle.  Full-width chunks that
-- start at offset 0 are pushed with 'mPushChunk' (O(1) per chunk
-- instead of O(bf) element pushes).
cunstream :: ChunkedBundle a -> Vector a
cunstream cb = create $ \mv ->
  let chunks = CB.cbToChunkList cb
      go [] = pure ()
      go (Chunk arr off len : rest) = do
        st <- getMV mv
        if off == 0 && len == sizeofSmallArray arr && len == bf && mvTailSize st == 0
          then mPushChunk mv arr
          else do
            let pushElems !i
                  | i >= len  = pure ()
                  | otherwise = mPush mv (indexSmallArray arr (off + i)) >> pushElems (i + 1)
            pushElems 0
        go rest
  in go chunks
{-# INLINE [0] cunstream #-}

{-# RULES "cstream/cunstream" forall b. cstream (cunstream b) = b #-}

------------------------------------------------------------------------
-- Recycling framework (Leshchinskiy, "Recycle Your Arrays!", 2008)
------------------------------------------------------------------------

data New a = New (forall s. ST s (MVector s a))

new :: New a -> Vector a
new (New mk) = runST (mk >>= unsafeFreeze)
{-# INLINE [0] new #-}

fill :: Bundle a -> New a
fill (Bundle (MStream step s0) _) = New $ do
  mv <- mNew
  let go s = case unId (step s) of
        Done       -> pure mv
        Skip    s' -> go s'
        Yield a s' -> mPush mv a >> go s'
  go s0
{-# INLINE [1] fill #-}

clone :: Vector a -> New a
clone = fill . stream
{-# INLINE [0] clone #-}

updateNew :: New a -> [(Int, a)] -> New a
updateNew (New mk) ps = New $ do
  mv <- mk
  Prelude.mapM_ (\(i, x) -> mWrite mv i x) ps
  pure mv
{-# INLINE updateNew #-}

mapNew :: (a -> a) -> New a -> New a
mapNew f (New mk) = New $ do
  mv <- mk
  mMapInPlace f mv
  pure mv
{-# INLINE [0] mapNew #-}

transformNew :: (forall s. MVector s a -> ST s ()) -> New a -> New a
transformNew act (New mk) = New $ do
  mv <- mk
  act mv
  pure mv
{-# INLINE transformNew #-}

transform :: (forall m. Monad m => MStream m a -> MStream m a)
          -> (Size -> Size) -> New a -> New a
{-# INLINE [1] transform #-}
transform f _ (New mk) = New $ do
  mv <- mk
  n <- mLength mv
  consumeMStream mv (f (mstreamMV mv n))
  pure mv

mstreamMV :: MVector s a -> Int -> MStream (ST s) a
mstreamMV mv n = MStream step 0
  where
    step i
      | i >= n    = pure Done
      | otherwise = do
          x <- mRead mv i
          pure (Yield x (i + 1))
{-# INLINE mstreamMV #-}

consumeMStream :: MVector s a -> MStream (ST s) a -> ST s ()
consumeMStream mv (MStream step s0) = go 0 s0
  where
    go !i s = do
      r <- step s
      case r of
        Yield x s' -> mWrite mv i x >> go (i + 1) s'
        Skip    s' -> go i s'
        Done       -> pure ()
{-# INLINE consumeMStream #-}

mMapInPlace :: (a -> a) -> MVector s a -> ST s ()
mMapInPlace f mv = do
  st <- getMV mv
  newRoot <- mMapNodeInPlace f (mvShift st) (mvRoot st)
  mapMutableArr f (mvTail st) 0 (mvTailSize st)
  putMV mv st { mvRoot = newRoot }
{-# INLINE mMapInPlace #-}

inplace_map :: (a -> a) -> Vector a -> Vector a
inplace_map f = new . mapNew f . clone
{-# INLINE [1] inplace_map #-}

liftSmap :: (a -> b) -> (forall m. Monad m => MStream m a -> MStream m b)
liftSmap f (MStream step s0) = MStream step' s0
  where
    step' s = do
      r <- step s
      pure $ case r of
        Yield x s' -> Yield (f x) s'
        Skip    s' -> Skip s'
        Done       -> Done
{-# INLINE liftSmap #-}

------------------------------------------------------------------------
-- Rewrite rules: fusion + recycling
------------------------------------------------------------------------

{-# RULES

-- === Core fusion and recycling rules ===

"pvector/stream/unstream"
  forall s. stream (unstream s) = s

"pvector/fusion"
  forall s. stream (new (fill s)) = s

"pvector/recycling"
  forall p. fill (stream (new p)) = p

"pvector/transform/fill [New]"
  forall (f :: forall m. Monad m => MStream m a -> MStream m a)
         g s.
  transform f g (fill s) = fill (S.inplace f g s)

"pvector/transform/transform [New]"
  forall (f1 :: forall m. Monad m => MStream m a -> MStream m a)
         (f2 :: forall m. Monad m => MStream m a -> MStream m a)
         g1 g2 p.
  transform f1 g1 (transform f2 g2 p) = transform (f1 . f2) (g1 . g2) p

"pvector/mapNew/mapNew" forall f g p.
  mapNew f (mapNew g p) = mapNew (f . g) p

"pvector/uninplace/map" forall f p.
  stream (new (mapNew f p)) = S.smap f (stream (new p))

"pvector/map [stream]" [~1] forall f v.
  map f v = unstream (S.smap f (stream v))
"pvector/filter [stream]" [~1] forall f v.
  filter f v = unstream (S.sfilter f (stream v))
"pvector/take [stream]" [~1] forall n v.
  take n v = unstream (S.stake n (stream v))
"pvector/drop [stream]" [~1] forall n v.
  drop n v = unstream (S.sdrop n (stream v))
"pvector/takeWhile [stream]" [~1] forall p v.
  takeWhile p v = unstream (S.stakeWhile p (stream v))
"pvector/dropWhile [stream]" [~1] forall p v.
  dropWhile p v = unstream (S.sdropWhile p (stream v))
"pvector/zipWith [stream]" [~1] forall f v1 v2.
  zipWith f v1 v2 = unstream (S.szipWith f (stream v1) (stream v2))
"pvector/init [stream]" [~1] forall v.
  init v = unstream (S.sinit (stream v))
"pvector/tail [stream]" [~1] forall v.
  tail v = unstream (S.stail (stream v))
"pvector/mapMaybe [stream]" [~1] forall f v.
  mapMaybe f v = unstream (S.smapMaybe f (stream v))

"pvector/foldlDirect/unstream" forall f z s.
  foldlDirect f z (unstream s) = S.sfoldl' f z s
"pvector/foldrDirect/unstream" forall f z s.
  foldrDirect f z (unstream s) = S.sfoldr f z s

"pvector/index/unstream" forall s i.
  index (unstream s) i = S.sindex s i
"pvector/(!?)/unstream" forall s i.
  (unstream s) !? i = S.sindexM s i
"pvector/head/unstream" forall s.
  head (unstream s) = S.shead s
"pvector/last/unstream" forall s.
  last (unstream s) = S.slast s
"pvector/unsafeIndex/unstream" forall s i.
  unsafeIndex (unstream s) i = S.sindex s i

"pvector/take/unstream" forall n s.
  take n (unstream s) = unstream (S.stake n s)
"pvector/drop/unstream" forall n s.
  drop n (unstream s) = unstream (S.sdrop n s)
"pvector/init/unstream" forall s.
  init (unstream s) = unstream (S.sinit s)
"pvector/tail/unstream" forall s.
  tail (unstream s) = unstream (S.stail s)

"pvector/uninplace" forall (f :: forall m. Monad m => S.MStream m a -> S.MStream m a) g p.
  stream (new (transform f g p)) = S.inplace f g (stream (new p))

"pvector/map [direct]" [1] forall f v.
  unstream (S.smap f (stream v)) = mapDirect f v
"pvector/filter [direct]" [1] forall f v.
  unstream (S.sfilter f (stream v)) = filterDirect f v
"pvector/take [direct]" [1] forall n v.
  unstream (S.stake n (stream v)) = takeDirect n v
"pvector/drop [direct]" [1] forall n v.
  unstream (S.sdrop n (stream v)) = dropDirect n v
"pvector/takeWhile [direct]" [1] forall p v.
  unstream (S.stakeWhile p (stream v)) = takeWhileDirect p v
"pvector/dropWhile [direct]" [1] forall p v.
  unstream (S.sdropWhile p (stream v)) = dropWhileDirect p v
"pvector/zipWith [direct]" [1] forall f v1 v2.
  unstream (S.szipWith f (stream v1) (stream v2)) = zipWithDirect f v1 v2
"pvector/init [direct]" [1] forall v.
  unstream (S.sinit (stream v)) = initDirect v
"pvector/tail [direct]" [1] forall v.
  unstream (S.stail (stream v)) = tailDirect v
"pvector/mapMaybe [direct]" [1] forall f v.
  unstream (S.smapMaybe f (stream v)) = mapMaybeDirect f v

-- (Chunk-level fused fold rules moved to pre-stream phase [~2] above)

  #-}

------------------------------------------------------------------------
-- Internal tree operations
------------------------------------------------------------------------

newPath :: Int -> Node a -> Node a
newPath 0 node = node
newPath level node =
  let !arr = runST $ do
        a <- newSmallArray 1 Empty
        writeSmallArray a 0 (newPath (level - bfBits) node)
        unsafeFreezeSmallArray a
  in Internal arr

pushTail :: Int -> Int -> Node a -> SmallArray a -> Node a
pushTail size shift root tailArr = go shift root
  where
    go _ Empty = newPath shift (Leaf tailArr)
    go level (Internal arr) =
      let !nc = sizeofSmallArray arr
          !subIdx = indexAtLevel (size - 1) level
      in if level == bfBits
           then if subIdx < nc
                then Internal (cloneAndSet arr subIdx (Leaf tailArr))
                else Internal (snocArray arr (Leaf tailArr))
           else if subIdx < nc
                then case indexSmallArray arr subIdx of
                       Empty ->
                         let !path = newPath (level - bfBits) (Leaf tailArr)
                         in Internal (cloneAndSet arr subIdx path)
                       child ->
                         Internal (cloneAndSet arr subIdx (go (level - bfBits) child))
                else let !path = newPath (level - bfBits) (Leaf tailArr)
                     in Internal (snocArray arr path)
    go level (Relaxed arr sizes) =
      let !nc = sizeofSmallArray arr
          !newChild = go (level - bfBits) (indexSmallArray arr (nc - 1))
          !newArr = cloneAndSet arr (nc - 1) newChild
          !newSizes = computeSizeTable level newArr
      in Relaxed newArr newSizes
    go _ _ = error "pvector: pushTail invariant violation"

popTail :: Int -> Int -> Node a -> Maybe (Node a)
popTail size shift root = go shift root
  where
    go level (Internal arr) =
      let !nc = sizeofSmallArray arr
          !subIdx = min (nc - 1) (indexAtLevel (size - 2) level)
      in if level > bfBits
           then case go (level - bfBits) (indexSmallArray arr subIdx) of
                  Nothing
                    | subIdx == 0 -> Nothing
                    | otherwise   -> Just $ Internal (cloneSmallArray arr 0 subIdx)
                  Just child' -> Just $ Internal (cloneAndSet arr subIdx child')
           else if subIdx == 0
                  then Nothing
                  else Just $ Internal (cloneSmallArray arr 0 subIdx)
    go _ _ = Nothing

liftToShift :: Int -> Int -> Node a -> Node a
liftToShift targetShift currentShift node
  | currentShift >= targetShift = node
  | otherwise = liftToShift targetShift (currentShift + bfBits) $
      Internal $ runST $ do
        a <- newSmallArray 1 Empty
        writeSmallArray a 0 node
        unsafeFreezeSmallArray a

------------------------------------------------------------------------
-- Fold helpers (used by Front/Deque)
------------------------------------------------------------------------

-- | Reverse left fold (right to left).
rfoldl' :: (b -> a -> b) -> b -> Vector a -> b
rfoldl' f z0 v
  | n == 0    = z0
  | otherwise = go z0 (n - 1)
  where
    !n = vSize v
    go !z !i
      | i < 0     = z
      | otherwise = go (f z (unsafeIndex v i)) (i - 1)
{-# INLINE rfoldl' #-}

-- | Reverse right fold (right to left, lazy).
rfoldr :: (a -> b -> b) -> b -> Vector a -> b
rfoldr f z0 v
  | n == 0    = z0
  | otherwise = go (n - 1)
  where
    !n = vSize v
    go !i
      | i < 0     = z0
      | otherwise = f (unsafeIndex v i) (go (i - 1))
{-# INLINE rfoldr #-}

-- | Fold left with chunks, for compatibility.
foldlWithChunks :: (b -> SmallArray a -> Int -> b) -> b -> Vector a -> b
foldlWithChunks f z0 v = foldChunks (\b _ arr -> f b arr (sizeofSmallArray arr)) z0 v
