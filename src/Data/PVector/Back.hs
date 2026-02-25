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
  , fromListN
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
  , vSize, vShift, vRoot, vTail
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
import qualified GHC.Exts as Exts
import GHC.Base (build)

import Data.PVector.Internal
import Data.PVector.Internal.Stream
  (Bundle(..), Step(..), Size(..), MStream(..), Id(..), Stream)
import qualified Data.PVector.Internal.Stream as S

import Prelude hiding
  ( null, length, head, last, map, reverse, filter, take, drop
  , foldMap, zip, zip3, zipWith, unzip
  , replicate, unfoldr, foldr, foldl', foldl, foldl1, foldr1
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

data Vector a = Vector
  { vSize  :: {-# UNPACK #-} !Int
  , vShift :: {-# UNPACK #-} !Int
  , vRoot  :: !(Node a)
  , vTail  :: {-# UNPACK #-} !(SmallArray a)
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
    | otherwise = eqNodes (vShift v1) (vRoot v1) (vRoot v2)
                  && eqArrays (vTail v1) (vTail v2) 0 (sizeofSmallArray (vTail v1))

eqArrays :: Eq a => SmallArray a -> SmallArray a -> Int -> Int -> Bool
eqArrays a1 a2 !i !n
  | i >= n    = True
  | otherwise = indexSmallArray a1 i == indexSmallArray a2 i && eqArrays a1 a2 (i + 1) n

eqNodes :: Eq a => Int -> Node a -> Node a -> Bool
eqNodes _ Empty Empty = True
eqNodes _ (Leaf a1) (Leaf a2) = eqArrays a1 a2 0 bf
eqNodes shift (Internal a1) (Internal a2) = go 0
  where
    go i | i >= bf = True
         | otherwise = eqNodes (shift - bfBits) (indexSmallArray a1 i) (indexSmallArray a2 i)
                       && go (i + 1)
eqNodes _ _ _ = False

instance Ord a => Ord (Vector a) where
  compare v1 v2 =
    case cmpNodes (vShift v1) (vRoot v1) (vRoot v2) of
      EQ -> case cmpArrays (vTail v1) (vTail v2) 0 (min ts1 ts2) of
              EQ -> compare (vSize v1) (vSize v2)
              x  -> x
      x  -> x
    where
      !ts1 = sizeofSmallArray (vTail v1)
      !ts2 = sizeofSmallArray (vTail v2)

cmpArrays :: Ord a => SmallArray a -> SmallArray a -> Int -> Int -> Ordering
cmpArrays a1 a2 !i !n
  | i >= n    = EQ
  | otherwise = case compare (indexSmallArray a1 i) (indexSmallArray a2 i) of
      EQ -> cmpArrays a1 a2 (i + 1) n
      x  -> x

cmpNodes :: Ord a => Int -> Node a -> Node a -> Ordering
cmpNodes _ Empty Empty = EQ
cmpNodes _ Empty _     = LT
cmpNodes _ _     Empty = GT
cmpNodes _ (Leaf a1) (Leaf a2) = cmpArrays a1 a2 0 bf
cmpNodes shift (Internal a1) (Internal a2) = go 0
  where
    go i | i >= bf = EQ
         | otherwise = case cmpNodes (shift - bfBits) (indexSmallArray a1 i) (indexSmallArray a2 i) of
              EQ -> go (i + 1)
              x  -> x
cmpNodes _ (Leaf _) (Internal _) = LT
cmpNodes _ (Internal _) (Leaf _) = GT

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
  traverse f v = foldl' (\acc a -> snoc <$> acc <*> f a) (Prelude.pure empty) v
  {-# INLINE traverse #-}

instance Exts.IsList (Vector a) where
  type Item (Vector a) = a
  fromList  = Data.PVector.Back.fromList
  fromListN = Data.PVector.Back.fromListN
  toList    = Data.PVector.Back.toList
  {-# INLINE fromList #-}
  {-# INLINE toList #-}

instance NFData a => NFData (Vector a) where
  rnf (Vector _ _ root tail_) = rnf root `seq` rnfArray tail_

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
empty = Vector 0 bfBits emptyRoot emptyTail
{-# INLINE empty #-}

singleton :: a -> Vector a
singleton x = Vector 1 bfBits emptyRoot tail_
  where
    !tail_ = runST $ do
      a <- newSmallArray 1 x
      unsafeFreezeSmallArray a
{-# INLINE singleton #-}

-- | O(n). Build a vector from a list.
-- The inner loop writes directly into the tail buffer, only touching
-- the MutVar when the tail is full and needs flushing.
fromList :: [a] -> Vector a
fromList [] = empty
fromList xs = runST $ do
  mv <- mNew
  st0 <- getMV mv
  goFill mv (mvTail st0) 0 xs
  unsafeFreeze mv
  where
    -- Fast inner loop: write straight into the tail buffer.
    -- Only reads/writes the MutVar when the buffer fills up.
    goFill !mv !tail_ !ti [] = do
      st <- getMV mv
      putMV mv st { mvSize = mvSize st + ti - mvTailSize st
                   , mvTailSize = ti }
    goFill !mv !tail_ !ti (a:as)
      | ti < bf = do
          writeSmallArray tail_ ti a
          goFill mv tail_ (ti + 1) as
      | otherwise = do
          -- Tail is full at bf elements. Flush.
          st <- getMV mv
          let !n = mvSize st + (bf - mvTailSize st)
          frozenTail <- unsafeFreezeSmallArray tail_
          let !willOverflow = unsafeShiftR n bfBits > unsafeShiftL 1 (mvShift st)
          (newRoot, newShift) <-
            if willOverflow
              then do
                arr <- newSmallArray bf (Frozen Empty)
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

-- | O(n). Build a vector from the first @n@ elements of a list.
fromListN :: Int -> [a] -> Vector a
fromListN n xs
  | n <= 0    = empty
  | otherwise = runST $ do
      mv <- mNew
      st0 <- getMV mv
      goFillN mv (mvTail st0) 0 n xs
      unsafeFreeze mv
  where
    goFillN !mv !tail_ !ti !remaining [] = do
      st <- getMV mv
      putMV mv st { mvSize = mvSize st + ti - mvTailSize st
                   , mvTailSize = ti }
    goFillN !mv !tail_ !ti !remaining _
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
          let !n = mvSize st + (bf - mvTailSize st)
          frozenTail <- unsafeFreezeSmallArray tail_
          let !willOverflow = unsafeShiftR n bfBits > unsafeShiftL 1 (mvShift st)
          (newRoot, newShift) <-
            if willOverflow
              then do
                arr <- newSmallArray bf (Frozen Empty)
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

-- | O(n). Build a vector from a list of full 32-element SmallArrays
-- plus a partial tail.  Each chunk MUST have exactly 32 elements.
-- The tail can have 0-31 elements.  This is the fastest way to
-- construct a vector when you have pre-built chunks.
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

-- | O(n). Same as 'generate'.
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

-- | O(n). Prepend an element (rebuilds the vector).
cons :: a -> Vector a -> Vector a
cons x v = create $ \mv -> do
  mPush mv x
  forEach_ v $ \a -> mPush mv a
{-# INLINE cons #-}

snoc :: Vector a -> a -> Vector a
snoc v x
  | n == 0 = singleton x
  | tailSz < bf =
      v { vSize = n + 1
        , vTail = snocArray (vTail v) x
        }
  | otherwise =
      let !newShift | willOverflow = vShift v + bfBits
                    | otherwise    = vShift v
          !newRoot
            | willOverflow =
                let arr = runST $ do
                      a <- newSmallArray bf Empty
                      writeSmallArray a 0 (vRoot v)
                      writeSmallArray a 1 (newPath (vShift v) (Leaf (vTail v)))
                      unsafeFreezeSmallArray a
                in Internal arr
            | otherwise = pushTail n (vShift v) (vRoot v) (vTail v)
          !newTailArr = runST $ do
            a <- newSmallArray 1 x
            unsafeFreezeSmallArray a
      in Vector (n + 1) newShift newRoot newTailArr
  where
    !n = vSize v
    !tailSz = sizeofSmallArray (vTail v)
    !willOverflow = unsafeShiftR n bfBits > unsafeShiftL 1 (vShift v)
{-# INLINE snoc #-}

(|>) :: Vector a -> a -> Vector a
(|>) = snoc
{-# INLINE (|>) #-}

(<|) :: a -> Vector a -> Vector a
(<|) = cons
{-# INLINE (<|) #-}

-- | O(n+m). Concatenate.
(++) :: Vector a -> Vector a -> Vector a
(++) = append
{-# INLINE (++) #-}

concat :: [Vector a] -> Vector a
concat [] = empty
concat [v] = v
concat vs = create $ \mv -> do
  buf <- newSmallArray bf uninitElem
  let go !b !off [] = flushBuf b off mv
      go !b !off (v:vs')
        | vSize v == 0 = go b off vs'
        | off == 0 = do
            pushNodeChunks (vShift v) (vRoot v) mv
            let !tail_ = vTail v
                !k = sizeofSmallArray tail_
            if k == bf
              then do
                mPushChunk mv tail_
                go b 0 vs'
              else do
                copySmallArray b 0 tail_ 0 k
                go b k vs'
        | otherwise = do
            (b', off') <- pushNodeBuffered b off (vShift v) (vRoot v) mv
            let !tail_ = vTail v
                !tailSz = sizeofSmallArray tail_
            (b'', off'') <- copyBuffered b' off' tail_ 0 tailSz mv
            go b'' off'' vs'
  go buf 0 vs
{-# INLINE concat #-}

-- | Fully evaluate the vector and its elements.
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
{-# INLINE index #-}

(!) :: Vector a -> Int -> a
(!) = index
{-# INLINE (!) #-}

(!?) :: Vector a -> Int -> Maybe a
(!?) v i
  | i < 0 || i >= vSize v = Nothing
  | otherwise = Just $! unsafeIndex v i
{-# INLINE (!?) #-}

unsafeIndex :: Vector a -> Int -> a
unsafeIndex v i
  | i >= tailOff = indexSmallArray (vTail v) (i .&. bfMask)
  | otherwise    = indexSmallArray (leafFor (vShift v) i (vRoot v)) (i .&. bfMask)
  where
    !tailOff = tailOffset (vSize v)
{-# INLINE unsafeIndex #-}

head :: Vector a -> a
head v
  | vSize v == 0 = error "Data.PVector.Back.head: empty"
  | otherwise     = unsafeIndex v 0
{-# INLINE head #-}

last :: Vector a -> a
last v
  | vSize v == 0 = error "Data.PVector.Back.last: empty"
  | otherwise     = indexSmallArray (vTail v) (sizeofSmallArray (vTail v) - 1)
{-# INLINE last #-}

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
  | otherwise    = generate n (\j -> unsafeIndex v (i + j))
{-# INLINE slice #-}

init :: Vector a -> Vector a
init v = case unsnoc v of
  Nothing    -> error "Data.PVector.Back.init: empty"
  Just (v',_) -> v'
{-# INLINE init #-}

tail :: Vector a -> Vector a
tail v
  | vSize v == 0 = error "Data.PVector.Back.tail: empty"
  | otherwise     = drop 1 v
{-# INLINE tail #-}

take :: Int -> Vector a -> Vector a
take n v
  | n <= 0        = empty
  | n >= vSize v  = v
  | otherwise     = generate n (unsafeIndex v)
{-# INLINE take #-}

drop :: Int -> Vector a -> Vector a
drop n v
  | n <= 0        = v
  | n >= vSize v  = empty
  | otherwise     = generate (vSize v - n) (\i -> unsafeIndex v (i + n))
{-# INLINE drop #-}

splitAt :: Int -> Vector a -> (Vector a, Vector a)
splitAt n v = (take n v, drop n v)
{-# INLINE splitAt #-}

uncons :: Vector a -> Maybe (a, Vector a)
uncons v
  | vSize v == 0 = Nothing
  | vSize v == 1 = Just (unsafeIndex v 0, empty)
  | otherwise     = Just (unsafeIndex v 0, drop 1 v)
{-# INLINE uncons #-}

unsnoc :: Vector a -> Maybe (Vector a, a)
unsnoc v
  | n == 0 = Nothing
  | n == 1 = Just (empty, unsafeIndex v 0)
  | tailSz > 1 =
      let (!newTail, !x) = unsnocArray (vTail v)
      in Just (v { vSize = n - 1, vTail = newTail }, x)
  | otherwise =
      let !x        = indexSmallArray (vTail v) 0
          !newTail  = leafFor (vShift v) (n - 2) (vRoot v)
          !rootPop  = popTail n (vShift v) (vRoot v)
          !newRoot  = case rootPop of
                        Nothing -> emptyRoot
                        Just r  -> r
          (!newShift, !newRoot') = case newRoot of
            Internal arr
              | vShift v > bfBits
              , sizeofSmallArray arr > 1
              , isEmpty (indexSmallArray arr 1)
                -> (vShift v - bfBits, indexSmallArray arr 0)
            _ -> (vShift v, newRoot)
      in Just (Vector (n - 1) newShift newRoot' newTail, x)
  where
    !n = vSize v
    !tailSz = sizeofSmallArray (vTail v)
    isEmpty Empty = True
    isEmpty _     = False
{-# INLINE unsnoc #-}

------------------------------------------------------------------------
-- Update
------------------------------------------------------------------------

update :: Int -> a -> Vector a -> Vector a
update i x v
  | i < 0 || i >= vSize v = error "Data.PVector.Back.update: out of bounds"
  | i >= tailOff = v { vTail = cloneAndSet (vTail v) (i .&. bfMask) x }
  | otherwise    = v { vRoot = updateNodeST (vShift v) i x (vRoot v) }
  where
    !tailOff = tailOffset (vSize v)
{-# INLINE update #-}

-- Path-copy update: clones each node on the root-to-leaf path.
-- Uses cloneAndSet32 which avoids sizeofSmallArray overhead.
-- | Path-copy update with unrolled descent for depth 1 and 2.
updateNodeST :: Int -> Int -> a -> Node a -> Node a
updateNodeST !shift !i x root = case root of
  Internal arr0
    | shift == bfBits ->
        -- Depth 1: root children are leaves
        let !si0 = indexAtLevel i shift
            !child = indexSmallArray arr0 si0
            !child' = case child of
              Leaf la -> Leaf (cloneAndSet32 la (i .&. bfMask) x)
              _ -> error "pvector: update invariant"
        in Internal (cloneAndSet32 arr0 si0 child')
    | shift == 2 * bfBits ->
        -- Depth 2: root -> internal -> leaf
        let !si0 = indexAtLevel i shift
            !mid = indexSmallArray arr0 si0
            !mid' = case mid of
              Internal arr1 ->
                let !si1 = indexAtLevel i bfBits
                    !child = indexSmallArray arr1 si1
                    !child' = case child of
                      Leaf la -> Leaf (cloneAndSet32 la (i .&. bfMask) x)
                      _ -> error "pvector: update invariant"
                in Internal (cloneAndSet32 arr1 si1 child')
              _ -> error "pvector: update invariant"
        in Internal (cloneAndSet32 arr0 si0 mid')
    | otherwise -> goDeep shift root
  _ -> goDeep shift root
  where
    goDeep !level (Internal arr) =
      let !subIdx = indexAtLevel i level
          !child  = indexSmallArray arr subIdx
          !child' = goDeep (level - bfBits) child
      in Internal (cloneAndSet32 arr subIdx child')
    goDeep _ (Leaf arr) =
      Leaf (cloneAndSet32 arr (i .&. bfMask) x)
    goDeep _ Empty = error "pvector: updateNodeST hit Empty"
{-# INLINE updateNodeST #-}

-- | Bulk update from a list of (index, value) pairs.
-- Defined via the recycling framework: @xs // ps = new (updateNew (clone xs) ps)@.
-- Chained updates like @xs // ps // qs@ will share a single mutable copy.
(//) :: Vector a -> [(Int, a)] -> Vector a
xs // ps = new (updateNew (clone xs) ps)
{-# INLINE [1] (//) #-}

adjust :: (a -> a) -> Int -> Vector a -> Vector a
adjust f i v
  | i < 0 || i >= vSize v = error "Data.PVector.Back.adjust: out of bounds"
  | otherwise = update i (f (unsafeIndex v i)) v
{-# INLINE adjust #-}

-- | Strict version of 'adjust'.
adjust' :: (a -> a) -> Int -> Vector a -> Vector a
adjust' f i v
  | i < 0 || i >= vSize v = error "Data.PVector.Back.adjust': out of bounds"
  | otherwise = let !x = f (unsafeIndex v i) in update i x v
{-# INLINE adjust' #-}

------------------------------------------------------------------------
-- Mapping
------------------------------------------------------------------------

-- | O(n). Map a function over all elements.
-- Participates in stream fusion: @map f . map g@ becomes @map (f . g)@.
map :: (a -> b) -> Vector a -> Vector b
map f = mapDirect f
{-# NOINLINE [1] map #-}

mapDirect :: (a -> b) -> Vector a -> Vector b
mapDirect f v
  | n == 0 = empty
  | otherwise = Vector n (vShift v) newRoot newTail
  where
    !n = vSize v
    !newTail = mapArray' f (vTail v)
    !newRoot = goNode (vShift v) (vRoot v)

    goNode !_ Empty = Empty
    goNode !_ (Leaf arr) = Leaf (mapChunk32 f arr)
    goNode !shift (Internal arr) = Internal $ runST $ do
      marr <- newSmallArray bf Empty
      let go i
            | i >= bf = pure ()
            | otherwise = do
                writeSmallArray marr i (goNode (shift - bfBits) (indexSmallArray arr i))
                go (i + 1)
      go 0
      unsafeFreezeSmallArray marr
{-# INLINE mapDirect #-}

-- | O(n). Map with index.
imap :: (Int -> a -> b) -> Vector a -> Vector b
imap f v = generate (vSize v) (\i -> f i (unsafeIndex v i))
{-# INLINE imap #-}

concatMap :: (a -> Vector b) -> Vector a -> Vector b
concatMap f v = create $ \mv ->
  forEach_ v $ \a -> forEach_ (f a) $ \b -> mPush mv b
{-# INLINE concatMap #-}

mapMaybe :: (a -> Maybe b) -> Vector a -> Vector b
mapMaybe f v = create $ \mv ->
  forEach_ v $ \a -> case f a of
    Nothing -> pure ()
    Just b  -> mPush mv b
{-# INLINE mapMaybe #-}

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
mapM f v = foldl' (\acc a -> acc >>= \v' -> f a >>= \b -> pure (snoc v' b)) (pure empty) v
{-# INLINE mapM #-}

mapM_ :: Monad m => (a -> m b) -> Vector a -> m ()
mapM_ f = foldl' (\m a -> m >> f a >> pure ()) (pure ())
{-# INLINE mapM_ #-}

forM :: Monad m => Vector a -> (a -> m b) -> m (Vector b)
forM = flip mapM
{-# INLINE forM #-}

forM_ :: Monad m => Vector a -> (a -> m b) -> m ()
forM_ = flip mapM_
{-# INLINE forM_ #-}

imapM :: Monad m => (Int -> a -> m b) -> Vector a -> m (Vector b)
imapM f v = ifoldl' (\acc i a -> acc >>= \v' -> f i a >>= \b -> pure (snoc v' b)) (pure empty) v
{-# INLINE imapM #-}

imapM_ :: Monad m => (Int -> a -> m ()) -> Vector a -> m ()
imapM_ f = ifoldl' (\m i a -> m >> f i a) (pure ())
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
zipWith f v1 v2 = generate (min (length v1) (length v2)) $ \i ->
  f (unsafeIndex v1 i) (unsafeIndex v2 i)
{-# INLINE zipWith #-}

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

-- | O(n). Keep only elements satisfying the predicate.
filter :: (a -> Bool) -> Vector a -> Vector a
filter p = filterDirect p
{-# NOINLINE [1] filter #-}

filterDirect :: forall a. (a -> Bool) -> Vector a -> Vector a
filterDirect p v
  | vSize v == 0 = empty
  | otherwise = create $ \mv -> do
      goNode (vShift v) (vRoot v) mv
      goChunk (vTail v) 0 (sizeofSmallArray (vTail v)) mv
  where
    goNode :: Int -> Node a -> MVector s a -> ST s ()
    goNode !_ Empty !_ = pure ()
    goNode !_ (Leaf arr) !mv = goChunk arr 0 bf mv
    goNode !level (Internal arr) !mv = do
      let go i | i >= bf   = pure ()
               | otherwise = do
                   goNode (level - bfBits) (indexSmallArray arr i) mv
                   go (i + 1)
      go 0

    goChunk :: SmallArray a -> Int -> Int -> MVector s a -> ST s ()
    goChunk !arr !i !limit !mv
      | i >= limit = pure ()
      | otherwise = do
          let !a = indexSmallArray arr i
          when (p a) (mPush mv a)
          goChunk arr (i + 1) limit mv
{-# INLINE filterDirect #-}

-- | O(n). Filter with index.
ifilter :: (Int -> a -> Bool) -> Vector a -> Vector a
ifilter p v = create $ \mv ->
  forI_ v $ \i a -> when (p i a) (mPush mv a)
{-# INLINE ifilter #-}

takeWhile :: (a -> Bool) -> Vector a -> Vector a
takeWhile p v = case findIndex (not . p) v of
  Nothing -> v
  Just i  -> take i v
{-# INLINE takeWhile #-}

dropWhile :: (a -> Bool) -> Vector a -> Vector a
dropWhile p v = case findIndex (not . p) v of
  Nothing -> empty
  Just i  -> drop i v
{-# INLINE dropWhile #-}

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
foldl f z0 v = foldr (\x k z -> k (f z x)) id v z0
{-# INLINE foldl #-}

-- | O(n). Strict left fold.
-- The tree walk is in the where clause so that when foldl' is INLINE'd
-- at a call site with a known @f@, GHC specializes all inner loops.
foldl' :: (b -> a -> b) -> b -> Vector a -> b
foldl' f = \ !z0 v ->
  if vSize v == 0
  then z0
  else goTail (goNode z0 (vShift v) (vRoot v))
              (vTail v) 0 (sizeofSmallArray (vTail v))
  where
    goNode !z !level0 node0 = case node0 of
      Empty -> z
      Leaf arr -> foldlChunk32 f z arr
      Internal arr0
        | level0 == bfBits -> goLvs z arr0 0
        | level0 == 2 * bfBits -> goInts z arr0 0
        | otherwise -> goDeep z level0 node0

    goLvs !z !arr !i
      | i >= bf   = z
      | otherwise = case indexSmallArray arr i of
          Leaf la -> goLvs (foldlChunk32 f z la) arr (i + 1)
          Empty   -> goLvs z arr (i + 1)
          _       -> goLvs z arr (i + 1)

    goInts !z !arr !i
      | i >= bf   = z
      | otherwise = case indexSmallArray arr i of
          Internal inner -> goInts (goLvs z inner 0) arr (i + 1)
          Empty          -> goInts z arr (i + 1)
          _              -> goInts z arr (i + 1)

    goDeep !z !_ Empty = z
    goDeep !z !_ (Leaf arr) = foldlChunk32 f z arr
    goDeep !z !level (Internal arr) = goC z 0
      where
        goC !z' !i
          | i >= bf   = z'
          | otherwise = goC (goDeep z' (level - bfBits) (indexSmallArray arr i)) (i + 1)

    goTail !z !arr !i !limit
      | i >= limit = z
      | otherwise  = goTail (f z (indexSmallArray arr i)) arr (i + 1) limit
{-# INLINE foldl' #-}

-- | Walk the trie and fold over all leaf arrays.
-- Unrolls depth-1 and depth-2 cases to avoid per-child Node pattern match.
-- | Walk the trie applying a pre-specialized chunk fold at each leaf.
-- The chunk fold @fchunk@ is applied by the INLINE'd caller with @f@
-- already baked in, so GHC specializes the inner loop.
goNodeWith :: (b -> SmallArray a -> b) -> b -> Int -> Node a -> b
goNodeWith fchunk !z0 !level0 node0 = case node0 of
  Empty -> z0
  Leaf arr -> fchunk z0 arr
  Internal arr0
    | level0 == bfBits ->
        goLvs z0 arr0 0
    | level0 == 2 * bfBits ->
        goInts z0 arr0 0
    | otherwise ->
        goDeep z0 level0 node0
  where
    goLvs !z !arr !i
      | i >= bf   = z
      | otherwise = case indexSmallArray arr i of
          Leaf la  -> goLvs (fchunk z la) arr (i + 1)
          Empty    -> goLvs z arr (i + 1)
          _        -> goLvs z arr (i + 1)
    goInts !z !arr !i
      | i >= bf   = z
      | otherwise = case indexSmallArray arr i of
          Internal inner -> goInts (goLvs z inner 0) arr (i + 1)
          Empty          -> goInts z arr (i + 1)
          _              -> goInts z arr (i + 1)
    goDeep !z !_ Empty = z
    goDeep !z !_ (Leaf arr) = fchunk z arr
    goDeep !z !level (Internal arr) = goC z 0
      where
        goC !z' !i
          | i >= bf   = z'
          | otherwise = goC (goDeep z' (level - bfBits) (indexSmallArray arr i)) (i + 1)
{-# INLINEABLE goNodeWith #-}

{-# INLINEABLE goNode #-}
goNode :: (b -> a -> b) -> b -> Int -> Node a -> b
goNode f !z0 !level0 node0 = case node0 of
  Empty -> z0
  Leaf arr -> foldlChunk32 f z0 arr
  Internal arr0
    | level0 == bfBits ->
        -- Depth 1: all children are Leaf
        goLeaves z0 arr0 0
    | level0 == 2 * bfBits ->
        -- Depth 2: children are Internal whose children are Leaf
        goInternals z0 arr0 0
    | otherwise ->
        goGeneric f z0 level0 node0
  where
    -- Fold over 32 Leaf children (depth 1)
    goLeaves !z !arr !i
      | i >= bf   = z
      | otherwise = case indexSmallArray arr i of
          Leaf la  -> goLeaves (foldlChunk32 f z la) arr (i + 1)
          Empty    -> goLeaves z arr (i + 1)
          _        -> goLeaves z arr (i + 1)

    -- Fold over 32 Internal children each containing Leaf children (depth 2)
    goInternals !z !arr !i
      | i >= bf   = z
      | otherwise = case indexSmallArray arr i of
          Internal inner -> goInternals (goLeaves z inner 0) arr (i + 1)
          Empty          -> goInternals z arr (i + 1)
          _              -> goInternals z arr (i + 1)

-- Generic recursive fold for depth >= 3
goGeneric :: (b -> a -> b) -> b -> Int -> Node a -> b
goGeneric f = go
  where
    go !z !_ Empty = z
    go !z !_ (Leaf arr) = foldlChunk32 f z arr
    go !z !level (Internal arr) = goC z 0
      where
        goC !z' !i
          | i >= bf   = z'
          | otherwise = goC (go z' (level - bfBits) (indexSmallArray arr i)) (i + 1)
{-# INLINEABLE goGeneric #-}

goArr :: (b -> a -> b) -> b -> SmallArray a -> Int -> Int -> b
goArr f = go
  where
    go !z !arr !i !limit
      | i >= limit = z
      | otherwise  = go (f z (indexSmallArray arr i)) arr (i + 1) limit
{-# INLINE goArr #-}

foldlDirect :: (b -> a -> b) -> b -> Vector a -> b
foldlDirect = foldl'
{-# INLINE foldlDirect #-}

-- | Strict left fold, iterating a chunk function over all SmallArrays.
foldlWithChunks :: (b -> SmallArray a -> Int -> b) -> b -> Vector a -> b
foldlWithChunks f z0 v
  | vSize v == 0 = z0
  | otherwise    = f (goNode z0 (vShift v) (vRoot v)) (vTail v) tailSz
  where
    !tailSz = sizeofSmallArray (vTail v)
    goNode !z !_ Empty = z
    goNode !z !_ (Leaf arr) = f z arr bf
    goNode !z !level (Internal arr) =
      let goC !z' !i
            | i >= bf   = z'
            | otherwise = goC (goNode z' (level - bfBits) (indexSmallArray arr i)) (i + 1)
      in goC z 0

-- | Strict left fold over a SmallArray range [i, limit).
foldlArr :: (b -> a -> b) -> b -> SmallArray a -> Int -> Int -> b
foldlArr f = go
  where
    go !z !arr !i !limit
      | i >= limit = z
      | otherwise  = go (f z (indexSmallArray arr i)) arr (i + 1) limit
{-# INLINE foldlArr #-}

foldl1 :: (a -> a -> a) -> Vector a -> a
foldl1 f v
  | vSize v == 0 = error "Data.PVector.Back.foldl1: empty"
  | otherwise     = foldl1ViaNode f (vShift v) (vRoot v) (vTail v)

foldl1' :: (a -> a -> a) -> Vector a -> a
foldl1' f v
  | vSize v == 0 = error "Data.PVector.Back.foldl1': empty"
  | otherwise     = foldl1ViaNode f (vShift v) (vRoot v) (vTail v)
{-# INLINE foldl1' #-}

foldl1ViaNode :: (a -> a -> a) -> Int -> Node a -> SmallArray a -> a
foldl1ViaNode f shift root tail_ =
  let (!z, !started) = firstElemNode shift root
  in if started
     then goTail (goNode z shift root)
     else case tailSz of
            0 -> error "Data.PVector.Back.foldl1: empty"
            _ -> goTail1 (indexSmallArray tail_ 0)
  where
    !tailSz = sizeofSmallArray tail_
    goArr !z arr !i !limit
      | i >= limit = z
      | otherwise  = goArr (f z (indexSmallArray arr i)) arr (i + 1) limit
    goNode !z !_ Empty = z
    goNode !z !_ (Leaf arr) = goArr z arr 0 bf
    goNode !z !level (Internal arr) =
      let goC !z' !i
            | i >= bf   = z'
            | otherwise = goC (goNode z' (level - bfBits) (indexSmallArray arr i)) (i + 1)
      in goC z 0
    goTail !z = goArr z tail_ 0 tailSz
    goTail1 !z = goArr z tail_ 1 tailSz
    firstElemNode _ Empty = (error "pvector: no first elem", False)
    firstElemNode _ (Leaf arr)
      | sizeofSmallArray arr > 0 = (indexSmallArray arr 0, True)
      | otherwise = (error "pvector: no first elem", False)
    firstElemNode lev (Internal arr)
      | sizeofSmallArray arr > 0 = firstElemNode (lev - bfBits) (indexSmallArray arr 0)
      | otherwise = (error "pvector: no first elem", False)

-- | O(n). Lazy right fold.
foldr :: (a -> b -> b) -> b -> Vector a -> b
foldr f z0 v
  | vSize v == 0 = z0
  | otherwise    = goNode (vShift v) (vRoot v) (goArr (vTail v) 0 tailSz z0)
  where
    !tailSz = sizeofSmallArray (vTail v)
    goNode !_ Empty rest = rest
    goNode !_ (Leaf arr) rest = foldrChunk32 f arr rest
    goNode !level (Internal arr) rest =
      let goC !i !r
            | i < 0     = r
            | otherwise = goC (i - 1) (goNode (level - bfBits) (indexSmallArray arr i) r)
      in goC (bf - 1) rest
    goArr !arr !i !limit rest
      | i >= limit = rest
      | otherwise  = f (indexSmallArray arr i) (goArr arr (i + 1) limit rest)
{-# INLINE foldr #-}

foldrDirect :: (a -> b -> b) -> b -> Vector a -> b
foldrDirect = foldr
{-# INLINE foldrDirect #-}

-- | Lazy right fold, calling a chunk function for each SmallArray.
foldrWithChunks :: (SmallArray a -> Int -> b -> b) -> Vector a -> b -> b
foldrWithChunks f v z0
  | vSize v == 0 = z0
  | otherwise    = goNode (vShift v) (vRoot v) (f (vTail v) tailSz z0)
  where
    !tailSz = sizeofSmallArray (vTail v)
    goNode !_ Empty rest = rest
    goNode !_ (Leaf arr) rest = f arr bf rest
    goNode !level (Internal arr) rest =
      let goC !i !r
            | i < 0     = r
            | otherwise = goC (i - 1) (goNode (level - bfBits) (indexSmallArray arr i) r)
      in goC (bf - 1) rest

-- | Right fold over SmallArray range [i, limit).
foldrArr :: (a -> b -> b) -> SmallArray a -> Int -> Int -> b -> b
foldrArr f arr = go
  where
    go !i !limit rest
      | i >= limit = rest
      | otherwise  = f (indexSmallArray arr i) (go (i + 1) limit rest)
{-# INLINE foldrArr #-}

foldr' :: (a -> b -> b) -> b -> Vector a -> b
foldr' f z0 v = foldl' (\k x -> k . f x) id v z0
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
  | otherwise =
      let (!z1, !off1) = ifoldNode f z0 0 (vShift v) (vRoot v)
      in fst (ifoldArr f z1 off1 (vTail v) 0 tailSz)
  where
    !n      = vSize v
    !tailSz = sizeofSmallArray (vTail v)
{-# INLINE ifoldl' #-}

ifoldNode :: (b -> Int -> a -> b) -> b -> Int -> Int -> Node a -> (b, Int)
ifoldNode f = go
  where
    go !z !off !_ Empty = (z, off)
    go !z !off !_ (Leaf arr) = ifoldArr f z off arr 0 bf
    go !z !off !level (Internal arr) =
      let goC !z' !off' !i
            | i >= bf = (z', off')
            | otherwise =
                let (!z'', !off'') = go z' off' (level - bfBits) (indexSmallArray arr i)
                in goC z'' off'' (i + 1)
      in goC z off 0

ifoldArr :: (b -> Int -> a -> b) -> b -> Int -> SmallArray a -> Int -> Int -> (b, Int)
ifoldArr f = go
  where
    go !z !off !arr !i !limit
      | i >= limit = (z, off)
      | otherwise  = go (f z off (indexSmallArray arr i)) (off + 1) arr (i + 1) limit

ifoldr :: (Int -> a -> b -> b) -> b -> Vector a -> b
ifoldr f z0 v
  | n == 0    = z0
  | otherwise = ifoldrChunks 0 (ifoldrChunk tail_ tailOff 0 tailSz z0)
  where
    !n       = vSize v
    !tailOff = tailOffset n
    !shift   = vShift v
    !root    = vRoot v
    !tail_   = vTail v
    !tailSz  = sizeofSmallArray tail_

    ifoldrChunks chunkStart tailResult
      | chunkStart >= tailOff = tailResult
      | otherwise =
          let !arr = leafFor shift chunkStart root
          in ifoldrChunk arr chunkStart 0 bf (ifoldrChunks (chunkStart + bf) tailResult)

    ifoldrChunk arr !base !i !limit rest
      | i >= limit = rest
      | otherwise  = f (base + i) (indexSmallArray arr i) (ifoldrChunk arr base (i + 1) limit rest)
{-# INLINE ifoldr #-}

foldMap :: Monoid m => (a -> m) -> Vector a -> m
foldMap f = foldl' (\acc a -> acc <> f a) mempty
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
sum = foldl' (+) 0
{-# INLINE sum #-}

product :: Num a => Vector a -> a
product = foldl' (*) 1
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
  -- result is in reverse, need to reverse in-place
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
toList v = build (\c n -> foldr c n v)
{-# INLINE toList #-}

fromVector :: F.Foldable f => f a -> Vector a
fromVector = F.foldl' snoc empty
{-# INLINE fromVector #-}

reverse :: Vector a -> Vector a
reverse v
  | n <= 1    = v
  | otherwise = create $ \mv -> do
      buf <- newSmallArray bf uninitElem
      let !tail_ = vTail v
          !tailSz = sizeofSmallArray tail_
      copyReversedArr buf 0 tail_ (tailSz - 1) tailSz
      (buf', off') <- revNodeBuffered buf tailSz (vShift v) (vRoot v) mv
      flushBuf buf' off' mv
  where
    !n = vSize v

    copyReversedArr !dst !dstOff !src !srcIdx !count = do
      let go !di !si !c
            | c <= 0    = pure ()
            | otherwise = do
                writeSmallArray dst di (indexSmallArray src si)
                go (di + 1) (si - 1) (c - 1)
      go dstOff srcIdx count

    revNodeBuffered !buf !off !_ Empty !_ = pure (buf, off)
    revNodeBuffered !buf !off !_ (Leaf arr) !mv
      | off == 0 = do
          revArr <- reverseSmallArr arr bf
          mPushChunk mv revArr
          pure (buf, 0)
      | otherwise = do
          let !avail = bf - off
          copyReversedArr buf off arr (bf - 1) avail
          frozen <- unsafeFreezeSmallArray buf
          mPushChunk mv frozen
          newBuf <- newSmallArray bf uninitElem
          let !remaining = off
          copyReversedArr newBuf 0 arr (off - 1) remaining
          pure (newBuf, remaining)
    revNodeBuffered !buf !off !level (Internal arr) !mv = do
      let go !b !o !i
            | i < 0     = pure (b, o)
            | otherwise = do
                (b', o') <- revNodeBuffered b o (level - bfBits) (indexSmallArray arr i) mv
                go b' o' (i - 1)
      go buf off (bf - 1)

    reverseSmallArr !src !len = do
      marr <- newSmallArray len uninitElem
      let go !i !j
            | i >= len  = pure ()
            | otherwise = do
                writeSmallArray marr i (indexSmallArray src j)
                go (i + 1) (j - 1)
      go 0 (len - 1)
      unsafeFreezeSmallArray marr
{-# INLINE reverse #-}

------------------------------------------------------------------------
-- Chunk-based operations
------------------------------------------------------------------------

-- | Fold over each leaf chunk and the tail as SmallArrays.
-- The callback receives the chunk offset and array.
foldChunks :: (b -> Int -> SmallArray a -> b) -> b -> Vector a -> b
foldChunks f z0 v
  | n == 0    = z0
  | otherwise = f (goTree z0 0) tailOff (vTail v)
  where
    !n       = vSize v
    !tailOff = tailOffset n
    !shift   = vShift v
    !root    = vRoot v
    goTree !z chunkStart
      | chunkStart >= tailOff = z
      | otherwise =
          let !arr = leafFor shift chunkStart root
          in goTree (f z chunkStart arr) (chunkStart + bf)
{-# INLINE foldChunks #-}

-- | Strict fold over chunks.
foldChunks' :: (b -> Int -> SmallArray a -> b) -> b -> Vector a -> b
foldChunks' = foldChunks
{-# INLINE foldChunks' #-}

-- | Map over each leaf chunk, producing new SmallArrays.
-- The callback receives the chunk offset and array.
mapChunks :: (Int -> SmallArray a -> SmallArray b) -> Vector a -> Vector b
mapChunks f v
  | n == 0    = empty
  | otherwise = Vector n (vShift v) newRoot newTail
  where
    !n = vSize v
    !tailOff = tailOffset n
    !newTail = f tailOff (vTail v)
    !newRoot = mapChunksNode (vShift v) 0 f (vRoot v) tailOff
{-# INLINE mapChunks #-}

mapChunksNode :: Int -> Int -> (Int -> SmallArray a -> SmallArray b) -> Node a -> Int -> Node b
mapChunksNode _ _ _ Empty _ = Empty
mapChunksNode _ offset f (Leaf arr) _ = Leaf (f offset arr)
mapChunksNode shift offset f (Internal arr) tailOff = Internal $ runST $ do
  let !n = sizeofSmallArray arr
  marr <- newSmallArray n Empty
  let go !i !off
        | i >= n || off >= tailOff = pure ()
        | otherwise = do
            let !child = indexSmallArray arr i
                !childSize = unsafeShiftL 1 shift
            writeSmallArray marr i (mapChunksNode (shift - bfBits) off f child tailOff)
            go (i + 1) (off + childSize)
  go 0 offset
  unsafeFreezeSmallArray marr

-- | Perform a monadic action for each chunk.
forChunks_ :: Monad m => Vector a -> (Int -> SmallArray a -> m ()) -> m ()
forChunks_ v f
  | n == 0    = pure ()
  | otherwise = goTree 0 >> f tailOff (vTail v)
  where
    !n       = vSize v
    !tailOff = tailOffset n
    !shift   = vShift v
    !root    = vRoot v
    goTree chunkStart
      | chunkStart >= tailOff = pure ()
      | otherwise = do
          f chunkStart (leafFor shift chunkStart root)
          goTree (chunkStart + bf)
{-# INLINE forChunks_ #-}

------------------------------------------------------------------------
-- Internal helpers
------------------------------------------------------------------------

-- | Iterate over all elements with index via direct tree walk.
forI_ :: Vector a -> (Int -> a -> ST s ()) -> ST s ()
forI_ v f
  | n == 0    = pure ()
  | otherwise = do
      off <- forNodeI_ f 0 (vShift v) (vRoot v)
      forArrI_ f off (vTail v) 0 (sizeofSmallArray (vTail v))
      pure ()
  where !n = vSize v
{-# INLINE forI_ #-}

{-# INLINEABLE forNodeI_ #-}
forNodeI_ :: (Int -> a -> ST s ()) -> Int -> Int -> Node a -> ST s Int
forNodeI_ _ off _ Empty = pure off
forNodeI_ f off _ (Leaf arr) = forArrI_ f off arr 0 bf
forNodeI_ f off level (Internal arr) = do
  let go !o !i
        | i >= bf = pure o
        | otherwise = do
            o' <- forNodeI_ f o (level - bfBits) (indexSmallArray arr i)
            go o' (i + 1)
  go off 0

{-# INLINEABLE forArrI_ #-}
forArrI_ :: (Int -> a -> ST s ()) -> Int -> SmallArray a -> Int -> Int -> ST s Int
forArrI_ f = go
  where
    go !off !arr !i !limit
      | i >= limit = pure off
      | otherwise = f off (indexSmallArray arr i) >> go (off + 1) arr (i + 1) limit

-- | Iterate over all elements via direct tree walk (no index).
-- Tree walk is in the where clause so INLINE exposes it and the
-- callback @f@ gets specialized at each call site.
forEach_ :: Vector a -> (a -> ST s ()) -> ST s ()
forEach_ v f
  | vSize v == 0 = pure ()
  | otherwise = do
      goNode (vShift v) (vRoot v)
      goArr (vTail v) 0 (sizeofSmallArray (vTail v))
  where
    goNode !_ Empty = pure ()
    goNode !_ (Leaf arr) = goArr arr 0 bf
    goNode !level (Internal arr) = do
      let go !i
            | i >= bf = pure ()
            | otherwise = goNode (level - bfBits) (indexSmallArray arr i) >> go (i + 1)
      go 0

    goArr !arr !i !limit
      | i >= limit = pure ()
      | otherwise = f (indexSmallArray arr i) >> goArr arr (i + 1) limit
{-# INLINE forEach_ #-}

append :: Vector a -> Vector a -> Vector a
append v1 v2
  | null v1   = v2
  | null v2   = v1
  | otherwise = create $ \mv -> appendChunked mv v1 v2
{-# INLINE append #-}

appendChunked :: MVector s a -> Vector a -> Vector a -> ST s ()
appendChunked mv v1 v2 = do
  pushNodeChunks (vShift v1) (vRoot v1) mv
  let !k = sizeofSmallArray (vTail v1)
  if k == bf
    then do
      mPushChunk mv (vTail v1)
      pushNodeChunks (vShift v2) (vRoot v2) mv
      pushTailElems (vTail v2) (sizeofSmallArray (vTail v2)) mv
    else do
      buf <- newSmallArray bf uninitElem
      copySmallArray buf 0 (vTail v1) 0 k
      (buf', off') <- pushNodeBuffered buf k (vShift v2) (vRoot v2) mv
      (buf'', off'') <- copyBuffered buf' off' (vTail v2) 0 (sizeofSmallArray (vTail v2)) mv
      flushBuf buf'' off'' mv
{-# INLINE appendChunked #-}

pushNodeChunks :: Int -> Node a -> MVector s a -> ST s ()
pushNodeChunks !_ Empty !_ = pure ()
pushNodeChunks !_ (Leaf arr) !mv = mPushChunk mv arr
pushNodeChunks !level (Internal arr) !mv = do
  let go !i
        | i >= bf = pure ()
        | otherwise = pushNodeChunks (level - bfBits) (indexSmallArray arr i) mv >> go (i + 1)
  go 0
{-# INLINEABLE pushNodeChunks #-}

pushNodeBuffered :: SmallMutableArray s a -> Int -> Int -> Node a -> MVector s a
                 -> ST s (SmallMutableArray s a, Int)
pushNodeBuffered !buf !off !_ Empty !_ = pure (buf, off)
pushNodeBuffered !buf !off !_ (Leaf arr) !mv
  | off == 0 = do
      mPushChunk mv arr
      pure (buf, 0)
  | otherwise = copyBuffered buf off arr 0 bf mv
pushNodeBuffered !buf !off !level (Internal arr) !mv = do
  let go !b !o !i
        | i >= bf = pure (b, o)
        | otherwise = do
            (b', o') <- pushNodeBuffered b o (level - bfBits) (indexSmallArray arr i) mv
            go b' o' (i + 1)
  go buf off 0
{-# INLINEABLE pushNodeBuffered #-}

copyBuffered :: SmallMutableArray s a -> Int -> SmallArray a -> Int -> Int -> MVector s a
             -> ST s (SmallMutableArray s a, Int)
copyBuffered !buf !bufOff !src !srcOff !srcLen !mv = go buf bufOff srcOff srcLen
  where
    go !b !bo !so !sl
      | sl <= 0 = pure (b, bo)
      | otherwise = do
          let !avail = bf - bo
              !toCopy = min sl avail
          copySmallArray b bo src so toCopy
          let !newBo = bo + toCopy
          if newBo >= bf
            then do
              frozen <- unsafeFreezeSmallArray b
              mPushChunk mv frozen
              newBuf <- newSmallArray bf uninitElem
              go newBuf 0 (so + toCopy) (sl - toCopy)
            else
              pure (b, newBo)
{-# INLINEABLE copyBuffered #-}

flushBuf :: SmallMutableArray s a -> Int -> MVector s a -> ST s ()
flushBuf !buf !off !mv = go 0
  where
    go !i
      | i >= off = pure ()
      | otherwise = do
          x <- readSmallArray buf i
          mPush mv x
          go (i + 1)
{-# INLINE flushBuf #-}

pushTailElems :: SmallArray a -> Int -> MVector s a -> ST s ()
pushTailElems !arr !len !mv = go 0
  where
    go !i
      | i >= len  = pure ()
      | otherwise = mPush mv (indexSmallArray arr i) >> go (i + 1)
{-# INLINE pushTailElems #-}

uninitElem :: a
uninitElem = error "pvector: uninitialised element"
{-# NOINLINE uninitElem #-}

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
  copySmallArray mtail 0 (vTail v) 0 tailSz
  ref <- newMutVar $! MVState
    { mvSize     = vSize v
    , mvShift    = vShift v
    , mvRoot     = thawNode (vRoot v)
    , mvTail     = mtail
    , mvTailSize = tailSz
    }
  pure (MVector ref)
  where
    !tailSz = sizeofSmallArray (vTail v)
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
      !frozenRoot = case frozenRoot0 of
        Empty -> emptyRoot
        _     -> frozenRoot0
  pure $! Vector (mvSize st) (mvShift st) frozenRoot tail_
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
  let !tailOff = tailOffset (mvSize st)
  if i >= tailOff
    then readSmallArray (mvTail st) (i .&. bfMask)
    else mReadTree (mvShift st) i (mvRoot st)
{-# INLINE mRead #-}

mReadTree :: PrimMonad m => Int -> Int -> MNode (PrimState m) a -> m a
mReadTree shift i = go shift
  where
    go level node = case node of
      MInternal arr -> readSmallArray arr (indexAtLevel i level) >>= go (level - bfBits)
      MLeaf arr     -> readSmallArray arr (i .&. bfMask)
      Frozen n      -> pure $! readFrozen level n
    readFrozen level (Internal arr) =
      readFrozen (level - bfBits) (indexSmallArray arr (indexAtLevel i level))
    readFrozen _ (Leaf arr) = indexSmallArray arr (i .&. bfMask)
    readFrozen _ _ = error "pvector: mReadTree invariant violation"

mWrite :: PrimMonad m => MVector (PrimState m) a -> Int -> a -> m ()
mWrite mv i x = do
  st <- getMV mv
  let !tailOff = tailOffset (mvSize st)
  if i >= tailOff
    then writeSmallArray (mvTail st) (i .&. bfMask) x
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
      let !willOverflow = unsafeShiftR n bfBits > unsafeShiftL 1 (mvShift st)
      frozenTail <- unsafeFreezeSmallArray (mvTail st)
      (newRoot, newShift) <-
        if willOverflow
          then do
            arr <- newSmallArray bf (Frozen Empty)
            writeSmallArray arr 0 (mvRoot st)
            let !path = thawNode (newPath (mvShift st) (Leaf frozenTail))
            writeSmallArray arr 1 path
            pure (MInternal arr, mvShift st + bfBits)
          else do
            r <- mPushTail n (mvShift st) (mvRoot st) frozenTail
            pure (r, mvShift st)
      newTail <- newSmallArray bf undefinedElem'
      writeSmallArray newTail 0 x
      putMV mv $! MVState (n + 1) newShift newRoot newTail 1
  where
    undefinedElem' = error "pvector: uninitialised element"
{-# INLINE mPush #-}

-- | Push a full 32-element SmallArray directly as a new leaf chunk.
-- The current tail MUST be empty (tailSize == 0).  This is much faster
-- than pushing elements one at a time because it skips per-element
-- tail filling entirely.  Call only when the tail has just been
-- flushed (size is a multiple of 32) or on a fresh mutable vector.
-- | Push a full 32-element SmallArray as a new leaf, bypassing
-- per-element tail filling.  Tail MUST be empty (tailSize == 0).
mPushChunk :: PrimMonad m => MVector (PrimState m) a -> SmallArray a -> m ()
mPushChunk mv chunk = do
  st <- getMV mv
  let !n  = mvSize st
      !n' = n + bf
      -- mPushTail navigates using (size-1), and n' is the size AFTER adding.
      -- So (n'-1) = (n+31) gives the index of the last element in this chunk.
      !willOverflow = unsafeShiftR n' bfBits > unsafeShiftL 1 (mvShift st)
  (newRoot, newShift) <-
    if willOverflow
      then do
        arr <- newSmallArray bf (Frozen Empty)
        writeSmallArray arr 0 (mvRoot st)
        let !path = thawNode (newPath (mvShift st) (Leaf chunk))
        writeSmallArray arr 1 path
        pure (MInternal arr, mvShift st + bfBits)
      else do
        r <- mPushTail n' (mvShift st) (mvRoot st) chunk
        pure (r, mvShift st)
  newTail <- newSmallArray bf undefinedElem'
  putMV mv $! MVState n' newShift newRoot newTail 0
  where
    undefinedElem' = error "pvector: uninitialised element"
{-# INLINE mPushChunk #-}

mPushTail
  :: PrimMonad m
  => Int -> Int -> MNode (PrimState m) a -> SmallArray a
  -> m (MNode (PrimState m) a)
mPushTail size shift root tailArr = go shift root
  where
    go level node = do
      arr <- editableInternal node
      let !subIdx = indexAtLevel (size - 1) level
      if level == bfBits
        then do
          writeSmallArray arr subIdx (Frozen (Leaf tailArr))
          pure (MInternal arr)
        else do
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

-- | Convert a vector to a 'Bundle' (chunk-aware for efficiency).
stream :: Vector a -> Bundle a
stream v = Bundle (MStream step (SInit 0)) (Exact n)
  where
    !n       = vSize v
    !tailOff = tailOffset n
    !shift   = vShift v
    !root    = vRoot v
    !tail_   = vTail v
    !tailSz  = sizeofSmallArray tail_

    step (SInit i)
      | i >= n    = Id Done
      | i >= tailOff = Id (Yield (indexSmallArray tail_ 0) (STail 1))
      | otherwise =
          let !arr = leafFor shift i root
          in Id (Yield (indexSmallArray arr 0) (SChunk 1 arr i))
    step (SChunk j arr base)
      | j >= bf =
          let !nextBase = base + bf
          in if nextBase >= tailOff
               then if tailSz > 0
                      then Id (Yield (indexSmallArray tail_ 0) (STail 1))
                      else Id Done
               else let !arr' = leafFor shift nextBase root
                    in Id (Yield (indexSmallArray arr' 0) (SChunk 1 arr' nextBase))
      | otherwise = Id (Yield (indexSmallArray arr j) (SChunk (j + 1) arr base))
    step (STail j)
      | j >= tailSz = Id Done
      | otherwise   = Id (Yield (indexSmallArray tail_ j) (STail (j + 1)))
{-# INLINE [0] stream #-}

data SState a
  = SInit  {-# UNPACK #-} !Int
  | SChunk {-# UNPACK #-} !Int !(SmallArray a) {-# UNPACK #-} !Int
  | STail  {-# UNPACK #-} !Int

-- | Build a vector from a 'Bundle'. Defined as @new . fill@.
unstream :: Bundle a -> Vector a
unstream = new . fill
{-# INLINE [0] unstream #-}

------------------------------------------------------------------------
-- Recycling framework (Leshchinskiy, "Recycle Your Arrays!", 2008)
------------------------------------------------------------------------

-- | A deferred mutable vector initializer. Wraps a monadic computation
-- that allocates and populates a mutable vector. The 'New' type is the
-- key to the recycling optimization: consecutive operations on 'New'
-- values share a single mutable vector allocation instead of freezing
-- and re-cloning between each step.
data New a = New (forall s. ST s (MVector s a))

-- | Materialize a 'New' into a persistent 'Vector' by running the
-- initializer and freezing the result.
new :: New a -> Vector a
new (New mk) = runST (mk >>= unsafeFreeze)
{-# INLINE [0] new #-}

-- | Initialize a 'New' from a 'Bundle'. This is the bridge between
-- stream fusion and the recycling framework.
fill :: Bundle a -> New a
fill (Bundle (MStream step s0) _) = New $ do
  mv <- mNew
  let go s = case unId (step s) of
        Done       -> pure mv
        Skip    s' -> go s'
        Yield a s' -> mPush mv a >> go s'
  go s0
{-# INLINE [1] fill #-}

-- | Create a 'New' by cloning a persistent vector. Defined as
-- @fill . stream@.
clone :: Vector a -> New a
clone = fill . stream
{-# INLINE [0] clone #-}

-- | Perform a bulk update on a 'New' in-place.
updateNew :: New a -> [(Int, a)] -> New a
updateNew (New mk) ps = New $ do
  mv <- mk
  Prelude.mapM_ (\(i, x) -> mWrite mv i x) ps
  pure mv
{-# INLINE updateNew #-}

-- | Apply a same-type function to every element of a 'New' in-place,
-- walking the trie structure directly (O(n), no intermediate allocation).
mapNew :: (a -> a) -> New a -> New a
mapNew f (New mk) = New $ do
  mv <- mk
  mMapInPlace f mv
  pure mv
{-# INLINE [0] mapNew #-}

-- | Apply an arbitrary ST action to the mutable vector inside a 'New'.
transformNew :: (forall s. MVector s a -> ST s ()) -> New a -> New a
transformNew act (New mk) = New $ do
  mv <- mk
  act mv
  pure mv
{-# INLINE transformNew #-}

-- | Execute a monadic stream transformer in-place on the mutable
-- vector inside a 'New'. This is the key combinator from the
-- recycling paper: it reads from the mutable vector, transforms
-- via the stream, and writes back in-place.
transform :: (forall m. Monad m => MStream m a -> MStream m a)
          -> (Size -> Size) -> New a -> New a
{-# INLINE [1] transform #-}
transform f _ (New mk) = New $ do
  mv <- mk
  n <- mLength mv
  consumeMStream mv (f (mstreamMV mv n))
  pure mv

-- | Stream elements from a mutable vector (read in sequence).
mstreamMV :: MVector s a -> Int -> MStream (ST s) a
mstreamMV mv n = MStream step 0
  where
    step i
      | i >= n    = pure Done
      | otherwise = do
          x <- mRead mv i
          pure (Yield x (i + 1))
{-# INLINE mstreamMV #-}

-- | Write a monadic stream into a mutable vector, overwriting
-- elements starting at index 0. Keeps the existential contained.
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

-- | In-place map over a mutable vector. Walks the trie, applying the
-- function to each leaf element. Frozen (shared) nodes are cloned on
-- write.
mMapInPlace :: (a -> a) -> MVector s a -> ST s ()
mMapInPlace f mv = do
  st <- getMV mv
  newRoot <- mMapNodeInPlace f (mvShift st) (mvRoot st)
  mapMutableArr f (mvTail st) 0 (mvTailSize st)
  putMV mv st { mvRoot = newRoot }
{-# INLINE mMapInPlace #-}

-- | Version of 'map' restricted to @(a -> a)@, enabling in-place
-- execution via the recycling framework.  When the recycling rule fires,
-- this becomes @new (mapNew f (clone v))@ which maps in-place on the
-- mutable trie.  When it doesn't fire, it falls back to the stream path.
inplace_map :: (a -> a) -> Vector a -> Vector a
inplace_map f = new . mapNew f . clone
{-# INLINE [1] inplace_map #-}

-- | Lift a pure function into a polymorphic monadic stream transformer
-- (needed for 'inplace' and 'transform').
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
--
-- The six core rules from the paper:
--
--   fusion:    stream (new (fill s))       → s
--   recycling: fill (stream (new p))       → p
--   inplace:   fill (smap f (stream (new p))) → mapNew f p   (when a→a)
--   uninplace: stream (new (mapNew f p))   → smap f (stream (new p))
--   inplace2:  mapNew f (mapNew g p)       → mapNew (f . g) p
--   mfusion:   transformNew f (transformNew g p) → transformNew (g >> f) p
--
-- Plus stream‐level forwarding for map/filter/fold.

{-# RULES

-- === Core fusion and recycling rules ===

-- Eliminates stream→new→fill→stream roundtrip
"pvector/fusion"
  forall s. stream (new (fill s)) = s

-- Eliminates fill→stream→new roundtrip (array recycling)
"pvector/recycling"
  forall p. fill (stream (new p)) = p

-- === Transform/New rules (from vector package's New.hs) ===

-- When a transform follows an unstream, fold it into the bundle via inplace
"pvector/transform/fill [New]"
  forall (f :: forall m. Monad m => MStream m a -> MStream m a)
         g s.
  transform f g (fill s) = fill (S.inplace f g s)

-- Compose adjacent transforms
"pvector/transform/transform [New]"
  forall (f1 :: forall m. Monad m => MStream m a -> MStream m a)
         (f2 :: forall m. Monad m => MStream m a -> MStream m a)
         g1 g2 p.
  transform f1 g1 (transform f2 g2 p) = transform (f1 . f2) (g1 . g2) p

-- === In-place map recycling ===

-- Compose adjacent in-place maps
"pvector/mapNew/mapNew" forall f g p.
  mapNew f (mapNew g p) = mapNew (f . g) p

-- Uninplace: undo in-place when result is immediately streamed
"pvector/uninplace/map" forall f p.
  stream (new (mapNew f p)) = S.smap f (stream (new p))

-- === Stream forwarding: expose stream form for cross-operation fusion ===
"pvector/map [stream]" [~1] forall f v.
  map f v = unstream (S.smap f (stream v))
"pvector/filter [stream]" [~1] forall f v.
  filter f v = unstream (S.sfilter f (stream v))
"pvector/foldl' [stream]" [~1] forall f z v.
  foldl' f z v = S.sfoldl' f z (stream v)
"pvector/foldr [stream]" [~1] forall f z v.
  foldr f z v = S.sfoldr f z (stream v)

-- === Fallback rules: revert to direct implementations when not fused ===
"pvector/map [direct]" [1] forall f v.
  unstream (S.smap f (stream v)) = mapDirect f v
"pvector/filter [direct]" [1] forall f v.
  unstream (S.sfilter f (stream v)) = filterDirect f v
"pvector/foldl' [direct]" [1] forall f z v.
  S.sfoldl' f z (stream v) = foldlDirect f z v
"pvector/foldr [direct]" [1] forall f z v.
  S.sfoldr f z (stream v) = foldrDirect f z v

  #-}

------------------------------------------------------------------------
-- Internal tree operations
------------------------------------------------------------------------

-- | Navigate the trie to the leaf containing index i.
-- Unrolls the first 3 levels of descent (covers vectors up to 32^4 = ~1M).
leafFor :: Int -> Int -> Node a -> SmallArray a
leafFor !level0 !i node0 = case node0 of
  Leaf arr -> arr
  Internal arr0
    | level0 == bfBits ->
        -- Depth 1: root children are leaves
        case indexSmallArray arr0 (indexAtLevel i level0) of
          Leaf arr -> arr
          _ -> error "pvector: leafFor invariant"
    | level0 == 2 * bfBits ->
        -- Depth 2: root → internal → leaf
        case indexSmallArray arr0 (indexAtLevel i level0) of
          Internal arr1 ->
            case indexSmallArray arr1 (indexAtLevel i bfBits) of
              Leaf arr -> arr
              _ -> error "pvector: leafFor invariant"
          _ -> error "pvector: leafFor invariant"
    | otherwise -> goDeep level0 i node0
  Empty -> error "pvector: leafFor hit Empty"
  where
    goDeep !level !i (Internal arr) =
      goDeep (level - bfBits) i (indexSmallArray arr (indexAtLevel i level))
    goDeep _ _ (Leaf arr) = arr
    goDeep _ _ Empty = error "pvector: leafFor hit Empty"
{-# INLINEABLE leafFor #-}

updateNode :: Int -> Int -> a -> Node a -> Node a
updateNode = go
  where
    go !level !i x (Internal arr) =
      let !subIdx = indexAtLevel i level
          !child  = indexSmallArray arr subIdx
      in Internal (cloneAndSet32 arr subIdx (go (level - bfBits) i x child))
    go _ !i x (Leaf arr) =
      Leaf (cloneAndSet32 arr (i .&. bfMask) x)
    go _ _ _ Empty = error "pvector: updateNode hit Empty"
{-# INLINE updateNode #-}

-- cloneAndSet32 is now in Internal.hs (Cmm primop)

newPath :: Int -> Node a -> Node a
newPath 0 node = node
newPath level node =
  let !arr = runST $ do
        a <- newSmallArray bf Empty
        writeSmallArray a 0 (newPath (level - bfBits) node)
        unsafeFreezeSmallArray a
  in Internal arr

pushTail :: Int -> Int -> Node a -> SmallArray a -> Node a
pushTail size shift root tailArr = go shift root
  where
    go level (Internal arr) =
      let !subIdx = indexAtLevel (size - 1) level
      in if level == bfBits
           then Internal (cloneAndSet arr subIdx (Leaf tailArr))
           else case indexSmallArray arr subIdx of
                  Empty ->
                    let !path = newPath (level - bfBits) (Leaf tailArr)
                    in Internal (cloneAndSet arr subIdx path)
                  child ->
                    Internal (cloneAndSet arr subIdx (go (level - bfBits) child))
    go _ _ = error "pvector: pushTail invariant violation"

popTail :: Int -> Int -> Node a -> Maybe (Node a)
popTail size shift root = go shift root
  where
    go level (Internal arr) =
      let !subIdx = indexAtLevel (size - 2) level
      in if level > bfBits
           then case go (level - bfBits) (indexSmallArray arr subIdx) of
                  Nothing
                    | subIdx == 0 -> Nothing
                    | otherwise   -> Just $ Internal (cloneAndSet arr subIdx Empty)
                  Just child' -> Just $ Internal (cloneAndSet arr subIdx child')
           else if subIdx == 0
                  then Nothing
                  else Just $ Internal (cloneAndSet arr subIdx Empty)
    go _ _ = Nothing

------------------------------------------------------------------------
-- Reverse fold (used by Front vector)
------------------------------------------------------------------------

rfoldl' :: (b -> a -> b) -> b -> Vector a -> b
rfoldl' f z0 v
  | n == 0    = z0
  | otherwise = foldChunksRL (foldChunkRL z0 tail_ (tailSz - 1)) (tailOff - bf)
  where
    !n       = vSize v
    !tailOff = tailOffset n
    !shift   = vShift v
    !root    = vRoot v
    !tail_   = vTail v
    !tailSz  = sizeofSmallArray tail_

    foldChunksRL !z chunkStart
      | chunkStart < 0 = z
      | otherwise =
          let !arr = leafFor shift chunkStart root
          in foldChunksRL (foldChunkRL z arr (bf - 1)) (chunkStart - bf)

    foldChunkRL !z arr !i
      | i < 0     = z
      | otherwise = foldChunkRL (f z (indexSmallArray arr i)) arr (i - 1)
{-# INLINE rfoldl' #-}

rfoldr :: (a -> b -> b) -> b -> Vector a -> b
rfoldr f z0 v
  | n == 0    = z0
  | otherwise =
      rfoldrChunkRL tail_ (tailSz - 1) (rfoldrChunksRL (tailOff - bf))
  where
    !n       = vSize v
    !tailOff = tailOffset n
    !shift   = vShift v
    !root    = vRoot v
    !tail_   = vTail v
    !tailSz  = sizeofSmallArray tail_

    rfoldrChunksRL chunkStart
      | chunkStart < 0 = z0
      | otherwise =
          let !arr = leafFor shift chunkStart root
          in rfoldrChunkRL arr (bf - 1) (rfoldrChunksRL (chunkStart - bf))

    rfoldrChunkRL arr !i rest
      | i < 0     = rest
      | otherwise = f (indexSmallArray arr i) (rfoldrChunkRL arr (i - 1) rest)
{-# INLINE rfoldr #-}
