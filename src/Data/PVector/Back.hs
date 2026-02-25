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
  , mapMaybe
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
  , mPop

    -- * Stream fusion
  , stream
  , unstream

    -- * Internal (used by Front/Deque)
  , rfoldl'
  , rfoldr
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
import Data.PVector.Internal.Stream (Stream(..), Step(..), Size(..))
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
    | otherwise = eqChunks v1 v2

eqChunks :: Eq a => Vector a -> Vector a -> Bool
eqChunks v1 v2 = go 0
  where
    !n = vSize v1
    go i
      | i >= n    = True
      | otherwise = unsafeIndex v1 i == unsafeIndex v2 i && go (i + 1)

instance Ord a => Ord (Vector a) where
  compare v1 v2 = go 0
    where
      !n1 = vSize v1
      !n2 = vSize v2
      !n  = min n1 n2
      go i
        | i >= n = compare n1 n2
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
  traverse f v = fromList <$> Prelude.traverse f (toList v)
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

fromList :: [a] -> Vector a
fromList xs = create $ \mv ->
  let go []     = pure ()
      go (a:as) = mPush mv a >> go as
  in go xs
{-# INLINE [1] fromList #-}

fromListN :: Int -> [a] -> Vector a
fromListN n xs = create $ \mv ->
  let go _ []     = pure ()
      go 0 _      = pure ()
      go i (a:as) = mPush mv a >> go (i - 1) as
  in go n xs
{-# INLINE [1] fromListN #-}

replicate :: Int -> a -> Vector a
replicate n x
  | n <= 0    = empty
  | otherwise = create $ \mv ->
      let go 0 = pure ()
          go i = mPush mv x >> go (i - 1)
      in go n
{-# INLINE replicate #-}

generate :: Int -> (Int -> a) -> Vector a
generate n f
  | n <= 0    = empty
  | otherwise = create $ \mv ->
      let go i | i >= n = pure ()
               | otherwise = mPush mv (f i) >> go (i + 1)
      in go 0
{-# INLINE generate #-}

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
  forI_ v $ \_ a -> mPush mv a
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

(++) :: Vector a -> Vector a -> Vector a
(++) = append
{-# INLINE (++) #-}

concat :: [Vector a] -> Vector a
concat vs = create $ \mv ->
  let go []     = pure ()
      go (v:vs') = forI_ v (\_ a -> mPush mv a) >> go vs'
  in go vs
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
  | otherwise    = v { vRoot = updateNode (vShift v) i x (vRoot v) }
  where
    !tailOff = tailOffset (vSize v)
{-# INLINE update #-}

-- | Bulk update from a list of (index, value) pairs.
(//) :: Vector a -> [(Int, a)] -> Vector a
(//) v us = modify (\mv -> Prelude.mapM_ (\(i,x) -> mWrite mv i x) us) v
{-# INLINE (//) #-}

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
    !newRoot = mapNode (vShift v) f (vRoot v)
{-# INLINE mapDirect #-}

mapNode :: Int -> (a -> b) -> Node a -> Node b
mapNode _ _ Empty = Empty
mapNode _ f (Leaf arr) = Leaf (mapArray' f arr)
mapNode shift f (Internal arr) = Internal $ runST $ do
  let !n = sizeofSmallArray arr
  marr <- newSmallArray n Empty
  let go i
        | i >= n = pure ()
        | otherwise = do
            let !child = indexSmallArray arr i
            writeSmallArray marr i (mapNode (shift - bfBits) f child)
            go (i + 1)
  go 0
  unsafeFreezeSmallArray marr

imap :: (Int -> a -> b) -> Vector a -> Vector b
imap f v = generate (vSize v) (\i -> f i (unsafeIndex v i))
{-# INLINE imap #-}

concatMap :: (a -> Vector b) -> Vector a -> Vector b
concatMap f v = create $ \mv ->
  forI_ v $ \_ a -> forI_ (f a) $ \_ b -> mPush mv b
{-# INLINE concatMap #-}

mapMaybe :: (a -> Maybe b) -> Vector a -> Vector b
mapMaybe f v = create $ \mv ->
  forI_ v $ \_ a -> case f a of
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
mapM f v = fromList <$> Prelude.mapM f (toList v)
{-# INLINE mapM #-}

mapM_ :: Monad m => (a -> m b) -> Vector a -> m ()
mapM_ f v = Prelude.mapM_ f (toList v)
{-# INLINE mapM_ #-}

forM :: Monad m => Vector a -> (a -> m b) -> m (Vector b)
forM v f = mapM f v
{-# INLINE forM #-}

forM_ :: Monad m => Vector a -> (a -> m b) -> m ()
forM_ v f = mapM_ f v
{-# INLINE forM_ #-}

imapM :: Monad m => (Int -> a -> m b) -> Vector a -> m (Vector b)
imapM f v = fromList <$> Prelude.mapM (\i -> f i (unsafeIndex v i)) [0 .. vSize v - 1]
{-# INLINE imapM #-}

imapM_ :: Monad m => (Int -> a -> m ()) -> Vector a -> m ()
imapM_ f v = go 0
  where
    !n = vSize v
    go i | i >= n = pure ()
         | otherwise = f i (unsafeIndex v i) >> go (i + 1)
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

filter :: (a -> Bool) -> Vector a -> Vector a
filter p = filterDirect p
{-# NOINLINE [1] filter #-}

filterDirect :: (a -> Bool) -> Vector a -> Vector a
filterDirect p v = create $ \mv ->
  forI_ v $ \_ a -> when (p a) (mPush mv a)
{-# INLINE filterDirect #-}

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
  forI_ v $ \i a -> when (p a) (mPush mv i)
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

foldl' :: (b -> a -> b) -> b -> Vector a -> b
foldl' f z = foldlDirect f z
{-# NOINLINE [1] foldl' #-}

foldlDirect :: (b -> a -> b) -> b -> Vector a -> b
foldlDirect f z0 v
  | n == 0    = z0
  | otherwise = foldArr f (foldNode f z0 (vShift v) (vRoot v)) (vTail v) 0 tailSz
  where
    !n       = vSize v
    !tailSz  = sizeofSmallArray (vTail v)
{-# INLINE foldlDirect #-}

foldNode :: (b -> a -> b) -> b -> Int -> Node a -> b
foldNode f = go
  where
    go !z !_ Empty = z
    go !z !_ (Leaf arr) = foldArr f z arr 0 (sizeofSmallArray arr)
    go !z !level (Internal arr) =
      let !n = sizeofSmallArray arr
          goChildren !z' !i
            | i >= n = z'
            | otherwise = goChildren (go z' (level - bfBits) (indexSmallArray arr i)) (i + 1)
      in goChildren z 0
{-# INLINE foldNode #-}

foldArr :: (b -> a -> b) -> b -> SmallArray a -> Int -> Int -> b
foldArr f = go
  where
    go !z !arr !i !limit
      | i >= limit = z
      | otherwise  = go (f z (indexSmallArray arr i)) arr (i + 1) limit
{-# INLINE foldArr #-}

foldl1 :: (a -> a -> a) -> Vector a -> a
foldl1 f v
  | vSize v == 0 = error "Data.PVector.Back.foldl1: empty"
  | otherwise     = foldl f (unsafeIndex v 0) (drop 1 v)

foldl1' :: (a -> a -> a) -> Vector a -> a
foldl1' f v
  | vSize v == 0 = error "Data.PVector.Back.foldl1': empty"
  | otherwise     = foldl' f (unsafeIndex v 0) (drop 1 v)
{-# INLINE foldl1' #-}

foldr :: (a -> b -> b) -> b -> Vector a -> b
foldr f z = foldrDirect f z
{-# NOINLINE [1] foldr #-}

foldrDirect :: (a -> b -> b) -> b -> Vector a -> b
foldrDirect f z0 v
  | n == 0    = z0
  | otherwise = foldrNode f (vShift v) (vRoot v) (foldrArr f (vTail v) 0 tailSz z0)
  where
    !n      = vSize v
    !tailSz = sizeofSmallArray (vTail v)
{-# INLINE foldrDirect #-}

foldrNode :: (a -> b -> b) -> Int -> Node a -> b -> b
foldrNode f = go
  where
    go !_ Empty rest = rest
    go !_ (Leaf arr) rest = foldrArr f arr 0 (sizeofSmallArray arr) rest
    go !level (Internal arr) rest =
      let !n = sizeofSmallArray arr
          goChildren !i !r
            | i < 0     = r
            | otherwise = goChildren (i - 1) (go (level - bfBits) (indexSmallArray arr i) r)
      in goChildren (n - 1) rest
{-# INLINE foldrNode #-}

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
  | otherwise     = foldr f (Data.PVector.Back.last v) (Data.PVector.Back.init v)
{-# INLINE foldr1 #-}

foldr1' :: (a -> a -> a) -> Vector a -> a
foldr1' f v
  | vSize v == 0 = error "Data.PVector.Back.foldr1': empty"
  | otherwise     = foldr' f (Data.PVector.Back.last v) (Data.PVector.Back.init v)

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
    go !z !off !_ (Leaf arr) = ifoldArr f z off arr 0 (sizeofSmallArray arr)
    go !z !off !level (Internal arr) =
      let !n = sizeofSmallArray arr
          goC !z' !off' !i
            | i >= n = (z', off')
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
prescanl' f z v = generate (vSize v) step
  where
    step 0 = z
    step i = f (step (i - 1)) (unsafeIndex v (i - 1))
{-# INLINE prescanl' #-}

scanl :: (a -> b -> a) -> a -> Vector b -> Vector a
scanl f z v = generate (vSize v + 1) step
  where
    step 0 = z
    step i = f (step (i - 1)) (unsafeIndex v (i - 1))
{-# INLINE scanl #-}

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
scanr' f z v = reverse (scanl' (flip f) z (reverse v))
{-# INLINE scanr' #-}

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
  | otherwise = generate n (\i -> unsafeIndex v (n - 1 - i))
  where !n = vSize v
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

-- | Iterate over all elements with index, performing an ST action.
forI_ :: Vector a -> (Int -> a -> ST s ()) -> ST s ()
forI_ v f = go 0
  where
    !n = vSize v
    go i | i >= n = pure ()
         | otherwise = f i (unsafeIndex v i) >> go (i + 1)
{-# INLINE forI_ #-}

append :: Vector a -> Vector a -> Vector a
append v1 v2
  | null v1   = v2
  | null v2   = v1
  | otherwise = create $ \mv -> do
      forI_ v1 $ \_ a -> mPush mv a
      forI_ v2 $ \_ a -> mPush mv a
{-# INLINE append #-}

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
  mtail <- thawSmallArray (vTail v) 0 tailSz
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

stream :: Vector a -> Stream a
stream v = Stream step (SInit 0) (Exact n)
  where
    !n       = vSize v
    !tailOff = tailOffset n
    !shift   = vShift v
    !root    = vRoot v
    !tail_   = vTail v
    !tailSz  = sizeofSmallArray tail_

    step (SInit i)
      | i >= n    = Done
      | i >= tailOff = Yield (indexSmallArray tail_ 0) (STail 1)
      | otherwise =
          let !arr = leafFor shift i root
          in Yield (indexSmallArray arr 0) (SChunk 1 arr i)
    step (SChunk j arr base)
      | j >= bf =
          let !nextBase = base + bf
          in if nextBase >= tailOff
               then if tailSz > 0
                      then Yield (indexSmallArray tail_ 0) (STail 1)
                      else Done
               else let !arr' = leafFor shift nextBase root
                    in Yield (indexSmallArray arr' 0) (SChunk 1 arr' nextBase)
      | otherwise = Yield (indexSmallArray arr j) (SChunk (j + 1) arr base)
    step (STail j)
      | j >= tailSz = Done
      | otherwise   = Yield (indexSmallArray tail_ j) (STail (j + 1))
{-# INLINE [0] stream #-}

data SState a
  = SInit  {-# UNPACK #-} !Int
  | SChunk {-# UNPACK #-} !Int !(SmallArray a) {-# UNPACK #-} !Int
  | STail  {-# UNPACK #-} !Int

unstream :: Stream a -> Vector a
unstream (Stream step s0 _sz) = runST $ do
  mv <- mNew
  let go s = case step s of
        Done       -> unsafeFreeze mv
        Skip    s' -> go s'
        Yield a s' -> mPush mv a >> go s'
  go s0
{-# INLINE [0] unstream #-}

------------------------------------------------------------------------
-- Rewrite rules
------------------------------------------------------------------------

{-# RULES

"pvector/stream/unstream"
  forall s. stream (unstream s) = s

"pvector/map [stream]" [~1]
  forall f v. map f v = unstream (S.smap f (stream v))
"pvector/filter [stream]" [~1]
  forall p v. filter p v = unstream (S.sfilter p (stream v))
"pvector/foldl' [stream]" [~1]
  forall f z v. foldl' f z v = S.sfoldl' f z (stream v)
"pvector/foldr [stream]" [~1]
  forall f z v. foldr f z v = S.sfoldr f z (stream v)

"pvector/map [direct]" [1]
  forall f v. unstream (S.smap f (stream v)) = mapDirect f v
"pvector/filter [direct]" [1]
  forall p v. unstream (S.sfilter p (stream v)) = filterDirect p v
"pvector/foldl' [direct]" [1]
  forall f z v. S.sfoldl' f z (stream v) = foldlDirect f z v
"pvector/foldr [direct]" [1]
  forall f z v. S.sfoldr f z (stream v) = foldrDirect f z v

  #-}

------------------------------------------------------------------------
-- Internal tree operations
------------------------------------------------------------------------

leafFor :: Int -> Int -> Node a -> SmallArray a
leafFor = go
  where
    go !level !i (Internal arr) = go (level - bfBits) i (indexSmallArray arr (indexAtLevel i level))
    go _      _ (Leaf arr)      = arr
    go _      _ Empty           = error "pvector: leafFor hit Empty node"
{-# INLINE leafFor #-}

updateNode :: Int -> Int -> a -> Node a -> Node a
updateNode = go
  where
    go !level !i x (Internal arr) =
      let !subIdx = indexAtLevel i level
          !child  = indexSmallArray arr subIdx
      in Internal (cloneAndSet arr subIdx (go (level - bfBits) i x child))
    go _ !i x (Leaf arr) =
      Leaf (cloneAndSet arr (i .&. bfMask) x)
    go _ _ _ Empty = error "pvector: updateNode hit Empty"
{-# INLINE updateNode #-}

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
