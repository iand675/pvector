{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}

-- | Persistent vector with efficient prepend (cons) and random access.
--
-- Internally this is a 'Data.PVector.Back.Vector' with reversed
-- element ordering: @cons@ maps to @snoc@ on the underlying vector,
-- and index @i@ maps to @n − 1 − i@ internally.
module Data.PVector.Front
  ( FrontVector

    -- * Construction
  , empty
  , singleton
  , fromList
  , cons
  , (<|)
  , snoc

    -- * Query
  , null
  , length

    -- * Indexing
  , index
  , (!?)
  , unsafeIndex
  , head
  , last

    -- * Deconstruction
  , uncons
  , unsnoc
  , tail
  , init

    -- * Transformations
  , map
  , filter
  , reverse
  , take
  , drop

    -- * Folds
  , foldl'
  , foldr
  , foldMap

    -- * Conversions
  , toList
  , toBackVector
  , fromBackVector
  ) where

import Control.DeepSeq (NFData(..))
import Control.Monad.ST (ST)
import qualified GHC.Exts as Exts
import GHC.Base (build)

import qualified Data.PVector.Back as B

import Prelude hiding
  ( null, length, head, last, map, filter, reverse, take, drop
  , foldMap, tail, init, foldr, foldl'
  )
import qualified Data.Foldable as F

newtype FrontVector a = FrontVector (B.Vector a)

instance Show a => Show (FrontVector a) where
  showsPrec p v = showsPrec p (toList v)

instance Eq a => Eq (FrontVector a) where
  FrontVector v1 == FrontVector v2
    | B.length v1 /= B.length v2 = False
    | otherwise = B.rfoldl' (\ok a -> ok && a) True (B.zipWith (==) v1 v2)

instance Ord a => Ord (FrontVector a) where
  compare v1 v2 = compare (toList v1) (toList v2)

instance Semigroup (FrontVector a) where
  FrontVector a <> FrontVector b = FrontVector $ B.create $ \mv -> do
    B.forEach_ a (B.mPush mv)
    B.forEach_ b (B.mPush mv)
  {-# INLINE (<>) #-}

instance Monoid (FrontVector a) where
  mempty = empty
  {-# INLINE mempty #-}

instance Functor FrontVector where
  fmap = Data.PVector.Front.map
  {-# INLINE fmap #-}

instance F.Foldable FrontVector where
  foldr   = Data.PVector.Front.foldr
  foldl'  = Data.PVector.Front.foldl'
  foldMap = Data.PVector.Front.foldMap
  null    = Data.PVector.Front.null
  length  = Data.PVector.Front.length
  toList  = Data.PVector.Front.toList
  {-# INLINE foldr #-}
  {-# INLINE foldl' #-}
  {-# INLINE foldMap #-}
  {-# INLINE null #-}
  {-# INLINE length #-}
  {-# INLINE toList #-}

instance Traversable FrontVector where
  traverse f v = foldl' (\acc a -> B.snoc <$> acc <*> f a) (Prelude.pure B.empty) v
    <&> FrontVector
    where
      (<&>) = flip Prelude.fmap

instance Exts.IsList (FrontVector a) where
  type Item (FrontVector a) = a
  fromList  = Data.PVector.Front.fromList
  toList    = Data.PVector.Front.toList

instance NFData a => NFData (FrontVector a) where
  rnf (FrontVector v) = rnf v

------------------------------------------------------------------------
-- Construction
------------------------------------------------------------------------

empty :: FrontVector a
empty = FrontVector B.empty
{-# INLINE empty #-}

singleton :: a -> FrontVector a
singleton x = FrontVector (B.singleton x)
{-# INLINE singleton #-}

-- | Build from a list. The first list element becomes the head.
-- Internally reverses into the underlying back vector.
fromList :: [a] -> FrontVector a
fromList xs = FrontVector $ B.create $ \mv ->
  let collect []     acc = pushAll mv acc
      collect (a:as) acc = collect as (a : acc)
  in collect xs []
{-# INLINE fromList #-}

pushAll :: B.MVector s a -> [a] -> ST s ()
pushAll mv = go
  where
    go []     = pure ()
    go (a:as) = B.mPush mv a >> go as

cons :: a -> FrontVector a -> FrontVector a
cons x (FrontVector v) = FrontVector (B.snoc v x)
{-# INLINE cons #-}

(<|) :: a -> FrontVector a -> FrontVector a
(<|) = cons
{-# INLINE (<|) #-}

-- | Append to the back. O(n) since the underlying vector must be rebuilt.
snoc :: FrontVector a -> a -> FrontVector a
snoc (FrontVector v) x = FrontVector $ B.create $ \mv -> do
  B.mPush mv x
  B.forEach_ v (B.mPush mv)
{-# INLINE snoc #-}

------------------------------------------------------------------------
-- Query
------------------------------------------------------------------------

null :: FrontVector a -> Bool
null (FrontVector v) = B.null v
{-# INLINE null #-}

length :: FrontVector a -> Int
length (FrontVector v) = B.length v
{-# INLINE length #-}

------------------------------------------------------------------------
-- Indexing
------------------------------------------------------------------------

index :: FrontVector a -> Int -> a
index (FrontVector v) i
  | i < 0 || i >= n = error "Data.PVector.Front.index: out of bounds"
  | otherwise = B.unsafeIndex v (n - 1 - i)
  where !n = B.length v
{-# INLINE index #-}

(!?) :: FrontVector a -> Int -> Maybe a
(!?) (FrontVector v) i
  | i < 0 || i >= n = Nothing
  | otherwise = Just $! B.unsafeIndex v (n - 1 - i)
  where !n = B.length v
{-# INLINE (!?) #-}

unsafeIndex :: FrontVector a -> Int -> a
unsafeIndex (FrontVector v) i = B.unsafeIndex v (B.length v - 1 - i)
{-# INLINE unsafeIndex #-}

head :: FrontVector a -> a
head (FrontVector v) = B.last v
{-# INLINE head #-}

last :: FrontVector a -> a
last (FrontVector v) = B.head v
{-# INLINE last #-}

------------------------------------------------------------------------
-- Deconstruction
------------------------------------------------------------------------

uncons :: FrontVector a -> Maybe (a, FrontVector a)
uncons (FrontVector v) = case B.unsnoc v of
  Nothing     -> Nothing
  Just (v', x) -> Just (x, FrontVector v')
{-# INLINE uncons #-}

unsnoc :: FrontVector a -> Maybe (FrontVector a, a)
unsnoc (FrontVector v)
  | B.length v == 0 = Nothing
  | otherwise =
      let !x = B.head v
          !v' = B.drop 1 v
      in Just (FrontVector v', x)
{-# INLINE unsnoc #-}

tail :: FrontVector a -> FrontVector a
tail fv = case uncons fv of
  Nothing     -> error "Data.PVector.Front.tail: empty"
  Just (_, t) -> t
{-# INLINE tail #-}

init :: FrontVector a -> FrontVector a
init fv = case unsnoc fv of
  Nothing     -> error "Data.PVector.Front.init: empty"
  Just (i, _) -> i
{-# INLINE init #-}

------------------------------------------------------------------------
-- Transformations
------------------------------------------------------------------------

map :: (a -> b) -> FrontVector a -> FrontVector b
map f (FrontVector v) = FrontVector (B.map f v)
{-# INLINE map #-}

filter :: (a -> Bool) -> FrontVector a -> FrontVector a
filter p (FrontVector v) = FrontVector $ B.create $ \mv ->
  B.rfoldr (\a rest -> (if p a then B.mPush mv a else pure ()) >> rest) (pure ()) v
{-# INLINE filter #-}

reverse :: FrontVector a -> FrontVector a
reverse (FrontVector v) = FrontVector v
{-# INLINE reverse #-}

take :: Int -> FrontVector a -> FrontVector a
take n (FrontVector v)
  | n <= 0        = empty
  | n >= B.length v = FrontVector v
  | otherwise     = FrontVector (B.drop (B.length v - n) v)
{-# INLINE take #-}

drop :: Int -> FrontVector a -> FrontVector a
drop n (FrontVector v)
  | n <= 0        = FrontVector v
  | n >= B.length v = empty
  | otherwise     = FrontVector (B.take (B.length v - n) v)
{-# INLINE drop #-}

------------------------------------------------------------------------
-- Folds
------------------------------------------------------------------------

foldl' :: (b -> a -> b) -> b -> FrontVector a -> b
foldl' f z (FrontVector v) = B.rfoldl' f z v
{-# INLINE foldl' #-}

foldr :: (a -> b -> b) -> b -> FrontVector a -> b
foldr f z (FrontVector v) = B.rfoldr f z v
{-# INLINE foldr #-}

foldMap :: Monoid m => (a -> m) -> FrontVector a -> m
foldMap f = foldl' (\acc a -> acc <> f a) mempty
{-# INLINE foldMap #-}

------------------------------------------------------------------------
-- Conversions
------------------------------------------------------------------------

toList :: FrontVector a -> [a]
toList fv = build (\c n -> Data.PVector.Front.foldr c n fv)
{-# INLINE toList #-}

toBackVector :: FrontVector a -> B.Vector a
toBackVector (FrontVector v) = v
{-# INLINE toBackVector #-}

fromBackVector :: B.Vector a -> FrontVector a
fromBackVector = FrontVector
{-# INLINE fromBackVector #-}
