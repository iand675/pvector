{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}

-- | Persistent vector with efficient prepend (cons) and random access.
--
-- Internally this is a 'Data.PVector.Back.Vector' with reversed
-- element ordering: @cons@ maps to @snoc@ on the underlying vector,
-- and index @i@ maps to @n − 1 − i@ internally.
--
-- * @cons@  — O(1) amortized
-- * @head@  — O(1)
-- * @index@ — O(log₃₂ n)
module Data.PVector.Front
  ( -- * Type
    FrontVector

    -- * Construction
  , empty
  , singleton
  , fromList
  , cons
  , (<|)

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
  , tail

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
import qualified GHC.Exts as Exts
import GHC.Base (build)

import qualified Data.PVector.Back as B

import Prelude hiding
  ( null, length, head, last, map, filter, reverse, take, drop
  , foldMap, tail, foldr, foldl'
  )
import qualified Data.List as L
import qualified Data.Foldable as F

-- | A persistent vector optimized for prepend (cons) operations.
newtype FrontVector a = FrontVector (B.Vector a)

------------------------------------------------------------------------
-- Instances
------------------------------------------------------------------------

instance Show a => Show (FrontVector a) where
  showsPrec p v = showsPrec p (toList v)

instance Eq a => Eq (FrontVector a) where
  v1 == v2 = toList v1 == toList v2

instance Ord a => Ord (FrontVector a) where
  compare v1 v2 = compare (toList v1) (toList v2)

instance Semigroup (FrontVector a) where
  a <> b = fromList (toList a ++ toList b)
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
  traverse f v = fromList <$> traverse f (toList v)

instance Exts.IsList (FrontVector a) where
  type Item (FrontVector a) = a
  fromList  = Data.PVector.Front.fromList
  toList    = Data.PVector.Front.toList

instance NFData a => NFData (FrontVector a) where
  rnf (FrontVector v) = rnf v

------------------------------------------------------------------------
-- Construction
------------------------------------------------------------------------

-- | O(1). The empty vector.
empty :: FrontVector a
empty = FrontVector B.empty
{-# INLINE empty #-}

-- | O(1). A vector with a single element.
singleton :: a -> FrontVector a
singleton x = FrontVector (B.singleton x)
{-# INLINE singleton #-}

-- | O(n). Build from a list, preserving order.
fromList :: [a] -> FrontVector a
fromList xs = FrontVector (B.fromList (L.reverse xs))
{-# INLINE fromList #-}

-- | O(1) amortized. Prepend an element.
cons :: a -> FrontVector a -> FrontVector a
cons x (FrontVector v) = FrontVector (B.snoc v x)
{-# INLINE cons #-}

-- | O(1) amortized. Infix 'cons'.
(<|) :: a -> FrontVector a -> FrontVector a
(<|) = cons
{-# INLINE (<|) #-}

------------------------------------------------------------------------
-- Query
------------------------------------------------------------------------

-- | O(1).
null :: FrontVector a -> Bool
null (FrontVector v) = B.null v
{-# INLINE null #-}

-- | O(1).
length :: FrontVector a -> Int
length (FrontVector v) = B.length v
{-# INLINE length #-}

------------------------------------------------------------------------
-- Indexing
------------------------------------------------------------------------

-- | O(log₃₂ n). Safe index.
index :: FrontVector a -> Int -> a
index (FrontVector v) i
  | i < 0 || i >= n = error "Data.PVector.Front.index: out of bounds"
  | otherwise = B.unsafeIndex v (n - 1 - i)
  where !n = B.length v
{-# INLINE index #-}

-- | O(log₃₂ n). Safe index returning Maybe.
(!?) :: FrontVector a -> Int -> Maybe a
(!?) (FrontVector v) i
  | i < 0 || i >= n = Nothing
  | otherwise = Just $! B.unsafeIndex v (n - 1 - i)
  where !n = B.length v
{-# INLINE (!?) #-}

-- | O(log₃₂ n). No bounds check.
unsafeIndex :: FrontVector a -> Int -> a
unsafeIndex (FrontVector v) i = B.unsafeIndex v (B.length v - 1 - i)
{-# INLINE unsafeIndex #-}

-- | O(1). First element (most recently consed). Partial.
head :: FrontVector a -> a
head (FrontVector v) = B.last v
{-# INLINE head #-}

-- | O(log₃₂ n). Last element. Partial.
last :: FrontVector a -> a
last (FrontVector v) = B.head v
{-# INLINE last #-}

------------------------------------------------------------------------
-- Deconstruction
------------------------------------------------------------------------

-- | O(1) amortized. Split into head and tail.
uncons :: FrontVector a -> Maybe (a, FrontVector a)
uncons (FrontVector v) = case B.unsnoc v of
  Nothing     -> Nothing
  Just (v', x) -> Just (x, FrontVector v')
{-# INLINE uncons #-}

-- | O(1) amortized. Everything except the first element. Partial.
tail :: FrontVector a -> FrontVector a
tail fv = case uncons fv of
  Nothing     -> error "Data.PVector.Front.tail: empty vector"
  Just (_, t) -> t
{-# INLINE tail #-}

------------------------------------------------------------------------
-- Transformations
------------------------------------------------------------------------

-- | O(n).
map :: (a -> b) -> FrontVector a -> FrontVector b
map f (FrontVector v) = FrontVector (B.map f v)
{-# INLINE map #-}

-- | O(n).
filter :: (a -> Bool) -> FrontVector a -> FrontVector a
filter p fv = fromList (L.filter p (toList fv))
{-# INLINE filter #-}

-- | O(n).
reverse :: FrontVector a -> FrontVector a
reverse (FrontVector v) = FrontVector v
{-# INLINE reverse #-}

-- | O(n). Take the first n logical elements.
take :: Int -> FrontVector a -> FrontVector a
take n fv
  | n <= 0          = empty
  | n >= length fv  = fv
  | otherwise       = fromList (L.take n (toList fv))
{-# INLINE take #-}

-- | O(n). Drop the first n logical elements.
drop :: Int -> FrontVector a -> FrontVector a
drop n fv
  | n <= 0          = fv
  | n >= length fv  = empty
  | otherwise       = fromList (L.drop n (toList fv))
{-# INLINE drop #-}

------------------------------------------------------------------------
-- Folds (traverse in logical order: newest-consed first)
------------------------------------------------------------------------

-- | O(n). Strict left fold in logical (front-to-back) order.
foldl' :: (b -> a -> b) -> b -> FrontVector a -> b
foldl' f z (FrontVector v) = B.rfoldl' f z v
{-# INLINE foldl' #-}

-- | O(n). Lazy right fold in logical order.
foldr :: (a -> b -> b) -> b -> FrontVector a -> b
foldr f z (FrontVector v) = B.rfoldr f z v
{-# INLINE foldr #-}

-- | O(n). foldMap in logical order.
foldMap :: Monoid m => (a -> m) -> FrontVector a -> m
foldMap f = foldl' (\acc a -> acc <> f a) mempty
{-# INLINE foldMap #-}

------------------------------------------------------------------------
-- Conversions
------------------------------------------------------------------------

-- | O(n). Convert to a list in logical (front-to-back) order.
toList :: FrontVector a -> [a]
toList fv = build (\c n -> Data.PVector.Front.foldr c n fv)
{-# INLINE toList #-}

-- | O(1). Extract the underlying back vector.
toBackVector :: FrontVector a -> B.Vector a
toBackVector (FrontVector v) = v
{-# INLINE toBackVector #-}

-- | O(1). Wrap a back vector as a front vector. The element ordering
-- will be reversed: the last element of the back vector becomes
-- the first element (head) of the front vector.
fromBackVector :: B.Vector a -> FrontVector a
fromBackVector = FrontVector
{-# INLINE fromBackVector #-}
