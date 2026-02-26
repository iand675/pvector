{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}

-- | Double-ended persistent vector supporting efficient operations
-- at both ends (banker's deque backed by two 'Data.PVector.Back.Vector's).
--
-- * @cons@, @snoc@ — O(1) amortized
-- * @head@, @last@ — O(1)
-- * @uncons@, @unsnoc@ — O(1) amortized (O(n) worst-case rebalance)
-- * @index@ — O(log₃₂ n)
module Data.PVector.Deque
  ( -- * Type
    Deque

    -- * Construction
  , empty
  , singleton
  , fromList

    -- * Query
  , null
  , length

    -- * Adding elements
  , cons
  , snoc
  , (<|)
  , (|>)

    -- * Removing elements
  , uncons
  , unsnoc
  , head
  , last
  , tail
  , init

    -- * Indexing
  , index
  , (!?)

    -- * Transformations
  , map
  , filter
  , reverse

    -- * Folds
  , foldl'
  , foldr
  , foldMap

    -- * Conversions
  , toList
  , toBackVector
  ) where

import Control.DeepSeq (NFData(..))
import qualified GHC.Exts as Exts
import GHC.Base (build)

import Control.Monad (when)
import qualified Data.PVector.Back as B

import Prelude hiding
  ( null, length, head, last, map, filter, reverse
  , foldMap, tail, init, foldr
  )
import qualified Data.Foldable as F

-- | A double-ended persistent vector.
--
-- Internally represented as two back vectors: @dFront@ stores the
-- front half in reverse order (so @snoc@ on it is O(1) cons), and
-- @dBack@ stores the back half in forward order.
data Deque a = Deque
  { dFront :: !(B.Vector a)
  , dBack  :: !(B.Vector a)
  }

------------------------------------------------------------------------
-- Instances
------------------------------------------------------------------------

instance Show a => Show (Deque a) where
  showsPrec p d = showsPrec p (toList d)

instance Eq a => Eq (Deque a) where
  Deque f1 b1 == Deque f2 b2
    | length (Deque f1 b1) /= length (Deque f2 b2) = False
    | otherwise = B.rfoldl' (\ok x -> ok && x) True (B.zipWith (==) f1 f2)
                  && b1 == b2

instance Ord a => Ord (Deque a) where
  compare d1 d2 = go 0
    where
      !n1 = length d1
      !n2 = length d2
      !n = min n1 n2
      go i | i >= n = compare n1 n2
           | otherwise = case compare (index d1 i) (index d2 i) of
               EQ -> go (i + 1)
               x  -> x

instance Semigroup (Deque a) where
  Deque f1 b1 <> Deque f2 b2 = Deque f1 (B.create $ \mv -> do
    B.forEach_ b1 (B.mPush mv)
    B.rfoldr (\a rest -> B.mPush mv a >> rest) (pure ()) f2
    B.forEach_ b2 (B.mPush mv))

instance Monoid (Deque a) where
  mempty = empty

instance Functor Deque where
  fmap = Data.PVector.Deque.map

instance F.Foldable Deque where
  foldr   = Data.PVector.Deque.foldr
  foldl'  = Data.PVector.Deque.foldl'
  foldMap = Data.PVector.Deque.foldMap
  null    = Data.PVector.Deque.null
  length  = Data.PVector.Deque.length
  toList  = Data.PVector.Deque.toList

instance Traversable Deque where
  traverse f d = foldl' (\acc a -> snoc <$> acc <*> f a) (Prelude.pure empty) d

instance Exts.IsList (Deque a) where
  type Item (Deque a) = a
  fromList = Data.PVector.Deque.fromList
  toList   = Data.PVector.Deque.toList

instance NFData a => NFData (Deque a) where
  rnf (Deque f b) = rnf f `seq` rnf b

------------------------------------------------------------------------
-- Construction
------------------------------------------------------------------------

-- | O(1).
empty :: Deque a
empty = Deque B.empty B.empty
{-# INLINE empty #-}

-- | O(1).
singleton :: a -> Deque a
singleton x = Deque B.empty (B.singleton x)
{-# INLINE singleton #-}

-- | O(n).
fromList :: [a] -> Deque a
fromList xs = Deque B.empty (B.fromList xs)
{-# INLINE fromList #-}

------------------------------------------------------------------------
-- Query
------------------------------------------------------------------------

-- | O(1).
null :: Deque a -> Bool
null (Deque f b) = B.null f && B.null b
{-# INLINE null #-}

-- | O(1).
length :: Deque a -> Int
length (Deque f b) = B.length f + B.length b
{-# INLINE length #-}

------------------------------------------------------------------------
-- Adding elements
------------------------------------------------------------------------

-- | O(1) amortized. Prepend.
cons :: a -> Deque a -> Deque a
cons x (Deque f b) = Deque (B.snoc f x) b
{-# INLINE cons #-}

-- | O(1) amortized. Append.
snoc :: Deque a -> a -> Deque a
snoc (Deque f b) x = Deque f (B.snoc b x)
{-# INLINE snoc #-}

-- | O(1) amortized. Infix cons.
(<|) :: a -> Deque a -> Deque a
(<|) = cons
{-# INLINE (<|) #-}

-- | O(1) amortized. Infix snoc.
(|>) :: Deque a -> a -> Deque a
(|>) = snoc
{-# INLINE (|>) #-}

------------------------------------------------------------------------
-- Removing elements
------------------------------------------------------------------------

-- | O(1). First element. Partial.
head :: Deque a -> a
head d = case uncons d of
  Nothing    -> error "Data.PVector.Deque.head: empty deque"
  Just (x,_) -> x
{-# INLINE head #-}

-- | O(1). Last element. Partial.
last :: Deque a -> a
last d = case unsnoc d of
  Nothing    -> error "Data.PVector.Deque.last: empty deque"
  Just (_,x) -> x
{-# INLINE last #-}

-- | O(1) amortized. Remove the first element.
uncons :: Deque a -> Maybe (a, Deque a)
uncons (Deque f b)
  | not (B.null f) =
      case B.unsnoc f of
        Just (f', x) -> Just (x, Deque f' b)
        Nothing      -> Nothing
  | not (B.null b) =
      let !n  = B.length b
          !h  = (n + 1) `div` 2
          !f' = buildReversed b 0 (h - 1)
          !b' = B.drop h b
      in case B.unsnoc f' of
           Just (f'', x) -> Just (x, Deque f'' b')
           Nothing        -> Nothing
  | otherwise = Nothing
{-# INLINE uncons #-}

-- | O(1) amortized. Remove the last element.
unsnoc :: Deque a -> Maybe (Deque a, a)
unsnoc (Deque f b)
  | not (B.null b) =
      case B.unsnoc b of
        Just (b', x) -> Just (Deque f b', x)
        Nothing      -> Nothing
  | not (B.null f) =
      let !n  = B.length f
          !h  = (n + 1) `div` 2
          !b' = buildReversed f 0 (h - 1)
          !f' = B.drop h f
      in case B.unsnoc b' of
           Just (b'', x) -> Just (Deque f' b'', x)
           Nothing        -> Nothing
  | otherwise = Nothing
{-# INLINE unsnoc #-}

-- | O(1) amortized. Tail. Partial.
tail :: Deque a -> Deque a
tail d = case uncons d of
  Nothing    -> error "Data.PVector.Deque.tail: empty deque"
  Just (_,t) -> t
{-# INLINE tail #-}

-- | O(1) amortized. Init. Partial.
init :: Deque a -> Deque a
init d = case unsnoc d of
  Nothing    -> error "Data.PVector.Deque.init: empty deque"
  Just (i,_) -> i
{-# INLINE init #-}

-- Build a back vector from elements of src at indices [lo..hi] in reverse order.
buildReversed :: B.Vector a -> Int -> Int -> B.Vector a
buildReversed src lo hi = B.create $ \mv ->
  let go i
        | i < lo    = pure ()
        | otherwise = B.mPush mv (B.unsafeIndex src i) >> go (i - 1)
  in go hi

------------------------------------------------------------------------
-- Indexing
------------------------------------------------------------------------

-- | O(log₃₂ n). Index.
index :: Deque a -> Int -> a
index d i
  | i < 0 || i >= n = error "Data.PVector.Deque.index: out of bounds"
  | i < fl = B.unsafeIndex (dFront d) (fl - 1 - i)
  | otherwise = B.unsafeIndex (dBack d) (i - fl)
  where
    !fl = B.length (dFront d)
    !n  = fl + B.length (dBack d)
{-# INLINE index #-}

-- | O(log₃₂ n). Safe index.
(!?) :: Deque a -> Int -> Maybe a
(!?) d i
  | i < 0 || i >= n = Nothing
  | i < fl = Just $! B.unsafeIndex (dFront d) (fl - 1 - i)
  | otherwise = Just $! B.unsafeIndex (dBack d) (i - fl)
  where
    !fl = B.length (dFront d)
    !n  = fl + B.length (dBack d)
{-# INLINE (!?) #-}

------------------------------------------------------------------------
-- Transformations
------------------------------------------------------------------------

-- | O(n).
map :: (a -> b) -> Deque a -> Deque b
map f (Deque fr bk) = Deque (B.map f fr) (B.map f bk)
{-# INLINE map #-}

-- | O(n).
filter :: (a -> Bool) -> Deque a -> Deque a
filter p d = Deque B.empty (B.create $ \mv ->
  let go (Deque fr bk) = do
        B.rfoldr (\a rest -> when (p a) (B.mPush mv a) >> rest) (pure ()) fr
        B.forEach_ bk $ \a -> when (p a) (B.mPush mv a)
  in go d)
{-# INLINE filter #-}

-- | O(n).
reverse :: Deque a -> Deque a
reverse (Deque f b) = Deque b f
{-# INLINE reverse #-}

------------------------------------------------------------------------
-- Folds
------------------------------------------------------------------------

-- | O(n). Strict left fold.
foldl' :: (b -> a -> b) -> b -> Deque a -> b
foldl' f z (Deque fr bk) =
  let !z1 = B.rfoldl' f z fr
  in B.foldl' f z1 bk
{-# INLINE foldl' #-}

-- | O(n). Lazy right fold.
foldr :: (a -> b -> b) -> b -> Deque a -> b
foldr f z (Deque fr bk) =
  B.rfoldr f (B.foldr f z bk) fr
{-# INLINE foldr #-}

-- | O(n).
foldMap :: Monoid m => (a -> m) -> Deque a -> m
foldMap f = foldl' (\acc a -> acc <> f a) mempty
{-# INLINE foldMap #-}

------------------------------------------------------------------------
-- Conversions
------------------------------------------------------------------------

-- | O(n). Convert to a list.
toList :: Deque a -> [a]
toList d = build (\c n -> Data.PVector.Deque.foldr c n d)
{-# INLINE toList #-}

-- | O(n). Flatten into a single back vector.
toBackVector :: Deque a -> B.Vector a
toBackVector (Deque f b) = B.create $ \mv -> do
  B.rfoldr (\a rest -> B.mPush mv a >> rest) (pure ()) f
  B.forEach_ b (B.mPush mv)
{-# INLINE toBackVector #-}
