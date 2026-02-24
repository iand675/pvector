{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}

-- | Persistent vector with efficient append (snoc) and random access.
--
-- Based on Clojure's persistent vector: a 32-way branching trie with
-- a tail buffer for amortized O(1) snoc. Random access is O(log₃₂ n).
--
-- For efficient prepend, see "Data.PVector.Front".
-- For efficient operations at both ends, see "Data.PVector.Deque".
module Data.PVector.Back
  ( -- * Persistent vector
    Vector

    -- * Construction
  , empty
  , singleton
  , fromList
  , replicate
  , generate
  , unfoldr

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

    -- * Update
  , update
  , adjust

    -- * Append / Remove
  , snoc
  , (|>)
  , unsnoc

    -- * Transformations
  , map
  , imap
  , reverse
  , filter
  , ifilter
  , take
  , drop

    -- * Folds
  , foldl'
  , ifoldl'
  , foldr
  , ifoldr
  , foldl1'
  , foldr1
  , foldMap
  , foldMap'

    -- * Zips
  , zip
  , zipWith
  , unzip

    -- * Conversions
  , toList
  , fromVector
  , toVector

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
  ) where

import Control.DeepSeq (NFData(..))
import Control.Monad (when)
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
  , foldMap, zip, zipWith, unzip
  , replicate, unfoldr, foldr, foldl', foldl1, foldr1
  )
import qualified Prelude
import qualified Data.Foldable as F

------------------------------------------------------------------------
-- Vector type
------------------------------------------------------------------------

-- | A persistent vector supporting O(1) amortized snoc (append)
-- and O(log₃₂ n) random access.
data Vector a = Vector
  { vSize  :: {-# UNPACK #-} !Int
  , vShift :: {-# UNPACK #-} !Int
  , vRoot  :: !(Node a)
  , vTail  :: {-# UNPACK #-} !(SmallArray a)
  }

------------------------------------------------------------------------
-- Mutable (transient) vector
------------------------------------------------------------------------

-- | A mutable vector backed by a transient trie. Use 'thaw' to create
-- one from a persistent 'Vector', and 'freeze' (or 'unsafeFreeze')
-- to convert back.
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
    | otherwise = toList v1 == toList v2

instance Ord a => Ord (Vector a) where
  compare v1 v2 = compare (toList v1) (toList v2)

instance Semigroup (Vector a) where
  (<>) = append
  {-# INLINE (<>) #-}

instance Monoid (Vector a) where
  mempty = empty
  {-# INLINE mempty #-}

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
  {-# INLINE foldr #-}
  {-# INLINE foldl' #-}
  {-# INLINE foldMap #-}
  {-# INLINE null #-}
  {-# INLINE length #-}
  {-# INLINE toList #-}

instance Traversable Vector where
  traverse f v = fromList <$> traverse f (toList v)
  {-# INLINE traverse #-}

instance Exts.IsList (Vector a) where
  type Item (Vector a) = a
  fromList  = Data.PVector.Back.fromList
  fromListN = \n xs -> Data.PVector.Back.fromList (Prelude.take n xs)
  toList    = Data.PVector.Back.toList
  {-# INLINE fromList #-}
  {-# INLINE toList #-}

instance NFData a => NFData (Vector a) where
  rnf (Vector _ _ root tail_) = rnf root `seq` rnfArray tail_

------------------------------------------------------------------------
-- Construction
------------------------------------------------------------------------

-- | O(1). The empty vector.
empty :: Vector a
empty = Vector 0 bfBits emptyRoot emptyTail
{-# INLINE empty #-}

-- | O(1). A vector with a single element.
singleton :: a -> Vector a
singleton x = Vector 1 bfBits emptyRoot tail_
  where
    !tail_ = runST $ do
      a <- newSmallArray 1 x
      unsafeFreezeSmallArray a
{-# INLINE singleton #-}

-- | O(n). Build a vector from a list.
fromList :: [a] -> Vector a
fromList xs = create $ \mv ->
  let go []     = pure ()
      go (a:as) = mPush mv a >> go as
  in go xs
{-# INLINE [1] fromList #-}

-- | O(n). Build a vector by replicating a value.
replicate :: Int -> a -> Vector a
replicate n x
  | n <= 0    = empty
  | otherwise = unstream (S.replicate n x)
{-# INLINE replicate #-}

-- | O(n). Build a vector from an index function.
generate :: Int -> (Int -> a) -> Vector a
generate n f
  | n <= 0    = empty
  | otherwise = unstream (S.generate n f)
{-# INLINE generate #-}

-- | O(n). Build a vector from an unfolding function.
unfoldr :: (b -> Maybe (a, b)) -> b -> Vector a
unfoldr f = unstream . S.unfoldr f
{-# INLINE unfoldr #-}

------------------------------------------------------------------------
-- Query
------------------------------------------------------------------------

-- | O(1). Is the vector empty?
null :: Vector a -> Bool
null v = vSize v == 0
{-# INLINE null #-}

-- | O(1). The number of elements.
length :: Vector a -> Int
length = vSize
{-# INLINE length #-}

------------------------------------------------------------------------
-- Indexing
------------------------------------------------------------------------

-- | O(log₃₂ n). Index with bounds checking.
index :: Vector a -> Int -> a
index v i
  | i < 0 || i >= vSize v = error $ "Data.PVector.Back.index: index " ++ show i
                                  ++ " out of bounds [0," ++ show (vSize v) ++ ")"
  | otherwise = unsafeIndex v i
{-# INLINE index #-}

-- | O(log₃₂ n). Index with bounds checking (operator form).
(!) :: Vector a -> Int -> a
(!) = index
{-# INLINE (!) #-}

-- | O(log₃₂ n). Safe indexing that returns Nothing for out-of-bounds.
(!?) :: Vector a -> Int -> Maybe a
(!?) v i
  | i < 0 || i >= vSize v = Nothing
  | otherwise = Just $! unsafeIndex v i
{-# INLINE (!?) #-}

-- | O(log₃₂ n). Index without bounds checking.
unsafeIndex :: Vector a -> Int -> a
unsafeIndex v i
  | i >= tailOff = indexSmallArray (vTail v) (i .&. bfMask)
  | otherwise    = indexSmallArray (leafFor (vShift v) i (vRoot v)) (i .&. bfMask)
  where
    !tailOff = tailOffset (vSize v)
{-# INLINE unsafeIndex #-}

-- | O(~1). First element. Partial.
head :: Vector a -> a
head v
  | vSize v == 0 = error "Data.PVector.Back.head: empty vector"
  | otherwise     = unsafeIndex v 0
{-# INLINE head #-}

-- | O(1). Last element. Partial.
last :: Vector a -> a
last v
  | vSize v == 0 = error "Data.PVector.Back.last: empty vector"
  | otherwise     = indexSmallArray (vTail v) (sizeofSmallArray (vTail v) - 1)
{-# INLINE last #-}

------------------------------------------------------------------------
-- Update
------------------------------------------------------------------------

-- | O(log₃₂ n). Return a new vector with the element at the given index replaced.
update :: Int -> a -> Vector a -> Vector a
update i x v
  | i < 0 || i >= vSize v = error "Data.PVector.Back.update: index out of bounds"
  | i >= tailOff = v { vTail = cloneAndSet (vTail v) (i .&. bfMask) x }
  | otherwise    = v { vRoot = updateNode (vShift v) i x (vRoot v) }
  where
    !tailOff = tailOffset (vSize v)
{-# INLINE update #-}

-- | O(log₃₂ n). Apply a function to the element at the given index.
adjust :: (a -> a) -> Int -> Vector a -> Vector a
adjust f i v
  | i < 0 || i >= vSize v = error "Data.PVector.Back.adjust: index out of bounds"
  | otherwise = update i (f (unsafeIndex v i)) v
{-# INLINE adjust #-}

------------------------------------------------------------------------
-- Append / Remove
------------------------------------------------------------------------

-- | O(1) amortized. Append an element to the end.
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

-- | O(1) amortized. Infix synonym for 'snoc'.
(|>) :: Vector a -> a -> Vector a
(|>) = snoc
{-# INLINE (|>) #-}

-- | O(1) amortized. Remove the last element, returning the shortened
-- vector and the removed element, or 'Nothing' if empty.
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
                        Nothing -> Empty
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
-- Transformations
------------------------------------------------------------------------

-- | O(n). Map a function over all elements.
map :: (a -> b) -> Vector a -> Vector b
map f = mapDirect f
{-# NOINLINE [1] map #-}

mapDirect :: (a -> b) -> Vector a -> Vector b
mapDirect f v = create $ \mv ->
  let !n = vSize v
      go i
        | i >= n    = pure ()
        | otherwise = mPush mv (f (unsafeIndex v i)) >> go (i + 1)
  in go 0
{-# INLINE mapDirect #-}

-- | O(n). Map with index.
imap :: (Int -> a -> b) -> Vector a -> Vector b
imap f v = create $ \mv ->
  let !n = vSize v
      go i
        | i >= n    = pure ()
        | otherwise = mPush mv (f i (unsafeIndex v i)) >> go (i + 1)
  in go 0
{-# NOINLINE [1] imap #-}

-- | O(n). Reverse the vector.
reverse :: Vector a -> Vector a
reverse v = create $ \mv -> do
  let !n = vSize v
      go i
        | i < 0     = pure ()
        | otherwise = mPush mv (unsafeIndex v i) >> go (i - 1)
  go (n - 1)
{-# INLINE reverse #-}

-- | O(n). Keep only elements satisfying the predicate.
filter :: (a -> Bool) -> Vector a -> Vector a
filter p = filterDirect p
{-# NOINLINE [1] filter #-}

filterDirect :: (a -> Bool) -> Vector a -> Vector a
filterDirect p v = create $ \mv ->
  let !n = vSize v
      go i
        | i >= n    = pure ()
        | otherwise = let !a = unsafeIndex v i
                      in when (p a) (mPush mv a) >> go (i + 1)
  in go 0
{-# INLINE filterDirect #-}

-- | O(n). Filter with index.
ifilter :: (Int -> a -> Bool) -> Vector a -> Vector a
ifilter p v = create $ \mv ->
  let !n = vSize v
      go i
        | i >= n    = pure ()
        | otherwise = let !a = unsafeIndex v i
                      in when (p i a) (mPush mv a) >> go (i + 1)
  in go 0
{-# INLINE ifilter #-}

-- | O(n). Take the first n elements.
take :: Int -> Vector a -> Vector a
take n v
  | n <= 0        = empty
  | n >= vSize v  = v
  | otherwise     = unstream (S.stake n (stream v))
{-# INLINE take #-}

-- | O(n). Drop the first n elements.
drop :: Int -> Vector a -> Vector a
drop n v
  | n <= 0        = v
  | n >= vSize v  = empty
  | otherwise     = unstream (S.sdrop n (stream v))
{-# INLINE drop #-}

-- | O(n + m). Concatenate two vectors.
append :: Vector a -> Vector a -> Vector a
append v1 v2
  | null v1   = v2
  | null v2   = v1
  | otherwise = create $ \mv -> do
      let pushAll v =
            let !n = vSize v
                go i
                  | i >= n    = pure ()
                  | otherwise = mPush mv (unsafeIndex v i) >> go (i + 1)
            in go 0
      pushAll v1
      pushAll v2
{-# INLINE append #-}

------------------------------------------------------------------------
-- Folds
------------------------------------------------------------------------

-- | O(n). Strict left fold over the elements.
foldl' :: (b -> a -> b) -> b -> Vector a -> b
foldl' f z = foldlDirect f z
{-# NOINLINE [1] foldl' #-}

foldlDirect :: (b -> a -> b) -> b -> Vector a -> b
foldlDirect f z0 v
  | n == 0    = z0
  | otherwise = finishTail (foldChunks z0 0)
  where
    !n       = vSize v
    !tailOff = tailOffset n
    !shift   = vShift v
    !root    = vRoot v
    !tail_   = vTail v
    !tailSz  = sizeofSmallArray tail_

    foldChunks !z chunkStart
      | chunkStart >= tailOff = z
      | otherwise =
          let !arr = leafFor shift chunkStart root
              !z'  = foldChunk z arr 0 bf
          in foldChunks z' (chunkStart + bf)

    foldChunk !z arr !i !limit
      | i >= limit = z
      | otherwise  = foldChunk (f z (indexSmallArray arr i)) arr (i + 1) limit

    finishTail !z = foldChunk z tail_ 0 tailSz
{-# INLINE foldlDirect #-}

-- | O(n). Strict left fold with index.
ifoldl' :: (b -> Int -> a -> b) -> b -> Vector a -> b
ifoldl' f z0 v
  | n == 0    = z0
  | otherwise = finishTail (foldChunks z0 0)
  where
    !n       = vSize v
    !tailOff = tailOffset n
    !shift   = vShift v
    !root    = vRoot v
    !tail_   = vTail v
    !tailSz  = sizeofSmallArray tail_

    foldChunks !z chunkStart
      | chunkStart >= tailOff = z
      | otherwise =
          let !arr = leafFor shift chunkStart root
              !z'  = foldChunk z arr chunkStart 0 bf
          in foldChunks z' (chunkStart + bf)

    foldChunk !z arr !base !i !limit
      | i >= limit = z
      | otherwise  = foldChunk (f z (base + i) (indexSmallArray arr i)) arr base (i + 1) limit

    finishTail !z = foldChunk z tail_ tailOff 0 tailSz
{-# INLINE ifoldl' #-}

-- | O(n). Lazy right fold.
foldr :: (a -> b -> b) -> b -> Vector a -> b
foldr f z = foldrDirect f z
{-# NOINLINE [1] foldr #-}

foldrDirect :: (a -> b -> b) -> b -> Vector a -> b
foldrDirect f z0 v
  | n == 0    = z0
  | otherwise = foldrChunks 0 (foldrChunk tail_ 0 tailSz z0)
  where
    !n       = vSize v
    !tailOff = tailOffset n
    !shift   = vShift v
    !root    = vRoot v
    !tail_   = vTail v
    !tailSz  = sizeofSmallArray tail_

    foldrChunks chunkStart tailResult
      | chunkStart >= tailOff = tailResult
      | otherwise =
          let !arr = leafFor shift chunkStart root
          in foldrChunk arr 0 bf (foldrChunks (chunkStart + bf) tailResult)

    foldrChunk arr !i !limit rest
      | i >= limit = rest
      | otherwise  = f (indexSmallArray arr i) (foldrChunk arr (i + 1) limit rest)
{-# INLINE foldrDirect #-}

-- | O(n). Right fold with index.
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

-- | O(n). Strict left fold with no starting value. Partial.
foldl1' :: (a -> a -> a) -> Vector a -> a
foldl1' f v
  | vSize v == 0 = error "Data.PVector.Back.foldl1': empty vector"
  | otherwise     = foldl' f (unsafeIndex v 0) (drop 1 v)
{-# INLINE foldl1' #-}

-- | O(n). Lazy right fold with no starting value. Partial.
foldr1 :: (a -> a -> a) -> Vector a -> a
foldr1 f v
  | vSize v == 0 = error "Data.PVector.Back.foldr1: empty vector"
  | otherwise     = foldr f (Data.PVector.Back.last v) (take (vSize v - 1) v)
{-# INLINE foldr1 #-}

-- | O(n). Map each element to a monoid and combine.
foldMap :: Monoid m => (a -> m) -> Vector a -> m
foldMap f = foldl' (\acc a -> acc <> f a) mempty
{-# INLINE foldMap #-}

-- | O(n). Strict 'foldMap'.
foldMap' :: Monoid m => (a -> m) -> Vector a -> m
foldMap' f = foldl' (\acc a -> acc <> f a) mempty
{-# INLINE foldMap' #-}

------------------------------------------------------------------------
-- Zips
------------------------------------------------------------------------

-- | O(min(n,m)). Zip two vectors into a vector of pairs.
zip :: Vector a -> Vector b -> Vector (a, b)
zip = zipWith (,)
{-# INLINE zip #-}

-- | O(min(n,m)). Zip with a combining function.
zipWith :: (a -> b -> c) -> Vector a -> Vector b -> Vector c
zipWith f v1 v2 = generate (min (length v1) (length v2)) $ \i ->
  f (unsafeIndex v1 i) (unsafeIndex v2 i)
{-# INLINE zipWith #-}

-- | O(n). Unzip a vector of pairs.
unzip :: Vector (a, b) -> (Vector a, Vector b)
unzip v = (map fst v, map snd v)
{-# INLINE unzip #-}

------------------------------------------------------------------------
-- Conversions
------------------------------------------------------------------------

-- | O(n). Convert to a list.
toList :: Vector a -> [a]
toList v = build (\c n -> foldr c n v)
{-# INLINE toList #-}

-- | O(n). Convert from a standard boxed Data.Vector.
fromVector :: F.Foldable f => f a -> Vector a
fromVector = F.foldl' snoc empty
{-# INLINE fromVector #-}

-- | O(n). Convert to a list (same as 'toList', useful for interop).
toVector :: Vector a -> [a]
toVector = toList
{-# INLINE toVector #-}

------------------------------------------------------------------------
-- Transient (mutable) operations
------------------------------------------------------------------------

-- | O(1). Create a persistent vector from an ST action that fills
-- a mutable vector.
create :: (forall s. MVector s a -> ST s ()) -> Vector a
create act = runST $ do
  mv <- mNew
  act mv
  unsafeFreeze mv
{-# INLINE create #-}

-- | O(n). Modify a persistent vector through a mutable interface.
modify :: (forall s. MVector s a -> ST s ()) -> Vector a -> Vector a
modify act v = runST $ do
  mv <- thaw v
  act mv
  unsafeFreeze mv
{-# INLINE modify #-}

-- | O(1). Create a mutable copy of a persistent vector.
-- The persistent vector can still be used independently.
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

-- | O(n). Convert a mutable vector to a persistent one.
-- The mutable vector should not be used after this.
freeze :: PrimMonad m => MVector (PrimState m) a -> m (Vector a)
freeze = unsafeFreeze
{-# INLINE freeze #-}

-- | O(n). Convert a mutable vector to persistent. The mutable vector
-- must not be used afterward.
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

-- | O(1). Create an empty mutable vector.
mNew :: PrimMonad m => m (MVector (PrimState m) a)
mNew = do
  mtail <- newSmallArray bf undefinedElem
  ref <- newMutVar $! MVState 0 bfBits (Frozen Empty) mtail 0
  pure (MVector ref)
  where
    undefinedElem = error "pvector: uninitialised element"
{-# INLINE mNew #-}

-- | O(1). Length of a mutable vector.
mLength :: PrimMonad m => MVector (PrimState m) a -> m Int
mLength mv = mvSize <$> getMV mv
{-# INLINE mLength #-}

-- | O(log₃₂ n). Read an element.
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

-- | O(log₃₂ n). Write an element (copy-on-write for shared nodes).
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

-- | O(1) amortized. Append an element.
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
      newTail <- newSmallArray bf undefinedElem
      writeSmallArray newTail 0 x
      putMV mv $! MVState (n + 1) newShift newRoot newTail 1
  where
    undefinedElem = error "pvector: uninitialised element"
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

-- | O(1) amortized. Remove and return the last element.
-- Note: this implementation freezes then re-thaws. For batch removals,
-- use the persistent 'unsnoc' directly.
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

-- | Convert a vector to a stream.
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

-- | Build a vector from a stream.
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

-- Stream/unstream elimination
"pvector/stream/unstream"
  forall s. stream (unstream s) = s

-- Forward rules: enable fusion by going through streams [phase ~1]
"pvector/map [stream]" [~1]
  forall f v. map f v = unstream (S.smap f (stream v))
"pvector/filter [stream]" [~1]
  forall p v. filter p v = unstream (S.sfilter p (stream v))
"pvector/foldl' [stream]" [~1]
  forall f z v. foldl' f z v = S.sfoldl' f z (stream v)
"pvector/foldr [stream]" [~1]
  forall f z v. foldr f z v = S.sfoldr f z (stream v)

-- Fallback rules: revert to direct implementations when not fused [phase 1]
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

-- | Navigate the trie to find the leaf array containing index i.
leafFor :: Int -> Int -> Node a -> SmallArray a
leafFor = go
  where
    go !level !i (Internal arr) = go (level - bfBits) i (indexSmallArray arr (indexAtLevel i level))
    go _      _ (Leaf arr)      = arr
    go _      _ Empty           = error "pvector: leafFor hit Empty node"
{-# INLINE leafFor #-}

-- | Update a single element in the trie, cloning the path.
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

-- | Build a new path from root to a leaf node.
newPath :: Int -> Node a -> Node a
newPath 0 node = node
newPath level node =
  let !arr = runST $ do
        a <- newSmallArray bf Empty
        writeSmallArray a 0 (newPath (level - bfBits) node)
        unsafeFreezeSmallArray a
  in Internal arr

-- | Push a full tail array into the trie.
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

-- | Remove the rightmost leaf from the trie.
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

-- | Strict left fold traversing elements from right to left.
-- Used internally by the Front vector.
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

-- | Lazy right fold traversing elements from right to left.
-- @rfoldr f z [a,b,c]@ computes @f c (f b (f a z))@.
-- Used internally by the Front vector.
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
