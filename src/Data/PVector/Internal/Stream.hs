{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}

module Data.PVector.Internal.Stream
  ( Step(..)
  , Stream(..)
  , Size(..)
    -- * Stream producers
  , empty
  , singleton
  , replicate
  , generate
  , unfoldr
  , enumFromStepN
  , fromList
  , fromListN
    -- * Stream transformers
  , smap
  , simap
  , sfilter
  , sifilter
  , stake
  , sdrop
  , sconcat
    -- * Stream consumers
  , sfoldl'
  , sifoldl'
  , sfoldr
  , sifoldr
  , slength
  , snull
  , stoList
  ) where

import Prelude hiding (replicate)

data Step s a
  = Yield a s
  | Skip  s
  | Done

data Size
  = Exact {-# UNPACK #-} !Int
  | Max   {-# UNPACK #-} !Int
  | Unknown

data Stream a = forall s. Stream (s -> Step s a) s !Size

------------------------------------------------------------------------
-- Size combinators
------------------------------------------------------------------------

smaller :: Size -> Size -> Size
smaller (Exact a) (Exact b) = Exact (min a b)
smaller (Exact a) (Max   b) = Max   (min a b)
smaller (Max   a) (Exact b) = Max   (min a b)
smaller (Max   a) (Max   b) = Max   (min a b)
smaller (Exact a) Unknown   = Max   a
smaller Unknown   (Exact b) = Max   b
smaller (Max   a) Unknown   = Max   a
smaller Unknown   (Max   b) = Max   b
smaller Unknown   Unknown   = Unknown
{-# INLINE smaller #-}

toMax :: Size -> Size
toMax (Exact n) = Max n
toMax s         = s
{-# INLINE toMax #-}

------------------------------------------------------------------------
-- Producers
------------------------------------------------------------------------

empty :: Stream a
empty = Stream (const Done) () (Exact 0)
{-# INLINE [1] empty #-}

singleton :: a -> Stream a
singleton x = Stream step True (Exact 1)
  where
    step True  = Yield x False
    step False = Done
{-# INLINE [1] singleton #-}

replicate :: Int -> a -> Stream a
replicate n x = Stream step 0 (Exact (max 0 n))
  where
    step i | i >= n    = Done
           | otherwise = Yield x (i + 1)
{-# INLINE [1] replicate #-}

generate :: Int -> (Int -> a) -> Stream a
generate n f = Stream step 0 (Exact (max 0 n))
  where
    step i | i >= n    = Done
           | otherwise = Yield (f i) (i + 1)
{-# INLINE [1] generate #-}

unfoldr :: (b -> Maybe (a, b)) -> b -> Stream a
unfoldr f s0 = Stream step (Just s0) Unknown
  where
    step Nothing  = Done
    step (Just s) = case f s of
      Nothing      -> Done
      Just (a, s') -> Yield a (Just s')
{-# INLINE [1] unfoldr #-}

enumFromStepN :: Num a => a -> a -> Int -> Stream a
enumFromStepN x0 dx n = Stream step (x0, 0) (Exact (max 0 n))
  where
    step (x, i)
      | i >= n    = Done
      | otherwise = Yield x (x + dx, i + 1)
{-# INLINE [1] enumFromStepN #-}

fromList :: [a] -> Stream a
fromList xs = Stream step xs Unknown
  where
    step []     = Done
    step (a:as) = Yield a as
{-# INLINE [1] fromList #-}

fromListN :: Int -> [a] -> Stream a
fromListN n xs = Stream step (xs, 0) (Exact (max 0 n))
  where
    step (_, i)  | i >= n = Done
    step ([], _)          = Done
    step (a:as, i)        = Yield a (as, i + 1)
{-# INLINE [1] fromListN #-}

------------------------------------------------------------------------
-- Transformers
------------------------------------------------------------------------

smap :: (a -> b) -> Stream a -> Stream b
smap f (Stream step s0 sz) = Stream step' s0 sz
  where
    step' s = case step s of
      Yield a s' -> Yield (f a) s'
      Skip    s' -> Skip s'
      Done       -> Done
{-# INLINE [1] smap #-}

simap :: (Int -> a -> b) -> Stream a -> Stream b
simap f (Stream step s0 sz) = Stream step' (s0, 0) sz
  where
    step' (s, !i) = case step s of
      Yield a s' -> Yield (f i a) (s', i + 1)
      Skip    s' -> Skip (s', i)
      Done       -> Done
{-# INLINE [1] simap #-}

sfilter :: (a -> Bool) -> Stream a -> Stream a
sfilter p (Stream step s0 sz) = Stream step' s0 (toMax sz)
  where
    step' s = case step s of
      Yield a s' | p a       -> Yield a s'
                 | otherwise -> Skip s'
      Skip    s'             -> Skip s'
      Done                   -> Done
{-# INLINE [1] sfilter #-}

sifilter :: (Int -> a -> Bool) -> Stream a -> Stream a
sifilter p (Stream step s0 sz) = Stream step' (s0, 0) (toMax sz)
  where
    step' (s, !i) = case step s of
      Yield a s' | p i a     -> Yield a (s', i + 1)
                 | otherwise -> Skip (s', i + 1)
      Skip    s'             -> Skip (s', i)
      Done                   -> Done
{-# INLINE [1] sifilter #-}

stake :: Int -> Stream a -> Stream a
stake n (Stream step s0 sz) = Stream step' (s0, 0) (sz `smaller` Exact n)
  where
    step' (s, i)
      | i >= n    = Done
      | otherwise = case step s of
          Yield a s' -> Yield a (s', i + 1)
          Skip    s' -> Skip (s', i)
          Done       -> Done
{-# INLINE [1] stake #-}

sdrop :: Int -> Stream a -> Stream a
sdrop n (Stream step s0 sz) = Stream step' (s0, 0) sz'
  where
    sz' = case sz of
      Exact m -> Exact (max 0 (m - n))
      Max   m -> Max   (max 0 (m - n))
      Unknown -> Unknown
    step' (s, i)
      | i < n     = case step s of
          Yield _ s' -> Skip (s', i + 1)
          Skip    s' -> Skip (s', i)
          Done       -> Done
      | otherwise = case step s of
          Yield a s' -> Yield a (s', i + 1)
          Skip    s' -> Skip (s', i)
          Done       -> Done
{-# INLINE [1] sdrop #-}

sconcat :: Stream a -> Stream a -> Stream a
sconcat (Stream stepL sL0 szL) (Stream stepR sR0 szR) =
  Stream step (Left sL0) sz
  where
    sz = case (szL, szR) of
      (Exact a, Exact b) -> Exact (a + b)
      (Exact a, Max   b) -> Max   (a + b)
      (Max   a, Exact b) -> Max   (a + b)
      (Max   a, Max   b) -> Max   (a + b)
      _                  -> Unknown
    step (Left s) = case stepL s of
      Yield a s' -> Yield a (Left s')
      Skip    s' -> Skip (Left s')
      Done       -> Skip (Right sR0)
    step (Right s) = case stepR s of
      Yield a s' -> Yield a (Right s')
      Skip    s' -> Skip (Right s')
      Done       -> Done
{-# INLINE [1] sconcat #-}

------------------------------------------------------------------------
-- Consumers
------------------------------------------------------------------------

sfoldl' :: (b -> a -> b) -> b -> Stream a -> b
sfoldl' f z0 (Stream step s0 _) = go z0 s0
  where
    go !z s = case step s of
      Yield a s' -> go (f z a) s'
      Skip    s' -> go z s'
      Done       -> z
{-# INLINE [1] sfoldl' #-}

sifoldl' :: (b -> Int -> a -> b) -> b -> Stream a -> b
sifoldl' f z0 (Stream step s0 _) = go z0 0 s0
  where
    go !z !i s = case step s of
      Yield a s' -> go (f z i a) (i + 1) s'
      Skip    s' -> go z i s'
      Done       -> z
{-# INLINE [1] sifoldl' #-}

sfoldr :: (a -> b -> b) -> b -> Stream a -> b
sfoldr f z (Stream step s0 _) = go s0
  where
    go s = case step s of
      Yield a s' -> f a (go s')
      Skip    s' -> go s'
      Done       -> z
{-# INLINE [1] sfoldr #-}

sifoldr :: (Int -> a -> b -> b) -> b -> Stream a -> b
sifoldr f z (Stream step s0 _) = go 0 s0
  where
    go !i s = case step s of
      Yield a s' -> f i a (go (i + 1) s')
      Skip    s' -> go i s'
      Done       -> z
{-# INLINE [1] sifoldr #-}

slength :: Stream a -> Int
slength = sfoldl' (\n _ -> n + 1) 0
{-# INLINE slength #-}

snull :: Stream a -> Bool
snull (Stream step s0 _) = go s0
  where
    go s = case step s of
      Yield _ _ -> False
      Skip   s' -> go s'
      Done      -> True
{-# INLINE snull #-}

stoList :: Stream a -> [a]
stoList = sfoldr (:) []
{-# INLINE stoList #-}

------------------------------------------------------------------------
-- Rewrite rules for stream-stream fusion
------------------------------------------------------------------------

{-# RULES
"smap/smap"       forall f g s.  smap f (smap g s)       = smap (f . g) s
"sfilter/sfilter" forall f g s.  sfilter f (sfilter g s) = sfilter (\x -> g x && f x) s
"smap/sfilter"    forall f p s.  sfilter p (smap f s)    = smap f (sfilter (p . f) s)
  #-}
