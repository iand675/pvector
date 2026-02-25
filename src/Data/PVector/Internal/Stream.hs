{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}

-- | Stream fusion framework for pvector.
--
-- Based on the approach from the @vector@ package:
--
--   * Pure 'Stream' for element-level stepping
--   * 'MStream' for monadic stepping (needed for in-place transforms)
--   * 'Bundle' carrying a stream + size hint
--   * 'Id' identity monad for pure instantiation
--
-- The 'Bundle' is what operations produce/consume. Stream-level rules
-- compose transformers; bundle-level @inplace@ marks candidates for
-- in-place execution via the recycling framework.
module Data.PVector.Internal.Stream
  ( -- * Step
    Step(..)

    -- * Identity monad
  , Id(..)

    -- * Monadic stream
  , MStream(..)

    -- * Pure stream (Id-instantiated)
  , Stream

    -- * Size hints
  , Size(..)
  , smaller
  , toMax
  , upperBound

    -- * Bundle (stream + size)
  , Bundle(..)
  , inplace
  , size
  , sized
  , elements
  , fromStream

    -- * Bundle producers
  , empty
  , singleton
  , replicate
  , generate
  , unfoldr
  , enumFromStepN
  , fromList
  , fromListN

    -- * Bundle transformers
  , smap
  , simap
  , sfilter
  , sifilter
  , stake
  , sdrop
  , sinit
  , stail
  , sslice
  , stakeWhile
  , sdropWhile
  , smapMaybe
  , szipWith
  , sconcat

    -- * Bundle consumers
  , sfoldl'
  , sifoldl'
  , sfoldr
  , sifoldr
  , slength
  , snull
  , stoList
  , shead
  , slast
  , sindex
  , sindexM

    -- * Monadic stream combinators (for inplace)
  , smapM
  , sfilterM
  , smapMaybeM
  , stakeWhileM
  , sdropWhileM
  , sindexedM
  , stakeM
  , sdropM
  , sprescanlM'
  , spostscanlM'
  , sscanlM'
  , sscanl1M'
  ) where

import Prelude hiding (replicate)

------------------------------------------------------------------------
-- Step
------------------------------------------------------------------------

data Step s a
  = Yield a s
  | Skip  s
  | Done

------------------------------------------------------------------------
-- Identity monad
------------------------------------------------------------------------

newtype Id a = Id { unId :: a }

instance Functor Id where
  fmap f (Id x) = Id (f x)
  {-# INLINE fmap #-}

instance Applicative Id where
  pure = Id
  Id f <*> Id x = Id (f x)
  {-# INLINE pure #-}
  {-# INLINE (<*>) #-}

instance Monad Id where
  return = pure
  Id x >>= f = f x
  {-# INLINE return #-}
  {-# INLINE (>>=) #-}

------------------------------------------------------------------------
-- Monadic stream
------------------------------------------------------------------------

data MStream m a = forall s. MStream (s -> m (Step s a)) s

------------------------------------------------------------------------
-- Pure stream (specialised to Id)
------------------------------------------------------------------------

type Stream = MStream Id

------------------------------------------------------------------------
-- Size hints
------------------------------------------------------------------------

data Size
  = Exact {-# UNPACK #-} !Int
  | Max   {-# UNPACK #-} !Int
  | Unknown

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

upperBound :: Size -> Maybe Int
upperBound (Exact n) = Just n
upperBound (Max   n) = Just n
upperBound Unknown   = Nothing

------------------------------------------------------------------------
-- Bundle = stream + size
------------------------------------------------------------------------

data Bundle a = Bundle
  { sElems :: Stream a
  , sSize  :: !Size
  }

-- | Mark a bundle for in-place execution.  The monadic stream
-- transformer @f@ will be applied to the elements; @g@ adjusts the
-- size.  Semantically, @inplace f g b = fromStream (f (sElems b)) (g (sSize b))@.
inplace :: (forall m. Monad m => MStream m a -> MStream m b)
        -> (Size -> Size) -> Bundle a -> Bundle b
{-# INLINE [0] inplace #-}
inplace f g (Bundle s sz) = Bundle (f s) (g sz)

-- | Extract the size.
size :: Bundle a -> Size
size = sSize
{-# INLINE size #-}

-- | Attach a new size hint.
sized :: Bundle a -> Size -> Bundle a
sized b sz = b { sSize = sz }
{-# INLINE sized #-}

-- | Extract the element stream.
elements :: Bundle a -> Stream a
elements = sElems
{-# INLINE elements #-}

-- | Wrap a stream and size into a bundle.
fromStream :: Stream a -> Size -> Bundle a
fromStream s sz = Bundle s sz
{-# INLINE fromStream #-}

------------------------------------------------------------------------
-- Producers
------------------------------------------------------------------------

empty :: Bundle a
empty = Bundle (MStream (\() -> Id Done) ()) (Exact 0)
{-# INLINE [1] empty #-}

singleton :: a -> Bundle a
singleton x = Bundle (MStream step True) (Exact 1)
  where
    step True  = Id (Yield x False)
    step False = Id Done
{-# INLINE [1] singleton #-}

replicate :: Int -> a -> Bundle a
replicate n x = Bundle (MStream step 0) (Exact (max 0 n))
  where
    step i | i >= n    = Id Done
           | otherwise = Id (Yield x (i + 1))
{-# INLINE [1] replicate #-}

generate :: Int -> (Int -> a) -> Bundle a
generate n f = Bundle (MStream step 0) (Exact (max 0 n))
  where
    step i | i >= n    = Id Done
           | otherwise = Id (Yield (f i) (i + 1))
{-# INLINE [1] generate #-}

unfoldr :: (b -> Maybe (a, b)) -> b -> Bundle a
unfoldr f s0 = Bundle (MStream step (Just s0)) Unknown
  where
    step Nothing  = Id Done
    step (Just s) = case f s of
      Nothing      -> Id Done
      Just (a, s') -> Id (Yield a (Just s'))
{-# INLINE [1] unfoldr #-}

enumFromStepN :: Num a => a -> a -> Int -> Bundle a
enumFromStepN x0 dx n = Bundle (MStream step (x0, 0)) (Exact (max 0 n))
  where
    step (x, i)
      | i >= n    = Id Done
      | otherwise = Id (Yield x (x + dx, i + 1))
{-# INLINE [1] enumFromStepN #-}

fromList :: [a] -> Bundle a
fromList xs = Bundle (MStream step xs) Unknown
  where
    step []     = Id Done
    step (a:as) = Id (Yield a as)
{-# INLINE [1] fromList #-}

fromListN :: Int -> [a] -> Bundle a
fromListN n xs = Bundle (MStream step (xs, 0)) (Exact (max 0 n))
  where
    step (_, i)  | i >= n = Id Done
    step ([], _)          = Id Done
    step (a:as, i)        = Id (Yield a (as, i + 1))
{-# INLINE [1] fromListN #-}

------------------------------------------------------------------------
-- Transformers
------------------------------------------------------------------------

smap :: (a -> b) -> Bundle a -> Bundle b
smap f (Bundle (MStream step s0) sz) = Bundle (MStream step' s0) sz
  where
    step' s = case unId (step s) of
      Yield a s' -> Id (Yield (f a) s')
      Skip    s' -> Id (Skip s')
      Done       -> Id Done
{-# INLINE [0] smap #-}

simap :: (Int -> a -> b) -> Bundle a -> Bundle b
simap f (Bundle (MStream step s0) sz) = Bundle (MStream step' (s0, 0)) sz
  where
    step' (s, !i) = case unId (step s) of
      Yield a s' -> Id (Yield (f i a) (s', i + 1))
      Skip    s' -> Id (Skip (s', i))
      Done       -> Id Done
{-# INLINE [1] simap #-}

sfilter :: (a -> Bool) -> Bundle a -> Bundle a
sfilter p (Bundle (MStream step s0) sz) = Bundle (MStream step' s0) (toMax sz)
  where
    step' s = case unId (step s) of
      Yield a s' | p a       -> Id (Yield a s')
                 | otherwise -> Id (Skip s')
      Skip    s'             -> Id (Skip s')
      Done                   -> Id Done
{-# INLINE [0] sfilter #-}

sifilter :: (Int -> a -> Bool) -> Bundle a -> Bundle a
sifilter p (Bundle (MStream step s0) sz) = Bundle (MStream step' (s0, 0)) (toMax sz)
  where
    step' (s, !i) = case unId (step s) of
      Yield a s' | p i a     -> Id (Yield a (s', i + 1))
                 | otherwise -> Id (Skip (s', i + 1))
      Skip    s'             -> Id (Skip (s', i))
      Done                   -> Id Done
{-# INLINE [1] sifilter #-}

stake :: Int -> Bundle a -> Bundle a
stake n (Bundle (MStream step s0) sz) = Bundle (MStream step' (s0, 0)) (sz `smaller` Exact n)
  where
    step' (s, i)
      | i >= n    = Id Done
      | otherwise = case unId (step s) of
          Yield a s' -> Id (Yield a (s', i + 1))
          Skip    s' -> Id (Skip (s', i))
          Done       -> Id Done
{-# INLINE [1] stake #-}

sdrop :: Int -> Bundle a -> Bundle a
sdrop n (Bundle (MStream step s0) sz) = Bundle (MStream step' (s0, 0)) sz'
  where
    sz' = case sz of
      Exact m -> Exact (max 0 (m - n))
      Max   m -> Max   (max 0 (m - n))
      Unknown -> Unknown
    step' (s, i)
      | i < n     = case unId (step s) of
          Yield _ s' -> Id (Skip (s', i + 1))
          Skip    s' -> Id (Skip (s', i))
          Done       -> Id Done
      | otherwise = case unId (step s) of
          Yield a s' -> Id (Yield a (s', i + 1))
          Skip    s' -> Id (Skip (s', i))
          Done       -> Id Done
{-# INLINE [1] sdrop #-}

sinit :: Bundle a -> Bundle a
sinit (Bundle (MStream step s0) sz) = Bundle (MStream step' (s0, Nothing)) sz'
  where
    sz' = case sz of
      Exact m -> Exact (max 0 (m - 1))
      Max   m -> Max   (max 0 (m - 1))
      Unknown -> Unknown
    step' (s, buf) = case unId (step s) of
      Yield a s' -> case buf of
        Nothing -> Id (Skip (s', Just a))
        Just b  -> Id (Yield b (s', Just a))
      Skip    s' -> Id (Skip (s', buf))
      Done       -> Id Done
{-# INLINE [1] sinit #-}

stail :: Bundle a -> Bundle a
stail (Bundle (MStream step s0) sz) = Bundle (MStream step' (s0, False)) sz'
  where
    sz' = case sz of
      Exact m -> Exact (max 0 (m - 1))
      Max   m -> Max   (max 0 (m - 1))
      Unknown -> Unknown
    step' (s, dropped)
      | dropped = case unId (step s) of
          Yield a s' -> Id (Yield a (s', True))
          Skip    s' -> Id (Skip (s', True))
          Done       -> Id Done
      | otherwise = case unId (step s) of
          Yield _ s' -> Id (Skip (s', True))
          Skip    s' -> Id (Skip (s', False))
          Done       -> Id Done
{-# INLINE [1] stail #-}

sslice :: Int -> Int -> Bundle a -> Bundle a
sslice i n s = stake n (sdrop i s)
{-# INLINE sslice #-}

sconcat :: Bundle a -> Bundle a -> Bundle a
sconcat (Bundle (MStream stepL sL0) szL) (Bundle (MStream stepR sR0) szR) =
  Bundle (MStream step (Left sL0)) sz
  where
    sz = case (szL, szR) of
      (Exact a, Exact b) -> Exact (a + b)
      (Exact a, Max   b) -> Max   (a + b)
      (Max   a, Exact b) -> Max   (a + b)
      (Max   a, Max   b) -> Max   (a + b)
      _                  -> Unknown
    step (Left s) = case unId (stepL s) of
      Yield a s' -> Id (Yield a (Left s'))
      Skip    s' -> Id (Skip (Left s'))
      Done       -> Id (Skip (Right sR0))
    step (Right s) = case unId (stepR s) of
      Yield a s' -> Id (Yield a (Right s'))
      Skip    s' -> Id (Skip (Right s'))
      Done       -> Id Done
{-# INLINE [1] sconcat #-}

stakeWhile :: (a -> Bool) -> Bundle a -> Bundle a
stakeWhile p (Bundle (MStream step s0) sz) = Bundle (MStream step' s0) (toMax sz)
  where
    step' s = case unId (step s) of
      Yield a s' | p a       -> Id (Yield a s')
                 | otherwise -> Id Done
      Skip    s'             -> Id (Skip s')
      Done                   -> Id Done
{-# INLINE [0] stakeWhile #-}

sdropWhile :: (a -> Bool) -> Bundle a -> Bundle a
sdropWhile p (Bundle (MStream step s0) sz) = Bundle (MStream step' (s0, True)) (toMax sz)
  where
    step' (s, dropping) = case unId (step s) of
      Yield a s'
        | dropping && p a -> Id (Skip (s', True))
        | otherwise       -> Id (Yield a (s', False))
      Skip    s'          -> Id (Skip (s', dropping))
      Done                -> Id Done
{-# INLINE [0] sdropWhile #-}

smapMaybe :: (a -> Maybe b) -> Bundle a -> Bundle b
smapMaybe f (Bundle (MStream step s0) sz) = Bundle (MStream step' s0) (toMax sz)
  where
    step' s = case unId (step s) of
      Yield a s' -> case f a of
        Just b  -> Id (Yield b s')
        Nothing -> Id (Skip s')
      Skip    s' -> Id (Skip s')
      Done       -> Id Done
{-# INLINE [0] smapMaybe #-}

szipWith :: (a -> b -> c) -> Bundle a -> Bundle b -> Bundle c
szipWith f (Bundle (MStream stepA sA0) szA) (Bundle (MStream stepB sB0) szB) =
  Bundle (MStream step' (sA0, sB0, Nothing)) (szA `smaller` szB)
  where
    step' (sA, sB, Nothing) = case unId (stepA sA) of
      Yield a sA' -> Id (Skip (sA', sB, Just a))
      Skip    sA' -> Id (Skip (sA', sB, Nothing))
      Done        -> Id Done
    step' (sA, sB, Just a) = case unId (stepB sB) of
      Yield b sB' -> Id (Yield (f a b) (sA, sB', Nothing))
      Skip    sB' -> Id (Skip (sA, sB', Just a))
      Done        -> Id Done
{-# INLINE [0] szipWith #-}

------------------------------------------------------------------------
-- Consumers
------------------------------------------------------------------------

sfoldl' :: (b -> a -> b) -> b -> Bundle a -> b
sfoldl' f z0 (Bundle (MStream step s0) _) = go z0 s0
  where
    go !z s = case unId (step s) of
      Yield a s' -> go (f z a) s'
      Skip    s' -> go z s'
      Done       -> z
{-# INLINE [1] sfoldl' #-}

sifoldl' :: (b -> Int -> a -> b) -> b -> Bundle a -> b
sifoldl' f z0 (Bundle (MStream step s0) _) = go z0 0 s0
  where
    go !z !i s = case unId (step s) of
      Yield a s' -> go (f z i a) (i + 1) s'
      Skip    s' -> go z i s'
      Done       -> z
{-# INLINE [1] sifoldl' #-}

sfoldr :: (a -> b -> b) -> b -> Bundle a -> b
sfoldr f z (Bundle (MStream step s0) _) = go s0
  where
    go s = case unId (step s) of
      Yield a s' -> f a (go s')
      Skip    s' -> go s'
      Done       -> z
{-# INLINE [1] sfoldr #-}

sifoldr :: (Int -> a -> b -> b) -> b -> Bundle a -> b
sifoldr f z (Bundle (MStream step s0) _) = go 0 s0
  where
    go !i s = case unId (step s) of
      Yield a s' -> f i a (go (i + 1) s')
      Skip    s' -> go i s'
      Done       -> z
{-# INLINE [1] sifoldr #-}

slength :: Bundle a -> Int
slength = sfoldl' (\n _ -> n + 1) 0
{-# INLINE slength #-}

snull :: Bundle a -> Bool
snull (Bundle (MStream step s0) _) = go s0
  where
    go s = case unId (step s) of
      Yield _ _ -> False
      Skip   s' -> go s'
      Done      -> True
{-# INLINE snull #-}

stoList :: Bundle a -> [a]
stoList = sfoldr (:) []
{-# INLINE stoList #-}

shead :: Bundle a -> a
shead (Bundle (MStream step s0) _) = go s0
  where
    go s = case unId (step s) of
      Yield a _ -> a
      Skip   s' -> go s'
      Done      -> error "pvector: shead of empty stream"
{-# INLINE [1] shead #-}

slast :: Bundle a -> a
slast (Bundle (MStream step s0) _) = start s0
  where
    start s = case unId (step s) of
      Yield a s' -> go a s'
      Skip    s' -> start s'
      Done       -> error "pvector: slast of empty stream"
    go !prev s = case unId (step s) of
      Yield a s' -> go a s'
      Skip    s' -> go prev s'
      Done       -> prev
{-# INLINE [1] slast #-}

sindex :: Bundle a -> Int -> a
sindex (Bundle (MStream step s0) _) i = go i s0
  where
    go !j s = case unId (step s) of
      Yield a s' | j == 0    -> a
                 | otherwise -> go (j - 1) s'
      Skip    s'             -> go j s'
      Done                   -> error "pvector: sindex out of bounds"
{-# INLINE [1] sindex #-}

sindexM :: Bundle a -> Int -> Maybe a
sindexM (Bundle (MStream step s0) _) i = go i s0
  where
    go !j s = case unId (step s) of
      Yield a s' | j == 0    -> Just a
                 | otherwise -> go (j - 1) s'
      Skip    s'             -> go j s'
      Done                   -> Nothing
{-# INLINE [1] sindexM #-}

------------------------------------------------------------------------
-- Monadic stream combinators (polymorphic in m, needed for inplace)
------------------------------------------------------------------------

smapM :: Monad m => (a -> b) -> MStream m a -> MStream m b
smapM f (MStream step s0) = MStream step' s0
  where
    step' s = do
      r <- step s
      pure $ case r of
        Yield x s' -> Yield (f x) s'
        Skip    s' -> Skip s'
        Done       -> Done
{-# INLINE [0] smapM #-}

sfilterM :: Monad m => (a -> Bool) -> MStream m a -> MStream m a
sfilterM p (MStream step s0) = MStream step' s0
  where
    step' s = do
      r <- step s
      pure $ case r of
        Yield x s' | p x       -> Yield x s'
                   | otherwise -> Skip s'
        Skip    s'             -> Skip s'
        Done                   -> Done
{-# INLINE [0] sfilterM #-}

smapMaybeM :: Monad m => (a -> Maybe b) -> MStream m a -> MStream m b
smapMaybeM f (MStream step s0) = MStream step' s0
  where
    step' s = do
      r <- step s
      pure $ case r of
        Yield x s' -> case f x of
          Just y  -> Yield y s'
          Nothing -> Skip s'
        Skip    s' -> Skip s'
        Done       -> Done
{-# INLINE [0] smapMaybeM #-}

stakeWhileM :: Monad m => (a -> Bool) -> MStream m a -> MStream m a
stakeWhileM p (MStream step s0) = MStream step' s0
  where
    step' s = do
      r <- step s
      pure $ case r of
        Yield x s' | p x       -> Yield x s'
                   | otherwise -> Done
        Skip    s'             -> Skip s'
        Done                   -> Done
{-# INLINE [0] stakeWhileM #-}

sdropWhileM :: Monad m => (a -> Bool) -> MStream m a -> MStream m a
sdropWhileM p (MStream step s0) = MStream step' (s0, True)
  where
    step' (s, dropping) = do
      r <- step s
      pure $ case r of
        Yield x s'
          | dropping && p x -> Skip (s', True)
          | otherwise       -> Yield x (s', False)
        Skip    s'          -> Skip (s', dropping)
        Done                -> Done
{-# INLINE [0] sdropWhileM #-}

sindexedM :: Monad m => MStream m a -> MStream m (Int, a)
sindexedM (MStream step s0) = MStream step' (s0, 0)
  where
    step' (s, !i) = do
      r <- step s
      pure $ case r of
        Yield x s' -> Yield (i, x) (s', i + 1)
        Skip    s' -> Skip (s', i)
        Done       -> Done
{-# INLINE [0] sindexedM #-}

stakeM :: Monad m => Int -> MStream m a -> MStream m a
stakeM n (MStream step s0) = MStream step' (s0, 0)
  where
    step' (s, i)
      | i >= n = pure Done
      | otherwise = do
          r <- step s
          pure $ case r of
            Yield x s' -> Yield x (s', i + 1)
            Skip    s' -> Skip (s', i)
            Done       -> Done
{-# INLINE [0] stakeM #-}

sdropM :: Monad m => Int -> MStream m a -> MStream m a
sdropM n (MStream step s0) = MStream step' (s0, 0)
  where
    step' (s, i) = do
      r <- step s
      pure $ case r of
        Yield x s'
          | i < n     -> Skip (s', i + 1)
          | otherwise -> Yield x (s', i + 1)
        Skip    s'    -> Skip (s', i)
        Done          -> Done
{-# INLINE [0] sdropM #-}

sprescanlM' :: Monad m => (a -> b -> a) -> a -> MStream m b -> MStream m a
sprescanlM' f z0 (MStream step s0) = MStream step' (s0, z0)
  where
    step' (s, !z) = do
      r <- step s
      pure $ case r of
        Yield x s' -> Yield z (s', f z x)
        Skip    s' -> Skip (s', z)
        Done       -> Done
{-# INLINE [0] sprescanlM' #-}

spostscanlM' :: Monad m => (a -> b -> a) -> a -> MStream m b -> MStream m a
spostscanlM' f z0 (MStream step s0) = MStream step' (s0, z0)
  where
    step' (s, !z) = do
      r <- step s
      pure $ case r of
        Yield x s' -> let !z' = f z x in Yield z' (s', z')
        Skip    s' -> Skip (s', z)
        Done       -> Done
{-# INLINE [0] spostscanlM' #-}

sscanlM' :: Monad m => (a -> b -> a) -> a -> MStream m b -> MStream m a
sscanlM' f z0 (MStream step s0) = MStream step' (s0, z0, True)
  where
    step' (s, !z, first_)
      | first_    = pure (Yield z (s, z, False))
      | otherwise = do
          r <- step s
          pure $ case r of
            Yield x s' -> let !z' = f z x in Yield z' (s', z', False)
            Skip    s' -> Skip (s', z, False)
            Done       -> Done
{-# INLINE [0] sscanlM' #-}

sscanl1M' :: Monad m => (a -> a -> a) -> MStream m a -> MStream m a
sscanl1M' f (MStream step s0) = MStream step' (s0, Nothing)
  where
    step' (s, macc) = do
      r <- step s
      pure $ case r of
        Yield x s' -> case macc of
          Nothing -> Yield x (s', Just x)
          Just !z -> let !z' = f z x in Yield z' (s', Just z')
        Skip    s' -> Skip (s', macc)
        Done       -> Done
{-# INLINE [0] sscanl1M' #-}

------------------------------------------------------------------------
-- Rewrite rules for stream-stream fusion
------------------------------------------------------------------------

{-# RULES

-- === Map composition ===
"smap/smap"       forall f g s.  smap f (smap g s)       = smap (f . g) s

-- === Filter composition ===
"sfilter/sfilter" forall f g s.  sfilter f (sfilter g s) = sfilter (\x -> g x && f x) s

-- === Map/filter interaction: push filter before map ===
"smap/sfilter"    forall f p s.  sfilter p (smap f s)    = smap f (sfilter (p . f) s)

-- === Take/drop through map: push map inside ===
"smap/stake"      forall f n s.  stake n (smap f s)      = smap f (stake n s)
"smap/sdrop"      forall f n s.  sdrop n (smap f s)      = smap f (sdrop n s)

-- === Filter through take/drop ===
"stake/sfilter"   forall n p s.  stake n (sfilter p s)   = sfilter p (stake n s)

-- === takeWhile/dropWhile through map ===
"smap/stakeWhile" forall f p s.  stakeWhile p (smap f s) = smap f (stakeWhile (p . f) s)
"smap/sdropWhile" forall f p s.  sdropWhile p (smap f s) = smap f (sdropWhile (p . f) s)

-- === Inplace composition ===
"inplace/inplace [pvector]"
  forall (f1 :: forall m. Monad m => MStream m a -> MStream m a)
         (f2 :: forall m. Monad m => MStream m a -> MStream m a)
         g1 g2 s.
  inplace f1 g1 (inplace f2 g2 s) = inplace (f1 . f2) (g1 . g2) s
  #-}
