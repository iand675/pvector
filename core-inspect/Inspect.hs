{-# LANGUAGE BangPatterns #-}
module Inspect where

import qualified Data.PVector.Back as P

-- Each top-level binding becomes a separate Core definition we can examine.

{-# NOINLINE inspectFoldl #-}
inspectFoldl :: P.Vector Int -> Int
inspectFoldl v = P.foldl' (+) 0 v

{-# NOINLINE inspectFoldlMap #-}
inspectFoldlMap :: P.Vector Int -> Int
inspectFoldlMap v = P.foldl' (+) 0 (P.map (+1) v)

{-# NOINLINE inspectFoldlFilter #-}
inspectFoldlFilter :: P.Vector Int -> Int
inspectFoldlFilter v = P.foldl' (+) 0 (P.filter even v)

{-# NOINLINE inspectMap #-}
inspectMap :: P.Vector Int -> P.Vector Int
inspectMap v = P.map (+1) v

{-# NOINLINE inspectFilter #-}
inspectFilter :: P.Vector Int -> P.Vector Int
inspectFilter v = P.filter even v

{-# NOINLINE inspectIndex #-}
inspectIndex :: P.Vector Int -> Int -> Int
inspectIndex v i = P.unsafeIndex v i

{-# NOINLINE inspectSnoc #-}
inspectSnoc :: P.Vector Int -> Int -> P.Vector Int
inspectSnoc v x = P.snoc v x

{-# NOINLINE inspectCons #-}
inspectCons :: Int -> P.Vector Int -> P.Vector Int
inspectCons x v = P.cons x v

{-# NOINLINE inspectAppend #-}
inspectAppend :: P.Vector Int -> P.Vector Int -> P.Vector Int
inspectAppend v1 v2 = v1 <> v2

{-# NOINLINE inspectHead #-}
inspectHead :: P.Vector Int -> Int
inspectHead v = P.head v

{-# NOINLINE inspectLast #-}
inspectLast :: P.Vector Int -> Int
inspectLast v = P.last v

{-# NOINLINE inspectTake #-}
inspectTake :: Int -> P.Vector Int -> P.Vector Int
inspectTake n v = P.take n v

{-# NOINLINE inspectReverse #-}
inspectReverse :: P.Vector Int -> P.Vector Int
inspectReverse v = P.reverse v

{-# NOINLINE inspectToList #-}
inspectToList :: P.Vector Int -> [Int]
inspectToList v = P.toList v
