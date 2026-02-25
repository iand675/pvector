module Main (main) where

import Criterion
import Criterion.Main
import Control.DeepSeq
import Data.Primitive.SmallArray (SmallArray, sizeofSmallArray, indexSmallArray)
import qualified Data.Vector as V
import qualified Data.PVector.Back as P
import qualified Data.List as L
import qualified Data.Sequence as Seq
import qualified Data.Foldable as F

main :: IO ()
main = defaultMain
  [ bgroup "snoc (build n)"
    [ bgroup (show n)
      [ bench "[] (++ [x])" $ nf (L.foldl' (\l x -> l Prelude.++ [x]) []) [1..n]
      , bench "Vector snoc" $ nf (L.foldl' V.snoc V.empty) [1..n]
      , bench "PVector snoc" $ nf (L.foldl' P.snoc P.empty) [1..n]
      , bench "Seq |>" $ nf (L.foldl' (Seq.|>) Seq.empty) [1..n]
      ]
    | n <- sizes
    ]
  , bgroup "fromList"
    [ env (pure $! force [1..n]) $ \xs ->
      bgroup (show n)
      [ bench "Vector" $ nf V.fromList xs
      , bench "PVector" $ nf P.fromList xs
      , bench "Seq" $ nf Seq.fromList xs
      ]
    | n <- sizes
    ]
  , bgroup "index (middle)"
    [ env (setupAll n) $ \ ~(xs, vec, pvec, sq) ->
      let !mid = n `div` 2 in
      bgroup (show n)
      [ bench "[] (!!)" $ nf (\l -> l !! mid) xs
      , bench "Vector" $ nf (\v -> v V.! mid) vec
      , bench "PVector" $ nf (\v -> P.index v mid) pvec
      , bench "Seq" $ nf (\s -> Seq.index s mid) sq
      ]
    | n <- sizes
    ]
  , bgroup "head"
    [ env (setupAll n) $ \ ~(xs, vec, pvec, sq) ->
      bgroup (show n)
      [ bench "[]" $ nf Prelude.head xs
      , bench "Vector" $ nf V.head vec
      , bench "PVector" $ nf P.head pvec
      , bench "Seq" $ nf (\s -> case Seq.viewl s of (x Seq.:< _) -> x; _ -> 0) sq
      ]
    | n <- sizes
    ]
  , bgroup "last"
    [ env (setupAll n) $ \ ~(xs, vec, pvec, sq) ->
      bgroup (show n)
      [ bench "[]" $ nf Prelude.last xs
      , bench "Vector" $ nf V.last vec
      , bench "PVector" $ nf P.last pvec
      , bench "Seq" $ nf (\s -> case Seq.viewr s of (_ Seq.:> x) -> x; _ -> 0) sq
      ]
    | n <- sizes
    ]
  , bgroup "update (single, middle, nf)"
    [ env (setupAll n) $ \ ~(_xs, vec, pvec, sq) ->
      let !mid = n `div` 2 in
      bgroup (show n)
      [ bench "Vector" $ nf (\v -> v V.// [(mid, 0)]) vec
      , bench "PVector" $ nf (P.update mid 0) pvec
      , bench "Seq" $ nf (Seq.update mid 0) sq
      ]
    | n <- sizes
    ]
  , bgroup "update (single, middle, whnf)"
    [ env (setupAll n) $ \ ~(_xs, vec, pvec, sq) ->
      let !mid = n `div` 2 in
      bgroup (show n)
      [ bench "Vector" $ whnf (\v -> v V.// [(mid, 0)]) vec
      , bench "PVector" $ whnf (P.update mid 0) pvec
      , bench "Seq" $ whnf (Seq.update mid 0) sq
      ]
    | n <- sizes
    ]
  , bgroup "foldl' (+)"
    [ env (setupAll n) $ \ ~(xs, vec, pvec, sq) ->
      bgroup (show n)
      [ bench "[]" $ nf (L.foldl' (+) (0 :: Int)) xs
      , bench "Vector" $ nf (V.foldl' (+) (0 :: Int)) vec
      , bench "PVector" $ nf (P.foldl' (+) (0 :: Int)) pvec
      , bench "Seq" $ nf (L.foldl' (+) (0 :: Int) . F.toList) sq
      ]
    | n <- sizes
    ]
  , bgroup "foldr (:) []"
    [ env (setupAll n) $ \ ~(xs, vec, pvec, sq) ->
      bgroup (show n)
      [ bench "[]" $ nf (Prelude.foldr (:) []) xs
      , bench "Vector" $ nf V.toList vec
      , bench "PVector" $ nf P.toList pvec
      , bench "Seq" $ nf F.toList sq
      ]
    | n <- sizes
    ]
  , bgroup "map (+1)"
    [ env (setupAll n) $ \ ~(xs, vec, pvec, sq) ->
      bgroup (show n)
      [ bench "[]" $ nf (Prelude.map (+1)) xs
      , bench "Vector" $ nf (V.map (+1)) vec
      , bench "PVector" $ nf (P.map (+1)) pvec
      , bench "Seq" $ nf (fmap (+1)) sq
      ]
    | n <- sizes
    ]
  , bgroup "filter even"
    [ env (setupAll n) $ \ ~(xs, vec, pvec, sq) ->
      bgroup (show n)
      [ bench "[]" $ nf (Prelude.filter even) xs
      , bench "Vector" $ nf (V.filter even) vec
      , bench "PVector" $ nf (P.filter even) pvec
      , bench "Seq" $ nf (Seq.filter even) sq
      ]
    | n <- sizes
    ]
  , bgroup "filter (> 0) (keeps all)"
    [ env (setupAll n) $ \ ~(xs, vec, pvec, sq) ->
      bgroup (show n)
      [ bench "[]" $ nf (Prelude.filter (> (0 :: Int))) xs
      , bench "Vector" $ nf (V.filter (> (0 :: Int))) vec
      , bench "PVector" $ nf (P.filter (> (0 :: Int))) pvec
      , bench "Seq" $ nf (Seq.filter (> (0 :: Int))) sq
      ]
    | n <- sizes
    ]
  , bgroup "reverse"
    [ env (setupAll n) $ \ ~(xs, vec, pvec, sq) ->
      bgroup (show n)
      [ bench "[]" $ nf Prelude.reverse xs
      , bench "Vector" $ nf V.reverse vec
      , bench "PVector" $ nf P.reverse pvec
      , bench "Seq" $ nf Seq.reverse sq
      ]
    | n <- sizes
    ]
  , bgroup "take (n/2)"
    [ env (setupAll n) $ \ ~(xs, vec, pvec, sq) ->
      let !h = n `div` 2 in
      bgroup (show n)
      [ bench "[]" $ nf (Prelude.take h) xs
      , bench "Vector" $ nf (V.take h) vec
      , bench "PVector" $ nf (P.take h) pvec
      , bench "Seq" $ nf (Seq.take h) sq
      ]
    | n <- sizes
    ]
  , bgroup "append"
    [ env (setupAll n) $ \ ~(xs, vec, pvec, sq) ->
      bgroup (show n)
      [ bench "[]" $ nf (\l -> l Prelude.++ l) xs
      , bench "Vector" $ nf (\v -> v V.++ v) vec
      , bench "PVector" $ nf (\v -> v P.++ v) pvec
      , bench "Seq" $ nf (\s -> s Seq.>< s) sq
      ]
    | n <- sizes
    ]
  , bgroup "cons (build n)"
    [ bgroup (show n)
      [ bench "[] (:)" $ nf (Prelude.foldr (:) []) [1..n]
      , bench "Vector cons" $ nf (Prelude.foldr V.cons V.empty) [1..n]
      , bench "PVector cons" $ nf (Prelude.foldr P.cons P.empty) [1..n]
      , bench "Seq <|" $ nf (Prelude.foldr (Seq.<|) Seq.empty) [1..n]
      ]
    | n <- sizes
    ]
  , bgroup "replicate"
    [ bgroup (show n)
      [ bench "[]" $ nf (Prelude.replicate n) (42 :: Int)
      , bench "Vector" $ nf (V.replicate n) (42 :: Int)
      , bench "PVector" $ nf (P.replicate n) (42 :: Int)
      , bench "Seq" $ nf (Seq.replicate n) (42 :: Int)
      ]
    | n <- sizes
    ]
  , bgroup "length"
    [ env (setupAll n) $ \ ~(xs, vec, pvec, sq) ->
      bgroup (show n)
      [ bench "[]" $ nf Prelude.length xs
      , bench "Vector" $ nf V.length vec
      , bench "PVector" $ nf P.length pvec
      , bench "Seq" $ nf Seq.length sq
      ]
    | n <- sizes
    ]
  , bgroup "null"
    [ env (setupAll n) $ \ ~(xs, vec, pvec, sq) ->
      bgroup (show n)
      [ bench "[]" $ nf Prelude.null xs
      , bench "Vector" $ nf V.null vec
      , bench "PVector" $ nf P.null pvec
      , bench "Seq" $ nf Seq.null sq
      ]
    | n <- sizes
    ]
  , bgroup "chunk foldl' (+)"
    [ env (setupPV n) $ \ ~pvec ->
      bgroup (show n)
      [ bench "PVector foldChunks" $ nf
          (P.foldChunks (\acc _ arr -> foldSmallArray (+) acc arr) (0::Int)) pvec
      ]
    | n <- sizes
    ]
  , bgroup "unsnoc (drain n)"
    [ env (setupPV n) $ \ ~pvec ->
      bgroup (show n)
      [ bench "PVector" $ nf drainUnsnoc pvec
      ]
    | n <- sizes
    ]
  , bgroup "foldl' . map (fused)"
    [ env (setupAll n) $ \ ~(_xs, vec, pvec, _sq) ->
      bgroup (show n)
      [ bench "Vector"  $ nf (V.foldl' (+) (0 :: Int) . V.map (+1)) vec
      , bench "PVector" $ nf (P.foldl' (+) (0 :: Int) . P.map (+1)) pvec
      ]
    | n <- sizes
    ]
  , bgroup "foldl' . filter (fused)"
    [ env (setupAll n) $ \ ~(_xs, vec, pvec, _sq) ->
      bgroup (show n)
      [ bench "Vector"  $ nf (V.foldl' (+) (0 :: Int) . V.filter even) vec
      , bench "PVector" $ nf (P.foldl' (+) (0 :: Int) . P.filter even) pvec
      ]
    | n <- sizes
    ]
  , bgroup "map . map (fused)"
    [ env (setupAll n) $ \ ~(_xs, vec, pvec, _sq) ->
      bgroup (show n)
      [ bench "Vector"  $ nf (V.map (+2) . V.map (+1)) vec
      , bench "PVector" $ nf (P.map (+2) . P.map (+1)) pvec
      ]
    | n <- sizes
    ]
  , bgroup "foldl' . map . filter (fused)"
    [ env (setupAll n) $ \ ~(_xs, vec, pvec, _sq) ->
      bgroup (show n)
      [ bench "Vector"  $ nf (V.foldl' (+) (0::Int) . V.map (+1) . V.filter even) vec
      , bench "PVector" $ nf (P.foldl' (+) (0::Int) . P.map (+1) . P.filter even) pvec
      ]
    | n <- sizes
    ]
  , bgroup "foldl' . take (fused)"
    [ env (setupAll n) $ \ ~(_xs, vec, pvec, _sq) ->
      let !h = n `div` 2 in
      bgroup (show n)
      [ bench "Vector"  $ nf (V.foldl' (+) (0::Int) . V.take h) vec
      , bench "PVector" $ nf (P.foldl' (+) (0::Int) . P.take h) pvec
      ]
    | n <- sizes
    ]
  ]

foldSmallArray :: (b -> a -> b) -> b -> SmallArray a -> b
foldSmallArray f z0 arr = go z0 0
  where
    !n = sizeofSmallArray arr
    go !z !i | i >= n = z
             | otherwise = go (f z (indexSmallArray arr i)) (i+1)

drainUnsnoc :: P.Vector Int -> Int
drainUnsnoc = go 0
  where go !acc v = case P.unsnoc v of
          Nothing     -> acc
          Just (v',x) -> go (acc+x) v'

sizes :: [Int]
sizes = [100, 1000, 10000]

setupAll :: Int -> IO ([Int], V.Vector Int, P.Vector Int, Seq.Seq Int)
setupAll n = do
  let !xs = force [1..n]
      !vec = V.fromList xs
      !pvec = P.fromList xs
      !sq = Seq.fromList xs
  pure (xs, vec, pvec, sq)

setupPV :: Int -> IO (P.Vector Int)
setupPV n = pure $! P.fromList [1..n]
