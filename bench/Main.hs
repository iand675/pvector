module Main (main) where

import Criterion
import Criterion.Main
import Control.DeepSeq
import qualified Data.Vector as V
import qualified Data.PVector.Back as P
import qualified Data.List as L

main :: IO ()
main = defaultMain
  [ bgroup "fromList"
    [ env (pure [1..n]) $ \xs ->
      bgroup (show n)
      [ bench "[]"     $ nf id xs
      , bench "Vector" $ nf V.fromList xs
      , bench "PVector" $ nf P.fromList xs
      ]
    | n <- sizes
    ]
  , bgroup "snoc"
    [ bgroup (show n)
      [ bench "[] (++ [x])" $ nf (L.foldl' (\l x -> l ++ [x]) []) [1..n]
      , bench "Vector snoc" $ nf (L.foldl' V.snoc V.empty) [1..n]
      , bench "PVector snoc" $ nf (L.foldl' P.snoc P.empty) [1..n]
      ]
    | n <- sizes
    ]
  , bgroup "index"
    [ env (setupEnv n) $ \ ~(xs, vec, pvec) ->
      bgroup (show n)
      [ bench "[] (!!)" $ nf (\l -> l !! (n `div` 2)) xs
      , bench "Vector (!)" $ nf (\v -> v V.! (n `div` 2)) vec
      , bench "PVector (!)" $ nf (\v -> P.index v (n `div` 2)) pvec
      ]
    | n <- sizes
    ]
  , bgroup "head"
    [ env (setupEnv n) $ \ ~(xs, vec, pvec) ->
      bgroup (show n)
      [ bench "[]" $ nf Prelude.head xs
      , bench "Vector" $ nf V.head vec
      , bench "PVector" $ nf P.head pvec
      ]
    | n <- sizes
    ]
  , bgroup "last"
    [ env (setupEnv n) $ \ ~(xs, vec, pvec) ->
      bgroup (show n)
      [ bench "[]" $ nf Prelude.last xs
      , bench "Vector" $ nf V.last vec
      , bench "PVector" $ nf P.last pvec
      ]
    | n <- sizes
    ]
  , bgroup "foldl' (+)"
    [ env (setupEnv n) $ \ ~(xs, vec, pvec) ->
      bgroup (show n)
      [ bench "[]" $ nf (L.foldl' (+) (0 :: Int)) xs
      , bench "Vector" $ nf (V.foldl' (+) (0 :: Int)) vec
      , bench "PVector" $ nf (P.foldl' (+) (0 :: Int)) pvec
      ]
    | n <- sizes
    ]
  , bgroup "map (+1)"
    [ env (setupEnv n) $ \ ~(xs, vec, pvec) ->
      bgroup (show n)
      [ bench "[]" $ nf (Prelude.map (+1)) xs
      , bench "Vector" $ nf (V.map (+1)) vec
      , bench "PVector" $ nf (P.map (+1)) pvec
      ]
    | n <- sizes
    ]
  , bgroup "filter even"
    [ env (setupEnv n) $ \ ~(xs, vec, pvec) ->
      bgroup (show n)
      [ bench "[]" $ nf (Prelude.filter even) xs
      , bench "Vector" $ nf (V.filter even) vec
      , bench "PVector" $ nf (P.filter even) pvec
      ]
    | n <- sizes
    ]
  , bgroup "foldr toList"
    [ env (setupEnv n) $ \ ~(xs, vec, pvec) ->
      bgroup (show n)
      [ bench "[]" $ nf (Prelude.foldr (:) []) xs
      , bench "Vector" $ nf V.toList vec
      , bench "PVector" $ nf P.toList pvec
      ]
    | n <- sizes
    ]
  , bgroup "update (middle)"
    [ env (setupEnv n) $ \ ~(_xs, vec, pvec) ->
      let !mid = n `div` 2 in
      bgroup (show n)
      [ bench "Vector (//) single" $ nf (\v -> v V.// [(mid, 0)]) vec
      , bench "PVector update" $ nf (P.update mid 0) pvec
      ]
    | n <- sizes
    ]
  , bgroup "length"
    [ env (setupEnv n) $ \ ~(xs, vec, pvec) ->
      bgroup (show n)
      [ bench "[]" $ nf Prelude.length xs
      , bench "Vector" $ nf V.length vec
      , bench "PVector" $ nf P.length pvec
      ]
    | n <- sizes
    ]
  , bgroup "null"
    [ env (setupEnv n) $ \ ~(xs, vec, pvec) ->
      bgroup (show n)
      [ bench "[]" $ nf Prelude.null xs
      , bench "Vector" $ nf V.null vec
      , bench "PVector" $ nf P.null pvec
      ]
    | n <- sizes
    ]
  , bgroup "recycling: v // ps // qs"
    [ env (setupRecycle n) $ \ ~(pvec, ps, qs) ->
      bgroup (show n)
      [ bench "two separate //" $ nf (\v -> (v P.// ps) P.// qs) pvec
      , bench "single // (merged)" $ nf (\v -> v P.// (ps Prelude.++ qs)) pvec
      ]
    | n <- sizes
    ]
  , bgroup "recycling: map (+1) (v // ps)"
    [ env (setupRecycle n) $ \ ~(pvec, ps, _) ->
      bgroup (show n)
      [ bench "map then //" $ nf (\v -> P.map (+1) (v P.// ps)) pvec
      , bench "// then map" $ nf (\v -> P.map (+1) v P.// ps) pvec
      ]
    | n <- sizes
    ]
  , bgroup "recycling: map (+1) . map (*2)"
    [ env (setupEnv n) $ \ ~(_xs, _vec, pvec) ->
      bgroup (show n)
      [ bench "Vector" $ nf (V.map (+1) . V.map (*2)) (V.fromList [1..n])
      , bench "PVector (fused)" $ nf (P.map (+1) . P.map (*2)) pvec
      ]
    | n <- sizes
    ]
  ]

sizes :: [Int]
sizes = [100, 1000, 10000]

setupEnv :: Int -> IO ([Int], V.Vector Int, P.Vector Int)
setupEnv n = do
  let !xs = force [1..n]
      !vec = V.fromList xs
      !pvec = P.fromList xs
  pure (xs, vec, pvec)

setupRecycle :: Int -> IO (P.Vector Int, [(Int, Int)], [(Int, Int)])
setupRecycle n = do
  let !pvec = P.fromList [1..n]
      !ps = force [(i, i * 10) | i <- [0, 3 .. n - 1]]
      !qs = force [(i, i * 20) | i <- [1, 4 .. n - 1]]
  pure (pvec, ps, qs)
