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
      [ bench "Vector snoc" $ nf (L.foldl' V.snoc V.empty) [1..n]
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
    [ env (setupAll n) $ \ ~(_, vec, pvec, sq) ->
      bgroup (show n)
      [ bench "Vector" $ nf (\v -> v V.! mid) vec
      , bench "PVector" $ nf (\v -> P.index v mid) pvec
      , bench "Seq" $ nf (\s -> Seq.index s mid) sq
      ]
    | n <- sizes, let mid = n `div` 2
    ]
  , bgroup "head"
    [ env (setupAll n) $ \ ~(_, vec, pvec, sq) ->
      bgroup (show n)
      [ bench "Vector" $ nf V.head vec
      , bench "PVector" $ nf P.head pvec
      , bench "Seq" $ nf (\s -> case Seq.viewl s of (x Seq.:< _) -> x; _ -> 0) sq
      ]
    | n <- sizes
    ]
  , bgroup "last"
    [ env (setupAll n) $ \ ~(_, vec, pvec, sq) ->
      bgroup (show n)
      [ bench "Vector" $ nf V.last vec
      , bench "PVector" $ nf P.last pvec
      , bench "Seq" $ nf (\s -> case Seq.viewr s of (_ Seq.:> x) -> x; _ -> 0) sq
      ]
    | n <- sizes
    ]
  , bgroup "update (single, middle)"
    [ env (setupAll n) $ \ ~(_, vec, pvec, sq) ->
      bgroup (show n)
      [ bench "Vector" $ nf (\v -> v V.// [(mid, 0)]) vec
      , bench "PVector" $ nf (P.update mid 0) pvec
      , bench "Seq" $ nf (Seq.update mid 0) sq
      ]
    | n <- sizes, let mid = n `div` 2
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
  , bgroup "reverse"
    [ env (setupAll n) $ \ ~(_, vec, pvec, sq) ->
      bgroup (show n)
      [ bench "Vector" $ nf V.reverse vec
      , bench "PVector" $ nf P.reverse pvec
      , bench "Seq" $ nf Seq.reverse sq
      ]
    | n <- sizes
    ]
  , bgroup "take (n/2)"
    [ env (setupAll n) $ \ ~(_, vec, pvec, sq) ->
      bgroup (show n)
      [ bench "Vector" $ nf (V.take (n `div` 2)) vec
      , bench "PVector" $ nf (P.take (n `div` 2)) pvec
      , bench "Seq" $ nf (Seq.take (n `div` 2)) sq
      ]
    | n <- sizes
    ]
  , bgroup "drop (n/2)"
    [ env (setupAll n) $ \ ~(_, vec, pvec, sq) ->
      bgroup (show n)
      [ bench "Vector" $ nf (V.drop (n `div` 2)) vec
      , bench "PVector" $ nf (P.drop (n `div` 2)) pvec
      , bench "Seq" $ nf (Seq.drop (n `div` 2)) sq
      ]
    | n <- sizes
    ]
  , bgroup "append"
    [ env (setupAll n) $ \ ~(_, vec, pvec, sq) ->
      bgroup (show n)
      [ bench "Vector" $ nf (\v -> v V.++ v) vec
      , bench "PVector" $ nf (\v -> v P.++ v) pvec
      , bench "Seq" $ nf (\s -> s Seq.>< s) sq
      ]
    | n <- sizes
    ]
  , bgroup "cons (build n)"
    [ bgroup (show n)
      [ bench "Vector cons" $ nf (Prelude.foldr V.cons V.empty) [1..n]
      , bench "PVector cons" $ nf (Prelude.foldr P.cons P.empty) [1..n]
      , bench "Seq <|" $ nf (Prelude.foldr (Seq.<|) Seq.empty) [1..n]
      ]
    | n <- sizes
    ]
  , bgroup "unsnoc (drain n)"
    [ env (setupAll n) $ \ ~(_, vec, pvec, _) ->
      bgroup (show n)
      [ bench "PVector" $ nf drainUnsnoc pvec
      ]
    | n <- sizes
    ]
  , bgroup "replicate"
    [ bgroup (show n)
      [ bench "Vector" $ nf (V.replicate n) (42 :: Int)
      , bench "PVector" $ nf (P.replicate n) (42 :: Int)
      , bench "Seq" $ nf (Seq.replicate n) (42 :: Int)
      ]
    | n <- sizes
    ]
  , bgroup "generate"
    [ bgroup (show n)
      [ bench "Vector" $ nf (V.generate n) id
      , bench "PVector" $ nf (P.generate n) id
      ]
    | n <- sizes
    ]
  , bgroup "zip"
    [ env (setupAll n) $ \ ~(_, vec, pvec, _) ->
      bgroup (show n)
      [ bench "Vector" $ nf (V.zip vec) vec
      , bench "PVector" $ nf (P.zip pvec) pvec
      ]
    | n <- sizes
    ]
  , bgroup "scanl' (+)"
    [ env (setupAll n) $ \ ~(_, vec, pvec, _) ->
      bgroup (show n)
      [ bench "Vector" $ nf (V.scanl' (+) (0::Int)) vec
      , bench "PVector" $ nf (P.scanl' (+) (0::Int)) pvec
      ]
    | n <- sizes
    ]
  , bgroup "length"
    [ env (setupAll n) $ \ ~(_, vec, pvec, sq) ->
      bgroup (show n)
      [ bench "Vector" $ nf V.length vec
      , bench "PVector" $ nf P.length pvec
      , bench "Seq" $ nf Seq.length sq
      ]
    | n <- sizes
    ]
  , bgroup "null"
    [ env (setupAll n) $ \ ~(_, vec, pvec, sq) ->
      bgroup (show n)
      [ bench "Vector" $ nf V.null vec
      , bench "PVector" $ nf P.null pvec
      , bench "Seq" $ nf Seq.null sq
      ]
    | n <- sizes
    ]
  , bgroup "chunk foldl' (+)"
    [ env (setupAll n) $ \ ~(_, _, pvec, _) ->
      bgroup (show n)
      [ bench "PVector foldChunks" $ nf
          (P.foldChunks (\acc _ arr -> foldSmallArray (+) acc arr) (0::Int)) pvec
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
