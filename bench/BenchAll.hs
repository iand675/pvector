module Main where

import Criterion
import Criterion.Main
import Data.List
import GHC.Exts
import qualified Data.Vector as V
import qualified Data.Vector.Persistent as P

testEnv :: Int -> IO ([Int], V.Vector Int, P.Vector Int)
testEnv pow = do
  let ns = [1 .. 32 ^ pow]
  return (ns, fromList ns, fromList ns)

main = defaultMain
  [
{-
  bgroup "Alternative" []
  bgroup "Monad" []
  bgroup "Functor" []
  bgroup "MonadPlus" []
  bgroup "Applicative" []
  bgroup "Foldable" []
  bgroup "Traversable" []
  bgroup "IsList" []
-}
    env (testEnv 3) $ \ ~(l, v, pv) ->
    bgroup "Eq" [
      bench "[Int] (==)" $ whnf (== l) l,
      bench "Vector Int (==)" $ whnf (== v) v,
      bench "PersistentVector Int (==)" $ whnf (== pv) pv
    ]
  ,
{-
  bgroup "Data" []
  bgroup "Ord" []
  bgroup "Read" []
  bgroup "Show" []
  bgroup "Monoid" []
  bgroup "NFData" []
-}
    env (testEnv 3) $ \ ~(l, v, pv) ->
    bgroup "length" [
      bench "[Int] length" $ whnf length l,
      bench "Vector Int length" $ whnf length v,
      bench "PersistentVector Int length" $ whnf length pv
    ]
  ,
    env (testEnv 3) $ \ ~(l, v, pv) ->
    bgroup "null" [
      bench "[Int] null" $ whnf null l,
      bench "Vector Int null" $ whnf null v,
      bench "PersistentVector Int null" $ whnf null pv
    ]
  ,
    let end = pred (32 ^ 3) in
    env (testEnv 3) $ \ ~(l, v, pv) ->
    bgroup "index" [
      bench "[]" $ nf (!! end) l,
      bench "Vector" $ nf (V.! end) v,
      bench "PersistentVector" $ nf (end P.!) pv
    ]
  ,
    let end = pred (32 ^ 3) in
    env (testEnv 3) $ \ ~(l, v, pv) ->
    bgroup "unsafeIndex" [
      bench "Vector" $ nf (`V.unsafeIndex` end) v,
      bench "PersistentVector" $ nf (`P.unsafeIndex` end) pv
    ]
{-
  bgroup "(!?)" []
-}
  ,
    env (testEnv 3) $ \ ~(l, v, pv) ->
    bgroup "head" [
      bench "[]" $ whnf head l,
      bench "Vector" $ whnf V.head v,
      bench "PersistentVector" $ whnf P.head pv
    ]
  ,
    env (testEnv 3) $ \ ~(l, v, pv) ->
    bgroup "unsafeHead" [
      bench "Vector" $ whnf V.unsafeHead v,
      bench "PersistentVector" $ whnf P.unsafeHead pv
    ]
  ,
    env (testEnv 3) $ \ ~(l, v, pv) ->
    bgroup "last" [
      bench "[]" $ whnf last l,
      bench "Vector" $ whnf V.last v,
      bench "PersistentVector" $ whnf P.last pv
    ]
  ,
    env (testEnv 3) $ \ ~(l, v, pv) ->
    bgroup "unsafeLast" [
      bench "Vector" $ whnf V.unsafeLast v,
      bench "PersistentVector" $ whnf P.unsafeLast pv
    ]
{-
  bgroup "indexM" []
  bgroup "headM" []
  bgroup "lastM" []
  bgroup "unsafeIndexM" []
  bgroup "unsafeHeadM" []
  bgroup "unsafeLastM" []
  bgroup "slice" []
  bgroup "init" []
  bgroup "tail" []
  bgroup "take" []
  bgroup "drop" []
  bgroup "splitAt" []
  bgroup "unsafeSlice" []
  bgroup "unsafeInit" []
  bgroup "unsafeTail" []
  bgroup "unsafeTake" []
  bgroup "unsafeDrop" []
-}
  ,
    env (testEnv 3) $ \ ~(l, v, pv) ->
    bgroup "empty" [
      bench "[]" $ whnf (const []) (),
      bench "Vector" $ whnf (const V.empty) (),
      bench "PersistentVector" $ whnf (const P.empty) ()
    ]
  ,
    env (testEnv 3) $ \ ~(l, v, pv) ->
    bgroup "singleton" [
      bench "[]" $ whnf (:[]) (),
      bench "Vector" $ whnf V.singleton (),
      bench "PersistentVector" $ whnf P.singleton ()
    ]
{-
  bgroup "replicate" []
  bgroup "generate" []
  bgroup "iterateN" []
  bgroup "replicateM" []
  bgroup "generateM" []
  bgroup "create" []
  bgroup "unfoldr" []
  bgroup "unfoldrN" []
  bgroup "constructN" []
  bgroup "constructrN" []
  bgroup "enumFromN" []
  bgroup "enumFromStepN" []
  bgroup "enumFromTo" []
  bgroup "enumFromThenTo" []
  bgroup "cons" []
-}
  ,
    let end = pred (32 ^ 3) in
    bgroup "snoc" [
      bench "[]" $ nf (foldr (\x l -> l ++ [x]) []) ([1..end] :: [Int]),
      bench "Vector" $ nf (foldr (\x l -> V.snoc l x) V.empty) ([1..end] :: [Int]),
      bench "PersistentVector" $ nf (foldr (\x l -> P.snoc l x) P.empty) ([1..end] :: [Int])
    ]
{-
  bgroup "(++)" []
  bgroup "concat" []
  bgroup "force" []
  bgroup "(//)" []
  bgroup "update" []
  bgroup "update_" []
  bgroup "unsafeUpd" []
  bgroup "unsafeUpdate" []
  bgroup "unsafeUpdate_" []
  bgroup "accum" []
  bgroup "accumulate" []
  bgroup "unsafeAccum" []
  bgroup "unsafeAccumulate_" []
  bgroup "reverse" []
  bgroup "backpermute" []
  bgroup "unsafeBackpermute" []
  bgroup "modify" []
  bgroup "indexed" []
  bgroup "map" []
  bgroup "imap" []
  bgroup "concatMap" []
  bgroup "mapM" []
  bgroup "imapM" []
  bgroup "mapM_" []
  bgroup "imapM_" []
  bgroup "forM" []
  bgroup "forM_" []
  bgroup "zipWith"
  bgroup "zipWith3"
  bgroup "zipWith4" []
  bgroup "zipWith5" []
  bgroup "zipWith6" []
  bgroup "izipWith" []
  bgroup "izipWith3" []
  bgroup "izipWith4" []
  bgroup "izipWith5" []
  bgroup "izipWith6" []
  bgroup "zip" []
  bgroup "zip3" []
  bgroup "zip4" []
  bgroup "zip5" []
  bgroup "zip6" []
  bgroup "zipWithM" []
  bgroup "izipWithM" []
  bgroup "zipWithM_" []
  bgroup "izipWithM_" []
  bgroup "unzip" []
  bgroup "unzip3" []
  bgroup "unzip4" []
  bgroup "unzip5" []
  bgroup "unzip6" []
  bgroup "filter" []
  bgroup "ifilter" []
  bgroup "filterM" []
  bgroup "takeWhile" []
  bgroup "dropWhile" []
  bgroup "partition" []
  bgroup "unstablePartition" []
  bgroup "span" []
  bgroup "break" []
  bgroup "elem" []
  bgroup "notElem" []
  bgroup "find" []
  bgroup "findIndex" []
  bgroup "elemIndex" []
  bgroup "elemIndices" []
  bgroup "foldl" []
  bgroup "foldl1" []
  bgroup "foldl'" []
  bgroup "foldl1'" []
  bgroup "foldr" []
  bgroup "foldr1" []
  bgroup "foldr'" []
  bgroup "foldr1'" []
  bgroup "ifoldl" []
  bgroup "ifoldl'" []
  bgroup "ifoldr" []
  bgroup "ifoldr'" []
  bgroup "all" []
  bgroup "any" []
  bgroup "and" []
  bgroup "or" []
  bgroup "sum" []
  bgroup "product" []
  bgroup "maximum" []
  bgroup "maximumBy" []
  bgroup "minimum" []
  bgroup "minimumBy" []
  bgroup "minIndex" []
  bgroup "minIndexBy" []
  bgroup "maxIndex" []
  bgroup "maxIndexBy" []
  bgroup "foldM" []
  bgroup "ifoldM" []
  bgroup "foldM'" []
  bgroup "ifoldM'" []
  bgroup "fold1M" []
  bgroup "fold1M'" []
  bgroup "foldM_" []
  bgroup "ifoldM_" []
  bgroup "foldM'_" []
  bgroup "ifoldM'_" []
  bgroup "fold1M_" []
  bgroup "fold1M'_" []
  bgroup "sequence" []
  bgroup "sequence_" []
  bgroup "prescanl" []
  bgroup "prescanl'" []
  bgroup "postscanl" []
  bgroup "postscanl'" []
  bgroup "scanl" []
  bgroup "scanl'" []
  bgroup "scanl1" []
  bgroup "scanl1'" []
  bgroup "prescanr" []
  bgroup "postscanr" []
  bgroup "postscanr'" []
  bgroup "scanr" []
  bgroup "scanr'" []
  bgroup "scanr1" []
  bgroup "scanr1'" []
  bgroup "toList" []
  bgroup "fromList" []
  bgroup "fromListN" []
  bgroup "transient" []
  -}
  {-
   -- transient
   bgroup "length" []
   bgroup "null" []
   bgroup "slice" []
   bgroup "init" []
   bgroup "tail" []
   bgroup "take" []
   bgroup "drop" []
   bgroup "splitAt" []
   bgroup "unsafeSlice" []
   bgroup "unsafeInit" []
   bgroup "unsafeTail" []
   bgroup "unsafeTake" []
   bgroup "unsafeDrop" []
   bgroup "overlaps" []
   bgroup "new"
   bgroup "unsafeNew"
   bgroup "replicate"
   bgroup "replicateM"
   bgroup "clone"
   bgroup "grow" []
   bgroup "unsafeGrow" []
   bgroup "clear" []
   bgroup "read"
   bgroup "write"
   bgroup "modify"
   bgroup "swap"
   bgroup "unsafeRead"
   bgroup "unsafeWrite"
   bgroup "unsafeModify"
   bgroup "unsafeSwap"
   bgroup "set"
   bgroup "copy"
   bgroup "move"
   bgroup "unsafeCopy"
   bgroup "unsafeMove"
   -}
  ]
