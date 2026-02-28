module Fuzz (fuzzTests) where

import Control.Monad (when)
import Control.Monad.ST (ST, runST)
import Data.Monoid (Sum(..))
import Data.Primitive.SmallArray (sizeofSmallArray)
import qualified Data.List as L
import qualified Data.Maybe as Maybe

import Test.Tasty
import Test.Tasty.Hedgehog
import qualified Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import qualified Data.PVector.Back as V
import Data.PVector.Back (Vector(..))
import qualified Data.PVector.Front as F
import qualified Data.PVector.Deque as D

------------------------------------------------------------------------
-- Action ADT
------------------------------------------------------------------------

data Action
  = Snoc Int
  | Cons Int
  | Unsnoc
  | Uncons
  | Append [Int]
  | UpdateAt Int Int
  | AdjustAt Int Int
  | TakeN Int
  | DropN Int
  | MapIncr Int
  | FilterMod Int
  | Reverse
  | Init
  | Tail
  | SliceOp Int Int
  | SplitConcat Int
  | BatchUpdate [(Int, Int)]
  | IMapAdd
  | ConcatMapId
  | MapMaybeEven
  | IFilterEvenIdx
  | TakeWhileLt Int
  | DropWhileLt Int
  | Scanl'Add
  | ReplicateOp Int Int
  | GenerateOp Int
  deriving (Show)

genVal :: Hedgehog.Gen Int
genVal = Gen.int (Range.linear 0 10000)

genSmallVal :: Hedgehog.Gen Int
genSmallVal = Gen.int (Range.linear 0 100)

genAction :: Hedgehog.Gen Action
genAction = Gen.frequency
  [ (5, Snoc <$> genVal)
  , (5, Cons <$> genVal)
  , (2, pure Unsnoc)
  , (2, pure Uncons)
  , (3, Append <$> Gen.list (Range.linear 0 50) genVal)
  , (3, UpdateAt <$> Gen.int (Range.linear 0 500) <*> genVal)
  , (2, AdjustAt <$> Gen.int (Range.linear 0 500) <*> genVal)
  , (2, TakeN <$> Gen.int (Range.linear 0 500))
  , (2, DropN <$> Gen.int (Range.linear 0 500))
  , (1, MapIncr <$> Gen.int (Range.linear (-100) 100))
  , (1, FilterMod <$> Gen.int (Range.linear 2 5))
  , (2, pure Reverse)
  , (1, pure Init)
  , (1, pure Tail)
  , (1, SliceOp <$> Gen.int (Range.linear 0 500) <*> Gen.int (Range.linear 0 500))
  , (2, SplitConcat <$> Gen.int (Range.linear 0 500))
  , (1, BatchUpdate <$> Gen.list (Range.linear 1 10)
         ((,) <$> Gen.int (Range.linear 0 500) <*> genVal))
  , (1, pure IMapAdd)
  , (1, pure ConcatMapId)
  , (1, pure MapMaybeEven)
  , (1, pure IFilterEvenIdx)
  , (1, TakeWhileLt <$> genVal)
  , (1, DropWhileLt <$> genVal)
  , (1, pure Scanl'Add)
  , (1, ReplicateOp <$> Gen.int (Range.linear 0 100) <*> genVal)
  , (1, GenerateOp <$> Gen.int (Range.linear 0 100))
  ]

------------------------------------------------------------------------
-- Applying actions to (PVector, model list) pairs
------------------------------------------------------------------------

applyAction :: Action -> (V.Vector Int, [Int]) -> (V.Vector Int, [Int])
applyAction act (v, xs) = case act of
  Snoc x -> (V.snoc v x, xs ++ [x])
  Cons x -> (V.cons x v, x : xs)

  Unsnoc
    | null xs   -> (v, xs)
    | otherwise -> case V.unsnoc v of
        Nothing      -> (v, xs)
        Just (v', _) -> (v', Prelude.init xs)

  Uncons
    | null xs   -> (v, xs)
    | otherwise -> case V.uncons v of
        Nothing      -> (v, xs)
        Just (_, v') -> (v', Prelude.tail xs)

  Append ys -> (v <> V.fromList ys, xs ++ ys)

  UpdateAt rawI val
    | null xs   -> (v, xs)
    | otherwise ->
        let i = rawI `mod` length xs
        in (V.update i val v, listUpdate i val xs)

  AdjustAt rawI n
    | null xs   -> (v, xs)
    | otherwise ->
        let i = rawI `mod` length xs
        in (V.adjust' (+ n) i v, listAdjust (+ n) i xs)

  TakeN rawN ->
    let n = clamp 0 (length xs) rawN
    in (V.take n v, take n xs)

  DropN rawN ->
    let n = clamp 0 (length xs) rawN
    in (V.drop n v, drop n xs)

  MapIncr n -> (V.map (+ n) v, Prelude.map (+ n) xs)

  FilterMod n ->
    let p x = x `mod` n /= 0
    in (V.filter p v, filter p xs)

  Reverse -> (V.reverse v, reverse xs)

  Init
    | null xs   -> (v, xs)
    | otherwise -> (V.init v, Prelude.init xs)

  Tail
    | null xs   -> (v, xs)
    | otherwise -> (V.tail v, Prelude.tail xs)

  SliceOp rawOff rawLen ->
    let sz     = length xs
        off    = clamp 0 sz rawOff
        maxLen = sz - off
        sLen   = clamp 0 maxLen rawLen
    in (V.slice off sLen v, take sLen (drop off xs))

  SplitConcat rawN ->
    let n = clamp 0 (length xs) rawN
        (vl, vr) = V.splitAt n v
    in (vl <> vr, xs)

  BatchUpdate rawUpdates
    | null xs   -> (v, xs)
    | otherwise ->
        let sz = length xs
            updates = Prelude.map (\(i, val) -> (i `mod` sz, val)) rawUpdates
        in (v V.// updates, L.foldl' (\acc (i, val) -> listUpdate i val acc) xs updates)

  IMapAdd ->
    (V.imap (\i x -> x + i) v, imapList (\i x -> x + i) xs)

  ConcatMapId ->
    (V.concatMap V.singleton v, xs)

  MapMaybeEven ->
    let f x = if even x then Just x else Nothing
    in (V.mapMaybe f v, Maybe.mapMaybe f xs)

  IFilterEvenIdx ->
    (V.ifilter (\i _ -> even i) v, ifilterList (\i _ -> even i) xs)

  TakeWhileLt n ->
    (V.takeWhile (< n) v, takeWhile (< n) xs)

  DropWhileLt n ->
    (V.dropWhile (< n) v, dropWhile (< n) xs)

  Scanl'Add ->
    (V.scanl' (+) 0 v, L.scanl' (+) 0 xs)

  ReplicateOp n x ->
    let n' = clamp 0 200 n
    in (V.replicate n' x, Prelude.replicate n' x)

  GenerateOp n ->
    let n' = clamp 0 200 n
    in (V.generate n' id, Prelude.map id [0 .. n' - 1])

------------------------------------------------------------------------
-- List helpers
------------------------------------------------------------------------

clamp :: Int -> Int -> Int -> Int
clamp lo hi x = max lo (min hi x)

listUpdate :: Int -> a -> [a] -> [a]
listUpdate _ _ [] = []
listUpdate 0 x (_:ys) = x : ys
listUpdate n x (y:ys) = y : listUpdate (n - 1) x ys

listAdjust :: (a -> a) -> Int -> [a] -> [a]
listAdjust _ _ [] = []
listAdjust f 0 (x:ys) = f x : ys
listAdjust f n (y:ys) = y : listAdjust f (n - 1) ys

imapList :: (Int -> a -> b) -> [a] -> [b]
imapList f = go 0
  where
    go _ []     = []
    go i (x:xs) = f i x : go (i + 1) xs

ifilterList :: (Int -> a -> Bool) -> [a] -> [a]
ifilterList p = go 0
  where
    go _ []     = []
    go i (x:xs)
      | p i x     = x : go (i + 1) xs
      | otherwise  = go (i + 1) xs

------------------------------------------------------------------------
-- Structural invariant checks
------------------------------------------------------------------------

checkInvariants :: V.Vector Int -> Hedgehog.PropertyT IO ()
checkInvariants v = do
  let Vector sz _shift prefix _root tail_ = v
      pLen = sizeofSmallArray prefix
      tLen = sizeofSmallArray tail_
  Hedgehog.annotate $
    "vSize=" ++ show sz
    ++ " prefixLen=" ++ show pLen
    ++ " tailLen=" ++ show tLen
  Hedgehog.assert (pLen >= 0 && pLen <= 32)
  Hedgehog.assert (tLen >= 0 && tLen <= 32)
  when (sz == 0) $ do
    pLen Hedgehog.=== 0
    tLen Hedgehog.=== 0

------------------------------------------------------------------------
-- Sequential state machine runner
------------------------------------------------------------------------

runActions :: (V.Vector Int, [Int]) -> [Action] -> Hedgehog.PropertyT IO ()
runActions _ [] = pure ()
runActions (v, xs) (a:as) = do
  let (v', xs') = applyAction a (v, xs)
  Hedgehog.annotate $ "Action: " ++ show a
  V.toList v' Hedgehog.=== xs'
  V.length v' Hedgehog.=== length xs'
  when (not (null xs')) $ do
    V.head v' Hedgehog.=== head xs'
    V.last v' Hedgehog.=== last xs'
    let midIdx = length xs' `div` 2
    V.index v' midIdx Hedgehog.=== xs' !! midIdx
  checkInvariants v'
  runActions (v', xs') as

------------------------------------------------------------------------
-- Front vector action ADT
------------------------------------------------------------------------

data FrontAction
  = FCons Int
  | FSnoc Int
  | FUncons
  | FUnsnoc
  | FMapIncr Int
  | FFilterMod Int
  | FReverse
  | FTakeN Int
  | FDropN Int
  | FInit
  | FTail
  | FAppend [Int]
  deriving (Show)

genFrontAction :: Hedgehog.Gen FrontAction
genFrontAction = Gen.frequency
  [ (5, FCons <$> genVal)
  , (3, FSnoc <$> genVal)
  , (2, pure FUncons)
  , (2, pure FUnsnoc)
  , (1, FMapIncr <$> genSmallVal)
  , (1, FFilterMod <$> Gen.int (Range.linear 2 5))
  , (1, pure FReverse)
  , (2, FTakeN <$> Gen.int (Range.linear 0 500))
  , (2, FDropN <$> Gen.int (Range.linear 0 500))
  , (1, pure FInit)
  , (1, pure FTail)
  , (2, FAppend <$> Gen.list (Range.linear 0 30) genVal)
  ]

applyFrontAction :: FrontAction -> (F.FrontVector Int, [Int]) -> (F.FrontVector Int, [Int])
applyFrontAction act (v, xs) = case act of
  FCons x -> (F.cons x v, x : xs)
  FSnoc x -> (F.snoc v x, xs ++ [x])
  FUncons
    | null xs   -> (v, xs)
    | otherwise -> case F.uncons v of
        Nothing      -> (v, xs)
        Just (_, v') -> (v', Prelude.tail xs)
  FUnsnoc
    | null xs   -> (v, xs)
    | otherwise -> case F.unsnoc v of
        Nothing       -> (v, xs)
        Just (v', _)  -> (v', Prelude.init xs)
  FMapIncr n -> (F.map (+ n) v, Prelude.map (+ n) xs)
  FFilterMod n ->
    let p x = x `mod` n /= 0
    in (F.filter p v, filter p xs)
  FReverse -> (F.reverse v, reverse xs)
  FTakeN rawN ->
    let n = clamp 0 (length xs) rawN
    in (F.take n v, take n xs)
  FDropN rawN ->
    let n = clamp 0 (length xs) rawN
    in (F.drop n v, drop n xs)
  FInit
    | null xs   -> (v, xs)
    | otherwise -> (F.init v, Prelude.init xs)
  FTail
    | null xs   -> (v, xs)
    | otherwise -> (F.tail v, Prelude.tail xs)
  FAppend ys -> (v <> F.fromList ys, xs ++ ys)

runFrontActions :: (F.FrontVector Int, [Int]) -> [FrontAction] -> Hedgehog.PropertyT IO ()
runFrontActions _ [] = pure ()
runFrontActions (v, xs) (a:as) = do
  let (v', xs') = applyFrontAction a (v, xs)
  Hedgehog.annotate $ "FrontAction: " ++ show a
  F.toList v' Hedgehog.=== xs'
  F.length v' Hedgehog.=== length xs'
  when (not (null xs')) $ do
    F.head v' Hedgehog.=== head xs'
    F.last v' Hedgehog.=== last xs'
  runFrontActions (v', xs') as

------------------------------------------------------------------------
-- Deque action ADT
------------------------------------------------------------------------

data DequeAction
  = DCons Int
  | DSnoc Int
  | DUncons
  | DUnsnoc
  | DMapIncr Int
  | DFilterMod Int
  | DReverse
  | DInit
  | DTail
  | DAppend [Int]
  deriving (Show)

genDequeAction :: Hedgehog.Gen DequeAction
genDequeAction = Gen.frequency
  [ (5, DCons <$> genVal)
  , (5, DSnoc <$> genVal)
  , (2, pure DUncons)
  , (2, pure DUnsnoc)
  , (1, DMapIncr <$> genSmallVal)
  , (1, DFilterMod <$> Gen.int (Range.linear 2 5))
  , (1, pure DReverse)
  , (1, pure DInit)
  , (1, pure DTail)
  , (2, DAppend <$> Gen.list (Range.linear 0 30) genVal)
  ]

applyDequeAction :: DequeAction -> (D.Deque Int, [Int]) -> (D.Deque Int, [Int])
applyDequeAction act (d, xs) = case act of
  DCons x -> (D.cons x d, x : xs)
  DSnoc x -> (D.snoc d x, xs ++ [x])
  DUncons
    | null xs   -> (d, xs)
    | otherwise -> case D.uncons d of
        Nothing      -> (d, xs)
        Just (_, d') -> (d', Prelude.tail xs)
  DUnsnoc
    | null xs   -> (d, xs)
    | otherwise -> case D.unsnoc d of
        Nothing       -> (d, xs)
        Just (d', _)  -> (d', Prelude.init xs)
  DMapIncr n -> (D.map (+ n) d, Prelude.map (+ n) xs)
  DFilterMod n ->
    let p x = x `mod` n /= 0
    in (D.filter p d, filter p xs)
  DReverse -> (D.reverse d, reverse xs)
  DInit
    | null xs   -> (d, xs)
    | otherwise -> (D.init d, Prelude.init xs)
  DTail
    | null xs   -> (d, xs)
    | otherwise -> (D.tail d, Prelude.tail xs)
  DAppend ys -> (d <> D.fromList ys, xs ++ ys)

runDequeActions :: (D.Deque Int, [Int]) -> [DequeAction] -> Hedgehog.PropertyT IO ()
runDequeActions _ [] = pure ()
runDequeActions (d, xs) (a:as) = do
  let (d', xs') = applyDequeAction a (d, xs)
  Hedgehog.annotate $ "DequeAction: " ++ show a
  D.toList d' Hedgehog.=== xs'
  D.length d' Hedgehog.=== length xs'
  when (not (null xs')) $ do
    D.head d' Hedgehog.=== head xs'
    D.last d' Hedgehog.=== last xs'
  runDequeActions (d', xs') as

------------------------------------------------------------------------
-- Test tree
------------------------------------------------------------------------

fuzzTests :: TestTree
fuzzTests = testGroup "Fuzz"
  [ sequentialTests
  , observationTests
  , instanceLawTests
  , constructionTests
  , transientTests
  , splitPairTests
  , frontFuzzTests
  , dequeFuzzTests
  ]

------------------------------------------------------------------------
-- Part 1: Sequential state machine tests
------------------------------------------------------------------------

sequentialTests :: TestTree
sequentialTests = testGroup "sequential"
  [ testProperty "ops from empty" $
      Hedgehog.withTests 200 $ Hedgehog.property $ do
        actions <- Hedgehog.forAll $ Gen.list (Range.linear 1 100) genAction
        runActions (V.empty, []) actions

  , testProperty "ops from fromList" $
      Hedgehog.withTests 200 $ Hedgehog.property $ do
        initial <- Hedgehog.forAll $ Gen.list (Range.linear 0 200) genVal
        actions <- Hedgehog.forAll $ Gen.list (Range.linear 1 50) genAction
        runActions (V.fromList initial, initial) actions

  , testProperty "ops from cons-built" $
      Hedgehog.withTests 200 $ Hedgehog.property $ do
        initial <- Hedgehog.forAll $ Gen.list (Range.linear 0 200) genVal
        actions <- Hedgehog.forAll $ Gen.list (Range.linear 1 50) genAction
        let v0 = foldr V.cons V.empty initial
        runActions (v0, initial) actions

  , testProperty "ops from appended" $
      Hedgehog.withTests 200 $ Hedgehog.property $ do
        xs <- Hedgehog.forAll $ Gen.list (Range.linear 0 100) genVal
        ys <- Hedgehog.forAll $ Gen.list (Range.linear 0 100) genVal
        actions <- Hedgehog.forAll $ Gen.list (Range.linear 1 50) genAction
        let v0 = V.fromList xs <> V.fromList ys
        runActions (v0, xs ++ ys) actions

  , testProperty "ops from snoc-built" $
      Hedgehog.withTests 200 $ Hedgehog.property $ do
        initial <- Hedgehog.forAll $ Gen.list (Range.linear 0 200) genVal
        actions <- Hedgehog.forAll $ Gen.list (Range.linear 1 50) genAction
        let v0 = L.foldl' V.snoc V.empty initial
        runActions (v0, initial) actions

  , testProperty "heavy append then ops" $
      Hedgehog.withTests 200 $ Hedgehog.property $ do
        chunks <- Hedgehog.forAll $ Gen.list (Range.linear 2 20)
                    (Gen.list (Range.linear 0 100) genVal)
        actions <- Hedgehog.forAll $ Gen.list (Range.linear 1 30) genAction
        let v0  = V.concat (Prelude.map V.fromList chunks)
            xs0 = Prelude.concat chunks
        runActions (v0, xs0) actions

  , testProperty "interleaved cons/snoc then ops" $
      Hedgehog.withTests 200 $ Hedgehog.property $ do
        ops <- Hedgehog.forAll $ Gen.list (Range.linear 1 200) $
          Gen.choice [ Left <$> genVal, Right <$> genVal ]
        let build (v', xs') e = case e of
              Left x  -> (V.cons x v', x : xs')
              Right x -> (V.snoc v' x, xs' ++ [x])
            (v0, xs0) = L.foldl' build (V.empty, []) ops
        V.toList v0 Hedgehog.=== xs0
        actions <- Hedgehog.forAll $ Gen.list (Range.linear 1 50) genAction
        runActions (v0, xs0) actions
  ]

------------------------------------------------------------------------
-- Part 2: Observation tests (query/fold/search/scan/zip on fromList)
------------------------------------------------------------------------

observationTests :: TestTree
observationTests = testGroup "observations"
  [ testGroup "folds" $
    [ testProperty "foldl' (+)" $ Hedgehog.property $ do
        xs <- Hedgehog.forAll $ Gen.list (Range.linear 0 300) genVal
        let v = V.fromList xs
        V.foldl' (+) 0 v Hedgehog.=== L.foldl' (+) 0 xs

    , testProperty "foldl (+)" $ Hedgehog.property $ do
        xs <- Hedgehog.forAll $ Gen.list (Range.linear 0 300) genVal
        let v = V.fromList xs
        V.foldl (+) 0 v Hedgehog.=== foldl (+) 0 xs

    , testProperty "foldr (:)" $ Hedgehog.property $ do
        xs <- Hedgehog.forAll $ Gen.list (Range.linear 0 300) genVal
        let v = V.fromList xs
        V.foldr (:) [] v Hedgehog.=== xs

    , testProperty "foldr' (+)" $ Hedgehog.property $ do
        xs <- Hedgehog.forAll $ Gen.list (Range.linear 0 300) genVal
        let v = V.fromList xs
        V.foldr' (+) 0 v Hedgehog.=== foldr (+) 0 xs

    , testProperty "foldl1' (+) on non-empty" $ Hedgehog.property $ do
        xs <- Hedgehog.forAll $ Gen.list (Range.linear 1 300) genVal
        let v = V.fromList xs
        V.foldl1' (+) v Hedgehog.=== L.foldl1' (+) xs

    , testProperty "foldr1 (+) on non-empty" $ Hedgehog.property $ do
        xs <- Hedgehog.forAll $ Gen.list (Range.linear 1 300) genVal
        let v = V.fromList xs
        V.foldr1 (+) v Hedgehog.=== foldr1 (+) xs

    , testProperty "ifoldl'" $ Hedgehog.property $ do
        xs <- Hedgehog.forAll $ Gen.list (Range.linear 0 300) genVal
        let v = V.fromList xs
            expected = L.foldl' (\acc (i, x) -> acc + i * x) 0 (zip [0..] xs)
        V.ifoldl' (\acc i x -> acc + i * x) 0 v Hedgehog.=== expected

    , testProperty "ifoldr" $ Hedgehog.property $ do
        xs <- Hedgehog.forAll $ Gen.list (Range.linear 0 300) genVal
        let v = V.fromList xs
            expected = foldr (\(i, x) acc -> i * x + acc) 0 (zip [0..] xs)
        V.ifoldr (\i x acc -> i * x + acc) 0 v Hedgehog.=== expected

    , testProperty "foldMap Sum" $ Hedgehog.property $ do
        xs <- Hedgehog.forAll $ Gen.list (Range.linear 0 300) genVal
        let v = V.fromList xs
        V.foldMap Sum v Hedgehog.=== foldMap Sum xs

    , testProperty "sum" $ Hedgehog.property $ do
        xs <- Hedgehog.forAll $ Gen.list (Range.linear 0 300) genVal
        let v = V.fromList xs
        V.sum v Hedgehog.=== sum xs

    , testProperty "product" $ Hedgehog.property $ do
        xs <- Hedgehog.forAll $ Gen.list (Range.linear 1 20) (Gen.int (Range.linear 1 10))
        let v = V.fromList xs
        V.product v Hedgehog.=== product xs

    , testProperty "maximum on non-empty" $ Hedgehog.property $ do
        xs <- Hedgehog.forAll $ Gen.list (Range.linear 1 300) genVal
        let v = V.fromList xs
        V.maximum v Hedgehog.=== maximum xs

    , testProperty "minimum on non-empty" $ Hedgehog.property $ do
        xs <- Hedgehog.forAll $ Gen.list (Range.linear 1 300) genVal
        let v = V.fromList xs
        V.minimum v Hedgehog.=== minimum xs

    , testProperty "all even" $ Hedgehog.property $ do
        xs <- Hedgehog.forAll $ Gen.list (Range.linear 0 300) genVal
        let v = V.fromList xs
        V.all even v Hedgehog.=== all even xs

    , testProperty "any even" $ Hedgehog.property $ do
        xs <- Hedgehog.forAll $ Gen.list (Range.linear 0 300) genVal
        let v = V.fromList xs
        V.any even v Hedgehog.=== any even xs
    ]

  , testGroup "searching" $
    [ testProperty "elem" $ Hedgehog.property $ do
        xs <- Hedgehog.forAll $ Gen.list (Range.linear 0 200) genVal
        x <- Hedgehog.forAll genVal
        let v = V.fromList xs
        V.elem x v Hedgehog.=== L.elem x xs

    , testProperty "notElem" $ Hedgehog.property $ do
        xs <- Hedgehog.forAll $ Gen.list (Range.linear 0 200) genVal
        x <- Hedgehog.forAll genVal
        let v = V.fromList xs
        V.notElem x v Hedgehog.=== L.notElem x xs

    , testProperty "find even" $ Hedgehog.property $ do
        xs <- Hedgehog.forAll $ Gen.list (Range.linear 0 200) genVal
        let v = V.fromList xs
        V.find even v Hedgehog.=== L.find even xs

    , testProperty "findIndex even" $ Hedgehog.property $ do
        xs <- Hedgehog.forAll $ Gen.list (Range.linear 0 200) genVal
        let v = V.fromList xs
        V.findIndex even v Hedgehog.=== L.findIndex even xs

    , testProperty "findIndices even" $ Hedgehog.property $ do
        xs <- Hedgehog.forAll $ Gen.list (Range.linear 0 200) genVal
        let v = V.fromList xs
        V.toList (V.findIndices even v) Hedgehog.=== L.findIndices even xs

    , testProperty "elemIndex" $ Hedgehog.property $ do
        xs <- Hedgehog.forAll $ Gen.list (Range.linear 0 200) genVal
        x <- Hedgehog.forAll genVal
        let v = V.fromList xs
        V.elemIndex x v Hedgehog.=== L.elemIndex x xs

    , testProperty "elemIndices" $ Hedgehog.property $ do
        xs <- Hedgehog.forAll $ Gen.list (Range.linear 0 200) genVal
        x <- Hedgehog.forAll genVal
        let v = V.fromList xs
        V.toList (V.elemIndices x v) Hedgehog.=== L.elemIndices x xs
    ]

  , testGroup "safe indexing" $
    [ testProperty "(!?) valid" $ Hedgehog.property $ do
        xs <- Hedgehog.forAll $ Gen.list (Range.linear 1 300) genVal
        let v = V.fromList xs
        i <- Hedgehog.forAll $ Gen.int (Range.linear 0 (length xs - 1))
        (v V.!? i) Hedgehog.=== Just (xs !! i)

    , testProperty "(!?) out of bounds" $ Hedgehog.property $ do
        xs <- Hedgehog.forAll $ Gen.list (Range.linear 0 300) genVal
        let v = V.fromList xs
            n = length xs
        (v V.!? n) Hedgehog.=== Nothing
        (v V.!? (-1)) Hedgehog.=== Nothing
    ]

  , testGroup "scans" $
    [ testProperty "scanl' (+) 0" $ Hedgehog.property $ do
        xs <- Hedgehog.forAll $ Gen.list (Range.linear 0 200) genSmallVal
        let v = V.fromList xs
        V.toList (V.scanl' (+) 0 v) Hedgehog.=== L.scanl' (+) 0 xs

    , testProperty "prescanl' (+) 0" $ Hedgehog.property $ do
        xs <- Hedgehog.forAll $ Gen.list (Range.linear 0 200) genSmallVal
        let v = V.fromList xs
        V.toList (V.prescanl' (+) 0 v) Hedgehog.=== Prelude.init (L.scanl' (+) 0 xs)

    , testProperty "scanl1 (+) on non-empty" $ Hedgehog.property $ do
        xs <- Hedgehog.forAll $ Gen.list (Range.linear 1 200) genSmallVal
        let v = V.fromList xs
        V.toList (V.scanl1 (+) v) Hedgehog.=== scanl1 (+) xs

    , testProperty "scanr (+) 0" $ Hedgehog.property $ do
        xs <- Hedgehog.forAll $ Gen.list (Range.linear 0 200) genSmallVal
        let v = V.fromList xs
        V.toList (V.scanr (+) 0 v) Hedgehog.=== scanr (+) 0 xs

    , testProperty "scanr1 (+) on non-empty" $ Hedgehog.property $ do
        xs <- Hedgehog.forAll $ Gen.list (Range.linear 1 200) genSmallVal
        let v = V.fromList xs
        V.toList (V.scanr1 (+) v) Hedgehog.=== scanr1 (+) xs
    ]

  , testGroup "zipping" $
    [ testProperty "zip" $ Hedgehog.property $ do
        xs <- Hedgehog.forAll $ Gen.list (Range.linear 0 200) genVal
        ys <- Hedgehog.forAll $ Gen.list (Range.linear 0 200) genVal
        let v1 = V.fromList xs
            v2 = V.fromList ys
        V.toList (V.zip v1 v2) Hedgehog.=== zip xs ys

    , testProperty "zipWith (+)" $ Hedgehog.property $ do
        xs <- Hedgehog.forAll $ Gen.list (Range.linear 0 200) genVal
        ys <- Hedgehog.forAll $ Gen.list (Range.linear 0 200) genVal
        let v1 = V.fromList xs
            v2 = V.fromList ys
        V.toList (V.zipWith (+) v1 v2) Hedgehog.=== zipWith (+) xs ys

    , testProperty "zip3" $ Hedgehog.property $ do
        xs <- Hedgehog.forAll $ Gen.list (Range.linear 0 100) genVal
        ys <- Hedgehog.forAll $ Gen.list (Range.linear 0 100) genVal
        zs <- Hedgehog.forAll $ Gen.list (Range.linear 0 100) genVal
        let v1 = V.fromList xs; v2 = V.fromList ys; v3 = V.fromList zs
        V.toList (V.zip3 v1 v2 v3) Hedgehog.=== zip3 xs ys zs

    , testProperty "izipWith" $ Hedgehog.property $ do
        xs <- Hedgehog.forAll $ Gen.list (Range.linear 0 200) genVal
        ys <- Hedgehog.forAll $ Gen.list (Range.linear 0 200) genVal
        let v1 = V.fromList xs; v2 = V.fromList ys
            expected = zipWith3 (\i x y -> i + x + y) [0..] xs ys
        V.toList (V.izipWith (\i x y -> i + x + y) v1 v2) Hedgehog.=== expected

    , testProperty "unzip" $ Hedgehog.property $ do
        xs <- Hedgehog.forAll $ Gen.list (Range.linear 0 200) genVal
        ys <- Hedgehog.forAll $ Gen.list (Range.linear 0 200) genVal
        let pairs = zip xs ys
            v = V.fromList pairs
            (va, vb) = V.unzip v
            (as, bs) = unzip pairs
        V.toList va Hedgehog.=== as
        V.toList vb Hedgehog.=== bs
    ]

  , testGroup "mapping" $
    [ testProperty "imap" $ Hedgehog.property $ do
        xs <- Hedgehog.forAll $ Gen.list (Range.linear 0 200) genVal
        let v = V.fromList xs
        V.toList (V.imap (\i x -> i + x) v) Hedgehog.=== imapList (\i x -> i + x) xs

    , testProperty "concatMap" $ Hedgehog.property $ do
        xs <- Hedgehog.forAll $ Gen.list (Range.linear 0 100) genSmallVal
        let v = V.fromList xs
            f x = V.fromList [x, x + 1]
            g x = [x, x + 1]
        V.toList (V.concatMap f v) Hedgehog.=== concatMap g xs

    , testProperty "mapMaybe" $ Hedgehog.property $ do
        xs <- Hedgehog.forAll $ Gen.list (Range.linear 0 200) genVal
        let v = V.fromList xs
            f x = if even x then Just (x `div` 2) else Nothing
        V.toList (V.mapMaybe f v) Hedgehog.=== Maybe.mapMaybe f xs

    , testProperty "imapMaybe" $ Hedgehog.property $ do
        xs <- Hedgehog.forAll $ Gen.list (Range.linear 0 200) genVal
        let v = V.fromList xs
            f i x = if even i then Just (i + x) else Nothing
            expected = Maybe.mapMaybe (\(i, x) -> f i x) (zip [0..] xs)
        V.toList (V.imapMaybe f v) Hedgehog.=== expected
    ]

  , testGroup "filtering" $
    [ testProperty "ifilter" $ Hedgehog.property $ do
        xs <- Hedgehog.forAll $ Gen.list (Range.linear 0 200) genVal
        let v = V.fromList xs
        V.toList (V.ifilter (\i _ -> even i) v) Hedgehog.=== ifilterList (\i _ -> even i) xs

    , testProperty "takeWhile" $ Hedgehog.property $ do
        xs <- Hedgehog.forAll $ Gen.list (Range.linear 0 200) genVal
        k <- Hedgehog.forAll genVal
        let v = V.fromList xs
        V.toList (V.takeWhile (< k) v) Hedgehog.=== takeWhile (< k) xs

    , testProperty "dropWhile" $ Hedgehog.property $ do
        xs <- Hedgehog.forAll $ Gen.list (Range.linear 0 200) genVal
        k <- Hedgehog.forAll genVal
        let v = V.fromList xs
        V.toList (V.dropWhile (< k) v) Hedgehog.=== dropWhile (< k) xs
    ]

  , testGroup "monadic mapping" $
    [ testProperty "mapM Just" $ Hedgehog.property $ do
        xs <- Hedgehog.forAll $ Gen.list (Range.linear 0 200) genVal
        let v = V.fromList xs
            result = V.mapM Just v :: Maybe (V.Vector Int)
        fmap V.toList result Hedgehog.=== Just xs

    , testProperty "forM Just" $ Hedgehog.property $ do
        xs <- Hedgehog.forAll $ Gen.list (Range.linear 0 200) genVal
        let v = V.fromList xs
            result = V.forM v Just :: Maybe (V.Vector Int)
        fmap V.toList result Hedgehog.=== Just xs

    , testProperty "imapM" $ Hedgehog.property $ do
        xs <- Hedgehog.forAll $ Gen.list (Range.linear 0 100) genVal
        let v = V.fromList xs
            result = V.imapM (\i x -> Just (i + x)) v :: Maybe (V.Vector Int)
            expected = Just (imapList (\i x -> i + x) xs)
        fmap V.toList result Hedgehog.=== expected
    ]
  ]

------------------------------------------------------------------------
-- Part 3: Split/pair operation tests
------------------------------------------------------------------------

splitPairTests :: TestTree
splitPairTests = testGroup "split pairs"
  [ testProperty "partition even" $ Hedgehog.property $ do
      xs <- Hedgehog.forAll $ Gen.list (Range.linear 0 200) genVal
      let v = V.fromList xs
          (vt, vf) = V.partition even v
          (lt, lf) = L.partition even xs
      V.toList vt Hedgehog.=== lt
      V.toList vf Hedgehog.=== lf

  , testProperty "span (< k)" $ Hedgehog.property $ do
      xs <- Hedgehog.forAll $ Gen.list (Range.linear 0 200) genVal
      k <- Hedgehog.forAll genVal
      let v = V.fromList xs
          (vl, vr) = V.span (< k) v
          (ll, lr) = L.span (< k) xs
      V.toList vl Hedgehog.=== ll
      V.toList vr Hedgehog.=== lr

  , testProperty "break (>= k)" $ Hedgehog.property $ do
      xs <- Hedgehog.forAll $ Gen.list (Range.linear 0 200) genVal
      k <- Hedgehog.forAll genVal
      let v = V.fromList xs
          (vl, vr) = V.break (>= k) v
          (ll, lr) = L.break (>= k) xs
      V.toList vl Hedgehog.=== ll
      V.toList vr Hedgehog.=== lr

  , testProperty "splitAt round-trip" $ Hedgehog.property $ do
      xs <- Hedgehog.forAll $ Gen.list (Range.linear 0 300) genVal
      n <- Hedgehog.forAll $ Gen.int (Range.linear 0 (length xs))
      let v = V.fromList xs
          (vl, vr) = V.splitAt n v
      V.toList vl Hedgehog.=== take n xs
      V.toList vr Hedgehog.=== drop n xs
      V.toList (vl <> vr) Hedgehog.=== xs

  , testProperty "unzip3" $ Hedgehog.property $ do
      xs <- Hedgehog.forAll $ Gen.list (Range.linear 0 100) genVal
      ys <- Hedgehog.forAll $ Gen.list (Range.linear 0 100) genVal
      zs <- Hedgehog.forAll $ Gen.list (Range.linear 0 100) genVal
      let triples = zip3 xs ys zs
          v = V.fromList triples
          (va, vb, vc) = V.unzip3 v
          (as, bs, cs) = unzip3 triples
      V.toList va Hedgehog.=== as
      V.toList vb Hedgehog.=== bs
      V.toList vc Hedgehog.=== cs
  ]

------------------------------------------------------------------------
-- Part 4: Instance law tests
------------------------------------------------------------------------

instanceLawTests :: TestTree
instanceLawTests = testGroup "instance laws"
  [ testGroup "Functor" $
    [ testProperty "map id == id" $ Hedgehog.property $ do
        xs <- Hedgehog.forAll $ Gen.list (Range.linear 0 200) genVal
        let v = V.fromList xs
        V.toList (V.map id v) Hedgehog.=== xs

    , testProperty "map (f . g) == map f . map g" $ Hedgehog.property $ do
        xs <- Hedgehog.forAll $ Gen.list (Range.linear 0 200) genVal
        let v = V.fromList xs
            f = (+ 1)
            g = (* 2)
        V.toList (V.map (f . g) v) Hedgehog.=== V.toList (V.map f (V.map g v))
    ]

  , testGroup "Applicative" $
    [ testProperty "pure x == singleton x" $ Hedgehog.property $ do
        x <- Hedgehog.forAll genVal
        V.toList (pure x :: V.Vector Int) Hedgehog.=== [x]

    , testProperty "<*> matches list" $ Hedgehog.property $ do
        xs <- Hedgehog.forAll $ Gen.list (Range.linear 0 10) genSmallVal
        ys <- Hedgehog.forAll $ Gen.list (Range.linear 0 10) genSmallVal
        let fs = V.fromList (Prelude.map (+) xs)
            vs = V.fromList ys
            listFs = Prelude.map (+) xs
        V.toList (fs <*> vs) Hedgehog.=== (listFs <*> ys)
    ]

  , testGroup "Monad" $
    [ testProperty "return x >>= f == f x (left identity)" $ Hedgehog.property $ do
        x <- Hedgehog.forAll genSmallVal
        let f a = V.fromList [a, a + 1]
        V.toList (return x >>= f :: V.Vector Int) Hedgehog.=== V.toList (f x)

    , testProperty "m >>= return == m (right identity)" $ Hedgehog.property $ do
        xs <- Hedgehog.forAll $ Gen.list (Range.linear 0 100) genVal
        let v = V.fromList xs
        V.toList (v >>= return) Hedgehog.=== xs

    , testProperty "(m >>= f) >>= g == m >>= (\\x -> f x >>= g) (associativity)" $ Hedgehog.property $ do
        xs <- Hedgehog.forAll $ Gen.list (Range.linear 0 20) genSmallVal
        let v = V.fromList xs
            f a = V.fromList [a, a + 1]
            g a = V.fromList [a * 2]
        V.toList ((v >>= f) >>= g) Hedgehog.=== V.toList (v >>= (\x -> f x >>= g))

    , testProperty ">>= matches list bind" $ Hedgehog.property $ do
        xs <- Hedgehog.forAll $ Gen.list (Range.linear 0 30) genSmallVal
        let v = V.fromList xs
            vf a = V.fromList [a, a + 1, a + 2]
            lf a = [a, a + 1, a + 2]
        V.toList (v >>= vf) Hedgehog.=== (xs >>= lf)
    ]

  , testGroup "Monoid" $
    [ testProperty "mempty <> v == v" $ Hedgehog.property $ do
        xs <- Hedgehog.forAll $ Gen.list (Range.linear 0 200) genVal
        let v = V.fromList xs
        V.toList (mempty <> v) Hedgehog.=== xs

    , testProperty "v <> mempty == v" $ Hedgehog.property $ do
        xs <- Hedgehog.forAll $ Gen.list (Range.linear 0 200) genVal
        let v = V.fromList xs
        V.toList (v <> mempty) Hedgehog.=== xs

    , testProperty "(v1 <> v2) <> v3 == v1 <> (v2 <> v3)" $ Hedgehog.property $ do
        xs <- Hedgehog.forAll $ Gen.list (Range.linear 0 100) genVal
        ys <- Hedgehog.forAll $ Gen.list (Range.linear 0 100) genVal
        zs <- Hedgehog.forAll $ Gen.list (Range.linear 0 100) genVal
        let v1 = V.fromList xs; v2 = V.fromList ys; v3 = V.fromList zs
        V.toList ((v1 <> v2) <> v3) Hedgehog.=== V.toList (v1 <> (v2 <> v3))
    ]

  , testGroup "Eq/Ord" $
    [ testProperty "Eq agrees with list" $ Hedgehog.property $ do
        xs <- Hedgehog.forAll $ Gen.list (Range.linear 0 100) genVal
        ys <- Hedgehog.forAll $ Gen.list (Range.linear 0 100) genVal
        let v1 = V.fromList xs; v2 = V.fromList ys
        (v1 == v2) Hedgehog.=== (xs == ys)

    , testProperty "Eq reflexive" $ Hedgehog.property $ do
        xs <- Hedgehog.forAll $ Gen.list (Range.linear 0 200) genVal
        let v = V.fromList xs
        Hedgehog.assert (v == v)

    , testProperty "Ord agrees with list" $ Hedgehog.property $ do
        xs <- Hedgehog.forAll $ Gen.list (Range.linear 0 100) genVal
        ys <- Hedgehog.forAll $ Gen.list (Range.linear 0 100) genVal
        let v1 = V.fromList xs; v2 = V.fromList ys
        compare v1 v2 Hedgehog.=== compare xs ys
    ]

  , testGroup "Foldable" $
    [ testProperty "toList via Foldable" $ Hedgehog.property $ do
        xs <- Hedgehog.forAll $ Gen.list (Range.linear 0 200) genVal
        let v = V.fromList xs
        foldr (:) [] v Hedgehog.=== xs

    , testProperty "sum via Foldable" $ Hedgehog.property $ do
        xs <- Hedgehog.forAll $ Gen.list (Range.linear 0 200) genVal
        let v = V.fromList xs
        sum v Hedgehog.=== sum xs

    , testProperty "null via Foldable" $ Hedgehog.property $ do
        xs <- Hedgehog.forAll $ Gen.list (Range.linear 0 200) genVal
        let v = V.fromList xs
        Prelude.null v Hedgehog.=== null xs

    , testProperty "length via Foldable" $ Hedgehog.property $ do
        xs <- Hedgehog.forAll $ Gen.list (Range.linear 0 200) genVal
        let v = V.fromList xs
        Prelude.length v Hedgehog.=== length xs
    ]

  , testGroup "Traversable" $
    [ testProperty "traverse Just == Just" $ Hedgehog.property $ do
        xs <- Hedgehog.forAll $ Gen.list (Range.linear 0 200) genVal
        let v = V.fromList xs
            result = traverse Just v :: Maybe (V.Vector Int)
        fmap V.toList result Hedgehog.=== Just xs

    , testProperty "traverse matches list traverse" $ Hedgehog.property $ do
        xs <- Hedgehog.forAll $ Gen.list (Range.linear 0 50) genSmallVal
        let v = V.fromList xs
            f x = if x > 50 then Nothing else Just (x + 1)
            vResult = fmap V.toList (traverse f v) :: Maybe [Int]
            lResult = traverse f xs :: Maybe [Int]
        vResult Hedgehog.=== lResult
    ]

  , testGroup "IsList" $
    [ testProperty "fromList . toList == id" $ Hedgehog.property $ do
        xs <- Hedgehog.forAll $ Gen.list (Range.linear 0 200) genVal
        let v = V.fromList xs
        V.toList (V.fromList (V.toList v)) Hedgehog.=== xs
    ]
  ]

------------------------------------------------------------------------
-- Part 5: Construction equivalence tests
------------------------------------------------------------------------

constructionTests :: TestTree
constructionTests = testGroup "construction"
  [ testProperty "replicate" $ Hedgehog.property $ do
      n <- Hedgehog.forAll $ Gen.int (Range.linear 0 500)
      x <- Hedgehog.forAll genVal
      V.toList (V.replicate n x) Hedgehog.=== Prelude.replicate n x

  , testProperty "generate" $ Hedgehog.property $ do
      n <- Hedgehog.forAll $ Gen.int (Range.linear 0 500)
      V.toList (V.generate n (* 3)) Hedgehog.=== Prelude.map (* 3) [0 .. n - 1]

  , testProperty "iterateN" $ Hedgehog.property $ do
      n <- Hedgehog.forAll $ Gen.int (Range.linear 0 200)
      x <- Hedgehog.forAll genSmallVal
      V.toList (V.iterateN n (+ 1) x) Hedgehog.=== take n (iterate (+ 1) x)

  , testProperty "unfoldr" $ Hedgehog.property $ do
      n <- Hedgehog.forAll $ Gen.int (Range.linear 0 200)
      let f s = if s >= n then Nothing else Just (s * 2, s + 1)
      V.toList (V.unfoldr f 0) Hedgehog.=== L.unfoldr f 0

  , testProperty "unfoldrN" $ Hedgehog.property $ do
      limit <- Hedgehog.forAll $ Gen.int (Range.linear 0 200)
      let f s = Just (s * 2, s + 1)
      V.toList (V.unfoldrN limit f 0) Hedgehog.=== take limit (L.unfoldr f 0)

  , testProperty "fromListN" $ Hedgehog.property $ do
      xs <- Hedgehog.forAll $ Gen.list (Range.linear 0 500) genVal
      n <- Hedgehog.forAll $ Gen.int (Range.linear 0 (length xs + 50))
      V.toList (V.fromListN n xs) Hedgehog.=== take n xs

  , testProperty "enumFromN" $ Hedgehog.property $ do
      start <- Hedgehog.forAll $ Gen.int (Range.linear 0 100)
      n <- Hedgehog.forAll $ Gen.int (Range.linear 0 200)
      V.toList (V.enumFromN start n) Hedgehog.=== take n [start ..]

  , testProperty "enumFromStepN" $ Hedgehog.property $ do
      start <- Hedgehog.forAll $ Gen.int (Range.linear 0 100)
      step <- Hedgehog.forAll $ Gen.int (Range.linear 1 10)
      n <- Hedgehog.forAll $ Gen.int (Range.linear 0 200)
      V.toList (V.enumFromStepN start step n) Hedgehog.=== take n [start, start + step ..]

  , testProperty "enumFromTo" $ Hedgehog.property $ do
      lo <- Hedgehog.forAll $ Gen.int (Range.linear 0 50)
      hi <- Hedgehog.forAll $ Gen.int (Range.linear 0 200)
      V.toList (V.enumFromTo lo hi) Hedgehog.=== [lo .. hi]

  , testProperty "enumFromThenTo" $ Hedgehog.property $ do
      lo <- Hedgehog.forAll $ Gen.int (Range.linear 0 50)
      step <- Hedgehog.forAll $ Gen.int (Range.linear 1 5)
      hi <- Hedgehog.forAll $ Gen.int (Range.linear 0 200)
      let next = lo + step
      V.toList (V.enumFromThenTo lo next hi) Hedgehog.=== [lo, next .. hi]

  , testProperty "fromVector (via Foldable)" $ Hedgehog.property $ do
      xs <- Hedgehog.forAll $ Gen.list (Range.linear 0 200) genVal
      V.toList (V.fromVector xs) Hedgehog.=== xs

  , testProperty "singleton" $ Hedgehog.property $ do
      x <- Hedgehog.forAll genVal
      V.toList (V.singleton x) Hedgehog.=== [x]

  , testProperty "force does not change content" $ Hedgehog.property $ do
      xs <- Hedgehog.forAll $ Gen.list (Range.linear 0 200) genVal
      let v = V.fromList xs
      V.toList (V.force v) Hedgehog.=== xs

  , testProperty "stream/unstream round-trip" $ Hedgehog.property $ do
      xs <- Hedgehog.forAll $ Gen.list (Range.linear 0 200) genVal
      let v = V.fromList xs
      V.toList (V.unstream (V.stream v)) Hedgehog.=== xs
  ]

------------------------------------------------------------------------
-- Part 6: Transient operation tests
------------------------------------------------------------------------

transientTests :: TestTree
transientTests = testGroup "transient"
  [ testProperty "create + mPush" $ Hedgehog.property $ do
      xs <- Hedgehog.forAll $ Gen.list (Range.linear 0 300) genVal
      let v = V.create $ \mv -> mapM_ (V.mPush mv) xs
      V.toList v Hedgehog.=== xs

  , testProperty "modify + mWrite" $ Hedgehog.property $ do
      xs <- Hedgehog.forAll $ Gen.list (Range.linear 1 200) genVal
      let v = V.fromList xs
          n = length xs
      i <- Hedgehog.forAll $ Gen.int (Range.linear 0 (n - 1))
      val <- Hedgehog.forAll genVal
      let v' = V.modify (\mv -> V.mWrite mv i val) v
      V.toList v' Hedgehog.=== listUpdate i val xs

  , testProperty "thaw/freeze round-trip" $ Hedgehog.property $ do
      xs <- Hedgehog.forAll $ Gen.list (Range.linear 0 200) genVal
      let v = V.fromList xs
          v' = runST (V.thaw v >>= V.freeze)
      V.toList v' Hedgehog.=== xs

  , testProperty "mPush then mPop" $ Hedgehog.property $ do
      xs <- Hedgehog.forAll $ Gen.list (Range.linear 0 200) genVal
      let result = runST $ do
            mv <- V.mNew
            mapM_ (V.mPush mv) xs
            popped <- drainMV mv []
            pure popped
      result Hedgehog.=== reverse xs

  , testProperty "mRead after mPush" $ Hedgehog.property $ do
      xs <- Hedgehog.forAll $ Gen.list (Range.linear 1 200) genVal
      i <- Hedgehog.forAll $ Gen.int (Range.linear 0 (length xs - 1))
      let result = runST $ do
            mv <- V.mNew
            mapM_ (V.mPush mv) xs
            V.mRead mv i
      result Hedgehog.=== xs !! i

  , testProperty "mLength" $ Hedgehog.property $ do
      xs <- Hedgehog.forAll $ Gen.list (Range.linear 0 200) genVal
      let result = runST $ do
            mv <- V.mNew
            mapM_ (V.mPush mv) xs
            V.mLength mv
      result Hedgehog.=== length xs
  ]

drainMV :: V.MVector s Int -> [Int] -> ST s [Int]
drainMV mv acc = do
  mx <- V.mPop mv
  case mx of
    Nothing -> pure acc
    Just x  -> drainMV mv (x : acc)

------------------------------------------------------------------------
-- Part 7: Front vector fuzz
------------------------------------------------------------------------

frontFuzzTests :: TestTree
frontFuzzTests = testGroup "Front fuzz"
  [ testProperty "ops from empty" $
      Hedgehog.withTests 200 $ Hedgehog.property $ do
        actions <- Hedgehog.forAll $ Gen.list (Range.linear 1 80) genFrontAction
        runFrontActions (F.empty, []) actions

  , testProperty "ops from fromList" $
      Hedgehog.withTests 200 $ Hedgehog.property $ do
        initial <- Hedgehog.forAll $ Gen.list (Range.linear 0 200) genVal
        actions <- Hedgehog.forAll $ Gen.list (Range.linear 1 50) genFrontAction
        runFrontActions (F.fromList initial, initial) actions
  ]

------------------------------------------------------------------------
-- Part 8: Deque fuzz
------------------------------------------------------------------------

dequeFuzzTests :: TestTree
dequeFuzzTests = testGroup "Deque fuzz"
  [ testProperty "ops from empty" $
      Hedgehog.withTests 200 $ Hedgehog.property $ do
        actions <- Hedgehog.forAll $ Gen.list (Range.linear 1 80) genDequeAction
        runDequeActions (D.empty, []) actions

  , testProperty "ops from fromList" $
      Hedgehog.withTests 200 $ Hedgehog.property $ do
        initial <- Hedgehog.forAll $ Gen.list (Range.linear 0 200) genVal
        actions <- Hedgehog.forAll $ Gen.list (Range.linear 1 50) genDequeAction
        runDequeActions (D.fromList initial, initial) actions

  , testProperty "interleaved cons/snoc" $
      Hedgehog.withTests 200 $ Hedgehog.property $ do
        actions <- Hedgehog.forAll $ Gen.list (Range.linear 1 200) $
          Gen.choice
            [ DCons <$> genVal
            , DSnoc <$> genVal
            , pure DUncons
            , pure DUnsnoc
            ]
        runDequeActions (D.empty, []) actions
  ]
