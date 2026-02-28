module Fuzz (fuzzTests) where

import Control.Monad (when)
import Data.Primitive.SmallArray (sizeofSmallArray)
import qualified Data.List as L

import Test.Tasty
import Test.Tasty.Hedgehog
import qualified Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import qualified Data.PVector.Back as V
import Data.PVector.Back (Vector(..))

-- | ADT of all operations on a PVector.
-- Raw index/size parameters are clamped to valid ranges at execution time.
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
  deriving (Show)

genVal :: Hedgehog.Gen Int
genVal = Gen.int (Range.linear 0 10000)

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
  ]

-- | Apply an action to both PVector and model list.
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

-- | Execute a sequence of actions, verifying equivalence after each step.
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

fuzzTests :: TestTree
fuzzTests = testGroup "Fuzz"
  [ testProperty "sequential ops from empty" $
      Hedgehog.withTests 200 $ Hedgehog.property $ do
        actions <- Hedgehog.forAll $ Gen.list (Range.linear 1 100) genAction
        runActions (V.empty, []) actions

  , testProperty "sequential ops from fromList" $
      Hedgehog.withTests 200 $ Hedgehog.property $ do
        initial <- Hedgehog.forAll $ Gen.list (Range.linear 0 200) genVal
        actions <- Hedgehog.forAll $ Gen.list (Range.linear 1 50) genAction
        runActions (V.fromList initial, initial) actions

  , testProperty "sequential ops from cons-built" $
      Hedgehog.withTests 200 $ Hedgehog.property $ do
        initial <- Hedgehog.forAll $ Gen.list (Range.linear 0 200) genVal
        actions <- Hedgehog.forAll $ Gen.list (Range.linear 1 50) genAction
        let v0 = foldr V.cons V.empty initial
        runActions (v0, initial) actions

  , testProperty "sequential ops from appended" $
      Hedgehog.withTests 200 $ Hedgehog.property $ do
        xs <- Hedgehog.forAll $ Gen.list (Range.linear 0 100) genVal
        ys <- Hedgehog.forAll $ Gen.list (Range.linear 0 100) genVal
        actions <- Hedgehog.forAll $ Gen.list (Range.linear 1 50) genAction
        let v0 = V.fromList xs <> V.fromList ys
        runActions (v0, xs ++ ys) actions

  , testProperty "sequential ops from snoc-built" $
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
