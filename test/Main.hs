module Main (main) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog

import qualified Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import qualified Data.PVector.Back as V
import qualified Data.PVector.Front as F
import qualified Data.PVector.Deque as D
import qualified Data.List as L

main :: IO ()
main = defaultMain $ testGroup "pvector"
  [ backVectorTests
  , frontVectorTests
  , dequeTests
  ]

------------------------------------------------------------------------
-- Back vector tests
------------------------------------------------------------------------

backVectorTests :: TestTree
backVectorTests = testGroup "Data.PVector.Back"
  [ testGroup "construction"
    [ testCase "empty" $
        V.length (V.empty :: V.Vector Int) @?= 0
    , testCase "singleton" $ do
        let v = V.singleton 42
        V.length v @?= 1
        V.head v @?= 42
    , testCase "fromList" $ do
        let xs = [1..100] :: [Int]
            v  = V.fromList xs
        V.length v @?= 100
        V.toList v @?= xs
    , testCase "fromList large" $ do
        let xs = [1..10000] :: [Int]
            v  = V.fromList xs
        V.toList v @?= xs
    ]
  , testGroup "snoc"
    [ testCase "snoc builds correctly" $ do
        let v = L.foldl' V.snoc V.empty [1..100 :: Int]
        V.toList v @?= [1..100]
    , testCase "snoc past one branch" $ do
        let v = L.foldl' V.snoc V.empty [1..33 :: Int]
        V.toList v @?= [1..33]
    , testCase "snoc past two branches" $ do
        let v = L.foldl' V.snoc V.empty [1..1025 :: Int]
        V.toList v @?= [1..1025]
    , testCase "snoc past three levels" $ do
        let v = L.foldl' V.snoc V.empty [1..33000 :: Int]
        V.length v @?= 33000
        V.toList v @?= [1..33000]
    ]
  , testGroup "unsnoc"
    [ testCase "unsnoc empty" $
        V.unsnoc (V.empty :: V.Vector Int) @?= Nothing
    , testCase "unsnoc singleton" $ do
        let Just (v', x) = V.unsnoc (V.singleton 42)
        x @?= (42 :: Int)
        V.null v' @?= True
    , testCase "unsnoc round-trip" $ do
        let v = V.fromList [1..100 :: Int]
            go acc vec = case V.unsnoc vec of
              Nothing        -> acc
              Just (vec', x) -> go (x : acc) vec'
        go [] v @?= [1..100]
    , testCase "unsnoc across branch boundary" $ do
        let v = V.fromList [1..33 :: Int]
            Just (v', x) = V.unsnoc v
        x @?= 33
        V.length v' @?= 32
        V.toList v' @?= [1..32]
        let Just (v'', y) = V.unsnoc v'
        y @?= 32
        V.length v'' @?= 31
    ]
  , testGroup "indexing"
    [ testCase "index small" $ do
        let v = V.fromList [10, 20, 30 :: Int]
        V.index v 0 @?= 10
        V.index v 1 @?= 20
        V.index v 2 @?= 30
    , testCase "index large" $ do
        let v = V.fromList [0..9999 :: Int]
        mapM_ (\i -> V.index v i @?= i) [0, 100, 999, 5000, 9999]
    , testCase "(!?) safe" $ do
        let v = V.fromList [1..3 :: Int]
        (v V.!? 0) @?= Just 1
        (v V.!? 3) @?= Nothing
        (v V.!? (-1)) @?= Nothing
    , testCase "head and last" $ do
        let v = V.fromList [1..50 :: Int]
        V.head v @?= 1
        V.last v @?= 50
    ]
  , testGroup "update"
    [ testCase "update single" $ do
        let v  = V.fromList [1..10 :: Int]
            v' = V.update 5 99 v
        V.index v' 5 @?= 99
        V.index v  5 @?= 6
    , testCase "update in tail" $ do
        let v  = V.fromList [1..5 :: Int]
            v' = V.update 3 99 v
        V.toList v' @?= [1, 2, 3, 99, 5]
    , testCase "update in tree" $ do
        let v  = V.fromList [1..100 :: Int]
            v' = V.update 0 99 v
        V.head v' @?= 99
        V.head v  @?= 1
    ]
  , testGroup "map"
    [ testCase "map identity" $ do
        let v = V.fromList [1..100 :: Int]
        V.toList (V.map id v) @?= [1..100]
    , testCase "map (+1)" $ do
        let v = V.fromList [1..50 :: Int]
        V.toList (V.map (+1) v) @?= [2..51]
    ]
  , testGroup "filter"
    [ testCase "filter even" $ do
        let v = V.fromList [1..20 :: Int]
        V.toList (V.filter even v) @?= [2, 4..20]
    ]
  , testGroup "fold"
    [ testCase "foldl' sum" $ do
        let v = V.fromList [1..100 :: Int]
        V.foldl' (+) 0 v @?= 5050
    , testCase "foldr toList" $ do
        let v = V.fromList [1..100 :: Int]
        V.foldr (:) [] v @?= [1..100]
    ]
  , testGroup "take/drop"
    [ testCase "take" $ do
        let v = V.fromList [1..100 :: Int]
        V.toList (V.take 10 v) @?= [1..10]
        V.toList (V.take 0 v)  @?= []
        V.toList (V.take 200 v) @?= [1..100]
    , testCase "drop" $ do
        let v = V.fromList [1..10 :: Int]
        V.toList (V.drop 5 v) @?= [6..10]
        V.toList (V.drop 0 v) @?= [1..10]
        V.toList (V.drop 20 v) @?= []
    ]
  , testGroup "zip"
    [ testCase "zip" $ do
        let v1 = V.fromList [1..5 :: Int]
            v2 = V.fromList ['a'..'e']
        V.toList (V.zip v1 v2) @?= L.zip [1..5] ['a'..'e']
    , testCase "zipWith (+)" $ do
        let v1 = V.fromList [1..5 :: Int]
            v2 = V.fromList [10, 20..50]
        V.toList (V.zipWith (+) v1 v2) @?= [11, 22, 33, 44, 55]
    ]
  , testGroup "instances"
    [ testCase "Eq" $ do
        let v1 = V.fromList [1..10 :: Int]
            v2 = V.fromList [1..10 :: Int]
            v3 = V.fromList [1..11 :: Int]
        (v1 == v2) @?= True
        (v1 == v3) @?= False
    , testCase "Ord" $ do
        let v1 = V.fromList [1, 2, 3 :: Int]
            v2 = V.fromList [1, 2, 4 :: Int]
        compare v1 v2 @?= LT
    , testCase "Semigroup" $ do
        let v1 = V.fromList [1, 2 :: Int]
            v2 = V.fromList [3, 4]
        V.toList (v1 <> v2) @?= [1, 2, 3, 4]
    , testCase "Monoid" $
        V.toList (mempty :: V.Vector Int) @?= []
    , testCase "Foldable" $ do
        let v = V.fromList [1..10 :: Int]
        sum v @?= 55
    ]
  , testGroup "transient"
    [ testCase "create and push" $ do
        let v = V.create $ \mv -> mapM_ (V.mPush mv) [1..100 :: Int]
        V.toList v @?= [1..100]
    , testCase "modify" $ do
        let v  = V.fromList [1..10 :: Int]
            v' = V.modify (\mv -> V.mWrite mv 5 99) v
        V.index v' 5 @?= 99
        V.index v  5 @?= 6
    ]
  , testGroup "property" $
    [ testProperty "snoc/unsnoc round-trip" $ Hedgehog.property $ do
        xs <- Hedgehog.forAll $ Gen.list (Range.linear 0 500) (Gen.int (Range.linear 0 10000))
        let v = L.foldl' V.snoc V.empty xs
        V.toList v Hedgehog.=== xs
    , testProperty "fromList/toList round-trip" $ Hedgehog.property $ do
        xs <- Hedgehog.forAll $ Gen.list (Range.linear 0 1000) (Gen.int (Range.linear 0 10000))
        V.toList (V.fromList xs) Hedgehog.=== xs
    , testProperty "index after fromList" $ Hedgehog.property $ do
        xs <- Hedgehog.forAll $ Gen.list (Range.linear 1 1000) (Gen.int (Range.linear 0 10000))
        let v = V.fromList xs
        i <- Hedgehog.forAll $ Gen.int (Range.linear 0 (Prelude.length xs - 1))
        V.index v i Hedgehog.=== xs !! i
    , testProperty "update preserves other elements" $ Hedgehog.property $ do
        xs <- Hedgehog.forAll $ Gen.list (Range.linear 1 500) (Gen.int (Range.linear 0 10000))
        let v = V.fromList xs
            n = Prelude.length xs
        i <- Hedgehog.forAll $ Gen.int (Range.linear 0 (n - 1))
        val <- Hedgehog.forAll $ Gen.int (Range.linear 0 10000)
        let v' = V.update i val v
        V.index v' i Hedgehog.=== val
        mapM_ (\j -> when (j /= i) $ V.index v' j Hedgehog.=== V.index v j) [0..n-1]
    , testProperty "snoc then unsnoc" $ Hedgehog.property $ do
        xs <- Hedgehog.forAll $ Gen.list (Range.linear 0 500) (Gen.int (Range.linear 0 10000))
        x <- Hedgehog.forAll $ Gen.int (Range.linear 0 10000)
        let v = V.fromList xs
            v' = V.snoc v x
        case V.unsnoc v' of
          Nothing -> Hedgehog.failure
          Just (v'', y) -> do
            y Hedgehog.=== x
            V.toList v'' Hedgehog.=== xs
    , testProperty "map f . toList == toList . map f" $ Hedgehog.property $ do
        xs <- Hedgehog.forAll $ Gen.list (Range.linear 0 500) (Gen.int (Range.linear 0 10000))
        let v = V.fromList xs
        V.toList (V.map (*2) v) Hedgehog.=== Prelude.map (*2) xs
    , testProperty "filter p . toList == toList . filter p" $ Hedgehog.property $ do
        xs <- Hedgehog.forAll $ Gen.list (Range.linear 0 500) (Gen.int (Range.linear 0 10000))
        let v = V.fromList xs
        V.toList (V.filter even v) Hedgehog.=== Prelude.filter even xs
    , testProperty "foldl' (+) 0 == sum" $ Hedgehog.property $ do
        xs <- Hedgehog.forAll $ Gen.list (Range.linear 0 500) (Gen.int (Range.linear 0 100))
        let v = V.fromList xs
        V.foldl' (+) 0 v Hedgehog.=== sum xs
    , testProperty "reverse . reverse == id" $ Hedgehog.property $ do
        xs <- Hedgehog.forAll $ Gen.list (Range.linear 0 200) (Gen.int (Range.linear 0 10000))
        let v = V.fromList xs
        V.toList (V.reverse (V.reverse v)) Hedgehog.=== xs
    ]
  ]

------------------------------------------------------------------------
-- Front vector tests
------------------------------------------------------------------------

frontVectorTests :: TestTree
frontVectorTests = testGroup "Data.PVector.Front"
  [ testCase "cons/head" $ do
      let fv = F.cons 1 (F.cons 2 (F.cons 3 F.empty)) :: F.FrontVector Int
      F.head fv @?= 1
      F.length fv @?= 3
  , testCase "fromList/toList" $ do
      let xs = [1..100 :: Int]
          fv = F.fromList xs
      F.toList fv @?= xs
  , testCase "uncons round-trip" $ do
      let fv = F.fromList [1..10 :: Int]
          go acc v = case F.uncons v of
            Nothing     -> L.reverse acc
            Just (x, t) -> go (x : acc) t
      go [] fv @?= [1..10]
  , testCase "index" $ do
      let fv = F.fromList [10, 20, 30 :: Int]
      F.index fv 0 @?= 10
      F.index fv 1 @?= 20
      F.index fv 2 @?= 30
  , testCase "cons builds in order" $ do
      let fv = F.cons 1 (F.cons 2 (F.cons 3 F.empty)) :: F.FrontVector Int
      F.toList fv @?= [1, 2, 3]
  , testProperty "cons/uncons round-trip" $ Hedgehog.property $ do
      xs <- Hedgehog.forAll $ Gen.list (Range.linear 0 200) (Gen.int (Range.linear 0 10000))
      let fv = L.foldl' (\v x -> F.cons x v) F.empty xs
          actual = drainFront fv
      actual Hedgehog.=== L.reverse xs
  , testProperty "fromList/toList identity" $ Hedgehog.property $ do
      xs <- Hedgehog.forAll $ Gen.list (Range.linear 0 500) (Gen.int (Range.linear 0 10000))
      F.toList (F.fromList xs) Hedgehog.=== xs
  ]

drainFront :: F.FrontVector a -> [a]
drainFront v = case F.uncons v of
  Nothing     -> []
  Just (x, t) -> x : drainFront t

------------------------------------------------------------------------
-- Deque tests
------------------------------------------------------------------------

dequeTests :: TestTree
dequeTests = testGroup "Data.PVector.Deque"
  [ testCase "cons/snoc" $ do
      let d = D.snoc (D.cons 1 D.empty) 2 :: D.Deque Int
      D.head d @?= 1
      D.last d @?= 2
      D.length d @?= 2
  , testCase "fromList/toList" $ do
      let xs = [1..100 :: Int]
          d  = D.fromList xs
      D.toList d @?= xs
  , testCase "uncons" $ do
      let d = D.fromList [1..5 :: Int]
      case D.uncons d of
        Nothing    -> assertFailure "unexpected Nothing"
        Just (x,d') -> do
          x @?= 1
          D.toList d' @?= [2..5]
  , testCase "unsnoc" $ do
      let d = D.fromList [1..5 :: Int]
      case D.unsnoc d of
        Nothing    -> assertFailure "unexpected Nothing"
        Just (d',x) -> do
          x @?= 5
          D.toList d' @?= [1..4]
  , testCase "interleaved cons/snoc" $ do
      let d = D.cons 1 (D.snoc (D.cons 2 (D.snoc D.empty 3)) 4) :: D.Deque Int
      D.toList d @?= [1, 2, 3, 4]
  , testCase "index across front/back" $ do
      let d = D.cons 1 (D.snoc (D.cons 2 D.empty) 3) :: D.Deque Int
      D.index d 0 @?= 1
      D.index d 1 @?= 2
      D.index d 2 @?= 3
  , testProperty "cons/snoc/toList" $ Hedgehog.property $ do
      xs <- Hedgehog.forAll $ Gen.list (Range.linear 0 200) (Gen.int (Range.linear 0 10000))
      ys <- Hedgehog.forAll $ Gen.list (Range.linear 0 200) (Gen.int (Range.linear 0 10000))
      let d0 = D.fromList ys
          d  = Prelude.foldr D.cons d0 xs
      D.toList d Hedgehog.=== (xs ++ ys)
  ]

when :: Applicative m => Bool -> m () -> m ()
when True  m = m
when False _ = pure ()
