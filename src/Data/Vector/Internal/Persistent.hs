{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies    #-}
module Data.Vector.Internal.Persistent where
import Control.Applicative (Alternative(..))
import Control.DeepSeq
import Control.Monad
import Control.Monad.Primitive
import Control.Monad.ST
import Data.Bits
import Data.Foldable
import Data.Maybe
import Data.Primitive.MutVar
import Data.STRef
import Data.Traversable
import Data.Vector.Internal
import qualified Data.Vector.Transient as TE
import qualified Data.Vector.Internal.Transient as T
import GHC.Exts

data Vector a = Vector
  { vCount     :: {-# UNPACK #-} !Int
  , vShift     :: {-# UNPACK #-} !Int
  , vRoot      :: !(Node a)
  , vTail      :: !(SmallArray a)
  , vTailCount :: {-# UNPACK #-} !Int
  }

instance Show a => Show (Vector a) where
  show v = "Vector { vCount = " ++ show (vCount v)
               ++ ", vShift = " ++ show (vShift v)
               ++ ", vRoot = " ++ show (vRoot v)
               ++ ", vTail = " ++ showSmallArr (vTail v) (vTailCount v)
               ++ ", vTailCount = " ++ show (vTailCount v)
               ++ " }"

tailOffset :: Vector a -> Int
tailOffset v = if vCount v < levelSize
  then 0
  else shiftL (shiftR (vCount v - 1) bitsPerLevel) bitsPerLevel

levelFor :: Vector a -> Int -> SmallArray a
levelFor v i = if i >= 0 && i < vCount v
  then unsafeLevelFor v i
  else error "Data.PersistentVector.nodeFor: index out of bounds"

unsafeLevelFor :: Vector a -> Int -> SmallArray a
unsafeLevelFor v i = if i >= tailOffset v
  then vTail v
  else go (vShift v) (vRoot v)
  where
    go level node = let !ix = maskLevel (i `shiftR` level) in case node of
      Branch (Level arr) -> case indexSmallArray arr ix of
        EmptyNode -> error "Data.PersistentVector.nodeFor: invariant violation, uninitialized branch should have been initialized already"
        br -> go (descendLevel level) br
      Leaf (Level arr) -> arr
      EmptyNode -> error "Data.PersistentVector.nodeFor: invariant violation, uninitialized branch should have been initialized already"

index :: Vector a -> Int -> a
index v i = indexSmallArray arr mi
  where
    arr = levelFor v i
    mi = maskLevel i

unsafeIndex :: Vector a -> Int -> a
unsafeIndex v i = indexSmallArray arr mi
  where
    arr = unsafeLevelFor v i
    mi = maskLevel i

(!) :: Int -> Vector a -> a
(!) = flip index

(!?) :: Int -> Vector a -> Maybe a
(!?) i v = if i >= 0 && i < vCount v
  then Just $! unsafeIndex v i
  else Nothing

update1 :: Int -> a -> Vector a -> Vector a
update1 i x v = if i >= 0 && i < vCount v
  then if i >= tailOffset v
    then runST $ do
      let c = vTailCount v
          ml = maskLevel i
      arr <- unsafeThawSmallArray (cloneSmallArray (vTail v) 0 c)
      writeSmallArray arr ml x
      arr' <- unsafeFreezeSmallArray arr
      return $ v { vTail = arr' }
    else v { vRoot = go (vShift v) (vRoot v) }
  else error "Data.PersistentVector.update: index out of bounds"
  where
    -- go :: Int -> Node a -> Node a
    go 0 (Leaf (Level arr)) = runST $ do
      let ml = maskLevel i
          ls = levelSize
      arr' <- unsafeThawSmallArray (cloneSmallArray arr 0 ls)
      writeSmallArray arr' ml x
      (Leaf . Level) <$> unsafeFreezeSmallArray arr'

    go n (Branch (Level arr)) = runST $ do
      let ls = levelSize
          si = maskLevel $ shiftR i n
      arr' <- unsafeThawSmallArray (cloneSmallArray arr 0 ls)
      case indexSmallArray arr si of
        EmptyNode -> error "Data.PersistentVector.update: invariant violation, node should be initialized, but hasn't been"
        child -> do
          writeSmallArray arr' si (go (descendLevel n) child)
          (Branch . Level) <$> unsafeFreezeSmallArray arr'

    go _ (Leaf _) = error "Data.PersistentVector.update: invariant violation, not at level 0 when updating leaf"
    go _ EmptyNode = error "Data.PersistentVector.update: invariant violation, node should be initialized, but isn't"


newPath :: Int -> Node a -> Node a
newPath level node = case level of
  0 -> node
  l -> runST $ do
    arr <- newSmallArray levelSize EmptyNode
    writeSmallArray arr 0 $ newPath (descendLevel l) node
    (Branch . Level) <$> unsafeFreezeSmallArray arr

pushTail :: Vector a -> Node a
pushTail v = pushTail' (vCount v) (vShift v) (vRoot v) (vTail v)

pushTail' :: Int -- ^ vCount v
          -> Int -- ^ level
          -> Node a
          -> SmallArray a
          -> Node a
pushTail' vc level node tailNode = case node of
  Branch (Level parent) -> runST $ do
    thawed <- unsafeThawSmallArray parent
    retArr <- cloneSmallMutableArray thawed 0 ls
    -- we find the next index for the current level
    let x = if level == bitsPerLevel
              -- If we are on the lowest level, then we can write the new tail node
              then Leaf $ Level tailNode
              -- We aren't on the lowest level, so we need to recurse
              else case indexSmallArray parent subIndex of
                -- If there isn't a path here yet, create one and insert the tail node
                -- once we've finished building out the path
                EmptyNode -> newPath (descendLevel level) . Leaf . Level $ tailNode
                -- If there's already a path here, we just recurse into it.
                parent' -> pushTail' vc (descendLevel level) parent' tailNode
    -- Now that we've constructed new 1+ nodes, write that chunk of the tree into place
    writeSmallArray retArr subIndex x
    (Branch . Level) <$> unsafeFreezeSmallArray retArr
  _ -> error "Data.PersistentVector.pushTail: invariant violation, Leaf should never be encountered while pushing tail"
  where
    ls = levelSize
    subIndex = maskLevel ((vc - 1) `shiftR` level)

-- public

empty :: Vector a
empty = Vector 0 bitsPerLevel (Branch $ Level bArr) lArr 0
  where
    bArr = runST (unsafeFreezeSmallArray =<< newSmallArray levelSize EmptyNode)
    lArr = runST (unsafeFreezeSmallArray =<< newSmallArray 0 (error "Data.PersistentVector.empty: uninitialized tail element"))

singleton :: a -> Vector a
singleton x = Vector 1 bitsPerLevel (Branch $ Level bArr) lArr 1
  where
    ls = levelSize
    bArr = runST $ (unsafeFreezeSmallArray =<< newSmallArray ls EmptyNode)
    lArr = runST $ (unsafeFreezeSmallArray =<< newSmallArray 1 x)

snoc :: Vector a -> a -> Vector a
snoc v x = if (vCount v - tailOffset v) < levelSize
  -- if there's room in tail, just return with a copied array and a new value tacked on
  then v { vCount = c'
         , vTailCount = tc'
         , vTail = runST $ do
             arr <- newSmallArray tc' x
             copySmallArray (vTail v) 0 arr 0 tc
             unsafeFreezeSmallArray arr
         }
  else let !(newRoot, newShift) = if branchesWillOverflow v
              then runST $ do
                arr <- newSmallArray levelSize EmptyNode
                writeSmallArray arr 0 (vRoot v)
                writeSmallArray arr 1 (newPath (vShift v) (Leaf . Level $ vTail v))
                farr <- unsafeFreezeSmallArray arr
                return (Branch . Level $ farr, ascendLevel $ vShift v)
              else (pushTail v, vShift v)
           !tail = runST (unsafeFreezeSmallArray =<< newSmallArray 1 x)
      -- check root overflow
       in v { vCount = c'
            , vShift = newShift
            , vRoot = newRoot
            , vTail = tail
            , vTailCount = 1
            }
  where
    c' = vCount v + 1
    tc = vTailCount v
    tc' = tc + 1

showBits :: FiniteBits b => b -> String
showBits b = fmap (\x -> if testBit b x then '1' else '0') s
  where
    s = reverse [0 .. finiteBitSize b]

branchesWillOverflow :: Vector a -> Bool
branchesWillOverflow v = fullLeaves v > lastShiftCapacity v

fullLeaves :: Vector a -> Int
fullLeaves = fullLevels . vCount

lastShiftCapacity :: Vector a -> Int
lastShiftCapacity v = shiftL 1 (vShift v)

(<|) :: Vector a -> a -> Vector a
(<|) = snoc

isEmpty :: Node a -> Bool
isEmpty EmptyNode = True
isEmpty _ = False

sameArray :: SmallArray a -> SmallArray a -> Bool
sameArray v1 v2
  = runST (sameSmallMutableArray <$> unsafeThawSmallArray v1 <*> unsafeThawSmallArray v2)

unsnoc :: Vector a -> Maybe (a, Vector a)
unsnoc v@(Vector{..}) = case vCount of
  0 -> Nothing
  1 -> Just (unsafeIndex v 0, Data.Vector.Internal.Persistent.empty)
  n -> if n - tailOffset v > 1
    then let newLen = pred vTailCount
         in Just ( indexSmallArray vTail newLen
                 , v { vCount = pred vCount
                     , vTail = cloneSmallArray vTail 0 newLen
                     , vTailCount = newLen
                     })
    else let -- (Branch rootArr) = vRoot
             newRootBase = fromMaybe emptyNode $ popTail v vShift vRoot
             (newShift, newRoot) = case newRootBase of
               b@(Branch (Level arr)) ->  if isEmpty $ indexSmallArray arr 1
                 then (vShift - bitsPerLevel, indexSmallArray arr 0)
                 else (vShift, b)
               leaf -> (vShift, leaf)
             newLen = pred vCount
             newTail = unsafeLevelFor v $ vCount - 2
             newTailCount | sameArray vTail newTail = pred vTailCount
                          | otherwise = levelSize
         in Just (unsafeIndex v newLen, v { vCount = newLen
                                          , vShift = newShift
                                          , vRoot = newRoot
                                          , vTail = newTail
                                          , vTailCount = newTailCount
                                          })

popTail :: Vector a -> Int -> Node a -> Maybe (Node a)
popTail v@(Vector{..}) level node = case node of
  Branch (Level arr) -> if level > bitsPerLevel
    then case (subIndex, popTail v (level - 5) $ indexSmallArray arr subIndex) of
      -- If we're at the front of the current node's array and don't find
      -- a value there, then remove the current node
      (0, Nothing) -> Nothing
      (_, Just newChild) -> runST $ do
        arr' <- unsafeThawSmallArray arr
        cloned <- cloneSmallMutableArray arr' 0 ls
        writeSmallArray cloned subIndex newChild
        (Just . Branch . Level) <$> unsafeFreezeSmallArray cloned
    else if subIndex == 0
      then Nothing
      else runST $ do
        thawed <- unsafeThawSmallArray arr
        ret <- cloneSmallMutableArray thawed 0 ls
        writeSmallArray ret 0 EmptyNode
        (Just . Branch . Level) <$> unsafeFreezeSmallArray ret
  _ -> Nothing
  where
    -- (vCount - 2) because of zero-based indexing
    subIndex = maskLevel $ shiftR (vCount - 2) level
    ls = levelSize

-- TODO copy & push tail rather than write one at a time
concat :: Foldable f => f (Vector a) -> Vector a
concat vs = runST $ do
  mv <- T.presized totalSize
  counter <- newSTRef 0
  forM_ vs $ \v ->
    forM_ v $ \x -> do
      i <- readSTRef counter
      T.write mv i x
      writeSTRef counter (i + 1)
  persist mv
  where
    totalSize = foldl' (\x y -> x + length y) 0 vs

concatMap :: (a -> Vector b) -> Vector a -> Vector b
concatMap f xs = Data.Vector.Internal.Persistent.concat $ Data.Vector.Internal.Persistent.map f xs

instance Alternative Vector where
  empty = Data.Vector.Internal.Persistent.empty
  (<|>) = mappend

instance Monad Vector where
  return = singleton
  (>>=) = flip Data.Vector.Internal.Persistent.concatMap

map :: (a -> b) -> Vector a -> Vector b
map f v@(Vector{..}) = runST $ do
  tv <- T.presized vCount
  foldM_ (\i x -> T.write tv i (f x) >> return (succ i)) 0 v
  persist tv
  -- foldM (\(ix, tv) x -> 

instance Functor Vector where
  fmap = Data.Vector.Internal.Persistent.map

instance MonadPlus Vector where
  mzero = Data.Vector.Internal.Persistent.empty
  mplus = mappend

instance Applicative Vector where
  pure = singleton
  (<*>) = ap

instance Monoid (Vector a) where
  mempty = Data.Vector.Internal.Persistent.empty
  mappend l r = runST $ do
    mv <- T.presized totalSize
    counter <- newSTRef 0
    forM_ l $ \x -> do
      i <- readSTRef counter
      T.write mv i x
      writeSTRef counter (i + 1)
    forM_ r $ \x -> do
      i <- readSTRef counter
      T.write mv i x
      writeSTRef counter (i + 1)
    persist mv
    where
      totalSize = length l + length r
  mconcat = Data.Vector.Internal.Persistent.concat

instance Foldable Vector where
  foldr f x v@(Vector c s n t tc) = foldr (foldrArray (pred levelSize)) (foldedTail x) vArrays
    where
      foldrArray !i a x = if i >= 0
        then foldrArray (pred i) a $ f (indexSmallArray a i) x
        else x
      foldedTail = foldrArray (pred tc) t
      vSegments = [0, levelSize .. tailOffset v - levelSize]
      vArrays = Prelude.map (unsafeLevelFor v) vSegments

  foldl f x v@(Vector c s n t tc) = foldl' (foldlArray levelBound) foldedTail vArrays
    where
      foldlArray !i x a = if i >= 0
        then foldlArray (pred i) (f x (indexSmallArray a i)) a
        else x
      foldedTail = foldlArray (pred tc) x t
      vSegments = [0, levelSize .. tailOffset v - levelSize]
      vArrays = Prelude.map (unsafeLevelFor v) vSegments

  -- TODO less reboxing
  foldl' f x v@(Vector c s n t tc) = foldl' (foldlArray levelBound) foldedTail vArrays
    where
      foldlArray !i x a = if i >= 0
        then let !res = f x $ indexSmallArray a i
             in foldlArray (pred i) res a
        else x
      foldedTail = foldlArray (pred tc) x t
      vSegments = [0, levelSize .. tailOffset v - levelSize]
      vArrays = Prelude.map (unsafeLevelFor v) vSegments

  null v = vCount v == 0
  length v = vCount v

-- check n = let v = foldl (|>) Data.PersistentVector.empty [0..n] in if (foldr (:) [] v) == [0..n]
  -- then Right ()
  -- else Left (n, length [0..n], vCount v, length $ Data.Foldable.toList v, v)

instance Traversable Vector where
  -- TODO replace with effecient version
  traverse f x = fromList <$> traverse f (Data.Foldable.toList x)

instance IsList (Vector a) where
  type Item (Vector a) = a
  fromList = foldl' snoc Data.Vector.Internal.Persistent.empty
  toList = foldr (:) []

{-
instance Data a => Data (Vector a) where
-}

-- TODO less reboxing
instance Eq a => Eq (Vector a) where
  (==) v1@(Vector c1 s1 n1 t1 tc1) v2@(Vector c2 s2 n2 t2 _) = if (length v1) /= (length v2)
    then False
    else foldedTail && all (foldlArray levelBound) vArrays
    where
      foldlArray !i as@(la, ra) = if i >= 0
        then if indexSmallArray la i == indexSmallArray ra i
               then foldlArray (pred i) as
               else False
        else True
      foldedTail = foldlArray (pred tc1) (t1, t2)
      vSegments = [0, levelSize .. tailOffset v1 - levelSize]
      vArrays = Prelude.map (\i -> (unsafeLevelFor v1 i, unsafeLevelFor v2 i)) vSegments

{-
instance Ord a => Ord (Vector a) where
instance Read a => Read (Vector a) where
-}

instance NFData a => NFData (Vector a) where
  rnf v = force 0
    where
      len = vCount v
      force !ix | ix < len = rnf (unsafeIndex v ix) `seq` force (ix + 1)
                | otherwise = ()

transient :: PrimMonad m => Vector a -> m (TE.TransientVector (PrimState m) a)
transient v@(Vector{..}) = do
  mTail <- unsafeThawSmallArray vTail
  mv <- newMutVar $ T.TransientVectorState vCount vShift (makeTransientNode vRoot) mTail False vTailCount
  return $ T.TransientVector mv

persist :: PrimMonad m => TE.TransientVector (PrimState m) a -> m (Vector a)
persist tv = do
  st <- T.getState tv
  n <- makePersistentNode $ T.tvRoot st
  farr <- unsafeFreezeSmallArray $ T.tvTail st
  return $ Vector (T.tvCount st) (T.tvShift st) n farr (T.tvTailCount st)

head :: Vector a -> a
head = (`index` 0)

unsafeHead :: Vector a -> a
unsafeHead = (`unsafeIndex` 0)

last :: Vector a -> a
last v = if vCount v > 0
           then unsafeLast v
           else error "Data.Vector.Persistent.last: empty vector"

unsafeLast :: Vector a -> a
unsafeLast v = unsafeIndex v (vCount v - 1)

