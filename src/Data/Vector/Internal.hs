{-# LANGUAGE BangPatterns           #-}
{-# LANGUAGE MagicHash              #-}
{-# LANGUAGE UnboxedTuples          #-}
module Data.Vector.Internal where
import Control.Loop
import Control.Monad.Primitive
import Control.Monad.State
import Control.Monad.ST
import Data.Bits
import Data.Primitive.MutVar
import GHC.Prim
import GHC.Types

data Node a
  = Leaf   {-# UNPACK #-} !(Level a)
  | Branch {-# UNPACK #-} !(Level (Node a))
  | EmptyNode
  deriving (Show)

data TransientNode s a
  = UneditedLeaf   {-# UNPACK #-} !(Level a)
  | UneditedBranch {-# UNPACK #-} !(Level (Node a))
  | EditedLeaf     {-# UNPACK #-} !(MutableLevel s a)
  | EditedBranch   {-# UNPACK #-} !(MutableLevel s (TransientNode s a))
  | EmptyTransientNode

newtype Level a = Level { fromLevel :: SmallArray a }

instance Show a => Show (Level a) where
  show (Level arr) = showSmallArr arr levelSize

testDump :: TransientNode (PrimState IO) a -> IO (Node a)
testDump = makePersistentNode

showSmallArr :: Show a => SmallArray a -> Int -> String
showSmallArr arr s = show . (flip evalState) 0 $ replicateM s $ do
  i <- get
  put (i + 1)
  indexSmallArrayM arr i

newtype MutableLevel s a = MutableLevel { fromMutableLevel :: SmallMutableArray s a }

cloneSmallArray :: SmallArray a -> Int -> Int -> SmallArray a
cloneSmallArray (SmallArray arr#) (I# off#) (I# len#)
  = SmallArray (cloneSmallArray# arr# off# len#)

cloneSmallMutableArray :: PrimMonad m => SmallMutableArray (PrimState m) a -> Int -> Int -> m (SmallMutableArray (PrimState m) a)
cloneSmallMutableArray (SmallMutableArray arr#) (I# off#) (I# len#)
  = primitive $ \s# -> case cloneSmallMutableArray# arr# off# len# s# of
      (# s#, arr'# #) -> (# s#, SmallMutableArray arr'# #)

copySmallArray :: PrimMonad m => SmallArray a -> Int -> SmallMutableArray (PrimState m) a -> Int -> Int -> m ()
copySmallArray (SmallArray arr#) (I# srcOff#) (SmallMutableArray marr#) (I# len#) (I# destOff#)
  = primitive_ (copySmallArray# arr# srcOff# marr# len# destOff#)

copySmallMutableArray :: PrimMonad m => SmallMutableArray (PrimState m) a -> Int -> SmallMutableArray (PrimState m) a -> Int -> Int -> m ()
copySmallMutableArray (SmallMutableArray src#) (I# srcOff#) (SmallMutableArray dest#) (I# len#) (I# destOff#)
  = primitive_ (copySmallMutableArray# src# srcOff# dest# len# destOff#)

indexSmallArray :: SmallArray a -> Int -> a
indexSmallArray (SmallArray arr#) (I# i#)
  = case indexSmallArray# arr# i# of
      (# x #) -> x

indexSmallArrayM :: Monad m => SmallArray a -> Int -> m a
indexSmallArrayM (SmallArray arr#) (I# i#)
  = case indexSmallArray# arr# i# of
      (# x #) -> return x

newSmallArray :: PrimMonad m => Int -> a -> m (SmallMutableArray (PrimState m) a)
newSmallArray (I# i#) x
  = primitive $ \s# -> case newSmallArray# i# x s# of
      (# s#, arr# #) -> (# s#, SmallMutableArray arr# #)

readSmallArray :: PrimMonad m => SmallMutableArray (PrimState m) a -> Int -> m a
readSmallArray (SmallMutableArray arr#) (I# i#)
  = primitive (readSmallArray# arr# i#)

sameSmallMutableArray :: SmallMutableArray s a -> SmallMutableArray s a -> Bool
sameSmallMutableArray (SmallMutableArray arr1#) (SmallMutableArray arr2#)
  = isTrue# (sameSmallMutableArray# arr1# arr2#)

unsafeFreezeSmallArray :: PrimMonad m => SmallMutableArray (PrimState m) a -> m (SmallArray a)
unsafeFreezeSmallArray (SmallMutableArray arr#)
  = primitive $ \s# -> case unsafeFreezeSmallArray# arr# s# of
      (# s#, arr'# #) -> (# s#, SmallArray arr'# #)

unsafeThawSmallArray :: PrimMonad m => SmallArray a -> m (SmallMutableArray (PrimState m) a)
unsafeThawSmallArray (SmallArray arr#)
  = primitive $ \s# -> case unsafeThawSmallArray# arr# s# of
      (# s#, arr'# #) -> (# s#, SmallMutableArray arr'# #)

writeSmallArray :: PrimMonad m => SmallMutableArray (PrimState m) a -> Int -> a -> m ()
writeSmallArray (SmallMutableArray arr#) (I# i#) x
  = primitive_ (writeSmallArray# arr# i# x)

levelSize :: Int
levelSize = 32

levelBound :: Int
levelBound = pred levelSize

bitsPerLevel :: Int
bitsPerLevel = ceiling $ logBase 2 $ fromIntegral levelSize

descendLevel :: Int -> Int
descendLevel = subtract bitsPerLevel

ascendLevel :: Int -> Int
ascendLevel = (+) bitsPerLevel

maskLevel :: Int -> Int
maskLevel = (.&.) (levelSize - 1)

emptyNode :: Node a
emptyNode = Branch $ Level emptyBranchArray

data SmallArray a = SmallArray (SmallArray# a)
data SmallMutableArray s a = SmallMutableArray (SmallMutableArray# s a)

emptyBranchArray :: SmallArray (Node a)
emptyBranchArray = runST $ primitive
  (\s# -> case newSmallArray# i# EmptyNode s# of
    (# s'#, arr'# #) -> case unsafeFreezeSmallArray# arr'# s'# of
      (# s'', arr''# #) -> (# s'', SmallArray arr''# #))
  where
    !(I# i#) = levelSize

fullLevels :: Int -> Int
fullLevels x = shiftR x bitsPerLevel

makePersistentNode :: PrimMonad m => TransientNode (PrimState m) a -> m (Node a)
makePersistentNode n = case n of
  UneditedLeaf arr -> return . Leaf $ arr
  UneditedBranch arr -> return . Branch $ arr
  EditedLeaf (MutableLevel arr) -> (Leaf . Level) <$> unsafeFreezeSmallArray arr
  EditedBranch (MutableLevel arr) -> do
    arr' <- newSmallArray levelSize EmptyNode
    counter <- newMutVar 0
    replicateM_ levelSize $ do
      i <- readMutVar counter
      t <- readSmallArray arr i
      p <- makePersistentNode t
      writeSmallArray arr' i p
      writeMutVar counter (i + 1)
    (Branch . Level) <$> unsafeFreezeSmallArray arr'
  EmptyTransientNode -> return EmptyNode

makeTransientNode :: Node a -> TransientNode s a
makeTransientNode n = case n of
  Leaf arr -> UneditedLeaf arr
  Branch arr -> UneditedBranch arr
  EmptyNode -> EmptyTransientNode

makeMutableLevel :: PrimMonad m => m (MutableLevel (PrimState m) (TransientNode (PrimState m) a))
makeMutableLevel = MutableLevel <$> newSmallArray levelSize EmptyTransientNode

makeMutableLeaf :: PrimMonad m => m (MutableLevel (PrimState m) a)
makeMutableLeaf = MutableLevel <$> newSmallArray levelSize (error "Data.PersistentVector.makeMutableLeaf: uninitialized value in presized transient vector")

transientLevel :: PrimMonad m => Level (Node a) -> m (MutableLevel (PrimState m) (TransientNode (PrimState m) a))
transientLevel (Level src) = do
  dest <- newSmallArray levelSize EmptyTransientNode
  numLoop 0 (levelSize - 1) $ \i -> do
    writeSmallArray dest i . makeTransientNode $ indexSmallArray src i
  return . MutableLevel $ dest

