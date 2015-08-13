{-# LANGUAGE BangPatterns #-}
module Data.Vector.Internal.Transient where
import Control.Monad
import Control.Monad.Primitive
import Data.Bits
import Data.Primitive.MutVar
import Data.Vector.Internal
import Debug.Trace

newtype TransientVector s a
  = TransientVector { tvState :: (MutVar s (TransientVectorState s a)) }

data TransientVectorState s a = TransientVectorState
  { tvCount      :: {-# UNPACK #-} !Int
  , tvShift      :: {-# UNPACK #-} !Int
  , tvRoot       :: !(TransientNode s a)
  , tvTail       :: {-# UNPACK #-} !(SmallMutableArray s a)
  , tvTailEdited :: {-# UNPACK #-} !Bool
  , tvTailCount  :: {-# UNPACK #-} !Int
  }

emptyState :: PrimMonad m => m (TransientVectorState (PrimState m) a)
emptyState = do
  root <- makeMutableLevel
  (MutableLevel leaf) <- makeMutableLeaf
  return $ TransientVectorState 0 bitsPerLevel (EditedBranch root) leaf True 0


empty :: PrimMonad m => m (TransientVector (PrimState m) a)
empty = do
  st <- newMutVar =<< emptyState
  return $ TransientVector st


getState :: PrimMonad m => TransientVector (PrimState m) a -> m (TransientVectorState (PrimState m) a)
getState = readMutVar . tvState


putState :: PrimMonad m => TransientVector (PrimState m) a -> TransientVectorState (PrimState m) a -> m ()
putState s = writeMutVar (tvState s)


newPath :: PrimMonad m => Int -> TransientNode (PrimState m) a -> m (TransientNode (PrimState m) a)
newPath level node = case level of
  0 -> return node
  l -> do
    arr <- newSmallArray levelSize EmptyTransientNode
    writeSmallArray arr 0 =<< newPath (descendLevel l) node
    return . EditedBranch . MutableLevel $ arr


pushTail' :: PrimMonad m
          => Int -- ^ vCount v
          -> Int -- ^ level
          -> TransientNode (PrimState m) a
          -> SmallMutableArray (PrimState m) a
          -> m (TransientNode (PrimState m) a)
pushTail' vc level node tailNode = do
  (MutableLevel retArr) <- case node of
    UneditedBranch parent -> transientLevel parent
    EditedBranch parent -> return parent
    _ -> error "Data.PersistentVector.pushTail: invariant violation, Leaf should never be encountered while pushing tail"

  -- we find the next index for the current level
  x <- if level == bitsPerLevel
         -- If we are on the lowest level, then we can write the new tail node
         then return . EditedLeaf . MutableLevel $ tailNode
         -- We aren't on the lowest level, so we need to recurse
         else do
           n <- readSmallArray retArr subIndex
           case n of
             -- If there isn't a path here yet, create one and insert the tail node
             -- once we've finished building out the path
             EmptyTransientNode -> newPath (descendLevel level) . EditedLeaf . MutableLevel $ tailNode
             -- If there's already a path here, we just recurse into it.
             parent'@(EditedBranch _) -> pushTail' vc (descendLevel level) parent' tailNode
             parent'@(UneditedBranch l) -> do
               editable <- transientLevel l
               pushTail' vc (descendLevel level) (EditedBranch editable) tailNode
             parent' -> pushTail' vc (descendLevel level) parent' tailNode

  -- Now that we've constructed new 1+ nodes, write that chunk of the tree into place
  writeSmallArray retArr subIndex x
  return . EditedBranch . MutableLevel $ retArr
  where
    ls = levelSize
    subIndex = maskLevel ((vc - 1) `shiftR` level)

fullLeaves :: TransientVectorState s a -> Int
fullLeaves = fullLevels . tvCount

lastShiftCapacity :: TransientVectorState s a -> Int
lastShiftCapacity v = shiftL 1 (tvShift v)

branchesWillOverflow :: TransientVectorState s a -> Bool
branchesWillOverflow v = fullLeaves v > lastShiftCapacity v

presized :: PrimMonad m => Int -> m (TransientVector (PrimState m) a)
presized n = do
  v <- emptyState
  v' <- if full == 0 then return v else go v 0
  tail <- newSmallArray rest (error "Data.PersistentVector.presized: uninitialized element")
  TransientVector <$> newMutVar (v' { tvCount = n, tvTail = tail, tvTailCount = rest })
  where
    (full, rest) = divMod n levelSize
    go v c = if c > levelBound 
      then return v
      else do
        let currentCount = shiftL c bitsPerLevel
            currentShift = if branchesWillOverflow v then ascendLevel $ tvShift v else tvShift v
        (MutableLevel leaf) <- makeMutableLeaf
        n <- pushTail' currentCount currentShift (tvRoot v) leaf
        go (v { tvCount = currentCount
              , tvShift = currentShift
              , tvRoot = n
              }) (c + 1)


write :: PrimMonad m => TransientVector (PrimState m) a -> Int -> a -> m ()
write = undefined


-- TODO, alter transient vector impl to treat length and tailLength as MutVars
push :: PrimMonad m => TransientVector (PrimState m) a -> a -> m ()
push v x = do
  s <- getState v
  if (tvCount s - tailOffset s) < levelSize
  -- if there's room in tail, just return with a copied array and a new value tacked on
  then s { tvCount = c'
         , tvTailCount = tc'
         , tvTail = runST $ do
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

pop :: PrimMonad m => TransientVector (PrimState m) a -> m (Maybe a)
pop = undefined


