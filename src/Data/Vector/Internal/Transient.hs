{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Vector.Internal.Transient where
import Control.Monad
import Control.Monad.Primitive
import Data.Bits
import Data.Primitive.MutVar
import Data.Vector.Internal
import Debug.Trace

newtype TransientVector s a
  = TransientVector { tvState :: MutVar s (TransientVectorState s a) }

type TransientIOVector = TransientVector RealWorld

data TransientVectorState s a = TransientVectorState
  { tvCount      :: {-# UNPACK #-} !Int
  , tvShift      :: {-# UNPACK #-} !Int
  , tvRoot       :: !(TransientNode s a)
  , tvTail       :: {-# UNPACK #-} !(SmallMutableArray s a)
  , tvTailEdited :: !Bool
  , tvTailCount  :: {-# UNPACK #-} !Int
  }

emptyState :: PrimMonad m => m (TransientVectorState (PrimState m) a)
emptyState = do
  root <- makeMutableLevel
  (MutableLevel leaf) <- makeMutableLeaf
  return $ TransientVectorState 0 bitsPerLevel (EditedBranch root) leaf True 0

tailOffset :: TransientVectorState s a -> Int
tailOffset v = if tvCount v < levelSize
  then 0
  else shiftL (shiftR (tvCount v - 1) bitsPerLevel) bitsPerLevel

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
    return . EditedBranch $ MutableLevel arr

pushTail :: PrimMonad m => TransientVectorState (PrimState m) a -> m (TransientNode (PrimState m) a)
pushTail v = pushTail' (tvCount v) (tvShift v) (tvRoot v) (tvTail v)

pushTail' :: PrimMonad m
          => Int -- ^ tvCount v
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

levelFor :: PrimMonad m => TransientVectorState (PrimState m) a -> Int -> m (SmallMutableArray (PrimState m) a)
levelFor v i = if i >= 0 && i < tvCount v
  then unsafeLevelFor v i
  else error "Data.PersistentVector.nodeFor: index out of bounds"

unsafeLevelFor :: forall m a. PrimMonad m => TransientVectorState (PrimState m) a -> Int -> m (SmallMutableArray (PrimState m) a)
unsafeLevelFor v i = if i >= tailOffset v
  then return $ tvTail v
  else go (tvShift v) (tvRoot v)
  where
    go :: Int -> TransientNode (PrimState m) a -> m (SmallMutableArray (PrimState m) a)
    go level node = let !ix = maskLevel (i `shiftR` level) in case node of
      UneditedBranch (Level arr) -> case indexSmallArray arr ix of
        EmptyNode -> error "Data.PersistentVector.nodeFor: invariant violation, uninitialized branch should have been initialized already"
        br -> unsafeThawSmallArray $ go' (descendLevel level) br
      UneditedLeaf (Level arr) -> unsafeThawSmallArray arr
      EditedBranch (MutableLevel arr) -> readSmallArray arr ix >>= go (descendLevel level)
      EditedLeaf (MutableLevel arr) -> return arr
      EmptyTransientNode -> error "Data.PersistentVector.nodeFor: invariant violation, uninitialized branch should have been initialized already"
    go' level node = let !ix = maskLevel (i `shiftR` level) in case node of
      Branch (Level arr) -> case indexSmallArray arr ix of
        EmptyNode -> error "Data.PersistentVector.nodeFor: invariant violation, uninitialized branch should have been initialized already"
        br -> go' (descendLevel level) br
      Leaf (Level arr) -> arr
      EmptyNode -> error "Data.PersistentVector.nodeFor: invariant violation, uninitialized branch should have been initialized already"

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


index :: PrimMonad m => TransientVector (PrimState m) a -> Int -> m a
index v i = do
  st <- getState v
  arr <- levelFor st i
  readSmallArray arr (maskLevel i)


unsafeIndex :: PrimMonad m => TransientVector (PrimState m) a -> Int -> m a
unsafeIndex v i = do
  st <- getState v
  arr <- unsafeLevelFor st i
  readSmallArray arr (maskLevel i)

-- copied on write or no?
data WriteStatus = Copied | NotCopied

write :: PrimMonad m => TransientVector (PrimState m) a -> Int -> a -> m ()
write v i x = do
  return ()
  {-
  st <- getState v
  st' <- if i >= 0 && i < tvCount st
    then if i >= tailOffset st
           then do
              let c = tvTailCount st
                  ml = maskLevel i
              writeSmallArray (tvTail st) ml x
              return $ st { tvTailEdited = True }
           else go (tvShift v) (tvRoot v)
    else error "Data.Vector.Transient.write: index out of bounds"
  putState v st'
  where
    go 0 node = case node of
      UneditedLeaf (Level a) -> do
        
        return $! Just a'
      EditedLeaf (MutableLevel a) -> do
        let ml = maskLevel i
            ls = levelSize
        writeSmallArray a ml x
        return Nothing

      EmptyTransientNode -> error "Data.Vector.Transient.update: invariant violation, node should be initialized, but isn't"
      _ -> error "Data.Vector.Transient.update: invariant violation, at level 0 but still on a branch"
    go n node = let ls = levelSize
                    si = maskeLevel $ shiftR i n in case node of
      UneditedBranch (Level a) -> do
        copied <- copy branch
        childUpdate <- go copied
        case childUpdate of
          Nothing
          Just child
        return $! Just copied
      EditedBranch (MutableLevel a) -> readSmallArray arr si >>= go (descendLevel n)
      EmptyTransientNode -> error "Data.Vector.Transient.update: invariant violation, empty node where not expected"
      _ -> error "Data.Vector.Transient.update: invariant violation, not at level 0 but at a leaf"
  -}

-- TODO, alter transient vector impl to treat length and tailLength as MutVars
push :: PrimMonad m => TransientVector (PrimState m) a -> a -> m ()
push v x = do
  s <- getState v
  -- check root overflow
  let c' = tvCount s + 1
      tc = tvTailCount s
      tc' = tc + 1
      off = tailOffset s
  s' <- if (tvCount s - off) < levelSize
    -- if there's room in tail, just return with a copied array and a new value tacked on
    then do
      arr <- newSmallArray tc' x
      copySmallMutableArray (tvTail s) 0 arr 0 tc
      return $ s { tvCount = c'
                 , tvTailCount = tc'
                 , tvTail = arr
                 }
    else do
      !(newRoot, newShift) <- if branchesWillOverflow s
        then do
          arr <- newSmallArray levelSize EmptyTransientNode
          writeSmallArray arr 0 (tvRoot s)
          r <- newPath (tvShift s) $ EditedLeaf $ MutableLevel $ tvTail s
          writeSmallArray arr 1 r
          return (EditedBranch $ MutableLevel arr, ascendLevel $ tvShift s)
        else do
          r <- pushTail s
          return (r, tvShift s)
      !tail <- newSmallArray 1 x
      -- check root overflow
      return $ s { tvCount = c'
                 , tvShift = newShift
                 , tvRoot = newRoot
                 , tvTail = tail
                 , tvTailCount = 1
                 }
  putState v s'

popTail :: PrimMonad m => TransientVectorState (PrimState m) a
        -> Int
        -> TransientNode (PrimState m) a
        -> m (Maybe (TransientNode (PrimState m) a))
popTail v level n = case n of
  EditedBranch (MutableLevel arr) -> if level > bitsPerLevel
    then undefined
    else undefined
  UneditedBranch l -> if level > bitsPerLevel
    then do
      l'@(MutableLevel arr') <- transientLevel l
      popped <- popTail v (level - bitsPerLevel) (EditedBranch l')
      case (subIndex, popped) of
        (0, Nothing) -> return Nothing
        (_, Just newChild) -> do
          writeSmallArray arr' subIndex newChild
          return $ Just $ EditedBranch l'
    else if subIndex == 0
      then return Nothing
      else do
        l'@(MutableLevel arr') <- transientLevel l
        writeSmallArray arr' 0 EmptyTransientNode
        return $ Just $ EditedBranch l'
  _ -> return Nothing
  where
    subIndex = maskLevel $ shiftR (tvCount v - 2) level


pop :: PrimMonad m => TransientVector (PrimState m) a -> m (Maybe a)
pop v = do
  s <- getState v
  case tvCount s of
    0 -> return Nothing
    n -> do
      let to = tailOffset s
      if n - to > 1
        then do
          let newLen = tvTailCount s - 1
          x <- readSmallArray (tvTail s) newLen
          writeSmallArray (tvTail s) newLen undefined
          putState v $ s { tvCount = tvCount s - 1
                         , tvTailEdited = True
                         , tvTailCount = newLen
                         }
          return $ Just x
        else do
          mnewRootBase <- popTail s (tvShift s) (tvRoot s)
          newRootBase <- case mnewRootBase of
            Nothing -> EditedBranch <$> makeMutableLevel
            Just b -> return b
          (newShift, newRoot) <- case newRootBase of
            (EditedBranch _) -> undefined
            (UneditedBranch _) -> undefined
            leaf -> return (tvShift s, leaf)
          let newLen = tvCount s - 1
          newTail <- unsafeLevelFor s $ tvCount s - 2
          let isSameArray = sameSmallMutableArray (tvTail s) newTail
          let newTailCount = if isSameArray then tvTailCount s - 1 else levelSize
          x <- unsafeIndex v newLen
          putState v $ s { tvCount = newLen
                         , tvShift = newShift
                         , tvRoot = newRoot
                         , tvTail = newTail
                         , tvTailCount = newTailCount
                         , tvTailEdited = True
                         }
          return $ Just x

{-
  let newCount = tvCount s - 1
      decreasedSize = tvTailCount s - 1
      pluckElemOff arr i = do
        elem <- readSmallArray arr i
        writeSmallArray arr i undefined
        return elem
  if tvTailCount s > 0
    then do
      e <- pluckElemOff (tvTail s) decreasedSize
      putState v $ s { tvCount = newCount, tvTailCount = decreasedSize, tvTailEdited = True }
      return $ Just e
    else do
      mt <- popTail s
      case mt of
        Nothing -> return Nothing
        Just n -> let levelSize' = levelSize - 1 in case n of
          UneditedLeaf (Level l) -> do
            arr <- newSmallArray levelSize' (error "Not initialized")
            copySmallArray l 0 arr levelSize' 0
            putState v $ s { tvCount = newCount, tvTail = arr, tvTailCount = levelSize', tvTailEdited = True }
            return $ Just $ indexSmallArray l levelSize'
          EditedLeaf (MutableLevel l) -> do
            e <- pluckElemOff l levelSize'
            putState v $ s { tvCount = newCount, tvTail = l, tvTailCount = levelSize', tvTailEdited = True }
            return $ Just e
          _ -> error "Invariant violation: popTail shouldn't return a branch"
-}
 
length :: PrimMonad m => TransientVector (PrimState m) a -> m Int
length = fmap tvCount . getState
