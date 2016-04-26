module Data.Vector.Transient (
  TransientVector,
  empty,
  presized,
  write,
  push,
  pop,
  read,
  size
) where
import Data.Vector.Internal.Transient
import Control.Monad.Primitive

size :: PrimMonad m => TransientVector (PrimState m) a -> m Int
size = fmap tvCount . getState

