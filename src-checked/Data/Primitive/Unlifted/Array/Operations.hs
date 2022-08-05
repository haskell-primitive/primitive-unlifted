module Data.Primitive.Unlifted.Array.Operations
  ( indexUnliftedArray
  , readUnliftedArray
  , writeUnliftedArray
  ) where

import Data.Primitive.Unlifted.Array.Internal (UnliftedArray(..),MutableUnliftedArray(..))
import Data.Primitive.Unlifted.Class (PrimUnlifted)
import Control.Monad.Primitive (PrimMonad,PrimState)
import qualified Data.Primitive.Unlifted.Array.Internal as Internal

indexUnliftedArray :: PrimUnlifted a => UnliftedArray a -> Int -> a
{-# noinline indexUnliftedArray #-}
indexUnliftedArray arr i 
  | i >= 0 && i < Internal.sizeofUnliftedArray arr = Internal.indexUnliftedArray arr i
  | otherwise = error "indexUnliftedArray: index out of bounds"

writeUnliftedArray :: (PrimMonad m, PrimUnlifted a) => MutableUnliftedArray (PrimState m) a -> Int -> a -> m ()
{-# noinline writeUnliftedArray #-}
writeUnliftedArray marr i x = do
  let siz = Internal.sizeofMutableUnliftedArray marr
  if i >= 0 && i < siz
    then Internal.writeUnliftedArray marr i x
    else error "writeUnliftedArray: index out of bounds"

readUnliftedArray :: (PrimMonad m, PrimUnlifted a) => MutableUnliftedArray (PrimState m) a -> Int -> m a
{-# noinline readUnliftedArray #-}
readUnliftedArray marr i = do
  let siz = Internal.sizeofMutableUnliftedArray marr
  if (i >= 0 && i < siz)
    then Internal.readUnliftedArray marr i
    else error "readUnliftedArray: index out of bounds"
