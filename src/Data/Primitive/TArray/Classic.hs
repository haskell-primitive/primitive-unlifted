{-# language MagicHash #-}
{-# language MultiParamTypeClasses #-}
{-# language ScopedTypeVariables #-}
{-# language BangPatterns #-}
{-# language FlexibleInstances #-}
{-# language RoleAnnotations #-}
{- OPTIONS_GHC -ddump-simpl #-}


{- |
This module is a drop-in replacement for @Control.Concurrent.STM.TArray@
in the @stm@ package. It has the same fundamental inefficiency of the
classic @TArray@, but it's a /little/ faster and more compact.
Specifically, this implementation uses two fewer words of memory
and one fewer indirection per element.
We also add an 'MArray' instance for working in 'IO' that the 'stm'
version lacks.
-}

module Data.Primitive.TArray.Classic (TArray) where
import GHC.Conc (STM, TVar, newTVar, readTVar, writeTVar
                , newTVarIO, readTVarIO, atomically)
import Data.Primitive.Unlifted.Box
import Data.Primitive.Unlifted.BoxArray
import Data.Array.Base (MArray (..))
import Data.Ix (Ix, rangeSize)
import GHC.Exts (TVar#, RealWorld)

data TArray i a = TArray {
    _lb :: !i         -- the lower bound
  , _ub :: !i         -- the upper bound
  , range :: !Int    -- A cache of (rangeSize (l, u))
                     -- used to make sure an index is really in range
  , arr :: !(BoxArray (TVar# RealWorld a))
  }
type role TArray nominal representational

-- This Ix constraint is copied from GHC.Arr, but I don't know
-- why it's there; we only really need Eq.
instance Ix i => Eq (TArray i a) where
  TArray lb1 ub1 range1 arr1 == TArray lb2 ub2 range2 arr2
    | range1 == 0 = range2 == 0
    | otherwise = lb1 == lb2 && ub1 == ub2 &&
        and [fromBox (indexBoxArray arr1 i) == (fromBox (indexBoxArray arr2 i) :: TVar a)
              | i <- [0 .. range1 - 1]]


instance MArray TArray e STM where
  getBounds (TArray l u _ _) = pure (l, u)
  newArray b e = do
    tvs <- rep (rangeSize b) (newTVar e)
    return $ listTArray b tvs
  -- The stm version defines newArray_, but the default does the
  -- same thing.
  unsafeRead tarr i = readTVar $ fromBox $ indexBoxArray (arr tarr) i
  unsafeWrite tarr i e = writeTVar (fromBox $ indexBoxArray (arr tarr) i) e
  getNumElements !tarr = pure (range tarr)

-- | Writes are slow in 'IO'.
instance MArray TArray e IO where
  getBounds (TArray l u _ _) = pure (l, u)
  newArray b e = do
    tvs <- rep (rangeSize b) (newTVarIO e)
    return $ listTArray b tvs
  -- The stm version defines newArray_, but the default does the
  -- same thing.
  unsafeRead tarr i = readTVarIO $ fromBox $ indexBoxArray (arr tarr) i
  unsafeWrite tarr i e = atomically $ writeTVar (fromBox $ indexBoxArray (arr tarr) i) e
  getNumElements !tarr = pure (range tarr)

-- | Like 'replicateM' but uses an accumulator to prevent stack overflows.
-- Unlike 'replicateM' the returned list is in reversed order.
-- This doesn't matter though since this function is only used to create
-- arrays with identical elements.
rep :: Monad m => Int -> m a -> m [a]
rep n m = go n []
    where
      go 0 xs = return xs
      go i xs = do
          x <- m
          go (i-1) (x:xs)

listTArray :: Ix i => (i, i) -> [TVar e] -> TArray i e
listTArray (l, u) tvs = TArray l u n (boxArrayFromListN n (map toBox tvs))
  where
    !n = rangeSize (l, u)
