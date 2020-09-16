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
import Data.Primitive.Unlifted.Array
import Data.Array.Base (MArray (..))
import Data.Ix (Ix, rangeSize)
import GHC.Exts (TVar#, RealWorld)

data TArray i a = TArray {
    _lb :: !i         -- the lower bound
  , _ub :: !i         -- the upper bound
  , range :: !Int    -- A cache of (rangeSize (l, u))
                     -- used to make sure an index is really in range
  , arr :: !(UnliftedArray_ (TVar a) (TVar# RealWorld a))
  }
type role TArray nominal representational

instance Eq (TArray i a) where
  -- There's no way for TVars to move from one TArray to another,
  -- so two of them are equal iff they're both empty or they're
  -- actually the same array. There's no "safe" way to check if
  -- they're the same array (though we can use `unsafeCoerce#` with
  -- `sameMutableUnliftedArray#` if we wan to). But we can just
  -- do a quick size check and then look at the first TVar of each.
  --
  -- Note: we consider any two empty TArrays the same, even if they
  -- have different bounds (e.g., (0, -1) and (4,2)). This matches
  -- the (somewhat surprising) behavior of the version in stm,
  -- based on the equally surprising instance for Data.Array.
  -- See GHC Gitlab issue #18700. If that issue gets resolved
  -- toward considering this a bug, we should fix it here too.
  TArray _lb1 _ub1 range1 arr1 == TArray _lb2 _ub2 range2 arr2
    = range1 == range2 && (range1 == 0
        || indexUnliftedArray arr1 0 == indexUnliftedArray arr2 0)

instance MArray TArray e STM where
  getBounds (TArray l u _ _) = pure (l, u)
  newArray b e = do
    tvs <- rep (rangeSize b) (newTVar e)
    return $ listTArray b tvs
  -- The stm version defines newArray_, but the default does the
  -- same thing.
  unsafeRead tarr i = readTVar $ indexUnliftedArray (arr tarr) i
  unsafeWrite tarr i e = writeTVar (indexUnliftedArray (arr tarr) i) e
  getNumElements !tarr = pure (range tarr)

-- | Writes are slow in 'IO'.
instance MArray TArray e IO where
  getBounds (TArray l u _ _) = pure (l, u)
  newArray b e = do
    tvs <- rep (rangeSize b) (newTVarIO e)
    return $ listTArray b tvs
  -- The stm version defines newArray_, but the default does the
  -- same thing.
  unsafeRead tarr i = readTVarIO $ indexUnliftedArray (arr tarr) i
  unsafeWrite tarr i e = atomically $ writeTVar (indexUnliftedArray (arr tarr) i) e
  getNumElements !tarr = pure (range tarr)

-- | Stolen from stm:
-- Like 'replicateM' but uses an accumulator to prevent stack overflows.
-- Unlike 'replicateM' the returned list is in reversed order.
-- This doesn't matter though since this function is only used to create
-- arrays with identical elements.
--
-- TODO: For `IO`, we should surely build the array directly, rather
-- than first making a list. For STM, I'm *guessing* this would be a
-- safe place to use unsafeIOtoSTM to do the same.
rep :: Monad m => Int -> m a -> m [a]
rep n m = go n []
    where
      go 0 xs = return xs
      go i xs = do
          x <- m
          go (i-1) (x:xs)

listTArray :: Ix i => (i, i) -> [TVar e] -> TArray i e
listTArray (l, u) tvs = TArray l u n (unliftedArrayFromListN n tvs)
  where
    !n = rangeSize (l, u)
