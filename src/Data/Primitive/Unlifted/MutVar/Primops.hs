{-# language UnboxedTuples #-}
{-# language MagicHash #-}
{-# language UnliftedNewtypes #-}
{-# language ScopedTypeVariables #-}
{-# language RoleAnnotations #-}
{-# language KindSignatures #-}
{-# language DataKinds #-}

-- See UnsafeCoercions.md for an explanation of why we coerce
-- things the way we do here, and why some operations are marked
-- NOINLINE.


module Data.Primitive.Unlifted.MutVar.Primops
  ( UnliftedMutVar#
  , newUnliftedMutVar#
  , readUnliftedMutVar#
  , writeUnliftedMutVar#
  , sameUnliftedMutVar#
  , casUnliftedMutVar#
  , atomicSwapUnliftedMutVar#
  ) where

import GHC.Exts (MutVar#, Any, State#, Int#, newMutVar#, unsafeCoerce#, readMutVar#, writeMutVar#, sameMutVar#, casMutVar#)

import Data.Primitive.Unlifted.Type

-- | An @UnliftedMutVar#@ behaves like a single-element mutable array.
newtype UnliftedMutVar# s (a :: UnliftedType) = UnliftedMutVar# (MutVar# s Any)
type role UnliftedMutVar# nominal representational

newUnliftedMutVar# :: a -> State# s -> (# State# s, UnliftedMutVar# s a #)
{-# NOINLINE newUnliftedMutVar# #-}
newUnliftedMutVar# a s = case newMutVar# (unsafeCoerce# a) s of
  (# s', mv #) -> (# s', UnliftedMutVar# mv #)

readUnliftedMutVar# :: UnliftedMutVar# s a -> State# s -> (# State# s, a #)
{-# NOINLINE readUnliftedMutVar# #-}
readUnliftedMutVar# (UnliftedMutVar# mv) s
  = unsafeCoerce# (readMutVar# mv s)

writeUnliftedMutVar# :: UnliftedMutVar# s a -> a -> State# s -> State# s
{-# NOINLINE writeUnliftedMutVar# #-}
writeUnliftedMutVar# (UnliftedMutVar# mv) a s
  = writeMutVar# mv (unsafeCoerce# a) s

-- | Check whether two 'UnliftedMutVar#'es refer to the same mutable
-- variable. This is a check on object identity, and not on contents.
sameUnliftedMutVar# :: UnliftedMutVar# s a -> UnliftedMutVar# s a -> Int#
{-# INLINE sameUnliftedMutVar# #-}
sameUnliftedMutVar# (UnliftedMutVar# mv1) (UnliftedMutVar# mv2)
  = sameMutVar# mv1 mv2

-- Note: it's impossible to implement analogues of atomicModifyMutVar2#
-- or atomicModifyMutVar_# because those rely on being able to store
-- thunks in the variable.

-- | Performs a machine-level compare and swap (CAS) operation on an
-- 'UnliftedMutVar#'. Returns a tuple containing an 'Int#' which is '1#' when a
-- swap is performed, along with the most "current" value from the
-- 'UnliftedMutVar#'.  This return value can be used as the expected value if a
-- CAS loop is required, though it may be better to get a fresh read.  Note
-- that this behavior differs from the more common CAS behavior, which is to
-- return the /old/ value before the CAS occured.
casUnliftedMutVar#
  :: UnliftedMutVar# s a   -- ^ The 'UnliftedMutVar#' on which to operate
  -> a -- ^ The expected value
  -> a -- ^ The new value to install if the 'UnliftedMutVar# contains the expected value
  -> State# s -> (# State# s, Int#, a #)
{-# NOINLINE casUnliftedMutVar# #-}
casUnliftedMutVar# (UnliftedMutVar# mv) old new s
  = unsafeCoerce# (casMutVar# mv (unsafeCoerce# old) (unsafeCoerce# new) s)

-- | Atomically replace the value in an 'UnliftedMutVar#' with the given one,
-- returning the old value.
--
-- Implementation note: this really should be a GHC primop, because it is
-- supported very efficiently in hardware, but unfortunately it's not (yet), so
-- we implement it as a CAS loop.
atomicSwapUnliftedMutVar#
  :: UnliftedMutVar# s a -> a -> State# s -> (# State# s, a #)
{-# NOINLINE atomicSwapUnliftedMutVar# #-}
atomicSwapUnliftedMutVar# (UnliftedMutVar# mv) a s
  = unsafeCoerce# (atomicSwapMutVar# mv (unsafeCoerce# a) s)

atomicSwapMutVar#
  :: MutVar# s a -> a -> State# s -> (# State# s, a #)
-- We don't bother inlining this because it's kind of slow regardless;
-- there doesn't seem to be much point. We don't use the "latest"
-- value reported by casUnliftedMutVar# because I'm told chances of
-- CAS success are better if we use a perfectly fresh value than if
-- we take the time to check for CAS success in between.
atomicSwapMutVar# mv a s
  = case readMutVar# mv s of { (# s', old #) ->
    case casMutVar# mv old a s' of
      (# s'', 0#, _ #) -> atomicSwapMutVar# mv a s''
      (# s'', _, _ #) -> (# s'', old #) }
