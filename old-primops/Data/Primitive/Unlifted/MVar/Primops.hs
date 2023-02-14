{-# language ScopedTypeVariables #-}
{-# language MagicHash #-}
{-# language KindSignatures #-}
{-# language UnboxedTuples #-}
{-# language UnboxedSums #-}
{-# language UnliftedNewtypes #-}
{-# language RoleAnnotations #-}
{-# language DataKinds #-}

module Data.Primitive.Unlifted.MVar.Primops
  ( UnliftedMVar#
  , newUnliftedMVar#
  , takeUnliftedMVar#
  , tryTakeUnliftedMVar#
  , putUnliftedMVar#
  , tryPutUnliftedMVar#
  , readUnliftedMVar#
  , tryReadUnliftedMVar#
  , sameUnliftedMVar#
  , isEmptyUnliftedMVar#
  ) where

import GHC.Exts (MVar#, Any, State#, Int#, newMVar#, unsafeCoerce#, takeMVar#, tryTakeMVar#, putMVar#, tryPutMVar#, readMVar#, tryReadMVar#, sameMVar#, isEmptyMVar#)

import Data.Primitive.Unlifted.Type

newtype UnliftedMVar# s (a :: UnliftedType) = UnliftedMVar# (MVar# s Any)
type role UnliftedMVar# nominal representational

newUnliftedMVar# :: State# s -> (# State# s, UnliftedMVar# s a #)
{-# INLINE newUnliftedMVar# #-}
newUnliftedMVar# s = case newMVar# s of
  (# s', mv #) -> (# s', UnliftedMVar# mv #)

takeUnliftedMVar# :: UnliftedMVar# s a -> State# s -> (# State# s, a #)
{-# NOINLINE takeUnliftedMVar# #-}
takeUnliftedMVar# (UnliftedMVar# mv) s = unsafeCoerce# (takeMVar# mv s)

tryTakeUnliftedMVar# :: UnliftedMVar# s a -> State# s -> (# State# s, (# (##) | a #) #)
{-# NOINLINE tryTakeUnliftedMVar# #-}
tryTakeUnliftedMVar# (UnliftedMVar# mv) s =
  case unsafeCoerce# (tryTakeMVar# mv s) of
    (# s', 0#, _ #) -> (# s', (#(##)| #)#)
    (# s', _, a #) -> (# s', (#|a #) #)

putUnliftedMVar# :: UnliftedMVar# s a -> a -> State# s -> State# s
{-# NOINLINE putUnliftedMVar# #-}
putUnliftedMVar# (UnliftedMVar# mv) a s
  = putMVar# mv (unsafeCoerce# a) s

tryPutUnliftedMVar# :: UnliftedMVar# s a -> a -> State# s -> (# State# s, Int# #)
{-# NOINLINE tryPutUnliftedMVar# #-}
tryPutUnliftedMVar# (UnliftedMVar# mv) a s
  = tryPutMVar# mv (unsafeCoerce# a) s

readUnliftedMVar# :: UnliftedMVar# s a -> State# s -> (# State# s, a #)
{-# NOINLINE readUnliftedMVar# #-}
readUnliftedMVar# (UnliftedMVar# mv) s = unsafeCoerce# (readMVar# mv s)

tryReadUnliftedMVar# :: UnliftedMVar# s a -> State# s -> (# State# s, (# (##) | a #) #)
{-# NOINLINE tryReadUnliftedMVar# #-}
tryReadUnliftedMVar# (UnliftedMVar# mv) s =
  case unsafeCoerce# (tryReadMVar# mv s) of
    (# s', 0#, _ #) -> (# s', (#(##)| #)#)
    (# s', _, a #) -> (# s', (#|a #) #)

sameUnliftedMVar# :: UnliftedMVar# s a -> UnliftedMVar# s a -> Int#
{-# INLINE sameUnliftedMVar# #-}
sameUnliftedMVar# (UnliftedMVar# mv1) (UnliftedMVar# mv2)
  = sameMVar# mv1 mv2

isEmptyUnliftedMVar# :: UnliftedMVar# s a -> State# s -> (# State# s, Int# #)
{-# INLINE isEmptyUnliftedMVar# #-}
isEmptyUnliftedMVar# (UnliftedMVar# mv) s
  = isEmptyMVar# mv s
