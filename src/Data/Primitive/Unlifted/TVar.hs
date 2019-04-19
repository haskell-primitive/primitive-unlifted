{-# language BangPatterns #-}
{-# language RankNTypes #-}
{-# language UnboxedTuples #-}
{-# language MagicHash #-}
{-# language ScopedTypeVariables #-}

module Data.Primitive.Unlifted.TVar
  ( UnliftedTVar(..)
  , newUnliftedTVarIO
  , writeUnliftedTVar
  , modifyUnliftedTVar
  , readUnliftedTVar
  ) where

import Data.Primitive.UnliftedArray (PrimUnlifted,toArrayArray#,fromArrayArray#)
import GHC.IO (IO(..))
import GHC.Conc (STM(..))
import GHC.Exts (Any,RealWorld,TVar#,ArrayArray#,State#,unsafeCoerce#)
import GHC.Exts (newTVar#,writeTVar#,readTVar#)

data UnliftedTVar a = UnliftedTVar (TVar# RealWorld Any)

instance PrimUnlifted (UnliftedTVar a) where
  toArrayArray# (UnliftedTVar x) = unsafeCoerce# x
  fromArrayArray# x = UnliftedTVar (unsafeCoerce# x)

-- | @IO@ version of 'newUnliftedTVar'.  This is useful for creating top-level
-- 'TVar's using 'System.IO.Unsafe.unsafePerformIO', because using
-- 'atomically' inside 'System.IO.Unsafe.unsafePerformIO' isn't
-- possible.
newUnliftedTVarIO :: PrimUnlifted a => a -> IO (UnliftedTVar a)
{-# inline newUnliftedTVarIO #-}
newUnliftedTVarIO a = IO $ \s1# -> let !a# = toArrayArray# a in
  case newTVar# (unsafeCoerce# a# :: Any) s1# of
    (# s2#, tvar# #) -> (# s2#, UnliftedTVar tvar# #)

-- |Write the supplied value into an 'UnliftedTVar'.
writeUnliftedTVar :: PrimUnlifted a => UnliftedTVar a -> a -> STM ()
{-# inline writeUnliftedTVar #-}
writeUnliftedTVar (UnliftedTVar tvar#) val = let !val# = toArrayArray# val in STM $ \s1# ->
  case writeTVar# tvar# (unsafeCoerce# val# :: Any) s1# of
    s2# -> (# s2#, () #)

-- |Write the supplied value into an 'UnliftedTVar'.
modifyUnliftedTVar :: PrimUnlifted a => UnliftedTVar a -> (a -> a) -> STM ()
{-# inline modifyUnliftedTVar #-}
modifyUnliftedTVar tv f = do
  x <- readUnliftedTVar tv
  writeUnliftedTVar tv (f x)

-- |Return the current value stored in an 'UnliftedTVar'.
readUnliftedTVar :: PrimUnlifted a => UnliftedTVar a -> STM a
{-# inline readUnliftedTVar #-}
readUnliftedTVar (UnliftedTVar tvar#) = STM $ \s1# -> case readTVar# tvar# s1# of
  (# s2, v #) -> (# s2, fromArrayArray# ((unsafeCoerce# :: Any -> ArrayArray#) v) #)
