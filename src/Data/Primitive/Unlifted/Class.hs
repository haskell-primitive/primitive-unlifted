{-# language MagicHash #-}
{-# language UnboxedTuples #-}
{-# language TypeFamilies #-}
{-# language ScopedTypeVariables #-}
{-# language CPP #-}
{-# language DataKinds #-}

module Data.Primitive.Unlifted.Class
  ( PrimUnlifted(..)
  ) where

import Data.ByteString.Short.Internal (ShortByteString(SBS))
import Data.Text.Short (ShortText,toShortByteString)
import Data.Text.Short.Unsafe (fromShortByteStringUnsafe)
import Data.Primitive.PrimArray (PrimArray(..),MutablePrimArray(..))
import Data.Primitive.ByteArray (ByteArray(..),MutableByteArray(..))
import GHC.MVar (MVar(..))
import GHC.IORef (IORef(..))
import GHC.STRef (STRef(..))
import GHC.Exts (State#,MutableByteArray#,ByteArray#,Int#)
import GHC.Exts (ArrayArray#,MutableArrayArray#)
import GHC.Exts (MVar#,MutVar#,RealWorld)
import GHC.Exts (TYPE,unsafeCoerce#)

import qualified Data.Primitive.MVar as PM
import qualified GHC.Exts as Exts

-- In GHC 9.2 the UnliftedRep constructor of RuntimeRep was removed
-- and replaced with a type synonym
#if __GLASGOW_HASKELL__  >= 902
import GHC.Exts (UnliftedRep)
#else
import GHC.Exts (RuntimeRep(UnliftedRep))
type UnliftedRep = 'UnliftedRep
#endif

class PrimUnlifted a where
  type Unlifted a :: TYPE UnliftedRep
  toUnlifted# :: a -> Unlifted a
  fromUnlifted# :: Unlifted a -> a
  writeUnliftedArray# ::
       MutableArrayArray# s
    -> Int#
    -> a
    -> State# s
    -> State# s
  readUnliftedArray# ::
       MutableArrayArray# s
    -> Int#
    -> State# s
    -> (# State# s, a #)
  indexUnliftedArray# ::
       ArrayArray#
    -> Int#
    -> a

instance PrimUnlifted (PrimArray a) where
  {-# inline writeUnliftedArray# #-}
  {-# inline readUnliftedArray# #-}
  {-# inline indexUnliftedArray# #-}
  type Unlifted (PrimArray a) = ByteArray#
  toUnlifted# (PrimArray x) = x
  fromUnlifted# x = PrimArray x
  writeUnliftedArray# a i (PrimArray x) = Exts.writeByteArrayArray# a i x
  readUnliftedArray# a i s0 = case Exts.readByteArrayArray# a i s0 of
    (# s1, x #) -> (# s1, PrimArray x #)
  indexUnliftedArray# a i = PrimArray (Exts.indexByteArrayArray# a i)

instance PrimUnlifted ByteArray where
  {-# inline writeUnliftedArray# #-}
  {-# inline readUnliftedArray# #-}
  {-# inline indexUnliftedArray# #-}
  type Unlifted ByteArray = ByteArray#
  toUnlifted# (ByteArray x) = x
  fromUnlifted# x = ByteArray x
  writeUnliftedArray# a i (ByteArray x) = Exts.writeByteArrayArray# a i x
  readUnliftedArray# a i s0 = case Exts.readByteArrayArray# a i s0 of
    (# s1, x #) -> (# s1, ByteArray x #)
  indexUnliftedArray# a i = ByteArray (Exts.indexByteArrayArray# a i)

instance PrimUnlifted ShortByteString where
  {-# inline writeUnliftedArray# #-}
  {-# inline readUnliftedArray# #-}
  {-# inline indexUnliftedArray# #-}
  type Unlifted ShortByteString = ByteArray#
  toUnlifted# (SBS x) = x
  fromUnlifted# x = SBS x
  writeUnliftedArray# a i (SBS x) = Exts.writeByteArrayArray# a i x
  readUnliftedArray# a i s0 = case Exts.readByteArrayArray# a i s0 of
    (# s1, x #) -> (# s1, SBS x #)
  indexUnliftedArray# a i = SBS (Exts.indexByteArrayArray# a i)

instance PrimUnlifted ShortText where
  {-# inline writeUnliftedArray# #-}
  {-# inline readUnliftedArray# #-}
  {-# inline indexUnliftedArray# #-}
  type Unlifted ShortText = ByteArray#
  toUnlifted# t = case toShortByteString t of { SBS x -> x }
  fromUnlifted# x = fromShortByteStringUnsafe (SBS x)
  writeUnliftedArray# a i t = case toShortByteString t of
    SBS x -> Exts.writeByteArrayArray# a i x
  readUnliftedArray# a i s0 = case Exts.readByteArrayArray# a i s0 of
    (# s1, x #) -> (# s1, fromShortByteStringUnsafe (SBS x) #)
  indexUnliftedArray# a i = fromShortByteStringUnsafe (SBS (Exts.indexByteArrayArray# a i))

-- This uses unsafeCoerce# in the implementation of
-- indexUnliftedArray#. This does not lead to corruption FFI codegen
-- since ByteArray# and MutableByteArray# have the same FFI offset
-- applied by add_shim.
-- This also uses unsafeCoerce# to relax the constraints on the
-- state token. The primitives in GHC.Prim are too restrictive.
instance PrimUnlifted (MutableByteArray s) where
  {-# inline writeUnliftedArray# #-}
  {-# inline readUnliftedArray# #-}
  {-# inline indexUnliftedArray# #-}
  type Unlifted (MutableByteArray s) = MutableByteArray# s
  toUnlifted# (MutableByteArray x) = x
  fromUnlifted# x = MutableByteArray x
  writeUnliftedArray# a i (MutableByteArray x) =
    Exts.writeMutableByteArrayArray# a i (retoken x)
  readUnliftedArray# a i s0 = case Exts.readMutableByteArrayArray# a i s0 of
    (# s1, x #) -> (# s1, MutableByteArray (retoken x) #)
  indexUnliftedArray# a i = MutableByteArray (baToMba (Exts.indexByteArrayArray# a i))

-- See the note on the PrimUnlifted instance for MutableByteArray.
-- The same uses of unsafeCoerce# happen here.
instance PrimUnlifted (MutablePrimArray s a) where
  {-# inline writeUnliftedArray# #-}
  {-# inline readUnliftedArray# #-}
  {-# inline indexUnliftedArray# #-}
  type Unlifted (MutablePrimArray s a) = MutableByteArray# s
  toUnlifted# (MutablePrimArray x) = x
  fromUnlifted# x = MutablePrimArray x
  writeUnliftedArray# a i (MutablePrimArray x) =
    Exts.writeMutableByteArrayArray# a i (retoken x)
  readUnliftedArray# a i s0 = case Exts.readMutableByteArrayArray# a i s0 of
    (# s1, x #) -> (# s1, MutablePrimArray (retoken x) #)
  indexUnliftedArray# a i = MutablePrimArray (baToMba (Exts.indexByteArrayArray# a i))

-- This uses unsafeCoerce# in the implementation of all of its
-- methods. This does not lead to corruption FFI codegen since ArrayArray#
-- and MVar# have the same FFI offset applied by add_shim. However, in
-- GHC 8.10, the offset of ArrayArray# changes. Consequently, this library
-- cannot build with GHC 8.10.
instance PrimUnlifted (PM.MVar s a) where
  {-# inline writeUnliftedArray# #-}
  {-# inline readUnliftedArray# #-}
  {-# inline indexUnliftedArray# #-}
  type Unlifted (PM.MVar s a) = MVar# s a
  toUnlifted# (PM.MVar x) = x
  fromUnlifted# x = PM.MVar x
  writeUnliftedArray# a i (PM.MVar x) =
    Exts.writeArrayArrayArray# a i (mvarToArrArr x)
  readUnliftedArray# a i s0 = case Exts.readArrayArrayArray# a i s0 of
    (# s1, x #) -> (# s1, PM.MVar (arrArrToMVar x) #)
  indexUnliftedArray# a i = PM.MVar (arrArrToMVar (Exts.indexArrayArrayArray# a i))

-- This uses unsafeCoerce# in the implementation of all of its
-- methods. See the note for the PrimUnlifted instance of
-- Data.Primitive.MVar.MVar.
instance PrimUnlifted (MVar a) where
  {-# inline writeUnliftedArray# #-}
  {-# inline readUnliftedArray# #-}
  {-# inline indexUnliftedArray# #-}
  type Unlifted (MVar a) = MVar# RealWorld a
  toUnlifted# (MVar x) = x
  fromUnlifted# x = MVar x
  writeUnliftedArray# a i (MVar x) =
    Exts.writeArrayArrayArray# a i (mvarToArrArr x)
  readUnliftedArray# a i s0 = case Exts.readArrayArrayArray# a i s0 of
    (# s1, x #) -> (# s1, MVar (arrArrToMVar x) #)
  indexUnliftedArray# a i = MVar (arrArrToMVar (Exts.indexArrayArrayArray# a i))

-- This uses unsafeCoerce# in the implementation of all of its
-- methods. This does not lead to corruption FFI codegen since ArrayArray#
-- and MutVar# have the same FFI offset applied by add_shim.
instance PrimUnlifted (STRef s a) where
  {-# inline writeUnliftedArray# #-}
  {-# inline readUnliftedArray# #-}
  {-# inline indexUnliftedArray# #-}
  type Unlifted (STRef s a) = MutVar# s a
  toUnlifted# (STRef x) = x
  fromUnlifted# x = STRef x
  writeUnliftedArray# a i (STRef x) =
    Exts.writeArrayArrayArray# a i (mutVarToArrArr x)
  readUnliftedArray# a i s0 = case Exts.readArrayArrayArray# a i s0 of
    (# s1, x #) -> (# s1, STRef (arrArrToMutVar x) #)
  indexUnliftedArray# a i =
    STRef (arrArrToMutVar (Exts.indexArrayArrayArray# a i))

instance PrimUnlifted (IORef a) where
  {-# inline writeUnliftedArray# #-}
  {-# inline readUnliftedArray# #-}
  {-# inline indexUnliftedArray# #-}
  type Unlifted (IORef a) = MutVar# RealWorld a
  toUnlifted# (IORef (STRef x)) = x
  fromUnlifted# x = IORef (STRef x)
  writeUnliftedArray# a i (IORef v) = writeUnliftedArray# a i v
  readUnliftedArray# a i s0 = case readUnliftedArray# a i s0 of
    (# s1, v #) -> (# s1, IORef v #)
  indexUnliftedArray# a i = IORef (indexUnliftedArray# a i)

arrArrToMutVar :: ArrayArray# -> MutVar# s a
{-# inline arrArrToMutVar #-}
arrArrToMutVar = unsafeCoerce#

mutVarToArrArr :: MutVar# s a -> ArrayArray#
{-# inline mutVarToArrArr #-}
mutVarToArrArr = unsafeCoerce#

arrArrToMVar :: ArrayArray# -> MVar# s a
{-# inline arrArrToMVar #-}
arrArrToMVar = unsafeCoerce#

mvarToArrArr :: MVar# s a -> ArrayArray#
{-# inline mvarToArrArr #-}
mvarToArrArr = unsafeCoerce#

baToMba :: ByteArray# -> MutableByteArray# s
{-# inline baToMba #-}
baToMba = unsafeCoerce#

retoken :: MutableByteArray# s -> MutableByteArray# r
{-# inline retoken #-}
retoken = unsafeCoerce#
