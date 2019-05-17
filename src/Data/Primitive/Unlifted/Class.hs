{-# language MagicHash #-}
{-# language UnboxedTuples #-}
{-# language TypeFamilies #-}

module Data.Primitive.Unlifted.Class
  ( PrimUnlifted(..)
  ) where

import Data.Primitive.ByteArray (ByteArray(..),MutableByteArray(..))
import GHC.Exts (State#,MutableByteArray#,ByteArray#,Int#)
import GHC.Exts (ArrayArray#,MutableArrayArray#,RuntimeRep(UnliftedRep))
import GHC.Exts (TYPE,unsafeCoerce#)

import qualified GHC.Exts as Exts

class PrimUnlifted a where
  type Unlifted a :: TYPE 'UnliftedRep
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

baToMba :: ByteArray# -> MutableByteArray# s
{-# inline baToMba #-}
baToMba = unsafeCoerce#

retoken :: MutableByteArray# s -> MutableByteArray# r
{-# inline retoken #-}
retoken = unsafeCoerce#
