{-# language KindSignatures #-}
{-# language TypeFamilies #-}
{-# language MagicHash #-}

module Data.Primitive.Unlifted.Box where
import GHC.Exts (TYPE, RuntimeRep (UnliftedRep))
import Data.Primitive.Unlifted.Class

data Box (a :: TYPE 'UnliftedRep) = Box { unBox# :: a }

instance PrimUnlifted (Box a) where
  {-# INLINE toUnlifted# #-}
  {-# INLINE fromUnlifted# #-}
  type Unlifted (Box a) = a

  toUnlifted# (Box a) = a
  fromUnlifted# a = Box a

toBox :: PrimUnlifted a => a -> Box (Unlifted a)
toBox a = Box (toUnlifted# a)

fromBox :: PrimUnlifted a => Box (Unlifted a) -> a
fromBox (Box a) = fromUnlifted# a
