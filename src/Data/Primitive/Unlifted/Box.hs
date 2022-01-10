{-# language KindSignatures #-}
{-# language TypeFamilies #-}
{-# language MagicHash #-}
{-# language DataKinds #-}

module Data.Primitive.Unlifted.Box where

import Data.Primitive.Unlifted.Class
import Data.Primitive.Unlifted.Type

data Box (a :: UnliftedType) = Box# { unBox# :: a }

instance PrimUnlifted (Box a) where
  {-# INLINE toUnlifted# #-}
  {-# INLINE fromUnlifted# #-}
  type Unlifted (Box a) = a

  toUnlifted# (Box# a) = a
  fromUnlifted# a = Box# a

toBox :: PrimUnlifted a => a -> Box (Unlifted a)
toBox a = Box# (toUnlifted# a)

fromBox :: PrimUnlifted a => Box (Unlifted a) -> a
fromBox (Box# a) = fromUnlifted# a
