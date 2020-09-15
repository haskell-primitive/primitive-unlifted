{-# language BangPatterns #-}
{-# language MagicHash #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language TypeFamilies #-}
{-# language UnboxedTuples #-}
{-# language RoleAnnotations #-}

-- |
-- GHC contains three general classes of value types:
--
--   1. Unboxed types: values are machine values made up of fixed numbers of bytes
--   2. Unlifted types: values are pointers, but strictly evaluated
--   3. Lifted types: values are pointers, lazily evaluated
--
-- The first category can be stored in a 'ByteArray', and this allows types in
-- category 3 that are simple wrappers around category 1 types to be stored
-- more efficiently using a 'ByteArray'. This module provides the same facility
-- for category 2 types.
--
-- GHC has two primitive types, 'ArrayArray#' and 'MutableArrayArray#'. These
-- are arrays of pointers, but of category 2 values, so they are known to not
-- be bottom. This allows types that are wrappers around such types to be stored
-- in an array without an extra level of indirection.
--
-- The way that the 'ArrayArray#' API works is that one can read and write
-- 'ArrayArray#' values to the positions. This works because all category 2
-- types share a uniform representation, unlike unboxed values which are
-- represented by varying (by type) numbers of bytes. However, using the
-- this makes the internal API very unsafe to use, as one has to coerce values
-- to and from 'ArrayArray#'.
--
-- The API presented by this module is more type safe. 'UnliftedArray' and
-- 'MutableUnliftedArray' are parameterized by the type of arrays they contain, and
-- the coercions necessary are abstracted into a class, 'PrimUnlifted', of things
-- that are eligible to be stored.
module Data.Primitive.Unlifted.BoxArray
  ( -- * Types
    BoxArray(..)
  , MutableBoxArray(..)
    -- * Operations
  , newBoxArray
  , unsafeNewBoxArray
  , BA.sizeofBoxArray
  , BA.sizeofMutableBoxArray
  , BA.sameMutableBoxArray
  , writeBoxArray
  , readBoxArray
  , BA.indexBoxArray
  , unsafeFreezeBoxArray
  , freezeBoxArray
  , thawBoxArray
  , setBoxArray
  , copyBoxArray
  , copyMutableBoxArray
  , BA.cloneBoxArray
  , cloneMutableBoxArray
  , BA.emptyBoxArray
  , BA.singletonBoxArray
  , BA.runBoxArray
    -- * List Conversion
  , BA.boxArrayToList
  , BA.boxArrayFromList
  , BA.boxArrayFromListN
    -- * Folding
  , BA.foldrBoxArray
  , BA.foldrBoxArray'
  , BA.foldlBoxArray
  , BA.foldlBoxArray'
  , BA.foldlBoxArrayM'
    -- * Traversals
  , BA.traverseBoxArray_
  , BA.itraverseBoxArray_
    -- * Mapping
  , BA.mapBoxArray
  ) where

import Data.Primitive.Unlifted.BoxArray.ST (BoxArray (..), MutableBoxArray (..))
import qualified Data.Primitive.Unlifted.BoxArray.ST as BA
import Control.Monad.Primitive (PrimMonad,PrimState,stToPrim)
import Data.Primitive.Unlifted.Box (Box)
{-
import Control.Monad.ST (ST)
import Data.Primitive.Unlifted.Class (PrimUnlifted (..))
import Data.Primitive.Unlifted.Array.Base
import Data.Primitive.Unlifted.Box
import GHC.Exts (Int(I#),State#)
import qualified GHC.ST as ST

import qualified Data.List as L
import qualified GHC.Exts as Exts

data BoxArray a
  = BoxArray (UnliftedArray# a)
type role BoxArray representational

data MutableBoxArray s a
  = MutableBoxArray (MutableUnliftedArray# s a)
type role MutableBoxArray nominal representational

instance PrimUnlifted (BoxArray a) where
  type Unlifted (BoxArray a) = UnliftedArray# a
  toUnlifted# (BoxArray a) = a
  fromUnlifted# x = BoxArray x

instance PrimUnlifted (MutableBoxArray s a) where
  type Unlifted (MutableBoxArray s a) = MutableUnliftedArray# s a
  toUnlifted# (MutableBoxArray a) = a
  fromUnlifted# x = MutableBoxArray x
-}

-- | Creates a new 'MutableBoxArray' with the specified value as initial
-- contents.
newBoxArray
  :: PrimMonad m
  => Int -- ^ size
  -> Box a -- ^ initial value
  -> m (MutableBoxArray (PrimState m) a)
newBoxArray len v = stToPrim (BA.newBoxArray len v)
{-# inline newBoxArray #-}

setBoxArray
  :: PrimMonad m
  => MutableBoxArray (PrimState m) a -- ^ destination
  -> Box a -- ^ value to fill with
  -> Int -- ^ offset
  -> Int -- ^ length
  -> m ()
{-# inline setBoxArray #-}
setBoxArray mua v off len = stToPrim $ BA.setBoxArray mua v off len

writeBoxArray :: PrimMonad m
  => MutableBoxArray (PrimState m) a
  -> Int
  -> Box a
  -> m ()
{-# inline writeBoxArray #-}
writeBoxArray ar x b = stToPrim $ BA.writeBoxArray ar x b

readBoxArray :: PrimMonad m
  => MutableBoxArray (PrimState m) a
  -> Int
  -> m (Box a)
{-# inline readBoxArray #-}
readBoxArray arr ix = stToPrim $ BA.readBoxArray arr ix

-- | Freezes a 'MutableBoxArray', yielding an 'BoxArray'. This simply
-- marks the array as frozen in place, so it should only be used when no further
-- modifications to the mutable array will be performed.
unsafeFreezeBoxArray
  :: PrimMonad m
  => MutableBoxArray (PrimState m) a
  -> m (BoxArray a)
unsafeFreezeBoxArray ma = stToPrim $ BA.unsafeFreezeBoxArray ma
{-# inline unsafeFreezeBoxArray #-}

-- | Copies the contents of an immutable array into a mutable array.
copyBoxArray
  :: PrimMonad m
  => MutableBoxArray (PrimState m) a -- ^ destination
  -> Int -- ^ offset into destination
  -> BoxArray a -- ^ source
  -> Int -- ^ offset into source
  -> Int -- ^ number of elements to copy
  -> m ()
{-# inline copyBoxArray #-}
copyBoxArray dst doff src soff ln = stToPrim $ BA.copyBoxArray dst doff src soff ln

-- | Copies the contents of one mutable array into another.
copyMutableBoxArray
  :: PrimMonad m
  => MutableBoxArray (PrimState m) a -- ^ destination
  -> Int -- ^ offset into destination
  -> MutableBoxArray (PrimState m) a -- ^ source
  -> Int -- ^ offset into source
  -> Int -- ^ number of elements to copy
  -> m ()
{-# inline copyMutableBoxArray #-}
copyMutableBoxArray dst doff src soff ln
  = stToPrim $ BA.copyMutableBoxArray dst doff src soff ln

-- | Freezes a portion of a 'MutableBoxArray', yielding an 'BoxArray'.
-- This operation is safe, in that it copies the frozen portion, and the
-- existing mutable array may still be used afterward.
freezeBoxArray
  :: PrimMonad m
  => MutableBoxArray (PrimState m) a -- ^ source
  -> Int -- ^ offset
  -> Int -- ^ length
  -> m (BoxArray a)
freezeBoxArray mary off len = stToPrim $ BA.freezeBoxArray mary off len
{-# inline freezeBoxArray #-}

-- | Thaws a portion of an 'BoxArray', yielding a 'MutableBoxArray'.
-- This copies the thawed portion, so mutations will not affect the original
-- array.
thawBoxArray
  :: PrimMonad m
  => BoxArray a -- ^ source
  -> Int -- ^ offset
  -> Int -- ^ length
  -> m (MutableBoxArray (PrimState m) a)
{-# inline thawBoxArray #-}
thawBoxArray ary off len = stToPrim $ BA.thawBoxArray ary off len

-- | Creates a new 'MutableBoxArray'. This function is unsafe because it
-- initializes all elements of the array as pointers to the empty array. Attempting
-- to read one of these elements before writing to it is in effect an unsafe
-- coercion from @'BoxArray' a@ to the element type.
unsafeNewBoxArray
  :: PrimMonad m
  => Int -- ^ size
  -> m (MutableBoxArray (PrimState m) a)
{-# inline unsafeNewBoxArray #-}
unsafeNewBoxArray i = stToPrim $ BA.unsafeNewBoxArray i

-- | Creates a new 'MutableBoxArray' containing a copy of a portion of
-- another mutable array.
cloneMutableBoxArray
  :: PrimMonad m
  => MutableBoxArray (PrimState m) a -- ^ source
  -> Int -- ^ offset
  -> Int -- ^ length
  -> m (MutableBoxArray (PrimState m) a)
{-# inline cloneMutableBoxArray #-}
cloneMutableBoxArray mary off len
  = stToPrim $ BA.cloneMutableBoxArray mary off len
