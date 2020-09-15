{-# language BangPatterns #-}
{-# language MagicHash #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language TypeFamilies #-}
{-# language UnboxedTuples #-}
{-# language RoleAnnotations #-}
{- OPTIONS_GHC -ddump-simpl #-}

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
module Data.Primitive.Unlifted.Array.ST
  ( -- * Types
    UnliftedArray(..)
  , MutableUnliftedArray(..)
    -- * Operations
  , newUnliftedArray
  , unsafeNewUnliftedArray
  , sizeofUnliftedArray
  , sizeofMutableUnliftedArray
  , sameMutableUnliftedArray
  , writeUnliftedArray
  , readUnliftedArray
  , indexUnliftedArray
  , unsafeFreezeUnliftedArray
  , freezeUnliftedArray
  , thawUnliftedArray
  , setUnliftedArray
  , copyUnliftedArray
  , copyMutableUnliftedArray
  , cloneUnliftedArray
  , cloneMutableUnliftedArray
  , emptyUnliftedArray
  , singletonUnliftedArray
  , runUnliftedArray
    -- * List Conversion
  , unliftedArrayToList
  , unliftedArrayFromList
  , unliftedArrayFromListN
    -- * Folding
  , foldrUnliftedArray
  , foldrUnliftedArray'
  , foldlUnliftedArray
  , foldlUnliftedArray'
  , foldlUnliftedArrayM'
    -- * Traversals
  , traverseUnliftedArray_
  , itraverseUnliftedArray_
    -- * Mapping
  , mapUnliftedArray
  ) where

import Control.Monad.ST (ST)
import Data.Primitive.Unlifted.Class (PrimUnlifted (..))
import Data.Primitive.Unlifted.Array.Base
import GHC.Exts (Int(I#),State#)
import qualified GHC.ST as ST

import qualified Data.List as L
import qualified GHC.Exts as Exts
import qualified Data.Primitive.Unlifted.BoxArray.ST as BA
import Data.Primitive.Unlifted.BoxArray.ST (BoxArray (..), MutableBoxArray (..))
import Data.Primitive.Unlifted.Box

newtype UnliftedArray a
  = UnliftedArray (BoxArray (Unlifted a))
type role UnliftedArray nominal

newtype MutableUnliftedArray s a
  = MutableUnliftedArray (MutableBoxArray s (Unlifted a))
type role MutableUnliftedArray nominal nominal

instance PrimUnlifted (UnliftedArray a) where
  {-# INLINE toUnlifted# #-}
  {-# INLINE fromUnlifted# #-}
  type Unlifted (UnliftedArray a) = UnliftedArray# (Unlifted a)
  toUnlifted# (UnliftedArray (BoxArray ba)) = ba
  fromUnlifted# x = UnliftedArray (BoxArray x)

instance PrimUnlifted (MutableUnliftedArray s a) where
  {-# INLINE toUnlifted# #-}
  {-# INLINE fromUnlifted# #-}
  type Unlifted (MutableUnliftedArray s a) = MutableUnliftedArray# s (Unlifted a)
  toUnlifted# (MutableUnliftedArray (MutableBoxArray ba)) = ba
  fromUnlifted# x = MutableUnliftedArray (MutableBoxArray x)

-- | Creates a new 'MutableUnliftedArray' with the specified value as initial
-- contents.
newUnliftedArray
  :: PrimUnlifted a
  => Int -- ^ size
  -> a -- ^ initial value
  -> ST s (MutableUnliftedArray s a)
newUnliftedArray len v = MutableUnliftedArray <$> BA.newBoxArray len (toBox v)

setUnliftedArray
  :: PrimUnlifted a
  => MutableUnliftedArray s a -- ^ destination
  -> a -- ^ value to fill with
  -> Int -- ^ offset
  -> Int -- ^ length
  -> ST s ()
{-# inline setUnliftedArray #-}
setUnliftedArray (MutableUnliftedArray mua) v off len = BA.setBoxArray mua (toBox v) off len

-- | Yields the length of an 'UnliftedArray'.
sizeofUnliftedArray :: UnliftedArray e -> Int
{-# inline sizeofUnliftedArray #-}
sizeofUnliftedArray (UnliftedArray ar) = BA.sizeofBoxArray ar

-- | Yields the length of a 'MutableUnliftedArray'.
sizeofMutableUnliftedArray :: MutableUnliftedArray s e -> Int
{-# inline sizeofMutableUnliftedArray #-}
sizeofMutableUnliftedArray (MutableUnliftedArray ma)
  = BA.sizeofMutableBoxArray ma

{-# SPECIALIZE writeUnliftedArray :: MutableUnliftedArray s (UnliftedArray a) -> Int -> UnliftedArray a -> ST s () #-}
writeUnliftedArray :: PrimUnlifted a
  => MutableUnliftedArray s a
  -> Int
  -> a
  -> ST s ()
{-# inline writeUnliftedArray #-}
writeUnliftedArray (MutableUnliftedArray arr) ix a = BA.writeBoxArray arr ix (toBox a)

readUnliftedArray :: PrimUnlifted a
  => MutableUnliftedArray s a
  -> Int
  -> ST s a
{-# inline readUnliftedArray #-}
readUnliftedArray (MutableUnliftedArray arr) ix = fromBox <$> BA.readBoxArray arr ix

indexUnliftedArray :: PrimUnlifted a
  => UnliftedArray a
  -> Int
  -> a
{-# inline indexUnliftedArray #-}
indexUnliftedArray (UnliftedArray arr) ix = fromBox $ BA.indexBoxArray arr ix

-- | Freezes a 'MutableUnliftedArray', yielding an 'UnliftedArray'. This simply
-- marks the array as frozen in place, so it should only be used when no further
-- modifications to the mutable array will be performed.
unsafeFreezeUnliftedArray
  :: MutableUnliftedArray s a
  -> ST s (UnliftedArray a)
unsafeFreezeUnliftedArray (MutableUnliftedArray ma)
  = UnliftedArray <$> BA.unsafeFreezeBoxArray ma
{-# inline unsafeFreezeUnliftedArray #-}

-- | Determines whether two 'MutableUnliftedArray' values are the same. This is
-- object/pointer identity, not based on the contents.
sameMutableUnliftedArray
  :: MutableUnliftedArray s a
  -> MutableUnliftedArray s a
  -> Bool
sameMutableUnliftedArray (MutableUnliftedArray ma1) (MutableUnliftedArray ma2)
  = BA.sameMutableBoxArray ma1 ma2
{-# inline sameMutableUnliftedArray #-}

-- | Copies the contents of an immutable array into a mutable array.
copyUnliftedArray
  :: MutableUnliftedArray s a -- ^ destination
  -> Int -- ^ offset into destination
  -> UnliftedArray a -- ^ source
  -> Int -- ^ offset into source
  -> Int -- ^ number of elements to copy
  -> ST s ()
{-# inline copyUnliftedArray #-}
copyUnliftedArray (MutableUnliftedArray dst) doff
  (UnliftedArray src) soff ln
  = BA.copyBoxArray dst doff src soff ln

-- | Copies the contents of one mutable array into another.
copyMutableUnliftedArray
  :: MutableUnliftedArray s a -- ^ destination
  -> Int -- ^ offset into destination
  -> MutableUnliftedArray s a -- ^ source
  -> Int -- ^ offset into source
  -> Int -- ^ number of elements to copy
  -> ST s ()
{-# inline copyMutableUnliftedArray #-}
copyMutableUnliftedArray
  (MutableUnliftedArray dst) doff
  (MutableUnliftedArray src) soff ln
  = BA.copyMutableBoxArray dst doff src soff ln

-- | Freezes a portion of a 'MutableUnliftedArray', yielding an 'UnliftedArray'.
-- This operation is safe, in that it copies the frozen portion, and the
-- existing mutable array may still be used afterward.
freezeUnliftedArray
  :: MutableUnliftedArray s a -- ^ source
  -> Int -- ^ offset
  -> Int -- ^ length
  -> ST s (UnliftedArray a)
freezeUnliftedArray (MutableUnliftedArray mary) off len =
  UnliftedArray <$> BA.freezeBoxArray mary off len
{-# inline freezeUnliftedArray #-}

-- | Thaws a portion of an 'UnliftedArray', yielding a 'MutableUnliftedArray'.
-- This copies the thawed portion, so mutations will not affect the original
-- array.
thawUnliftedArray
  :: UnliftedArray a -- ^ source
  -> Int -- ^ offset
  -> Int -- ^ length
  -> ST s (MutableUnliftedArray s a)
{-# inline thawUnliftedArray #-}
thawUnliftedArray (UnliftedArray ary) off len
  = MutableUnliftedArray <$> BA.thawBoxArray ary off len

-- | Execute a stateful computation and freeze the resulting array.
runUnliftedArray
  :: (forall s. ST s (MutableUnliftedArray s a))
  -> UnliftedArray a
{-# INLINE runUnliftedArray #-}
-- This is what we'd like to write, but GHC does not yet
-- produce properly unboxed code when we do
-- runUnliftedArray m = runST $ m >>= unsafeFreezeUnliftedArray
runUnliftedArray m = UnliftedArray $
  BA.runBoxArray $ m >>= \(MutableUnliftedArray ma) -> pure ma

-- | Creates a new 'MutableUnliftedArray'. This function is unsafe because it
-- initializes all elements of the array as pointers to the empty array. Attempting
-- to read one of these elements before writing to it is in effect an unsafe
-- coercion from @'UnliftedArray' a@ to the element type.
unsafeNewUnliftedArray
  :: Int -- ^ size
  -> ST s (MutableUnliftedArray s a)
{-# inline unsafeNewUnliftedArray #-}
unsafeNewUnliftedArray i = MutableUnliftedArray <$> BA.unsafeNewBoxArray i

-- | Creates a copy of a portion of an 'UnliftedArray'
cloneUnliftedArray
  :: UnliftedArray a -- ^ source
  -> Int -- ^ offset
  -> Int -- ^ length
  -> UnliftedArray a
{-# inline cloneUnliftedArray #-}
cloneUnliftedArray (UnliftedArray ary) off len
  = UnliftedArray (BA.cloneBoxArray ary off len)

-- | Creates a new 'MutableUnliftedArray' containing a copy of a portion of
-- another mutable array.
cloneMutableUnliftedArray
  :: MutableUnliftedArray s a -- ^ source
  -> Int -- ^ offset
  -> Int -- ^ length
  -> ST s (MutableUnliftedArray s a)
{-# inline cloneMutableUnliftedArray #-}
cloneMutableUnliftedArray (MutableUnliftedArray mary) off len
  = MutableUnliftedArray <$> BA.cloneMutableBoxArray mary off len

emptyUnliftedArray :: UnliftedArray a
emptyUnliftedArray = UnliftedArray BA.emptyBoxArray

singletonUnliftedArray :: PrimUnlifted a => a -> UnliftedArray a
{-# INLINE singletonUnliftedArray #-}
singletonUnliftedArray x = UnliftedArray $ BA.singletonBoxArray (toBox x)

foldrUnliftedArray :: forall a b. PrimUnlifted a => (a -> b -> b) -> b -> UnliftedArray a -> b
{-# INLINE foldrUnliftedArray #-}
foldrUnliftedArray f z (UnliftedArray arr)
  = BA.foldrBoxArray (\a b -> f (fromBox a) b) z arr

-- | Strict right-associated fold over the elements of an 'UnliftedArray.
{-# INLINE foldrUnliftedArray' #-}
foldrUnliftedArray' :: forall a b. PrimUnlifted a => (a -> b -> b) -> b -> UnliftedArray a -> b
foldrUnliftedArray' f z (UnliftedArray arr)
  = BA.foldrBoxArray' (\a b -> f (fromBox a) b) z arr

-- | Lazy left-associated fold over the elements of an 'UnliftedArray'.
{-# INLINE foldlUnliftedArray #-}
foldlUnliftedArray :: forall a b. PrimUnlifted a => (b -> a -> b) -> b -> UnliftedArray a -> b
foldlUnliftedArray f z (UnliftedArray arr)
  = BA.foldlBoxArray (\b a -> f b (fromBox a)) z arr

-- | Strict left-associated fold over the elements of an 'UnliftedArray'.
{-# INLINE foldlUnliftedArray' #-}
foldlUnliftedArray' :: forall a b. PrimUnlifted a => (b -> a -> b) -> b -> UnliftedArray a -> b
foldlUnliftedArray' f z (UnliftedArray arr)
  = BA.foldlBoxArray' (\b a -> f b (fromBox a)) z arr

-- | Strict effectful left-associated fold over the elements of an 'UnliftedArray'.
{-# INLINE foldlUnliftedArrayM' #-}
foldlUnliftedArrayM' :: (PrimUnlifted a, Monad m)
  => (b -> a -> m b) -> b -> UnliftedArray a -> m b
foldlUnliftedArrayM' f z (UnliftedArray arr)
  = BA.foldlBoxArrayM' (\b a -> f b (fromBox a)) z arr

-- | Effectfully traverse the elements of an 'UnliftedArray', discarding
-- the resulting values.
{-# INLINE traverseUnliftedArray_ #-}
traverseUnliftedArray_ :: (PrimUnlifted a, Applicative m)
  => (a -> m b) -> UnliftedArray a -> m ()
traverseUnliftedArray_ f (UnliftedArray arr)
  = BA.traverseBoxArray_ (f . fromBox) arr

-- | Effectful indexed traversal of the elements of an 'UnliftedArray',
-- discarding the resulting values.
{-# INLINE itraverseUnliftedArray_ #-}
itraverseUnliftedArray_ :: (PrimUnlifted a, Applicative m)
  => (Int -> a -> m b) -> UnliftedArray a -> m ()
itraverseUnliftedArray_ f (UnliftedArray arr)
  = BA.itraverseBoxArray_ (\i a -> f i (fromBox a)) arr

-- | Map over the elements of an 'UnliftedArray'.
{-# INLINE mapUnliftedArray #-}
mapUnliftedArray :: (PrimUnlifted a, PrimUnlifted b)
  => (a -> b)
  -> UnliftedArray a
  -> UnliftedArray b
mapUnliftedArray f (UnliftedArray arr) = UnliftedArray $
  BA.mapBoxArray (toBox . f . fromBox) arr

-- | Convert the unlifted array to a list.
{-# INLINE unliftedArrayToList #-}
unliftedArrayToList :: PrimUnlifted a => UnliftedArray a -> [a]
unliftedArrayToList xs = Exts.build (\c n -> foldrUnliftedArray c n xs)

unliftedArrayFromList :: PrimUnlifted a => [a] -> UnliftedArray a
{-# INLINE unliftedArrayFromList #-}
-- We use boxArrayFromListN rather than unliftedArrayFromListN to make the fmap
-- fuse with the list conversion. Otherwise everything is horrible.
unliftedArrayFromList xs = UnliftedArray $ BA.boxArrayFromListN (L.length xs) (fmap toBox xs)

unliftedArrayFromListN :: forall a. PrimUnlifted a => Int -> [a] -> UnliftedArray a
{-# INLINABLE unliftedArrayFromListN #-}
unliftedArrayFromListN len vs = UnliftedArray $ BA.boxArrayFromListN len (fmap toBox vs)

instance PrimUnlifted a => Exts.IsList (UnliftedArray a) where
  type Item (UnliftedArray a) = a
  fromList = unliftedArrayFromList
  fromListN = unliftedArrayFromListN
  toList = unliftedArrayToList

instance Semigroup (UnliftedArray a) where
  {-# INLINE (<>) #-}
  UnliftedArray a1 <> UnliftedArray a2
    = UnliftedArray (a1 <> a2)

instance Monoid (UnliftedArray a) where
  mempty = emptyUnliftedArray

instance (Show a, PrimUnlifted a) => Show (UnliftedArray a) where
  showsPrec p a = showParen (p > 10) $
    showString "fromListN " . shows (sizeofUnliftedArray a) . showString " "
      . shows (unliftedArrayToList a)

instance Eq (MutableUnliftedArray s a) where
  (==) = sameMutableUnliftedArray

instance (Eq a, PrimUnlifted a) => Eq (UnliftedArray a) where
  aa1 == aa2 = sizeofUnliftedArray aa1 == sizeofUnliftedArray aa2
            && loop (sizeofUnliftedArray aa1 - 1)
   where
   loop i
     | i < 0 = True
     | otherwise = indexUnliftedArray aa1 i == indexUnliftedArray aa2 i && loop (i-1)
