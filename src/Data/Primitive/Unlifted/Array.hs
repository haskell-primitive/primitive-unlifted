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
module Data.Primitive.Unlifted.Array
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

import Control.Monad.Primitive (PrimMonad,PrimState,primitive,primitive_)
import Control.Monad.ST (ST)
import Data.Primitive.Unlifted.Class (PrimUnlifted (..))
import Data.Primitive.Unlifted.Array.Base
import GHC.Exts (Int(I#),State#)
import qualified GHC.ST as ST

import qualified Data.List as L
import qualified GHC.Exts as Exts

data UnliftedArray a
  = UnliftedArray (UnliftedArray# (Unlifted a))
type role UnliftedArray nominal

data MutableUnliftedArray s a
  = MutableUnliftedArray (MutableUnliftedArray# s (Unlifted a))
type role MutableUnliftedArray nominal nominal

instance PrimUnlifted (UnliftedArray a) where
  type Unlifted (UnliftedArray a) = UnliftedArray# (Unlifted a)
  toUnlifted# (UnliftedArray a) = a
  fromUnlifted# x = UnliftedArray x

instance PrimUnlifted (MutableUnliftedArray s a) where
  type Unlifted (MutableUnliftedArray s a) = MutableUnliftedArray# s (Unlifted a)
  toUnlifted# (MutableUnliftedArray a) = a
  fromUnlifted# x = MutableUnliftedArray x

-- | Creates a new 'MutableUnliftedArray' with the specified value as initial
-- contents.
newUnliftedArray
  :: (PrimMonad m, PrimUnlifted a)
  => Int -- ^ size
  -> a -- ^ initial value
  -> m (MutableUnliftedArray (PrimState m) a)
newUnliftedArray (I# len) v = primitive $ \s -> case newUnliftedArray# len (toUnlifted# v) s of
  (# s', ma #) -> (# s', MutableUnliftedArray ma #)
{-# inline newUnliftedArray #-}

setUnliftedArray
  :: (PrimMonad m, PrimUnlifted a)
  => MutableUnliftedArray (PrimState m) a -- ^ destination
  -> a -- ^ value to fill with
  -> Int -- ^ offset
  -> Int -- ^ length
  -> m ()
{-# inline setUnliftedArray #-}
setUnliftedArray mua v off len = loop (len + off - 1)
 where
 loop i
   | i < off = pure ()
   | otherwise = writeUnliftedArray mua i v *> loop (i-1)

-- | Yields the length of an 'UnliftedArray'.
sizeofUnliftedArray :: UnliftedArray e -> Int
{-# inline sizeofUnliftedArray #-}
sizeofUnliftedArray (UnliftedArray ar) = I# (sizeofUnliftedArray# ar)

-- | Yields the length of a 'MutableUnliftedArray'.
sizeofMutableUnliftedArray :: MutableUnliftedArray s e -> Int
{-# inline sizeofMutableUnliftedArray #-}
sizeofMutableUnliftedArray (MutableUnliftedArray maa#)
  = I# (sizeofMutableUnliftedArray# maa#)

writeUnliftedArray :: (PrimMonad m, PrimUnlifted a)
  => MutableUnliftedArray (PrimState m) a
  -> Int
  -> a
  -> m ()
{-# inline writeUnliftedArray #-}
writeUnliftedArray (MutableUnliftedArray arr) (I# ix) a =
  primitive_ (writeUnliftedArray# arr ix (toUnlifted# a))

readUnliftedArray :: (PrimMonad m, PrimUnlifted a)
  => MutableUnliftedArray (PrimState m) a
  -> Int
  -> m a
{-# inline readUnliftedArray #-}
readUnliftedArray (MutableUnliftedArray arr) (I# ix) =
  primitive $ \s -> case readUnliftedArray# arr ix s of
    (# s', a #) -> (# s', fromUnlifted# a #)

indexUnliftedArray :: PrimUnlifted a
  => UnliftedArray a
  -> Int
  -> a
{-# inline indexUnliftedArray #-}
indexUnliftedArray (UnliftedArray arr) (I# ix) =
  fromUnlifted# (indexUnliftedArray# arr ix)

-- | Freezes a 'MutableUnliftedArray', yielding an 'UnliftedArray'. This simply
-- marks the array as frozen in place, so it should only be used when no further
-- modifications to the mutable array will be performed.
unsafeFreezeUnliftedArray
  :: PrimMonad m
  => MutableUnliftedArray (PrimState m) a
  -> m (UnliftedArray a)
unsafeFreezeUnliftedArray (MutableUnliftedArray maa#)
  = primitive $ \s -> case unsafeFreezeUnliftedArray# maa# s of
      (# s', aa# #) -> (# s', UnliftedArray aa# #)
{-# inline unsafeFreezeUnliftedArray #-}

-- | Determines whether two 'MutableUnliftedArray' values are the same. This is
-- object/pointer identity, not based on the contents.
sameMutableUnliftedArray
  :: MutableUnliftedArray s a
  -> MutableUnliftedArray s a
  -> Bool
sameMutableUnliftedArray (MutableUnliftedArray maa1#) (MutableUnliftedArray maa2#)
  = Exts.isTrue# (sameMutableUnliftedArray# maa1# maa2#)
{-# inline sameMutableUnliftedArray #-}

-- | Copies the contents of an immutable array into a mutable array.
copyUnliftedArray
  :: (PrimMonad m)
  => MutableUnliftedArray (PrimState m) a -- ^ destination
  -> Int -- ^ offset into destination
  -> UnliftedArray a -- ^ source
  -> Int -- ^ offset into source
  -> Int -- ^ number of elements to copy
  -> m ()
{-# inline copyUnliftedArray #-}
copyUnliftedArray
  (MutableUnliftedArray dst) (I# doff)
  (UnliftedArray src) (I# soff) (I# ln) =
    primitive_ $ copyUnliftedArray# src soff dst doff ln

-- | Copies the contents of one mutable array into another.
copyMutableUnliftedArray
  :: (PrimMonad m)
  => MutableUnliftedArray (PrimState m) a -- ^ destination
  -> Int -- ^ offset into destination
  -> MutableUnliftedArray (PrimState m) a -- ^ source
  -> Int -- ^ offset into source
  -> Int -- ^ number of elements to copy
  -> m ()
{-# inline copyMutableUnliftedArray #-}
copyMutableUnliftedArray
  (MutableUnliftedArray dst) (I# doff)
  (MutableUnliftedArray src) (I# soff) (I# ln) =
    primitive_ $ copyMutableUnliftedArray# src soff dst doff ln

-- | Freezes a portion of a 'MutableUnliftedArray', yielding an 'UnliftedArray'.
-- This operation is safe, in that it copies the frozen portion, and the
-- existing mutable array may still be used afterward.
freezeUnliftedArray
  :: (PrimMonad m)
  => MutableUnliftedArray (PrimState m) a -- ^ source
  -> Int -- ^ offset
  -> Int -- ^ length
  -> m (UnliftedArray a)
freezeUnliftedArray (MutableUnliftedArray mary) (I# off) (I# len) =
    primitive $ \s -> case freezeUnliftedArray# mary off len s of
      (# s', ary #) -> (# s', UnliftedArray ary #)
{-# inline freezeUnliftedArray #-}

-- | Thaws a portion of an 'UnliftedArray', yielding a 'MutableUnliftedArray'.
-- This copies the thawed portion, so mutations will not affect the original
-- array.
thawUnliftedArray
  :: (PrimMonad m)
  => UnliftedArray a -- ^ source
  -> Int -- ^ offset
  -> Int -- ^ length
  -> m (MutableUnliftedArray (PrimState m) a)
{-# inline thawUnliftedArray #-}
thawUnliftedArray (UnliftedArray ary) (I# off) (I# len) =
    primitive $ \s -> case thawUnliftedArray# ary off len s of
      (# s', mary #) -> (# s', MutableUnliftedArray mary #)

-- | Execute a stateful computation and freeze the resulting array.
runUnliftedArray
  :: (forall s. ST s (MutableUnliftedArray s a))
  -> UnliftedArray a
{-# INLINE runUnliftedArray #-}
-- This is what we'd like to write, but GHC does not yet
-- produce properly unboxed code when we do
-- runUnliftedArray m = runST $ m >>= unsafeFreezeUnliftedArray
runUnliftedArray m = UnliftedArray (runUnliftedArray# m)

runUnliftedArray#
  :: (forall s. ST s (MutableUnliftedArray s a))
  -> UnliftedArray# (Unlifted a)
runUnliftedArray# m = case Exts.runRW# $ \s ->
  case unST m s of { (# s', MutableUnliftedArray mary# #) ->
  unsafeFreezeUnliftedArray# mary# s'} of (# _, ary# #) -> ary#
{-# INLINE runUnliftedArray# #-}

unST :: ST s a -> State# s -> (# State# s, a #)
unST (ST.ST f) = f

unsafeCreateUnliftedArray
  :: Int
  -> (forall s. MutableUnliftedArray s a -> ST s ())
  -> UnliftedArray a
unsafeCreateUnliftedArray !n f = runUnliftedArray $ do
  mary <- unsafeNewUnliftedArray n
  f mary
  pure mary

-- | Creates a new 'MutableUnliftedArray'. This function is unsafe because it
-- initializes all elements of the array as pointers to the empty array. Attempting
-- to read one of these elements before writing to it is in effect an unsafe
-- coercion from @'UnliftedArray' a@ to the element type.
unsafeNewUnliftedArray
  :: (PrimMonad m)
  => Int -- ^ size
  -> m (MutableUnliftedArray (PrimState m) a)
{-# inline unsafeNewUnliftedArray #-}
unsafeNewUnliftedArray (I# i) = primitive $ \s -> case unsafeNewUnliftedArray# i s of
  (# s', ma #) -> (# s', MutableUnliftedArray ma #)


-- | Creates a copy of a portion of an 'UnliftedArray'
cloneUnliftedArray
  :: UnliftedArray a -- ^ source
  -> Int -- ^ offset
  -> Int -- ^ length
  -> UnliftedArray a
{-# inline cloneUnliftedArray #-}
cloneUnliftedArray (UnliftedArray ary) (I# off) (I# len)
  = UnliftedArray (cloneUnliftedArray# ary off len)

-- | Creates a new 'MutableUnliftedArray' containing a copy of a portion of
-- another mutable array.
cloneMutableUnliftedArray
  :: (PrimMonad m)
  => MutableUnliftedArray (PrimState m) a -- ^ source
  -> Int -- ^ offset
  -> Int -- ^ length
  -> m (MutableUnliftedArray (PrimState m) a)
{-# inline cloneMutableUnliftedArray #-}
cloneMutableUnliftedArray (MutableUnliftedArray mary) (I# off) (I# len)
  = primitive $ \s -> case cloneMutableUnliftedArray# mary off len s of
      (# s', mary' #) -> (# s', MutableUnliftedArray mary' #)

emptyUnliftedArray :: UnliftedArray a
emptyUnliftedArray = UnliftedArray (emptyUnliftedArray# Exts.void#)

singletonUnliftedArray :: PrimUnlifted a => a -> UnliftedArray a
{-# INLINE singletonUnliftedArray #-}
singletonUnliftedArray x = runUnliftedArray $ newUnliftedArray 1 x

concatUnliftedArray :: UnliftedArray a -> UnliftedArray a -> UnliftedArray a
{-# INLINE concatUnliftedArray #-}
concatUnliftedArray (UnliftedArray a1) (UnliftedArray a2)
  = UnliftedArray (concatUnliftedArray# a1 a2)

-- This junk is to make sure we unbox properly. Inlining this doesn't seem
-- likely to be much of a win ever, and could potentially lead to reboxing,
-- so we NOINLINE. It would be nice to find a prettier way to do this.
concatUnliftedArray# :: UnliftedArray# a -> UnliftedArray# a -> UnliftedArray# a
{-# NOINLINE concatUnliftedArray# #-}
concatUnliftedArray# a1 a2 =
  let !sza1 = sizeofUnliftedArray# a1
  in
    if Exts.isTrue# (sza1 Exts.==# 0#)
    then a2
    else
      let !sza2 = sizeofUnliftedArray# a2
      in
        if Exts.isTrue# (sza2 Exts.==# 0#)
        then a1
        else Exts.runRW# $ \s ->
          case unsafeNewUnliftedArray# (sza1 Exts.+# sza2) s of { (# s', ma #) ->
          case copyUnliftedArray# a1 0# ma 0# sza1 s' of { s'' ->
          case copyUnliftedArray# a2 0# ma sza1 sza2 s'' of { s''' ->
          case unsafeFreezeUnliftedArray# ma s''' of
            (# _, ar #) -> ar}}}

foldrUnliftedArray :: forall a b. PrimUnlifted a => (a -> b -> b) -> b -> UnliftedArray a -> b
{-# INLINE foldrUnliftedArray #-}
foldrUnliftedArray f z arr = go 0
  where
    !sz = sizeofUnliftedArray arr
    go !i
      | sz > i = f (indexUnliftedArray arr i) (go (i+1))
      | otherwise = z

-- | Strict right-associated fold over the elements of an 'UnliftedArray.
{-# INLINE foldrUnliftedArray' #-}
foldrUnliftedArray' :: forall a b. PrimUnlifted a => (a -> b -> b) -> b -> UnliftedArray a -> b
foldrUnliftedArray' f z0 arr = go (sizeofUnliftedArray arr - 1) z0
  where
    go !i !acc
      | i < 0 = acc
      | otherwise = go (i - 1) (f (indexUnliftedArray arr i) acc)

-- | Lazy left-associated fold over the elements of an 'UnliftedArray'.
{-# INLINE foldlUnliftedArray #-}
foldlUnliftedArray :: forall a b. PrimUnlifted a => (b -> a -> b) -> b -> UnliftedArray a -> b
foldlUnliftedArray f z arr = go (sizeofUnliftedArray arr - 1)
  where
    go !i
      | i < 0 = z
      | otherwise = f (go (i - 1)) (indexUnliftedArray arr i)

-- | Strict left-associated fold over the elements of an 'UnliftedArray'.
{-# INLINE foldlUnliftedArray' #-}
foldlUnliftedArray' :: forall a b. PrimUnlifted a => (b -> a -> b) -> b -> UnliftedArray a -> b
foldlUnliftedArray' f z0 arr = go 0 z0
  where
    !sz = sizeofUnliftedArray arr
    go !i !acc
      | i < sz = go (i + 1) (f acc (indexUnliftedArray arr i))
      | otherwise = acc

-- | Strict effectful left-associated fold over the elements of an 'UnliftedArray'.
{-# INLINE foldlUnliftedArrayM' #-}
foldlUnliftedArrayM' :: (PrimUnlifted a, Monad m)
  => (b -> a -> m b) -> b -> UnliftedArray a -> m b
foldlUnliftedArrayM' f z0 arr = go 0 z0
  where
    !sz = sizeofUnliftedArray arr
    go !i !acc
      | i < sz = f acc (indexUnliftedArray arr i) >>= go (i + 1) 
      | otherwise = pure acc

-- | Effectfully traverse the elements of an 'UnliftedArray', discarding
-- the resulting values.
{-# INLINE traverseUnliftedArray_ #-}
traverseUnliftedArray_ :: (PrimUnlifted a, Applicative m)
  => (a -> m b) -> UnliftedArray a -> m ()
traverseUnliftedArray_ f arr = go 0
  where
    !sz = sizeofUnliftedArray arr
    go !i
      | i < sz = f (indexUnliftedArray arr i) *> go (i + 1) 
      | otherwise = pure ()

-- | Effectful indexed traversal of the elements of an 'UnliftedArray',
-- discarding the resulting values.
{-# INLINE itraverseUnliftedArray_ #-}
itraverseUnliftedArray_ :: (PrimUnlifted a, Applicative m)
  => (Int -> a -> m b) -> UnliftedArray a -> m ()
itraverseUnliftedArray_ f arr = go 0
  where
    !sz = sizeofUnliftedArray arr
    go !i
      | i < sz = f i (indexUnliftedArray arr i) *> go (i + 1) 
      | otherwise = pure ()

-- | Map over the elements of an 'UnliftedArray'.
{-# INLINE mapUnliftedArray #-}
mapUnliftedArray :: (PrimUnlifted a, PrimUnlifted b)
  => (a -> b)
  -> UnliftedArray a
  -> UnliftedArray b
mapUnliftedArray f arr = unsafeCreateUnliftedArray sz $ \marr -> do
  let go !ix = if ix < sz
        then do
          let b = f (indexUnliftedArray arr ix)
          writeUnliftedArray marr ix b
          go (ix + 1)
        else return ()
  go 0
  where
  !sz = sizeofUnliftedArray arr

-- | Convert the unlifted array to a list.
{-# INLINE unliftedArrayToList #-}
unliftedArrayToList :: PrimUnlifted a => UnliftedArray a -> [a]
unliftedArrayToList xs = Exts.build (\c n -> foldrUnliftedArray c n xs)

unliftedArrayFromList :: PrimUnlifted a => [a] -> UnliftedArray a
unliftedArrayFromList xs = unliftedArrayFromListN (L.length xs) xs

unliftedArrayFromListN :: forall a. PrimUnlifted a => Int -> [a] -> UnliftedArray a
unliftedArrayFromListN len vs = unsafeCreateUnliftedArray len run where
  run :: forall s. MutableUnliftedArray s a -> ST s ()
  run arr = do
    let go :: [a] -> Int -> ST s ()
        go [] !ix = if ix == len
          -- The size check is mandatory since failure to initialize all elements
          -- introduces the possibility of a segfault happening when someone attempts
          -- to read the unitialized element. See the docs for unsafeNewUnliftedArray.
          then return ()
          else die "unliftedArrayFromListN" "list length less than specified size"
        go (a : as) !ix = if ix < len
          then do
            writeUnliftedArray arr ix a
            go as (ix + 1)
          else die "unliftedArrayFromListN" "list length greater than specified size"
    go vs 0

instance PrimUnlifted a => Exts.IsList (UnliftedArray a) where
  type Item (UnliftedArray a) = a
  fromList = unliftedArrayFromList
  fromListN = unliftedArrayFromListN
  toList = unliftedArrayToList

instance Semigroup (UnliftedArray a) where
  (<>) = concatUnliftedArray

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

die :: String -> String -> a
die fun problem = error $ "Data.Primitive.UnliftedArray." ++ fun ++ ": " ++ problem
