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
module Data.Primitive.Unlifted.BoxArray.ST
  ( -- * Types
    BoxArray(..)
  , MutableBoxArray(..)
    -- * Operations
  , newBoxArray
  , unsafeNewBoxArray
  , sizeofBoxArray
  , sizeofMutableBoxArray
  , sameMutableBoxArray
  , writeBoxArray
  , readBoxArray
  , indexBoxArray
  , unsafeFreezeBoxArray
  , freezeBoxArray
  , thawBoxArray
  , setBoxArray
  , copyBoxArray
  , copyMutableBoxArray
  , cloneBoxArray
  , cloneMutableBoxArray
  , emptyBoxArray
  , singletonBoxArray
  , runBoxArray
    -- * List Conversion
  , boxArrayToList
  , boxArrayFromList
  , boxArrayFromListN
    -- * Folding
  , foldrBoxArray
  , foldrBoxArray'
  , foldlBoxArray
  , foldlBoxArray'
  , foldlBoxArrayM'
    -- * Traversals
  , traverseBoxArray_
  , itraverseBoxArray_
    -- * Mapping
  , mapBoxArray
  ) where

--import Control.Monad.Primitive (primitive,primitive_)
import Data.Primitive.Unlifted.Class (PrimUnlifted (..))
import Data.Primitive.Unlifted.Array.Base
import Data.Primitive.Unlifted.Box (Box (..))
import GHC.Exts (Int(I#),State#)
import GHC.ST as ST (ST (..))

import qualified Data.List as L
import qualified GHC.Exts as Exts

-- We copy these from Control.Monad.Primitive purely to make
-- the Core easier to read. Using these knocks out the PrimState-related
-- coercions.
primitive :: (State# s -> (# State# s, a #)) -> ST s a
primitive m = ST m
{-# INLINE primitive #-}

primitive_ :: (State# s -> State# s) -> ST s ()
primitive_ m = ST (\s -> (# m s, () #))
{-# INLINE primitive_ #-}

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

-- | Creates a new 'MutableBoxArray' with the specified value as initial
-- contents.
newBoxArray
  :: Int -- ^ size
  -> Box a -- ^ initial value
  -> ST s (MutableBoxArray s a)
newBoxArray (I# len) (Box# v) = primitive $ \s -> case newUnliftedArray# len v s of
  (# s', ma #) -> (# s', MutableBoxArray ma #)
{-# inline newBoxArray #-}

setBoxArray
  :: MutableBoxArray s a -- ^ destination
  -> Box a -- ^ value to fill with
  -> Int -- ^ offset
  -> Int -- ^ length
  -> ST s ()
{-# inline setBoxArray #-}
setBoxArray !mua !v off len = loop (len + off - 1)
 where
 loop i
   | i < off = pure ()
   | otherwise = writeBoxArray mua i v *> loop (i-1)

-- | Yields the length of an 'BoxArray'.
sizeofBoxArray :: BoxArray e -> Int
{-# inline sizeofBoxArray #-}
sizeofBoxArray (BoxArray ar) = I# (sizeofUnliftedArray# ar)

-- | Yields the length of a 'MutableBoxArray'.
sizeofMutableBoxArray :: MutableBoxArray s e -> Int
{-# inline sizeofMutableBoxArray #-}
sizeofMutableBoxArray (MutableBoxArray maa#)
  = I# (sizeofMutableUnliftedArray# maa#)

writeBoxArray ::
     MutableBoxArray s a
  -> Int
  -> Box a
  -> ST s ()
{-# inline writeBoxArray #-}
writeBoxArray (MutableBoxArray arr) (I# ix) (Box# a) =
  primitive_ (writeUnliftedArray# arr ix a)

readBoxArray ::
     MutableBoxArray s a
  -> Int
  -> ST s (Box a)
{-# inline readBoxArray #-}
readBoxArray (MutableBoxArray arr) (I# ix) =
  primitive $ \s -> case readUnliftedArray# arr ix s of
    (# s', a #) -> (# s', Box# a #)

indexBoxArray :: BoxArray a
  -> Int
  -> Box a
{-# inline indexBoxArray #-}
indexBoxArray (BoxArray arr) (I# ix) =
  Box# (indexUnliftedArray# arr ix)

-- | Freezes a 'MutableBoxArray', yielding an 'BoxArray'. This simply
-- marks the array as frozen in place, so it should only be used when no further
-- modifications to the mutable array will be performed.
unsafeFreezeBoxArray
  :: MutableBoxArray s a
  -> ST s (BoxArray a)
unsafeFreezeBoxArray (MutableBoxArray maa#)
  = primitive $ \s -> case unsafeFreezeUnliftedArray# maa# s of
      (# s', aa# #) -> (# s', BoxArray aa# #)
{-# inline unsafeFreezeBoxArray #-}

-- | Determines whether two 'MutableBoxArray' values are the same. This is
-- object/pointer identity, not based on the contents.
sameMutableBoxArray
  :: MutableBoxArray s a
  -> MutableBoxArray s a
  -> Bool
sameMutableBoxArray (MutableBoxArray maa1#) (MutableBoxArray maa2#)
  = Exts.isTrue# (sameMutableUnliftedArray# maa1# maa2#)
{-# inline sameMutableBoxArray #-}

-- | Copies the contents of an immutable array into a mutable array.
copyBoxArray
  :: MutableBoxArray s a -- ^ destination
  -> Int -- ^ offset into destination
  -> BoxArray a -- ^ source
  -> Int -- ^ offset into source
  -> Int -- ^ number of elements to copy
  -> ST s ()
{-# inline copyBoxArray #-}
copyBoxArray
  (MutableBoxArray dst) (I# doff)
  (BoxArray src) (I# soff) (I# ln) =
    primitive_ $ copyUnliftedArray# src soff dst doff ln

-- | Copies the contents of one mutable array into another.
copyMutableBoxArray
  :: MutableBoxArray s a -- ^ destination
  -> Int -- ^ offset into destination
  -> MutableBoxArray s a -- ^ source
  -> Int -- ^ offset into source
  -> Int -- ^ number of elements to copy
  -> ST s ()
{-# inline copyMutableBoxArray #-}
copyMutableBoxArray
  (MutableBoxArray dst) (I# doff)
  (MutableBoxArray src) (I# soff) (I# ln) =
    primitive_ $ copyMutableUnliftedArray# src soff dst doff ln

-- | Freezes a portion of a 'MutableBoxArray', yielding an 'BoxArray'.
-- This operation is safe, in that it copies the frozen portion, and the
-- existing mutable array may still be used afterward.
freezeBoxArray
  :: MutableBoxArray s a -- ^ source
  -> Int -- ^ offset
  -> Int -- ^ length
  -> ST s (BoxArray a)
freezeBoxArray (MutableBoxArray mary) (I# off) (I# len) =
    primitive $ \s -> case freezeUnliftedArray# mary off len s of
      (# s', ary #) -> (# s', BoxArray ary #)
{-# inline freezeBoxArray #-}

-- | Thaws a portion of an 'BoxArray', yielding a 'MutableBoxArray'.
-- This copies the thawed portion, so mutations will not affect the original
-- array.
thawBoxArray
  :: BoxArray a -- ^ source
  -> Int -- ^ offset
  -> Int -- ^ length
  -> ST s (MutableBoxArray s a)
{-# inline thawBoxArray #-}
thawBoxArray (BoxArray ary) (I# off) (I# len) =
    primitive $ \s -> case thawUnliftedArray# ary off len s of
      (# s', mary #) -> (# s', MutableBoxArray mary #)

-- | Execute a stateful computation and freeze the resulting array.
runBoxArray
  :: (forall s. ST s (MutableBoxArray s a))
  -> BoxArray a
{-# INLINE runBoxArray #-}
-- This is what we'd like to write, but GHC does not yet
-- produce properly unboxed code when we do
-- runBoxArray m = runST $ m >>= unsafeFreezeBoxArray
runBoxArray m = BoxArray (runUnliftedArray# m)

runUnliftedArray#
  :: (forall s. ST s (MutableBoxArray s a))
  -> UnliftedArray# a
runUnliftedArray# m = case Exts.runRW# $ \s ->
  case unST m s of { (# s', MutableBoxArray mary# #) ->
  unsafeFreezeUnliftedArray# mary# s'} of (# _, ary# #) -> ary#
{-# INLINE runUnliftedArray# #-}

unST :: ST s a -> State# s -> (# State# s, a #)
unST (ST.ST f) = f

unsafeCreateBoxArray
  :: Int
  -> (forall s. MutableBoxArray s a -> ST s ())
  -> BoxArray a
unsafeCreateBoxArray !n f = runBoxArray $ do
  mary <- unsafeNewBoxArray n
  f mary
  pure mary

-- | Creates a new 'MutableBoxArray'. This function is unsafe because it
-- initializes all elements of the array as pointers to the empty array. Attempting
-- to read one of these elements before writing to it is in effect an unsafe
-- coercion from @'BoxArray' a@ to the element type.
unsafeNewBoxArray
  :: Int -- ^ size
  -> ST s (MutableBoxArray s a)
{-# inline unsafeNewBoxArray #-}
unsafeNewBoxArray (I# i) = primitive $ \s -> case unsafeNewUnliftedArray# i s of
  (# s', ma #) -> (# s', MutableBoxArray ma #)


-- | Creates a copy of a portion of an 'BoxArray'
cloneBoxArray
  :: BoxArray a -- ^ source
  -> Int -- ^ offset
  -> Int -- ^ length
  -> BoxArray a
{-# inline cloneBoxArray #-}
cloneBoxArray (BoxArray ary) (I# off) (I# len)
  = BoxArray (cloneUnliftedArray# ary off len)

-- | Creates a new 'MutableBoxArray' containing a copy of a portion of
-- another mutable array.
cloneMutableBoxArray
  :: MutableBoxArray s a -- ^ source
  -> Int -- ^ offset
  -> Int -- ^ length
  -> ST s (MutableBoxArray s a)
{-# inline cloneMutableBoxArray #-}
cloneMutableBoxArray (MutableBoxArray mary) (I# off) (I# len)
  = primitive $ \s -> case cloneMutableUnliftedArray# mary off len s of
      (# s', mary' #) -> (# s', MutableBoxArray mary' #)

emptyBoxArray :: BoxArray a
emptyBoxArray = BoxArray (emptyUnliftedArray# Exts.void#)

singletonBoxArray :: Box a -> BoxArray a
{-# INLINE singletonBoxArray #-}
singletonBoxArray x = runBoxArray $ newBoxArray 1 x

concatBoxArray :: BoxArray a -> BoxArray a -> BoxArray a
{-# INLINE concatBoxArray #-}
concatBoxArray (BoxArray a1) (BoxArray a2)
  = BoxArray (concatUnliftedArray# a1 a2)

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

foldrBoxArray :: forall a b. (Box a -> b -> b) -> b -> BoxArray a -> b
{-# INLINE foldrBoxArray #-}
foldrBoxArray f z arr = go 0
  where
    !sz = sizeofBoxArray arr
    go !i
      | sz > i = f (indexBoxArray arr i) (go (i+1))
      | otherwise = z

-- | Strict right-associated fold over the elements of an 'BoxArray.
{-# INLINE foldrBoxArray' #-}
foldrBoxArray' :: forall a b. (Box a -> b -> b) -> b -> BoxArray a -> b
foldrBoxArray' f z0 arr = go (sizeofBoxArray arr - 1) z0
  where
    go !i !acc
      | i < 0 = acc
      | otherwise = go (i - 1) (f (indexBoxArray arr i) acc)

-- | Lazy left-associated fold over the elements of an 'BoxArray'.
{-# INLINE foldlBoxArray #-}
foldlBoxArray :: forall a b. (b -> Box a -> b) -> b -> BoxArray a -> b
foldlBoxArray f z arr = go (sizeofBoxArray arr - 1)
  where
    go !i
      | i < 0 = z
      | otherwise = f (go (i - 1)) (indexBoxArray arr i)

-- | Strict left-associated fold over the elements of an 'BoxArray'.
{-# INLINE foldlBoxArray' #-}
foldlBoxArray' :: forall a b. (b -> Box a -> b) -> b -> BoxArray a -> b
foldlBoxArray' f z0 arr = go 0 z0
  where
    !sz = sizeofBoxArray arr
    go !i !acc
      | i < sz = go (i + 1) (f acc (indexBoxArray arr i))
      | otherwise = acc

-- | Strict effectful left-associated fold over the elements of an 'BoxArray'.
{-# INLINE foldlBoxArrayM' #-}
foldlBoxArrayM' :: Monad m
  => (b -> Box a -> m b) -> b -> BoxArray a -> m b
foldlBoxArrayM' f z0 arr = go 0 z0
  where
    !sz = sizeofBoxArray arr
    go !i !acc
      | i < sz = f acc (indexBoxArray arr i) >>= go (i + 1) 
      | otherwise = pure acc

-- | Effectfully traverse the elements of an 'BoxArray', discarding
-- the resulting values.
{-# INLINE traverseBoxArray_ #-}
traverseBoxArray_ :: Applicative m
  => (Box a -> m b) -> BoxArray a -> m ()
traverseBoxArray_ f arr = go 0
  where
    !sz = sizeofBoxArray arr
    go !i
      | i < sz = f (indexBoxArray arr i) *> go (i + 1) 
      | otherwise = pure ()

-- | Effectful indexed traversal of the elements of an 'BoxArray',
-- discarding the resulting values.
{-# INLINE itraverseBoxArray_ #-}
itraverseBoxArray_ :: Applicative m
  => (Int -> Box a -> m b) -> BoxArray a -> m ()
itraverseBoxArray_ f arr = go 0
  where
    !sz = sizeofBoxArray arr
    go !i
      | i < sz = f i (indexBoxArray arr i) *> go (i + 1) 
      | otherwise = pure ()

-- | Map over the elements of an 'BoxArray'.
{-# INLINE mapBoxArray #-}
mapBoxArray ::
     (Box a -> Box b)
  -> BoxArray a
  -> BoxArray b
mapBoxArray f arr = unsafeCreateBoxArray sz $ \marr -> do
  let go !ix = if ix < sz
        then do
          let b = f (indexBoxArray arr ix)
          writeBoxArray marr ix b
          go (ix + 1)
        else return ()
  go 0
  where
  !sz = sizeofBoxArray arr

-- | Convert the unlifted array to a list.
{-# INLINE boxArrayToList #-}
boxArrayToList :: BoxArray a -> [Box a]
boxArrayToList xs = Exts.build (\c n -> foldrBoxArray c n xs)

boxArrayFromList :: [Box a] -> BoxArray a
boxArrayFromList xs = boxArrayFromListN (L.length xs) xs

boxArrayFromListN :: forall a. Int -> [Box a] -> BoxArray a
{-# INLINE boxArrayFromListN #-}
boxArrayFromListN len vs = unsafeCreateBoxArray len run where
  run :: forall s. MutableBoxArray s a -> ST s ()
  run arr = foldr go stop vs 0
    where
      go !a r ix
        | ix < len
        = writeBoxArray arr ix a *> r (ix + 1)
        | otherwise
        = boxArrayTooLong
      stop ix
        -- The size check is mandatory since failure to initialize all elements
        -- introduces the possibility of a segfault happening when someone attempts
        -- to read the unitialized element. See the docs for unsafeNewBoxArray.
        | ix == len
        = pure ()
        | otherwise
        = boxArrayTooShort

boxArrayTooLong :: ST s ()
boxArrayTooLong = die "boxArrayFromListN" "list length greater than specified size"

boxArrayTooShort :: ST s ()
boxArrayTooShort = die "boxArrayFromListN" "list length less than specified size"

instance Exts.IsList (BoxArray a) where
  type Item (BoxArray a) = Box a
  fromList = boxArrayFromList
  fromListN = boxArrayFromListN
  toList = boxArrayToList

instance Semigroup (BoxArray a) where
  (<>) = concatBoxArray

instance Monoid (BoxArray a) where
  mempty = emptyBoxArray

instance Eq (MutableBoxArray s a) where
  (==) = sameMutableBoxArray

die :: String -> String -> a
die fun problem = error $ "Data.Primitive.Unlifted.BoxArray." ++ fun ++ ": " ++ problem

{-
-- TODO: Work out Show, Eq, and Ord instances for appropriate types.

instance (Show a, ???) => Show (BoxArray a) where
  showsPrec p a = showParen (p > 10) $
    showString "fromListN " . shows (sizeofBoxArray a) . showString " "
      . shows (unliftedArrayToList a)

instance ??? => Eq (BoxArray a) where
  aa1 == aa2 = sizeofBoxArray aa1 == sizeofBoxArray aa2
            && loop (sizeofBoxArray aa1 - 1)
   where
   loop i
     | i < 0 = True
     | otherwise = indexBoxArray aa1 i == indexBoxArray aa2 i && loop (i-1)
-}
