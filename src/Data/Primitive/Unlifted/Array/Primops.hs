{-# language MagicHash #-}
{-# language UnboxedTuples #-}
{-# language RoleAnnotations #-}
{-# language UnliftedNewtypes #-}
{-# language KindSignatures #-}
{-# language ScopedTypeVariables #-}

-- Oh what a mess this is! See UnsafeCoercions.md for an explanation
-- of the hodgepodge in this module.

-- |
-- Primitive types representing unlifted arrays and the
-- primops for manipulating them.
module Data.Primitive.Unlifted.Array.Primops
  ( -- * Types
    UnliftedArray#
  , MutableUnliftedArray#
    -- We don't export the newtype constructors because they're bogus and
    -- because there's basically no reason they'd ever be used. This module
    -- contains a wrapped version of every Array# primop.  Eventually, all this
    -- stuff will be supported by GHC.Prim using BoxedRep.

    -- * Operations
  , newUnliftedArray#
  , unsafeNewUnliftedArray#
  , emptyUnliftedArray#
  , sameMutableUnliftedArray#
  , readUnliftedArray#
  , writeUnliftedArray#
  , sizeofUnliftedArray#
  , sizeofMutableUnliftedArray#
  , indexUnliftedArray#
  , unsafeFreezeUnliftedArray#
  , unsafeThawUnliftedArray#
  , copyUnliftedArray#
  , copyMutableUnliftedArray#
  , cloneUnliftedArray#
  , cloneMutableUnliftedArray#
  , freezeUnliftedArray#
  , thawUnliftedArray#
  , casUnliftedArray#
  ) where

import GHC.Exts ( Int#, State#, ArrayArray#, MutableArrayArray#
                , TYPE, RuntimeRep (UnliftedRep), unsafeCoerce#)
import qualified GHC.Exts as Exts

unsafeCoerceUnlifted :: forall (a :: TYPE 'UnliftedRep) (b :: TYPE 'UnliftedRep). a -> b
{-# INLINE unsafeCoerceUnlifted #-}
unsafeCoerceUnlifted a = unsafeCoerce# a

unsafeCoerceUnliftedST :: forall s (a :: TYPE 'UnliftedRep) (b :: TYPE 'UnliftedRep). (# State# s, a #) -> (# State# s, b #)
{-# INLINE unsafeCoerceUnliftedST #-}
unsafeCoerceUnliftedST a = unsafeCoerce# a

newtype UnliftedArray# (a :: TYPE 'UnliftedRep) = UnliftedArray# ArrayArray#
type role UnliftedArray# representational

newtype MutableUnliftedArray# s (a :: TYPE 'UnliftedRep) = MutableUnliftedArray# (MutableArrayArray# s)
type role MutableUnliftedArray# nominal representational

newUnliftedArray# :: Int# -> a -> State# s -> (# State# s, MutableUnliftedArray# s a #)
newUnliftedArray# sz a s = unsafeCoerceUnliftedST (Exts.newArray# sz (unsafeCoerce# a) s)
{-# NOINLINE newUnliftedArray# #-}

-- | Create a 'MutableUnliftedArray#' whose entries contain some unspecified
-- static value. This may be more convenient than 'newUnliftedArray#' if there
-- is no value on hand with which to initialize the array. Each entry must be
-- initialized before being read and used. This condition is not checked.
unsafeNewUnliftedArray# :: Int# -> State# s -> (# State# s, MutableUnliftedArray# s a #)
unsafeNewUnliftedArray# sz s = case Exts.newArrayArray# sz s of
  (# s', mary #) -> (# s', MutableUnliftedArray# mary #)
{-# INLINE unsafeNewUnliftedArray# #-}

-- This represents a *statically allocated* value, preferably in a *read-only*
-- segment of memory.
--
-- Why do we bother to noDuplicate#? It generally doesn't much *matter* if
-- different threads have different global empty arrays. However, for
-- performance testing purposes, a user may well want to check whether the
-- empty arrays they expect to be the global ones really are. Such a test
-- is only possible if there's just *one* array to test against. The overhead
-- of the once-ever noDuplicate# call is sure to be trivial anyway.
empty_unlifted_array :: ULA a
empty_unlifted_array = ULA
  (Exts.runRW# $ \s ->
    case Exts.noDuplicate# s of { s' ->
    case unsafeNewUnliftedArray# 0# s' of { (# s'', mary #) ->
    case unsafeFreezeUnliftedArray# mary s'' of { (# _, ary #) ->
      ary }}})
{-# NOINLINE empty_unlifted_array #-}

data ULA a = ULA (UnliftedArray# a)

-- | Warning: Applying 'unsafeThawUnliftedArray#' to the array produced by
-- this function will make demons come out of your nose.
emptyUnliftedArray# :: (##) -> UnliftedArray# a
-- We make this primitive because it's the easiest way to get a
-- *shared* primitive unlifted array.
--
-- Why the stern warning above? GHC does not currently support resizing 'Array#',
-- and does not really meaningfully support *growing* arrays of any type. If,
-- however, that ever changes, growing the globally shared empty array would be
-- pretty disastrous.
emptyUnliftedArray# (##) = case empty_unlifted_array of
  ULA ary -> ary
{-# INLINE emptyUnliftedArray# #-}

sameMutableUnliftedArray# :: MutableUnliftedArray# s a -> MutableUnliftedArray# s a -> Int#
sameMutableUnliftedArray# (MutableUnliftedArray# ar1) (MutableUnliftedArray# ar2)
  = Exts.sameMutableArrayArray# ar1 ar2
{-# INLINE sameMutableUnliftedArray# #-}

readUnliftedArray# :: MutableUnliftedArray# s a -> Int# -> State# s -> (# State# s, a #)
readUnliftedArray# (MutableUnliftedArray# mary) i s
  = unsafeCoerceUnliftedST (Exts.readArrayArrayArray# mary i s)
{-# INLINE readUnliftedArray# #-}

writeUnliftedArray# :: MutableUnliftedArray# s a -> Int# -> a -> State# s -> State# s
writeUnliftedArray# (MutableUnliftedArray# mary) i a s
  = Exts.writeArrayArrayArray# mary i (unsafeCoerceUnlifted a) s
{-# INLINE writeUnliftedArray# #-}

sizeofUnliftedArray# :: UnliftedArray# a -> Int#
sizeofUnliftedArray# (UnliftedArray# ary) = Exts.sizeofArrayArray# ary
{-# INLINE sizeofUnliftedArray# #-}

sizeofMutableUnliftedArray# :: MutableUnliftedArray# s a -> Int#
sizeofMutableUnliftedArray# (MutableUnliftedArray# mary)
  = Exts.sizeofMutableArrayArray# mary
{-# INLINE sizeofMutableUnliftedArray# #-}

indexUnliftedArray# :: UnliftedArray# a -> Int# -> a
indexUnliftedArray# (UnliftedArray# ary) i
  = unsafeCoerceUnlifted (Exts.indexArrayArrayArray# ary i)
{-# INLINE indexUnliftedArray# #-}

unsafeFreezeUnliftedArray# :: MutableUnliftedArray# s a -> State# s -> (# State# s, UnliftedArray# a #)
unsafeFreezeUnliftedArray# (MutableUnliftedArray# mary) s
  = case Exts.unsafeFreezeArrayArray# mary s of
      (# s', ary #) -> (# s', UnliftedArray# ary #)
{-# INLINE unsafeFreezeUnliftedArray# #-}

unsafeThawUnliftedArray# :: UnliftedArray# a -> State# s -> (# State# s, MutableUnliftedArray# s a #)
unsafeThawUnliftedArray# (UnliftedArray# ary) s
  = case Exts.unsafeThawArray# (unsafeCoerceUnlifted ary) s of
     (# s', mary #) -> (# s', MutableUnliftedArray# (unsafeCoerceUnlifted mary) #)
{-# INLINE unsafeThawUnliftedArray# #-}

copyUnliftedArray# :: UnliftedArray# a -> Int# -> MutableUnliftedArray# s a -> Int# -> Int# -> State# s -> State# s
copyUnliftedArray# (UnliftedArray# ary) i1 (MutableUnliftedArray# mary) i2 n s
  = Exts.copyArrayArray# ary i1 mary i2 n s
{-# INLINE copyUnliftedArray# #-}

copyMutableUnliftedArray# :: MutableUnliftedArray# s a -> Int# -> MutableUnliftedArray# s a -> Int# -> Int# -> State# s -> State# s
copyMutableUnliftedArray# (MutableUnliftedArray# mary1) i1 (MutableUnliftedArray# mary2) i2 n s
  = Exts.copyMutableArrayArray# mary1 i1 mary2 i2 n s
{-# INLINE copyMutableUnliftedArray# #-}

cloneUnliftedArray# :: UnliftedArray# a -> Int# -> Int# -> UnliftedArray# a
cloneUnliftedArray# (UnliftedArray# ary) i n
  = UnliftedArray# (unsafeCoerceUnlifted (Exts.cloneArray# (unsafeCoerceUnlifted ary) i n))
{-# INLINE cloneUnliftedArray# #-}

cloneMutableUnliftedArray# :: MutableUnliftedArray# s a -> Int# -> Int# -> State# s
  -> (# State# s, MutableUnliftedArray# s a #)
cloneMutableUnliftedArray# (MutableUnliftedArray# mary) i n s
  = case Exts.cloneMutableArray# (unsafeCoerceUnlifted mary) i n s of
      (# s', mary' #) -> (# s', MutableUnliftedArray# (unsafeCoerceUnlifted mary') #)
{-# INLINE cloneMutableUnliftedArray# #-}

freezeUnliftedArray# :: MutableUnliftedArray# s a -> Int# -> Int# -> State# s -> (# State# s, UnliftedArray# a #)
freezeUnliftedArray# (MutableUnliftedArray# mary) i n s
  = case Exts.freezeArray# (unsafeCoerceUnlifted mary) i n s of
      (# s', ary #) -> (# s', UnliftedArray# (unsafeCoerceUnlifted ary) #)
{-# INLINE freezeUnliftedArray# #-}

thawUnliftedArray# :: UnliftedArray# a -> Int# -> Int# -> State# s -> (# State# s, MutableUnliftedArray# s a #)
thawUnliftedArray# (UnliftedArray# ary) i n s
  = case Exts.thawArray# (unsafeCoerceUnlifted ary) i n s of
      (# s', mary #) -> (# s', MutableUnliftedArray# (unsafeCoerceUnlifted mary) #)
{-# INLINE thawUnliftedArray# #-}

casUnliftedArray# :: MutableUnliftedArray# s a -> Int# -> a -> a -> State# s -> (# State# s, Int#, a #)
casUnliftedArray# (MutableUnliftedArray# mary) i x y s
  = unsafeCoerce# (Exts.casArray# (unsafeCoerceUnlifted mary) i (unsafeCoerce# x) (unsafeCoerce# y) s)
{-# NOINLINE casUnliftedArray# #-}
