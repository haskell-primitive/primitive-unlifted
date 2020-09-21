{-# language MagicHash #-}
{-# language UnboxedTuples #-}
{-# language RoleAnnotations #-}
{-# language UnliftedNewtypes #-}
{-# language KindSignatures #-}
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
    -- stuff will be in GHC.Prim, possibly with other names.

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

import GHC.Exts (Int#,State#,Array#,MutableArray#,Any,TYPE,RuntimeRep(UnliftedRep),unsafeCoerce#)
import qualified GHC.Exts as Exts

newtype UnliftedArray# (a :: TYPE 'UnliftedRep) = UnliftedArray# (Array# Any)
type role UnliftedArray# representational

newtype MutableUnliftedArray# s (a :: TYPE 'UnliftedRep) = MutableUnliftedArray# (MutableArray# s Any)
type role MutableUnliftedArray# nominal representational

newUnliftedArray# :: Int# -> a -> State# s -> (# State# s, MutableUnliftedArray# s a #)
newUnliftedArray# sz a s = case Exts.newArray# sz (unsafeCoerce# a) s of
  (# s', mary #) -> (# s', MutableUnliftedArray# mary #)
{-# INLINE newUnliftedArray# #-}

-- | Create a 'MutableUnliftedArray#' whose entries contain some unspecified
-- static value. This may be more convenient than 'newUnliftedArray#' if there
-- is no value on hand with which to initialize the array. Each entry must be
-- initialized before being read and used. This condition is not checked.
unsafeNewUnliftedArray# :: Int# -> State# s -> (# State# s, MutableUnliftedArray# s a #)
-- We fill the array with the Nonsense data constructor. It doesn't much matter
-- *what* we stick in there, as long as it's a pointer the garbage collector
-- can understand and isn't something that might otherwise be released as garbage.
-- There's no point trying to stick an `error` in there, because there's no
-- code anywhere to force the error thunk.
unsafeNewUnliftedArray# sz s = case Exts.newArray# sz (unsafeCoerce# Nonsense) s of
  (# s', mary #) -> (# s', MutableUnliftedArray# mary #)
{-# INLINE unsafeNewUnliftedArray# #-}

data Nonsense = Nonsense


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
    case Exts.newArray# 0# (unsafeCoerce# Nonsense) s' of { (# s'', mary #) ->
    case Exts.unsafeFreezeArray# mary s'' of { (# _, ary #) ->
      UnliftedArray# ary }}})
{-# NOINLINE empty_unlifted_array #-}

data ULA a = ULA (UnliftedArray# a)

-- | Warning: Applying 'unsafeThawUnliftedArray#' to the array produced by
-- this function will make demons come out of your nose.
emptyUnliftedArray# :: Exts.Void# -> UnliftedArray# a
-- We make this primitive because it's the easiest way to get a
-- *shared* primitive unlifted array.
--
-- Why the stern warning above? GHC does not currently support resizing 'Array#',
-- and does not really meaningfully support *growing* arrays of any type. If,
-- however, that ever changes, growing the globally shared empty array would be
-- pretty disastrous.
emptyUnliftedArray# _ = case empty_unlifted_array of
  ULA ary -> ary
{-# INLINE emptyUnliftedArray# #-}

sameMutableUnliftedArray# :: MutableUnliftedArray# s a -> MutableUnliftedArray# s a -> Int#
sameMutableUnliftedArray# (MutableUnliftedArray# ar1) (MutableUnliftedArray# ar2)
  = Exts.sameMutableArray# ar1 ar2
{-# INLINE sameMutableUnliftedArray# #-}

readUnliftedArray# :: MutableUnliftedArray# s a -> Int# -> State# s -> (# State# s, a #)
readUnliftedArray# (MutableUnliftedArray# mary) i s
  = unsafeCoerce# (Exts.readArray# mary i s)
{-# INLINE readUnliftedArray# #-}

writeUnliftedArray# :: MutableUnliftedArray# s a -> Int# -> a -> State# s -> State# s
writeUnliftedArray# (MutableUnliftedArray# mary) i a s
  = Exts.writeArray# mary i (unsafeCoerce# a) s
{-# INLINE writeUnliftedArray# #-}

sizeofUnliftedArray# :: UnliftedArray# a -> Int#
sizeofUnliftedArray# (UnliftedArray# ary) = Exts.sizeofArray# ary
{-# INLINE sizeofUnliftedArray# #-}

sizeofMutableUnliftedArray# :: MutableUnliftedArray# s a -> Int#
sizeofMutableUnliftedArray# (MutableUnliftedArray# mary)
  = Exts.sizeofMutableArray# mary
{-# INLINE sizeofMutableUnliftedArray# #-}

indexUnliftedArray# :: UnliftedArray# a -> Int# -> a
indexUnliftedArray# (UnliftedArray# ary) i
  = unsafeCoerce# (Exts.indexArray# ary i)
{-# INLINE indexUnliftedArray# #-}

unsafeFreezeUnliftedArray# :: MutableUnliftedArray# s a -> State# s -> (# State# s, UnliftedArray# a #)
unsafeFreezeUnliftedArray# (MutableUnliftedArray# mary) s
  = case Exts.unsafeFreezeArray# mary s of
      (# s', ary #) -> (# s', UnliftedArray# ary #)
{-# INLINE unsafeFreezeUnliftedArray# #-}

unsafeThawUnliftedArray# :: UnliftedArray# a -> State# s -> (# State# s, MutableUnliftedArray# s a #)
unsafeThawUnliftedArray# (UnliftedArray# ary) s
  = case Exts.unsafeThawArray# ary s of
     (# s', mary #) -> (# s', MutableUnliftedArray# mary #)
{-# INLINE unsafeThawUnliftedArray# #-}

copyUnliftedArray# :: UnliftedArray# a -> Int# -> MutableUnliftedArray# s a -> Int# -> Int# -> State# s -> State# s
copyUnliftedArray# (UnliftedArray# ary) i1 (MutableUnliftedArray# mary) i2 n s
  = Exts.copyArray# ary i1 mary i2 n s
{-# INLINE copyUnliftedArray# #-}

copyMutableUnliftedArray# :: MutableUnliftedArray# s a -> Int# -> MutableUnliftedArray# s a -> Int# -> Int# -> State# s -> State# s
copyMutableUnliftedArray# (MutableUnliftedArray# mary1) i1 (MutableUnliftedArray# mary2) i2 n s
  = Exts.copyMutableArray# mary1 i1 mary2 i2 n s
{-# INLINE copyMutableUnliftedArray# #-}

cloneUnliftedArray# :: UnliftedArray# a -> Int# -> Int# -> UnliftedArray# a
cloneUnliftedArray# (UnliftedArray# ary) i n
  = UnliftedArray# (Exts.cloneArray# ary i n)
{-# INLINE cloneUnliftedArray# #-}

cloneMutableUnliftedArray# :: MutableUnliftedArray# s a -> Int# -> Int# -> State# s
  -> (# State# s, MutableUnliftedArray# s a #)
cloneMutableUnliftedArray# (MutableUnliftedArray# mary) i n s
  = case Exts.cloneMutableArray# mary i n s of
      (# s', mary' #) -> (# s', MutableUnliftedArray# mary' #)
{-# INLINE cloneMutableUnliftedArray# #-}

freezeUnliftedArray# :: MutableUnliftedArray# s a -> Int# -> Int# -> State# s -> (# State# s, UnliftedArray# a #)
freezeUnliftedArray# (MutableUnliftedArray# mary) i n s
  = case Exts.freezeArray# mary i n s of
      (# s', ary #) -> (# s', UnliftedArray# ary #)
{-# INLINE freezeUnliftedArray# #-}

thawUnliftedArray# :: UnliftedArray# a -> Int# -> Int# -> State# s -> (# State# s, MutableUnliftedArray# s a #)
thawUnliftedArray# (UnliftedArray# ary) i n s
  = case Exts.thawArray# ary i n s of
      (# s', mary #) -> (# s', MutableUnliftedArray# mary #)
{-# INLINE thawUnliftedArray# #-}

casUnliftedArray# :: MutableUnliftedArray# s a -> Int# -> a -> a -> State# s -> (# State# s, Int#, a #)
casUnliftedArray# (MutableUnliftedArray# mary) i x y s
  = unsafeCoerce# (Exts.casArray# mary i (unsafeCoerce# x) (unsafeCoerce# y) s)
{-# INLINE casUnliftedArray# #-}
