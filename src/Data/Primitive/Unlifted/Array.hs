{-# language BangPatterns #-}
{-# language MagicHash #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language TypeFamilies #-}
{-# language UnboxedTuples #-}

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

import Data.Primitive.Unlifted.Array.Internal hiding
  ( indexUnliftedArray, readUnliftedArray, writeUnliftedArray )
import Data.Primitive.Unlifted.Array.Operations
