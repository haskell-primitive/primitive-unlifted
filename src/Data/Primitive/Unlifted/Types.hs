{-# language MagicHash #-}

-- | Some types may impose invariants that are not natively
-- enforced by their unlifted forms. This module exports
-- newtypes around those unlifted forms that can be used to
-- write safe @PrimUnlifted@ instances. At present, this is
-- only done for the 'ShortText' type, but others may be added.
--
-- This module exports only abstract types. To access their
-- constructors, import "Data.Primitive.Unlifted.Types.Unsafe".

module Data.Primitive.Unlifted.Types
  ( ShortText#
  ) where

import Data.Primitive.Unlifted.Types.Unsafe
