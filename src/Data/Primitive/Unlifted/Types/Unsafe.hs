{-# language MagicHash #-}
{-# language UnliftedNewtypes #-}

-- | Some types may impose invariants that are not natively
-- enforced by their unlifted forms. This module defines
-- newtypes around those unlifted forms that can be used to
-- write safe @PrimUnlifted@ instances. At present, this is
-- only done for the 'ShortText' type, but others may be added.

module Data.Primitive.Unlifted.Types.Unsafe
  ( ShortText# (..)
  ) where

import GHC.Exts (ByteArray#)

newtype ShortText# = ShortText# ByteArray#
