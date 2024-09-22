-- |
module Vecvec.LAPACK.Symmetric.Mutable
  ( -- * Data types
    MSymmetric
  , InSymmetric
    -- * Conversion
  , asMHermitian
    -- * Operations
    -- ** Creation
  , clone
  , new
  , unsafeNew
  , fromRowsFF
  , fromRowsFV
  , zeros
  , replicate
  , replicateM
  , eye
  , diag
  , diagF
  , generate
  , generateM
  ) where

import Prelude hiding (replicate)

import Vecvec.LAPACK.Internal.Symmetric.Mutable
