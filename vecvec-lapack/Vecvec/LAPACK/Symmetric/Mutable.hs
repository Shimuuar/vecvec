-- |
module Vecvec.LAPACK.Symmetric.Mutable
  ( -- * Data types
    MSymmetric
  , InSymmetric
  , LAPACKy
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

import Vecvec.LAPACK.Unsafe.Symmetric.Mutable
