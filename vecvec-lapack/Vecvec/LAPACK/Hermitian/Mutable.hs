-- |
module Vecvec.LAPACK.Hermitian.Mutable
  ( -- * Data types
    MHermitian
  , InHermitian
  , LAPACKy
    -- * Conversions
  , asMSymmetric
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

import Vecvec.LAPACK.Internal.Hermitian.Mutable
