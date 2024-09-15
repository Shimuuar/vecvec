-- |
module Vecvec.LAPACK.TrueSymmetric.Mutable
  ( -- * Data types
    MTrueSymmetric
  , InTrueSymmetric
  , symmetrizeMSymView
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

import Vecvec.LAPACK.Internal.TrueSymmetric.Mutable
