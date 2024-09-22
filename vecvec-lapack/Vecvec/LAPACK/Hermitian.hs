-- |
module Vecvec.LAPACK.Hermitian
  ( -- * Immutable matrix
    Hermitian(..)
    -- * Operations
    -- ** Conversion to\/from mutable
  , unsafeFreeze
  , freeze
  , thaw
  , toDense
    -- ** Access
  , reallyUnsafeIndex
    -- ** Creation
  , zeros
  , eye
  , fromRowsFF
  , fromRowsFV
  , diag
  , diagF
  , replicate
  , generate
  ) where

import Prelude hiding (replicate)

import Vecvec.LAPACK.Internal.Hermitian
