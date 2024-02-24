-- |
module Vecvec.LAPACK.Matrix.Symmetric.Mutable
  ( -- * Data types
    MSymmetric
  , AsSymInput
  , symmetrizeMSymView
    -- * Operations
    -- ** Creation
  , clone
  , new
  , unsafeNew
  , fromRowsFF
  , fromRowsFV
  , replicate
  , replicateM
  , generate
  , generateM
  ) where

import Prelude hiding (replicate)

import Vecvec.LAPACK.Internal.Matrix.Symmetric.Mutable
