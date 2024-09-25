{-# LANGUAGE PatternSynonyms #-}
-- |
-- Mutable dense matrices.
module Vecvec.LAPACK.Matrix.Mutable
  ( -- * Matrix
    MMatrix
  , pattern AsMVec
  , InMatrix
  , LAPACKy
    -- * Operations
    -- ** Construction
  , clone
  , new
  , unsafeNew
  , fromRowsFF
  , fromRowsFV
  , fromColsFF
  , fromColsFV
  , replicate
  , replicateM
  , generate
  , generateM
  , zeros
  , eye
  , diagF
  , diag
  , gdiagF
  , gdiag
    -- ** Access
  , getCol
  , getRow
    -- * BLAS wrappers
  , MatrixTranspose(..)
  , unsafeBlasGemv
  , unsafeBlasGemm
    -- * Unsafe functions
  , unsafeGetCol
  , unsafeGetRow
  ) where

import Vecvec.LAPACK.Unsafe.Matrix.Mutable
import Prelude ()
