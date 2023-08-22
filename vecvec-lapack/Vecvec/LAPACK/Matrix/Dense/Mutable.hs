{-# LANGUAGE PatternSynonyms #-}
-- |
-- Mutable dense matrices.
module Vecvec.LAPACK.Matrix.Dense.Mutable
  ( -- * Matrix
    MMatrix
  , pattern AsMVec
  , AsMInput
    -- * Operations
    -- ** Construction
  , clone
  , new
  , unsafeNew
  , fromRowsFF
  , eye
  , diagF
  , diag
    -- ** Access
  , read
  , write
  , getCol
  , getRow
    -- * BLAS wrappers
  , MatrixTranspose(..)
  , unsafeBlasGemv
  , unsafeBlasGemm
    -- * Unsafe functions
  , unsafeRead
  , unsafeWrite
  , unsafeGetCol
  , unsafeGetRow
  ) where

import Vecvec.LAPACK.Internal.Matrix.Dense.Mutable
import Prelude ()
