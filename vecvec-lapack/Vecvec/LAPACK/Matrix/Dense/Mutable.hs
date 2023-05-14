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
  , fromRowsFF
    -- ** Access
  , unsafeRead
  , unsafeWrite
  , unsafeCol
  , unsafeRow
    -- * BLAS wrappers
  , MatrixTranspose(..)
  , unsafeBlasGemv
  ) where

import Vecvec.LAPACK.Internal.Matrix.Dense.Mutable
