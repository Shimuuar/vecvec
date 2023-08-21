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
  , fromRowsFF
  , new
  , unsafeNew
    -- ** Access
  , unsafeRead
  , unsafeWrite
  , unsafeCol
  , unsafeRow
    -- * BLAS wrappers
  , MatrixTranspose(..)
  , unsafeBlasGemv
  , unsafeBlasGemm
  ) where

import Vecvec.LAPACK.Internal.Matrix.Dense.Mutable
