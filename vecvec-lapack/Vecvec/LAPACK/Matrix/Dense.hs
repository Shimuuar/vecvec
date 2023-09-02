{-# LANGUAGE PatternSynonyms #-}
-- |
-- Immutable dense matrices.
module Vecvec.LAPACK.Matrix.Dense
  ( -- * Matrix
    Matrix(..)
  , pattern AsVec
    -- * Operations
    -- ** Conversion to\/from mutable
  , unsafeFreeze
  , freeze
  , thaw
    -- ** Creation
  , fromRowsFF
  , replicate
  , generate
  , zeros
  , eye
  , diag
  , diagF
  , gdiag
  , gdiagF
    -- ** Access
  , getRow
  , getCol
    -- * Unsafe functions
  , unsafeIndex
  , unsafeGetRow
  , unsafeGetCol
  ) where

import Vecvec.LAPACK.Internal.Matrix.Dense
import Prelude ()
