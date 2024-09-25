{-# LANGUAGE PatternSynonyms #-}
-- |
-- Immutable dense matrices.
module Vecvec.LAPACK.Matrix
  ( -- * Matrix
    Matrix(..)
  , LAPACKy
  , pattern AsVec
    -- * Operations
    -- ** Conversion to\/from mutable
  , unsafeFreeze
  , freeze
  , thaw
    -- ** Creation
  , fromRowsFF
  , fromRowsFV
  , fromColsFF
  , fromColsFV
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
  , toColList
  , toRowList
  , all
  , any
    -- * Unsafe functions
  , unsafeIndex
  , unsafeGetRow
  , unsafeGetCol
  ) where

import Vecvec.LAPACK.Internal.Matrix
import Prelude ()
