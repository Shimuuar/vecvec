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
    -- ** Access
  , unsafeRead
  , unsafeRow
  , unsafeCol

  ) where

import Vecvec.LAPACK.Internal.Matrix.Dense
