-- |
-- Immutable storable vectors. Unlike standard vectors they have stride
-- which means that only N-th element in underlying buffer belong to
-- vector. This is needed to allow /O(1)/ getters for dense matrix
-- rows and columns.
module Vecvec.LAPACK.Vector
  ( -- * Vector data type
    Vec
  , LAPACKy
  ) where

import Vecvec.LAPACK.Unsafe.Vector
