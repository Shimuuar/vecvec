{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NegativeLiterals    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE ViewPatterns        #-}
-- |
-- Mutable storable vectors. Unlike standard vectors they have stride
-- which means that only N-th element in underlying buffer belong to
-- vector. This is needed to allow /O(1)/ getters for dense matrix
-- rows and columns.
module Vecvec.LAPACK.Vector.Mutable
  ( -- * Data type and type classes
    MVec
    -- * Type classes
  , LAPACKy
  , InVector
    -- * Mutable BLAS wrappers
    -- ** Checked varians
    -- $blas
  , clone
  , blasAxpy
  , blasScal
  , blasDotu
  , blasDotc
  , blasNrm2
    -- ** Unchecked variants
    -- $unchecked
  , unsafeBlasAxpy
  , unsafeBlasDotu
  , unsafeBlasDotc
  ) where

import Vecvec.LAPACK.Internal.Vector.Mutable

-- $blas
--
-- These function are thin wrappers for BLAS (basic linear algebra
-- system) level 1 function (operations on vector). They use in-place
-- mutation. Naming follows BLAS.
--
-- All read-only parameters to functions are polymorphic and
-- instances of 'AsInput' type class. This allows to pass both mutable
-- and immutable vectors.


-- $unchecked
--
-- These are unchecked BLAS operations. They perform no range checks
-- and will happily run all over your memory given chance. Use with
-- extreme care.
