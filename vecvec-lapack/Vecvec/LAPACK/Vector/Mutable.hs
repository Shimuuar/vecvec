{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NegativeLiterals    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE ViewPatterns        #-}
-- |
module Vecvec.LAPACK.Vector.Mutable
  ( -- * Data type and type classes
    MVec
  , LAPACKy
    -- * Mutable BLAS wrappers
    -- ** Checked varians
  , clone
  , blasAxpy
  , blasScal
  , blasDot
  , blasNrm2
    -- ** Unchecked variants
  , unsafeBlasAxpy
  , unsafeBlasDot
  ) where

import Vecvec.LAPACK.Internal.Vector.Mutable

