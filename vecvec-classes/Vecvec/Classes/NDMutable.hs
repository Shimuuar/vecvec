-- |
-- Type classes for mutable N-dimensional arrays.
module Vecvec.Classes.NDMutable
  ( -- * ND-arrays
    -- $ndarray
    -- ** Array shape
    Rank
  , HasShape(..)
  , shape
  , nCols
  , nRows
  , IsShape(..)
  , inBounds
  , pattern N1
  , pattern N2
    -- ** Slicing
  , Slice(..)
  , slice
  , Slice1D(..)
  , Range(..)
  , End(..)
  , Length(..)
  , Strided(..)
    -- ** Mutable arrays
  , NDMutable(..)
  , unsafeReadArr
  , readArr
  , unsafeWriteArr
  , writeArr
  ) where

import Vecvec.Classes.Internal.ND


-- $ndarray
--
-- Here we provide type classes for working with with N-dimensional
-- arrays (ND-arrays) of very generic form. They could be both mutable
-- and immutable. And there are multiple possible representations:
-- usual dense arrays, various sparse representations, representations
-- which exploit symmetry. Such as symmetric matrix which uses same in
-- memory layout as dense matrix but only elements at and above
-- diagonal could be referenced.
--
-- Such diversity constrains type class methods quite a bit and leads
-- to multiple type classes.
