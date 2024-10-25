-- |
-- Type classes for working with N-dimensional arrays. Arrays could be
-- scarce. All indices are assumed to be 0-based. No assumptions about
-- data layout in memory are made.
module Vecvec.Classes.NDArray
  ( -- * ND-arrays
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
    -- ** Immutable arrays
  , NDArray(..)
  , index
  , indexMaybe
  , (!)
  , (!?)
    -- * Slicing
    -- $slice
  , Slice(..)
  , slice
  , Slice1D(..)
    -- ** Slice parameters
  , Range(..)
  , End(..)
  , Length(..)
  , Strided(..)
    -- * Unsafe functions
  , unsafeIndex
  ) where

import Vecvec.Classes.Internal.ND

-- $slice
--
-- Here by slice we understand subset of N-dimensional array mutable
-- or immutable which shares underlying buffer with parent. By
-- necessity API is very general and different arrays have different
-- slice specification.
--
-- 1D arrays have many possible variants for specifying slice. Here is
-- an overview of possible variants:
--
-- >>> import Data.Vector qualified as V
-- >>> xs = V.generate 10 id
-- >>> xs
-- [0,1,2,3,4,5,6,7,8,9]
--
-- One way is to specify slice using 'Range': @i :.. j@ selects
-- semiopen range @[i,j)@:
--
-- >>> slice (1 :.. 5) xs
-- [1,2,3,4]
--
-- Negative indices are accepted and follow same convention as
-- python. -1 means last element so @i :.. -1@ means slice starting
-- from element @i@ and excluding last element:
--
-- >>> slice (3 :.. -1) xs
-- [3,4,5,6,7,8]
--
-- Note that is possible to use tuples @(Int, Int)@ as slice
-- specification too:
--
-- >>> slice (3 :: Int, -1 :: Int) xs
-- [3,4,5,6,7,8]
--
-- But they interact poorly with literals. Remember literals are
-- polymorphic so without type annotations GHC cannot infer that @3@
-- in example above it @Int@ and not some other instance of @Num@.
--
-- For selecting slice starting from given index and to the end of
-- vector 'End' could be used:
--
-- >>> slice (5, End) xs
-- [5,6,7,8,9]
-- >>> slice (-3, End) xs
-- [7,8,9]
--
-- For slice of given size 'Length' could be used:
--
-- >>> slice (5, Length 3) xs
-- [5,6,7]
-- >>> slice (-4, Length 3) xs
-- [6,7,8]

-- $setup
--
-- >>> :seti -XLexicalNegation

