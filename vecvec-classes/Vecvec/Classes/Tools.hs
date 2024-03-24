{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ImportQualifiedPost #-}
-- |
-- Collection of small utility functions
module Vecvec.Classes.Tools
  ( -- * Generate sequences
    linspace
  , logspace
  ) where


import Data.Vector.Generic qualified as VG


-- | Generate vector of numbers evenly spaced in the
--   interval. Endpoints are included.
--
-- ==== __Examples__
--
-- >>> import Data.Vector.Unboxed qualified as VU
-- >>> linspace (1,10) 9 :: VU.Vector Double
-- [1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0]
linspace
  :: (VG.Vector v a, Fractional a)
  => (a, a) -- ^ Range
  -> Int    -- ^ Number of points. Must be nonegative
  -> v a
{-# INLINE linspace #-}
linspace (!a,!b) n
  | n <= 1    = error "linspace: negative number of elements"
  | otherwise = VG.generate n $ \i -> a + step * fromIntegral i
  where
    step = (b - a) / fromIntegral n

-- | Generate vector of numbers evenly spaced in the logarithmic
--   space. Endpoints are included.
--
-- ==== __Examples__
--
-- >>> import Data.Vector.Unboxed qualified as VU
-- >>> logspace (1,1e5) 5 :: VU.Vector Double
-- [1.0,10.000000000000002,100.00000000000004,1000.0000000000007,10000.00000000001]
logspace
  :: (VG.Vector v a, Floating a, Ord a)
  => (a, a) -- ^ Range
  -> Int    -- ^ Number of points. Must be at least 2
  -> v a
{-# INLINE logspace #-}
logspace (a,b) n
  | a > 0 && b > 0 = VG.map exp $ linspace (log a, log b) n
  | otherwise      = error "logspace: negative parameters"

-- $setup
-- >>> :set -XImportQualifiedPost
