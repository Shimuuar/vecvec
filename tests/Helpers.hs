-- | Helpers for testing
module Helpers (
    -- * helpers
    T(..)
  , typeName
  , eq
  , eqC
  ) where

import Data.Complex
import Data.Typeable

import Numeric.MathFunctions.Comparison



----------------------------------------------------------------
-- Helpers
----------------------------------------------------------------

-- | Phantom typed value used to select right instance in QC tests
data T a = T

-- | String representation of type name
typeName :: Typeable a => T a -> String
typeName = show . typeOf . typeParam
  where
    typeParam :: T a -> a
    typeParam _ = undefined

-- | Approximate equality for 'Double'. Doesn't work well for numbers
--   which are almost zero.
eq :: Double                    -- ^ Relative error
   -> Double -> Double -> Bool
eq = eqRelErr

-- | Approximate equality for 'Complex Double'
eqC :: Double                   -- ^ Relative error
    -> Complex Double
    -> Complex Double
    -> Bool
eqC eps a@(ar :+ ai) b@(br :+ bi)
  | a == 0 && b == 0 = True
  | otherwise        = abs (ar - br) <= eps * d
                    && abs (ai - bi) <= eps * d
  where
    d = max (realPart $ abs a) (realPart $ abs b)
