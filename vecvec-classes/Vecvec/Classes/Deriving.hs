-- |
-- Newtype for use with deriving via and default implementation of
-- type classes methods.
module Vecvec.Classes.Deriving
  ( -- * Range checks
    inRange
  , inBoundsCVec
    -- * Deriving via
  , AsNum(..)
  , AsVector(..)
  , AsMVector(..)
  , ViaFixed(..)
    -- * Default implementations of methods
  , implSliceVector
  , implSliceMVector
  ) where

import Data.Vector.Fixed (ViaFixed(..))
import Vecvec.Classes.Internal.ND
import Vecvec.Classes.Internal.Types
