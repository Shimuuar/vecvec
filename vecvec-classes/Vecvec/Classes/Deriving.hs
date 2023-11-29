{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ImportQualifiedPost #-}
-- |
-- Newtype for use with deriving via.
module Vecvec.Classes.Deriving
  ( -- * Range checks
    RangeCheck(..)
  , inRange
    -- * Deriving via
  , AsNum(..)
  , AsVector(..)
  , AsMVector(..)
  , AsFixedVec(..)
    -- * Default implementations of methods
  , implMVectorRangeCheck
  , implVectorRangeCheck
  , implSliceVector
  , implSliceMVector
  ) where

import Vecvec.Classes.Internal.ND
import Vecvec.Classes.Internal.Types
