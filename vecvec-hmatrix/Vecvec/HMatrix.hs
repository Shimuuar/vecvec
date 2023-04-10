{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE ImportQualifiedPost   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
-- |
{-# OPTIONS_GHC -Wno-orphans #-}
module Vecvec.HMatrix where

import Numeric.LinearAlgebra as HM
import Numeric.LinearAlgebra.Devel
import Vecvec.Classes

import Data.Vector.Storable qualified as VS
-- FIXME: Avoid weirdness

instance (Num a, Element a) => AdditiveSemigroup (Matrix a) where
  (.+.) = liftMatrix2 (VS.zipWith (+))

instance (Num a, Element a) => AdditiveQuasigroup (Matrix a) where
  (.-.)   = liftMatrix2 (VS.zipWith (-))
  negateV = liftMatrix  (VS.map negate)

instance (Num a, Element a) => VectorSpace (Matrix a) where
  type Scalar (Matrix a) = a
  a *. m = liftMatrix (VS.map (a*)) m
  m .* a = liftMatrix (VS.map (*a)) m

instance (a ~ b, a ~ c, Num a, Numeric a) => MatMul (Matrix a) (Matrix b) (Matrix c) where
  (@@) = (HM.<>)

instance (a ~ b, a ~ c, Num a, Numeric a) => MatMul (Matrix a) (Vector b) (Vector c) where
  (@@) = (HM.#>)
