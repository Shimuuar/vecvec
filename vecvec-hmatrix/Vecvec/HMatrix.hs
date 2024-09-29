{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE ImportQualifiedPost   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
-- |
{-# OPTIONS_GHC -Wno-orphans #-}
module Vecvec.HMatrix where

import Numeric.LinearAlgebra as HM
import Numeric.LinearAlgebra.Devel
import Vecvec.Classes

import Data.Vector.Storable qualified as VS

instance (Num a, Element a) => AdditiveSemigroup (Matrix a) where
  (.+.) = liftMatrix2 (VS.zipWith (+))

instance (Num a, Element a) => AdditiveQuasigroup (Matrix a) where
  (.-.)   = liftMatrix2 (VS.zipWith (-))
  negateV = liftMatrix  (VS.map negate)

instance (Num a, Element a) => VectorSpace (Matrix a) where
  type Scalar (Matrix a) = a
  a *. m = liftMatrix (VS.map (a*)) m
  m .* a = liftMatrix (VS.map (*a)) m

-- NOTE: Here we hope that tr' and our NormedScalar are compatible. What to do about it?

-- FIXME: Do we really have to conjugate whole matrix???

----------------------------------------------------------------
-- Matrix-matrix

instance (Num a, Numeric a) => MatMul a Matrix Matrix Matrix where
  (@@) = (HM.<>)

instance (Num a, Numeric a) => MatMul a (Tr Matrix) Matrix Matrix where
  Tr a @@ b = tr a HM.<> b

instance (Num a, Numeric a) => MatMul a (Conj Matrix) Matrix Matrix where
  Conj a @@ b = tr' a HM.<> b


instance (Num a, Numeric a) => MatMul a Matrix (Tr Matrix) Matrix where
  a @@ Tr b = a HM.<> tr b

instance (Num a, Numeric a) => MatMul a (Tr Matrix) (Tr Matrix) Matrix where
  Tr a @@ Tr b = tr a HM.<> tr b

instance (Num a, Numeric a) => MatMul a (Conj Matrix) (Tr Matrix) Matrix where
  Conj a @@ Tr b = tr' a HM.<> tr b


instance (Num a, Numeric a) => MatMul a Matrix (Conj Matrix) Matrix where
  a @@ Conj b = a HM.<> tr' b

instance (Num a, Numeric a) => MatMul a (Tr Matrix) (Conj Matrix) Matrix where
  Tr a @@ Conj b = tr a HM.<> tr' b

instance (Num a, Numeric a) => MatMul a (Conj Matrix) (Conj Matrix) Matrix where
  Conj a @@ Conj b = tr' a HM.<> tr' b


----------------------------------------------------------------
-- Matrix-vector

instance (Num a, Numeric a) => MatMul a Matrix Vector Vector where
  (@@) = (HM.#>)

instance (Num a, Numeric a) => MatMul a (Tr Matrix) Vector Vector where
  Tr m @@ v = tr m HM.#> v

instance (Num a, Numeric a) => MatMul a (Conj Matrix) Vector Vector where
  Conj m @@ v = tr' m HM.#> v

instance (Num a, Numeric a) => MatMul a (Tr Vector) Matrix (Tr Vector) where
  Tr v @@ m = Tr (tr m #> v)

instance (Num a, Numeric a) => MatMul a (Conj Vector) Matrix (Conj Vector) where
  Conj v @@ m = Conj (tr' m #> v)
