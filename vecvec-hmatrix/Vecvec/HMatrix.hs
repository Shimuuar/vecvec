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

instance (a ~ b, Num a, Numeric a) => MatMul (Matrix a) (Matrix b) (Matrix a) where
  (@@) = (HM.<>)

instance (a ~ b, Num a, Numeric a) => MatMul (Tr Matrix a) (Matrix b) (Matrix a) where
  Tr a @@ b = tr a HM.<> b

instance (a ~ b, Num a, Numeric a) => MatMul (Conj Matrix a) (Matrix b) (Matrix a) where
  Conj a @@ b = tr' a HM.<> b


instance (a ~ b, Num a, Numeric a) => MatMul (Matrix a) (Tr Matrix b) (Matrix a) where
  a @@ Tr b = a HM.<> tr b

instance (a ~ b, Num a, Numeric a) => MatMul (Tr Matrix a) (Tr Matrix b) (Matrix a) where
  Tr a @@ Tr b = tr a HM.<> tr b

instance (a ~ b, Num a, Numeric a) => MatMul (Conj Matrix a) (Tr Matrix b) (Matrix a) where
  Conj a @@ Tr b = tr' a HM.<> tr b


instance (a ~ b, Num a, Numeric a) => MatMul (Matrix a) (Conj Matrix b) (Matrix a) where
  a @@ Conj b = a HM.<> tr' b

instance (a ~ b, Num a, Numeric a) => MatMul (Tr Matrix a) (Conj Matrix b) (Matrix a) where
  Tr a @@ Conj b = tr a HM.<> tr' b

instance (a ~ b, Num a, Numeric a) => MatMul (Conj Matrix a) (Conj Matrix b) (Matrix a) where
  Conj a @@ Conj b = tr' a HM.<> tr' b


----------------------------------------------------------------
-- Matrix-vector

instance (a ~ b, Num a, Numeric a) => MatMul (Matrix a) (Vector b) (Vector a) where
  (@@) = (HM.#>)

instance (a ~ b, Num a, Numeric a) => MatMul (Tr Matrix a) (Vector b) (Vector a) where
  Tr m @@ v = tr m HM.#> v

instance (a ~ b, Num a, Numeric a) => MatMul (Conj Matrix a) (Vector b) (Vector a) where
  Conj m @@ v = tr' m HM.#> v

instance (a ~ b, Num a, Numeric a) => MatMul (Tr Vector a) (Matrix b) (Tr Vector a) where
  Tr v @@ m = Tr (tr m #> v)

instance (a ~ b, Num a, Numeric a) => MatMul (Conj Vector a) (Matrix b) (Conj Vector a) where
  Conj v @@ m = Conj (tr' m #> v)
