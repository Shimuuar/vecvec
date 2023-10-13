{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost        #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ViewPatterns               #-}
-- |
-- We want to test that we correctly defined all instances for vectors
-- and matrices. However testing floating point arithmetics is
-- notoriously tricky. In this case we however have a cure. Our
-- operations involve only addition and small number of multiplication
-- so if we limit ourselves to small integers we wont encounter any
-- rounding!
module TST.VectorSpace (tests) where

import Test.Tasty
import Test.Tasty.QuickCheck

import Data.Typeable
import Data.Vector           qualified as V
import Data.Vector.Unboxed   qualified as VU
import Data.Vector.Storable  qualified as VS

import Vecvec.Classes
import Vecvec.LAPACK                       qualified as VV
import Vecvec.LAPACK.Internal.Matrix.Dense (Matrix)
import Vecvec.LAPACK.FFI                   (S,D,C,Z)
import TST.Tools.MatModel
import TST.Tools.Model
import TST.Tools.Util

tests :: TestTree
tests = testGroup "VectorSpace instances"
  [ props_inner_space @(VV.Vec S)
  , props_inner_space @(VV.Vec D)
  , props_inner_space @(VV.Vec C)
  , props_inner_space @(VV.Vec Z)
    -- Matrix
  , props_vector_space @(Matrix S)
  , props_vector_space @(Matrix D)
  , props_vector_space @(Matrix C)
  , props_vector_space @(Matrix Z)
    -- Vector instances
  , props_inner_space @(V.Vector  Double)
  , props_inner_space @(VU.Vector Double)
  , props_inner_space @(VS.Vector Double)
  ]

  

----------------------------------------------------------------
--
---------------------------------------------------------------

-- Tests for vector space implementation
props_inner_space
  :: forall v a. ( TestMatrix v, TestMatrix a, TestMatrix (R a), TestEquiv v, TestEquiv a, TestEquiv (R a)
                 , ArbitraryShape (ModelM v)
                 , InnerSpace v, InnerSpace (ModelM v)
                 , Typeable v, Show v, Show (ModelM v)
                 , a ~ Scalar v
                 , a ~ Scalar (ModelM v)
                 , a ~ ModelM a
                 , R a ~ ModelM (R a)
                 , Show (R a), SmallScalar a, Show a
                 )
  => TestTree
props_inner_space = testGroup (qualTypeName @v)
  [ prop_addition_correct    @v
  , prop_subtraction_correct @v
  , prop_negation_correct    @v
  , prop_lmul_scalar         @v
  , prop_rmul_scalar         @v
  , prop_scalar_product      @v
  , prop_magnitude           @v
  ]

-- Tests for vector space implementation
props_vector_space
  :: forall v a. ( TestMatrix v, TestEquiv v, VectorSpace v, VectorSpace (ModelM v), ArbitraryShape (ModelM v)
                 , Typeable v, Show v, Show (ModelM v)
                 , Scalar v ~ a, Scalar (ModelM v) ~ a
                 , SmallScalar a, Show a
                 )
  => TestTree
props_vector_space = testGroup (qualTypeName @v)
  [ prop_addition_correct    @v
  , prop_subtraction_correct @v
  , prop_negation_correct    @v
  , prop_lmul_scalar         @v
  , prop_rmul_scalar         @v
  ]



-- Model evaluate addition in the same way as implementation
prop_addition_correct
  :: forall v. ( TestMatrix v, AdditiveSemigroup v, AdditiveSemigroup (ModelM v), ArbitraryShape (ModelM v)
               , TestEquiv v, Show (ModelM v)
               )
  => TestTree
prop_addition_correct
  = testProperty "Addition"
  $ (mdl @(Pair v) $ eq @v)
    (\(Pair v1 v2) -> v1 .+. v2)
    (\(Pair v1 v2) -> v1 .+. v2)

-- Model evaluate subtraction in the same way as implementation
prop_subtraction_correct
  :: forall v. ( TestMatrix v, TestEquiv v, AdditiveQuasigroup v, AdditiveQuasigroup (ModelM v), ArbitraryShape (ModelM v)
               , Show (ModelM v)
               )
  => TestTree
prop_subtraction_correct
  = testProperty "Subtraction"
  $ (mdl $ eq @v)
    (\(Pair v1 v2) -> v1 .-. v2)
    (\(Pair v1 v2) -> v1 .-. v2)

-- Model evaluate negation in the same way as implementation
prop_negation_correct
  :: forall v. ( TestMatrix v, AdditiveQuasigroup v, AdditiveQuasigroup (ModelM v), ArbitraryShape (ModelM v)
               , TestEquiv v, Show (ModelM v)
               )
  => TestTree
prop_negation_correct
  = testProperty "Negation"
  $ (mdl $ eq @v)
    negateV
    negateV

-- Model evaluates multiplication by scalar on the left
prop_lmul_scalar
  :: forall v a. ( TestMatrix v, VectorSpace v, VectorSpace (ModelM v), ArbitraryShape (ModelM v)
                 , TestEquiv v, Show v, Show (ModelM v)
                 , Scalar v ~ a, Scalar (ModelM v) ~ a
                 , SmallScalar a, Show a
                 )
  => TestTree
prop_lmul_scalar
  = testProperty "Left scalar multiplication"
  $ (val @(X a)  $  mdl @v  $  eqV)
    (\(X a) v -> a *. v)
    (\(X a) v -> a *. v)

-- Model evaluates multiplication by scalar on the right
prop_rmul_scalar
  :: forall v a. ( TestMatrix v, VectorSpace v, VectorSpace (ModelM v), ArbitraryShape (ModelM v)
                 , TestEquiv v, Show v, Show (ModelM v)
                 , Scalar v ~ a, Scalar (ModelM v) ~ a
                 , SmallScalar a, Show a
                 )
  => TestTree
prop_rmul_scalar
  = testProperty "Right scalar multiplication"
  $ (val @(X a)  $  mdl @v  $  eqV)
    (\(X a) v -> v .* a)
    (\(X a) v -> v .* a)

-- Model evaluates scalar product in the same way
prop_scalar_product
  :: forall v a. ( TestMatrix v, TestMatrix a, ArbitraryShape (ModelM v)
                 , InnerSpace v, InnerSpace (ModelM v)
                 , Show (ModelM v)
                 , a ~ Scalar v
                 , a ~ Scalar (ModelM v)
                 , a ~ ModelM a 
                 , TestEquiv a)
  => TestTree
prop_scalar_product
  = testProperty "Scalar product"
  $ (mdl @(Pair v)  $  eq)
    (\(Pair v1 v2) -> v1 <.> v2)
    (\(Pair v1 v2) -> v1 <.> v2)
 
-- Model evaluates magnitude in the same way
prop_magnitude
  :: forall v a. ( TestMatrix v, TestMatrix (R a), TestEquiv (R a), ArbitraryShape (ModelM v)
                 , InnerSpace v, InnerSpace (ModelM v)
                 , Show (ModelM v)
                 , a ~ Scalar v
                 , a ~ Scalar (ModelM v)
                 , R a ~ ModelM (R a)
                 , Show (R a))
  => TestTree
prop_magnitude
  = testProperty "Magnitude"
  $ (mdl @v  $  eqV)
     magnitudeSq
     magnitudeSq
