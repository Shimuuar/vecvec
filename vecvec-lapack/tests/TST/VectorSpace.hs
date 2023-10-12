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

import Data.Vector           qualified as V
import Data.Vector.Unboxed   qualified as VU
import Data.Vector.Storable  qualified as VS

import Vecvec.Classes
import Vecvec.LAPACK                       qualified as VV
import Vecvec.LAPACK.Internal.Matrix.Dense (Matrix)
import Vecvec.LAPACK.FFI                   (S,D,C,Z)
import TST.Tools.MatModel
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
  :: forall v a. ( IsModel v, InnerSpace v, InnerSpace (Model v), ArbitraryShape (Model v)
                 , Scalar v ~ a, Scalar (Model v) ~ a
                 , Eq (R a), Show (R a), SmallScalar a, Show a, Eq a
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
  :: forall v a. ( IsModel v, VectorSpace v, VectorSpace (Model v), ArbitraryShape (Model v)
                 , Scalar v ~ a, Scalar (Model v) ~ a
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
  :: forall v. ( IsModel v, AdditiveSemigroup v, AdditiveSemigroup (Model v), ArbitraryShape (Model v))
  => TestTree
prop_addition_correct
  = testProperty "Addition"
  $ \(Pair m1 m2 :: Pair v) ->
      let v1 = fromModel m1 :: v
          v2 = fromModel m2 :: v
          m  = m1 .+. m2
          v  = v1 .+. v2
      in id $ counterexample ("MODEL = " ++ show m)
            $ counterexample ("IMPL  = " ++ show v)
            $ v == fromModel m

-- Model evaluate subtraction in the same way as implementation
prop_subtraction_correct
  :: forall v. ( IsModel v, AdditiveQuasigroup v, AdditiveQuasigroup (Model v), ArbitraryShape (Model v))
  => TestTree
prop_subtraction_correct
  = testProperty "Subtraction"
  $ \(Pair m1 m2 :: Pair v) ->
      let v1 = fromModel m1 :: v
          v2 = fromModel m2 :: v
          m  = m1 .-. m2
          v  = v1 .-. v2
      in v == fromModel m

-- Model evaluate negation in the same way as implementation
prop_negation_correct
  :: forall v. ( IsModel v, AdditiveQuasigroup v, AdditiveQuasigroup (Model v), ArbitraryShape (Model v))
  => TestTree
prop_negation_correct
  = testProperty "Negation"
  $ \m1 -> let v1 = fromModel m1 :: v
               m  = negateV m1
               v  = negateV v1
           in v == fromModel m

-- Model evaluates multiplication by scalar on the left
prop_lmul_scalar
  :: forall v a. ( IsModel v, VectorSpace v, VectorSpace (Model v), ArbitraryShape (Model v)
                 , Scalar v ~ a, Scalar (Model v) ~ a
                 , SmallScalar a, Show a
                 )
  => TestTree
prop_lmul_scalar
  = testProperty "Left scalar multiplication"
  $ \(X a) m1 ->
      let v1 = fromModel m1 :: v
          m  = a *. m1
          v  = a *. v1
      in id $ counterexample ("MODEL = " ++ show m)
            $ counterexample ("IMPL  = " ++ show v)
            $ v == fromModel m

-- Model evaluates multiplication by scalar on the right
prop_rmul_scalar
  :: forall v a. ( IsModel v, VectorSpace v, VectorSpace (Model v), ArbitraryShape (Model v)
                 , Scalar v ~ a, Scalar (Model v) ~ a
                 , SmallScalar a, Show a
                 )
  => TestTree
prop_rmul_scalar
  = testProperty "Right scalar multiplication"
  $ \(X a) m1 ->
      let v1 = fromModel m1 :: v
          m  = m1 .* a
          v  = v1 .* a
      in id $ counterexample ("MODEL = " ++ show m)
            $ counterexample ("IMPL  = " ++ show v)
            $ v == fromModel m


-- Model evaluates scalar product in the same way
prop_scalar_product
  :: forall v a. ( IsModel v, InnerSpace v, InnerSpace (Model v), ArbitraryShape (Model v)
                 , a ~ Scalar v
                 , a ~ Scalar (Model v)
                 , Eq a)
  => TestTree
prop_scalar_product
  = testProperty "Scalar product"
  $ \(Pair m1 m2 :: Pair v) ->
      let v1 = fromModel m1 :: v
          v2 = fromModel m2 :: v
          rV = v1 <.> v2
          rM = (m1 <.> m2)
      in rV == rM

-- Model evaluates magnitude in the same way
prop_magnitude
  :: forall v a. ( IsModel v, InnerSpace v, InnerSpace (Model v), ArbitraryShape (Model v)
                 , a ~ Scalar v
                 , a ~ Scalar (Model v)
                 , Eq (R a), Show (R a))
  => TestTree
prop_magnitude
  = testProperty "Magnitude"
  $ \m -> let v  = fromModel m :: v
              rV = magnitudeSq v
              rM = magnitudeSq m
          in id $ counterexample ("Model: " ++ show rM)
                $ counterexample ("Impl:  " ++ show rV)
                $ rV == rM
