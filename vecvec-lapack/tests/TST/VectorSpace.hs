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
import Vecvec.LAPACK                    qualified as VV
import Vecvec.LAPACK.Internal.Matrix    (Matrix)
import Vecvec.LAPACK.Internal.Symmetric (Symmetric)
import Vecvec.LAPACK.FFI                (S,D,C,Z)
import TST.Tools.MatModel
import TST.Tools.Model
import TST.Tools.Util

tests :: TestTree
tests = testGroup "VectorSpace instances"
  [ props_inner_space @VV.Vec @S
  , props_inner_space @VV.Vec @D
  , props_inner_space @VV.Vec @C
  , props_inner_space @VV.Vec @Z
    -- Matrix
  , props_vector_space @Matrix @S
  , props_vector_space @Matrix @D
  , props_vector_space @Matrix @C
  , props_vector_space @Matrix @Z
    -- Symmetric
  , props_vector_space @Symmetric @S
  , props_vector_space @Symmetric @D
  , props_vector_space @Symmetric @C
  , props_vector_space @Symmetric @Z
    -- Vector instances
  , props_inner_space @V.Vector  @D
  , props_inner_space @VU.Vector @D
  , props_inner_space @VS.Vector @D
  , props_inner_space @V.Vector  @Z
  , props_inner_space @VU.Vector @Z
  , props_inner_space @VS.Vector @Z
  ]



----------------------------------------------------------------
--
---------------------------------------------------------------

-- Tests for vector space implementation
props_inner_space
  :: forall v a. ( TestMatrix1 v a, TestEquiv (v a), TestEquiv a, TestEquiv (R a)
                 , InnerSpace (v a)
                 , InnerSpace (Model1M v a)
                 , a ~ Scalar (v a)
                 , a ~ Scalar (Model1M v a)
                 , Show a
                 , Typeable v, Typeable a
                 , ArbitraryShape (Model1M v) a
                 , Arbitrary      (Model1M v a)
                 , SmallScalar    a
                 , Show (Model1M v a)
                 )
  => TestTree
props_inner_space = testGroup (qualTypeName @v ++ " (" ++ qualTypeName @a ++ ")")
  [ prop_addition_correct    @v @a
  , prop_subtraction_correct @v @a
  , prop_negation_correct    @v @a
  , prop_lmul_scalar         @v @a
  , prop_rmul_scalar         @v @a
  , prop_scalar_product      @v @a
  , prop_magnitude           @v @a
  ]

-- Tests for vector space implementation
props_vector_space
  :: forall v a. ( TestMatrix1 v a
                 , VectorSpace (v a)
                 , VectorSpace (Model1M v a)
                 , a ~ Scalar (v a)
                 , a ~ Scalar (Model1M v a)
                 , TestEquiv (v a)
                 , SmallScalar a
                 , Show a
                 , Typeable v, Typeable a
                 , ArbitraryShape (Model1M v) a
                 , Arbitrary      (Model1M v a)
                 , Show (Model1M v a)
                 )
  => TestTree
props_vector_space = testGroup (qualTypeName @v ++ " (" ++ qualTypeName @a ++ ")")
  [ prop_addition_correct    @v @a
  , prop_subtraction_correct @v @a
  , prop_negation_correct    @v @a
  , prop_lmul_scalar         @v @a
  , prop_rmul_scalar         @v @a
  ]

-- Model evaluate addition in the same way as implementation
prop_addition_correct
  :: forall v a. ( TestMatrix1 v a, TestEquiv (v a)
                 , AdditiveSemigroup (v a), AdditiveSemigroup (Model1M v a)
                 , ArbitraryShape (Model1M v) a
                 , Show           (Model1M v a)
                 )
  => TestTree
prop_addition_correct
  = testProperty "Addition"
  $ (mdl1 $ eq1 @v @a)
    (\(Pair1 v1 v2) -> v1 .+. v2)
    (\(Pair1 v1 v2) -> v1 .+. v2)

-- Model evaluate subtraction in the same way as implementation
prop_subtraction_correct
  :: forall v a. ( TestMatrix1 v a, TestEquiv (v a)
                 , AdditiveQuasigroup (v a), AdditiveQuasigroup (Model1M v a)
                 , ArbitraryShape (Model1M v) a
                 , Show           (Model1M v a)
                 )
  => TestTree
prop_subtraction_correct
  = testProperty "Subtraction"
  $ (mdl1 $ eq1 @v @a)
    (\(Pair1 v1 v2) -> v1 .-. v2)
    (\(Pair1 v1 v2) -> v1 .-. v2)

-- Model evaluate negation in the same way as implementation
prop_negation_correct
  :: forall v a. ( TestMatrix1 v a, TestEquiv (v a)
                 , AdditiveQuasigroup (v a), AdditiveQuasigroup (Model1M v a)
                 , Arbitrary (Model1M v a)
                 , Show      (Model1M v a)
                 )
  => TestTree
prop_negation_correct
  = testProperty "Negation"
  $ (mdl1 $ eq1 @v @a)
    negateV
    negateV

-- Model evaluates multiplication by scalar on the left
prop_lmul_scalar
  :: forall v a. ( TestMatrix1 v a, TestEquiv (v a)
                 , VectorSpace (v a), VectorSpace (Model1M v a)
                 , a ~ Scalar (v a)
                 , a ~ Scalar (Model1M v a)
                 , Show a, Show (Model1M v a)
                 , Arbitrary (Model1M v a)
                 , SmallScalar a
                 )
  => TestTree
prop_lmul_scalar
  = testProperty "Left scalar multiplication"
  $ (val @(X a)  $  mdl1 $ eq1 @v @a)
    (\(X a) v -> a *. v)
    (\(X a) v -> a *. v)

-- Model evaluates multiplication by scalar on the right
prop_rmul_scalar
  :: forall v a. ( TestMatrix1 v a, TestEquiv (v a)
                 , VectorSpace (v a), VectorSpace (Model1M v a)
                 , a ~ Scalar (v a)
                 , a ~ Scalar (Model1M v a)
                 , Show a, Show (Model1M v a)
                 , Arbitrary (Model1M v a)
                 , SmallScalar a
                 )
  => TestTree
prop_rmul_scalar
  = testProperty "Right scalar multiplication"
  $ (val @(X a)  $  mdl1  $  eq1 @v @a)
    (\(X a) v -> v .* a)
    (\(X a) v -> v .* a)

-- Model evaluates scalar product in the same way
prop_scalar_product
  :: forall v a. ( TestMatrix1 v a, TestEquiv a
                 , InnerSpace (v a), InnerSpace (Model1M v a)
                 , a ~ Scalar (v a)
                 , a ~ Scalar (Model1M v a)
                 , ArbitraryShape (Model1M v) a
                 , Show (Model1M v a)
                 )
  => TestTree
prop_scalar_product
  = testProperty "Scalar product"
  $ (mdl1 @(Pair1 v) @a  $  plainEq)
    (\(Pair1 v1 v2) -> v1 <.> v2)
    (\(Pair1 v1 v2) -> v1 <.> v2)

-- Model evaluates magnitude in the same way
prop_magnitude
  :: forall v a. ( TestMatrix1 v a, TestEquiv (R a)
                 , InnerSpace (v a), InnerSpace (Model1M v a)
                 , Show (Model1M v a)
                 , a   ~ Scalar (v a)
                 , a   ~ Scalar (Model1M v a)
                 , Arbitrary (Model1M v a)
                 )
  => TestTree
prop_magnitude
  = testProperty "Magnitude"
  $ (mdl1 @v @a  $  plainEq @(R a))
     magnitudeSq
     magnitudeSq
