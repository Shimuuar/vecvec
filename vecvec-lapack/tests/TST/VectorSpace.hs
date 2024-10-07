{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies        #-}
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
import Data.Vector          qualified as V
import Data.Vector.Unboxed  qualified as VU
import Data.Vector.Storable qualified as VS

import Vecvec.Classes
import Vecvec.LAPACK.Vector    (Vec)
import Vecvec.LAPACK.Matrix    (Matrix)
import Vecvec.LAPACK.Symmetric (Symmetric)
import Vecvec.LAPACK.Hermitian (Hermitian)
import Vecvec.LAPACK.FFI       (S,D,C,Z)
import TST.Tools.MatModel
import TST.Tools.Util

tests :: TestTree
tests = testGroup "VectorSpace instances"
  [ props_inner_space @Vec @S
  , props_inner_space @Vec @D
  , props_inner_space @Vec @C
  , props_inner_space @Vec @Z
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
    -- Hermitian
  , props_vector_space   @Hermitian @S
  , props_vector_space   @Hermitian @D
  , props_additive_space @Hermitian @C
  , props_additive_space @Hermitian @Z
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
  :: forall v a. ( HasModel v a
                 , InnerSpace (v a)
                 , InnerSpace (Model1M v a)
                 , ArbitraryShape (Model1M v) a
                 , Arbitrary      (Model1M v a)
                 , Eq (R a)
                 , Eq (v a)
                 , Eq a
                 , Typeable a
                 , Typeable v
                 , SmallScalar a
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
  :: forall v a. (HasModel v a
                 , VectorSpace (v a)
                 , VectorSpace (Model1M v a)
                 , ArbitraryShape (Model1M v) a
                 , Arbitrary      (Model1M v a)
                 , Eq (v a)
                 , Typeable a
                 , Typeable v
                 , SmallScalar a
                 )
  => TestTree
props_vector_space = testGroup (qualTypeName @v ++ " (" ++ qualTypeName @a ++ ")")
  [ prop_addition_correct    @v @a
  , prop_subtraction_correct @v @a
  , prop_negation_correct    @v @a
  , prop_lmul_scalar         @v @a
  , prop_rmul_scalar         @v @a
  ]

-- Tests for vector space implementation
props_additive_space
  :: forall v a. (HasModel v a
                 , AdditiveQuasigroup (v a)
                 , AdditiveQuasigroup (Model1M v a)
                 , ArbitraryShape (Model1M v) a
                 , Arbitrary      (Model1M v a)
                 , Eq (v a)
                 , Typeable a
                 , Typeable v
                 )
  => TestTree
props_additive_space = testGroup (qualTypeName @v ++ " (" ++ qualTypeName @a ++ ")")
  [ prop_addition_correct    @v @a
  , prop_subtraction_correct @v @a
  , prop_negation_correct    @v @a
  ]

prop_addition_correct
  :: forall v a. ( HasModel v a
                 , AdditiveSemigroup (v a)
                 , AdditiveSemigroup (Model1M v a)
                 , ArbitraryShape (Model1M v) a
                 , Eq (v a)
                 )
  => TestTree
prop_addition_correct
  = testProperty "Addition"
  $ \(Pair1 (v1 :>: m1 :: M v a)
            (v2 :>: m2 :: M v a))
     -> (v1 .+. v2) == unmodelMat (m1 .+. m2)

prop_subtraction_correct
  :: forall v a. ( HasModel v a
                 , AdditiveQuasigroup (v a)
                 , AdditiveQuasigroup (Model1M v a)
                 , ArbitraryShape (Model1M v) a
                 , Eq (v a)
                 )
  => TestTree
prop_subtraction_correct
  = testProperty "Addition"
  $ \(Pair1 (v1 :>: m1 :: M v a)
            (v2 :>: m2 :: M v a))
     -> (v1 .-. v2) == unmodelMat (m1 .-. m2)

prop_negation_correct
  :: forall v a. ( HasModel v a
                 , AdditiveQuasigroup (v a)
                 , AdditiveQuasigroup (Model1M v a)
                 , Arbitrary (Model1M v a)
                 , Eq (v a)
                 )
  => TestTree
prop_negation_correct
  = testProperty "Negation"
  $ \(v :>: m :: M v a)
    -> negateV v == unmodelMat (negateV m)

-- Model evaluates multiplication by scalar on the left
prop_lmul_scalar
  :: forall v a. ( HasModel v a
                 , VectorSpace (v a)
                 , VectorSpace (Model1M v a)
                 , Arbitrary (Model1M v a)
                 , SmallScalar a
                 , Eq (v a)
                 )
  => TestTree
prop_lmul_scalar
  = testProperty "Left scalar multiplication"
  $ \(X a)
     (v :>: m :: M v a)
    -> (a *. v) == unmodelMat (a *. m)


-- Model evaluates multiplication by scalar on the right
prop_rmul_scalar
  :: forall v a. ( HasModel v a
                 , VectorSpace (v a)
                 , VectorSpace (Model1M v a)
                 , Arbitrary (Model1M v a)
                 , SmallScalar a
                 , Eq (v a)
                 )
  => TestTree
prop_rmul_scalar
  = testProperty "Right scalar multiplication"
  $ \(X a)
     (v :>: m :: M v a)
    -> (v .* a) == unmodelMat (m .* a)


-- Model evaluates scalar product in the same way
prop_scalar_product
  :: forall v a. ( HasModel v a
                 , InnerSpace (v a)
                 , InnerSpace     (Model1M v a)
                 , ArbitraryShape (Model1M v) a
                 , Eq a
                 )
  => TestTree
prop_scalar_product
  = testProperty "Scalar product"
  $ \(Pair1 (v1 :>: m1 :: M v a)
            (v2 :>: m2 :: M v a)
     ) -> (v1 <.> v2) == (m1 <.> m2)

-- Model evaluates magnitude in the same way
prop_magnitude
  :: forall v a. ( HasModel v a
                 , InnerSpace (v a)
                 , InnerSpace (Model1M v a)
                 , Arbitrary  (Model1M v a)
                 , Eq (R a)
                 )
  => TestTree
prop_magnitude
  = testProperty "Magnitude"
  $ \(v :>: mdl :: M v a) -> magnitudeSq v == magnitudeSq mdl

