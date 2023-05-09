{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE ImportQualifiedPost        #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
{- HLINT ignore "Use camelCase" -}
-- |
-- We want to test that we correctly defined all instances for vectors
-- and matrices. However testing floating point arithmetics is
-- notoriously tricky. In this case we however have a cure. Our
-- operations involve only addition and small number of multiplication
-- so if we limit ourselves to small integers we wont encounter any
-- rounding!
module TST.VectorSpace (tests) where

import Data.Complex          (Complex(..))
import Data.Typeable
import Test.Tasty
import Test.Tasty.QuickCheck


import Vecvec.Classes
import Vecvec.LAPACK         qualified as VV
import Data.Vector           qualified as V
import Data.Vector.Unboxed   qualified as VU
import Data.Vector.Storable  qualified as VS

import TST.Model


tests :: TestTree
tests = testGroup "model"
  [ testGroup "Vector spaces"
    [ props_vector_space @VV.Vec @Float
    , props_vector_space @VV.Vec @Double
    , props_vector_space @VV.Vec @(Complex Float)
    , props_vector_space @VV.Vec @(Complex Double)
      -- Vector instances
    , props_vector_space @V.Vector  @Double
    , props_vector_space @VU.Vector @Double
    , props_vector_space @VS.Vector @Double
    ]
  ]

-- Tests for vector space implementation
props_vector_space
  :: forall v a. ( IsModel v a, InnerSpace (v a), InnerSpace (Model v a)
                 , Scalar (v a) ~ a, Scalar (Model v a) ~ a
                 , Eq (R a), Show (R a)
                 )
  => TestTree
props_vector_space = testGroup title
  [ prop_addition_correct    @v @a
  , prop_subtraction_correct @v @a
  , prop_negation_correct    @v @a
  , prop_lmul_scalar         @v @a
  , prop_rmul_scalar         @v @a
  , prop_scalar_product      @v @a
  , prop_magnitude           @v @a
  ]
  where
    tyA = typeRep (Proxy @a)
    tyV = typeRep (Proxy @v)
    (con,_) = splitTyConApp tyV
    title = tyConModule con <> "." <> tyConName con <> " " <> show tyA

-- Model evaluate addition in the same way as implementation
prop_addition_correct
  :: forall v a. ( IsModel v a, AdditiveSemigroup (v a), AdditiveSemigroup (Model v a))
  => TestTree
prop_addition_correct
  = testProperty "Addition"
  $ \(Pair m1 m2) -> let v1 = fromModel m1 :: v a
                         v2 = fromModel m2 :: v a
                         m  = m1 .+. m2
                         v  = v1 .+. v2
                     in v == fromModel m

-- Model evaluate subtraction in the same way as implementation
prop_subtraction_correct
  :: forall v a. ( IsModel v a, AdditiveQuasigroup (v a), AdditiveQuasigroup (Model v a))
  => TestTree
prop_subtraction_correct
  = testProperty "Subtraction"
  $ \(Pair m1 m2) -> let v1 = fromModel m1 :: v a
                         v2 = fromModel m2 :: v a
                         m  = m1 .-. m2
                         v  = v1 .-. v2
                     in v == fromModel m

-- Model evaluate negation in the same way as implementation
prop_negation_correct
  :: forall v a. ( IsModel v a, AdditiveQuasigroup (v a), AdditiveQuasigroup (Model v a))
  => TestTree
prop_negation_correct
  = testProperty "Negation"
  $ \m1 -> let v1 = fromModel m1 :: v a
               m  = negateV m1
               v  = negateV v1
           in v == fromModel m

-- Model evaluates multiplication by scalar on the left
prop_lmul_scalar
  :: forall v a. ( IsModel v a, VectorSpace (v a), VectorSpace (Model v a), Scalar (v a) ~ a, Scalar (Model v a) ~ a)
  => TestTree
prop_lmul_scalar
  = testProperty "Left scalar multiplication"
  $ \(X a) m1 -> let v1 = fromModel m1 :: v a
                     m  = a *. m1
                     v  = a *. v1
           in v == fromModel m

-- Model evaluates multiplication by scalar on the right
prop_rmul_scalar
  :: forall v a. ( IsModel v a, VectorSpace (v a), VectorSpace (Model v a), Scalar (v a) ~ a, Scalar (Model v a) ~ a)
  => TestTree
prop_rmul_scalar
  = testProperty "Right scalar multiplication"
  $ \(X a) m1 -> let v1 = fromModel m1 :: v a
                     m  = m1 .* a
                     v  = v1 .* a
           in v == fromModel m

-- Model evaluates scalar product in the same way
prop_scalar_product
  :: forall v a. ( IsModel v a, InnerSpace (v a), InnerSpace (Model v a), Scalar (v a) ~ a, Scalar (Model v a) ~ a)
  => TestTree
prop_scalar_product
  = testProperty "Scalar product"
  $ \(Pair m1 m2) -> let v1 = fromModel m1 :: v a
                         v2 = fromModel m2 :: v a
                         rV = v1 <.> v2
                         rM = (m1 <.> m2)
                     in rV == rM

-- Model evaluates magnitude in the same way
prop_magnitude
  :: forall v a. ( IsModel v a, InnerSpace (v a), InnerSpace (Model v a), Scalar (v a) ~ a, Scalar (Model v a) ~ a
                 , Eq (R a), Show (R a))
  => TestTree
prop_magnitude
  = testProperty "Magnitude"
  $ \m -> let v = fromModel m :: v a
              rV = magnitudeSq v
              rM = magnitudeSq m
          in id $ counterexample ("Model: " ++ show rM)
                $ counterexample ("Impl:  " ++ show rV)
                $ rV == rM
