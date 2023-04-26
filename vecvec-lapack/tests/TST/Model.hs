{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost        #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
-- |
-- We want to test that we correctly defined all instances for vectors
-- and matrices. However testing floating point arithmetics is
-- notoriously tricky. In this case we however have a cure. Our
-- operations involve only addition and small number of multiplication
-- so if we limit ourselves to small integers we wont encounter any
-- rounding!
module TST.Model (tests) where

import Data.Complex          (Complex(..))
import Data.Coerce
import Data.Kind             (Type)
import Data.Typeable
import Foreign.Storable      (Storable)
import Test.Tasty
import Test.Tasty.QuickCheck


import Vecvec.Classes
import Vecvec.LAPACK         qualified as VV
import Data.Vector.Generic   qualified as VG
import Data.Vector           qualified as V
import Data.Vector.Unboxed   qualified as VU
import Data.Vector.Storable  qualified as VS


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

----------------------------------------------------------------
-- Models
----------------------------------------------------------------

-- | Generate value with same size
class Arbitrary a => GenSameSize a where
  sameSize :: a -> Gen a


-- | Generate small exactly representable numbers
class Arbitrary a => ScalarModel a where
  genScalar :: Gen a

instance ScalarModel Float where
  genScalar = fromIntegral <$> choose @Int (-10, 10)
instance ScalarModel Double where
  genScalar = fromIntegral <$> choose @Int (-10, 10)
instance ScalarModel a => ScalarModel (Complex a) where
  genScalar = (:+) <$> genScalar <*> genScalar

-- | New type for model.
newtype Model v a = Model { unModel :: ModelRepr v a }

deriving newtype instance Show        (ModelRepr v a) => Show        (Model v a)
deriving newtype instance Arbitrary   (ModelRepr v a) => Arbitrary   (Model v a)
deriving newtype instance GenSameSize (ModelRepr v a) => GenSameSize (Model v a)
deriving newtype instance AdditiveSemigroup  (ModelRepr v a) => AdditiveSemigroup  (Model v a)
deriving newtype instance AdditiveQuasigroup (ModelRepr v a) => AdditiveQuasigroup (Model v a)
deriving newtype instance VectorSpace        (ModelRepr v a) => VectorSpace        (Model v a)
deriving newtype instance InnerSpace         (ModelRepr v a) => InnerSpace         (Model v a)


-- | Type class which
class ( GenSameSize (ModelRepr v a)
      , Show        (ModelRepr v a)
      , Eq   (v a)
      , Eq   a
      , Show a
      , ScalarModel a
      , Typeable a
      , Typeable v
      ) => IsModel v a where
  type ModelRepr v :: Type -> Type
  fromModel :: Model v a -> v a

instance (Typeable a, Show a, Eq a, Storable a, ScalarModel a) => IsModel VV.Vec a where
  type ModelRepr VV.Vec = ModelVec
  fromModel = VG.fromList . unModelVec . unModel

instance (Typeable a, Show a, Eq a, ScalarModel a) => IsModel V.Vector a where
  type ModelRepr V.Vector = ModelVec
  fromModel = VG.fromList . unModelVec . unModel

instance (Typeable a, Show a, Eq a, VU.Unbox a, ScalarModel a) => IsModel VU.Vector a where
  type ModelRepr VU.Vector = ModelVec
  fromModel = VG.fromList . unModelVec . unModel

instance (Typeable a, Show a, Eq a, Storable a, ScalarModel a) => IsModel VS.Vector a where
  type ModelRepr VS.Vector = ModelVec
  fromModel = VG.fromList . unModelVec . unModel


-- | We use lists as model for vectors.
newtype ModelVec a = ModelVec { unModelVec :: [a] }
  deriving newtype (Show)

instance ScalarModel a => Arbitrary (ModelVec a) where
  arbitrary = ModelVec <$> listOf genScalar
  shrink    = coerce (shrinkList @a (const []))

instance ScalarModel a => GenSameSize (ModelVec a) where
  sameSize (ModelVec xs) = ModelVec <$> sequence (genScalar <$ xs)

-- FIXME: We need implement checks than values have same size

instance Num a => AdditiveSemigroup (ModelVec a) where
  (.+.) = coerce (zipWith ((+) @a))

instance Num a => AdditiveQuasigroup (ModelVec a) where
  (.-.)   = coerce (zipWith ((-) @a))
  negateV = coerce (map (negate @a))

instance Num a => VectorSpace (ModelVec a) where
  type Scalar (ModelVec a) = a
  a *. ModelVec xs = ModelVec $ fmap (a*) xs
  ModelVec xs .* a = ModelVec $ fmap (*a) xs

instance NormedScalar a => InnerSpace (ModelVec a) where
  ModelVec xs <.> ModelVec ys = sum $ zipWith (\x y -> conjugate x * y) xs ys
  magnitudeSq (ModelVec xs) = sum $ scalarNormSq <$> xs


----------------------------------------------------------------
-- Helpers
----------------------------------------------------------------

-- | Pair of models with same size
data Pair v a = Pair (Model v a) (Model v a)

deriving stock instance Show (Model v a) => Show (Pair v a)

instance IsModel v a => Arbitrary (Pair v a) where
  arbitrary = do m1 <- arbitrary
                 m2 <- sameSize m1
                 pure $ Pair m1 m2

-- | Scalar which uses @ScalarModel@ for generation
newtype X a = X a
  deriving newtype Show

instance ScalarModel a => Arbitrary (X a) where
  arbitrary = X <$> genScalar
