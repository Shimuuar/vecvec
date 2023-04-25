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

import Data.Complex
import Data.Coerce
import Data.Kind             (Type)
import Data.Typeable
import Foreign.Storable      (Storable)
import Test.Tasty
import Test.Tasty.QuickCheck


import Vecvec.Classes
import Vecvec.LAPACK         qualified as VV
import Data.Vector.Generic   qualified as VG


tests :: TestTree
tests = testGroup "model"
  [ testGroup "Addition"
    [ prop_addition_correct @VV.Vec @Float
    , prop_addition_correct @VV.Vec @Double
    , prop_addition_correct @VV.Vec @(Complex Float)
    , prop_addition_correct @VV.Vec @(Complex Double)
    ]
  , testGroup "Subtraction"
    [ prop_subtraction_correct @VV.Vec @Float
    , prop_subtraction_correct @VV.Vec @Double
    , prop_subtraction_correct @VV.Vec @(Complex Float)
    , prop_subtraction_correct @VV.Vec @(Complex Double)
    ]
  , testGroup "Negation"
    [ prop_negation_correct @VV.Vec @Float
    , prop_negation_correct @VV.Vec @Double
    , prop_negation_correct @VV.Vec @(Complex Float)
    , prop_negation_correct @VV.Vec @(Complex Double)
    ]
  ]


prop_addition_correct
  :: forall v a. ( IsModel v a
                 , AdditiveSemigroup (v a), AdditiveSemigroup (Model v a))
  => TestTree
prop_addition_correct
  = testProperty (show (typeRep (Proxy @v)) <> " " <> show (typeRep (Proxy @a)))
  $ \(Pair m1 m2) -> let v1 = fromModel m1 :: v a
                         v2 = fromModel m2 :: v a
                         m  = m1 .+. m2
                         v  = v1 .+. v2
                     in v == fromModel m

prop_subtraction_correct
  :: forall v a. ( IsModel v a
                 , AdditiveQuasigroup (v a), AdditiveQuasigroup (Model v a))
  => TestTree
prop_subtraction_correct
  = testProperty (show (typeRep (Proxy @v)) <> " " <> show (typeRep (Proxy @a)))
  $ \(Pair m1 m2) -> let v1 = fromModel m1 :: v a
                         v2 = fromModel m2 :: v a
                         m  = m1 .-. m2
                         v  = v1 .-. v2
                     in v == fromModel m

prop_negation_correct
  :: forall v a. ( IsModel v a
                 , AdditiveQuasigroup (v a), AdditiveQuasigroup (Model v a))
  => TestTree
prop_negation_correct
  = testProperty (show (typeRep (Proxy @v)) <> " " <> show (typeRep (Proxy @a)))
  $ \m1 -> let v1 = fromModel m1 :: v a
               m  = negateV m1
               v  = negateV v1
           in v == fromModel m


----------------------------------------------------------------
-- Models
----------------------------------------------------------------



-- data Pair v a = Pair (Model v a) (Model v a)


class ScalarModel a where
  genScalar :: Gen a

instance ScalarModel Float where
  genScalar = fromIntegral <$> choose @Int (-10, 10)
instance ScalarModel Double where
  genScalar = fromIntegral <$> choose @Int (-10, 10)
instance ScalarModel a => ScalarModel (Complex a) where
  genScalar = (:+) <$> genScalar <*> genScalar


class ( Arbitrary (Model v a)
      , Eq (v a)
      , Show (Model v a)
      , Typeable a, Typeable v
      ) => IsModel v a where
  data family Model v :: Type -> Type
  fromModel :: Model v a -> v a
  sameSize  :: Model v a -> Gen (Model v a)

instance ScalarModel a => Arbitrary (Model VV.Vec a) where
  arbitrary = ModelVec <$> listOf genScalar
  shrink    = coerce (shrinkList @a (const []))


instance (Typeable a, Show a, Eq a, Storable a, ScalarModel a) => IsModel VV.Vec a where
  newtype Model VV.Vec a = ModelVec { unModelVec :: [a] }
    deriving newtype Show
  fromModel = VG.fromList . unModelVec
  sameSize (ModelVec xs) = ModelVec <$> sequence (genScalar <$ xs)

instance Num a => AdditiveSemigroup (Model VV.Vec a) where
  (.+.) = coerce (zipWith ((+) @a))

instance Num a => AdditiveQuasigroup (Model VV.Vec a) where
  (.-.)   = coerce (zipWith ((-) @a))
  negateV = coerce (map (negate @a))


----------------------------------------------------------------
-- Helpers
----------------------------------------------------------------

data Pair v a = Pair (Model v a) (Model v a)

deriving stock instance Show (Model v a) => Show (Pair v a)

instance IsModel v a => Arbitrary (Pair v a) where
  arbitrary = do m1 <- arbitrary
                 m2 <- sameSize m1
                 pure $ Pair m1 m2
