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
import Data.Function         (on)
import Data.Typeable
import Foreign.Storable      (Storable)
import Test.Tasty
import Test.Tasty.QuickCheck

import Data.Vector.Generic   qualified as VG
import Data.Vector           qualified as V
import Data.Vector.Unboxed   qualified as VU
import Data.Vector.Storable  qualified as VS

import Vecvec.Classes
import Vecvec.Classes.Slice
import Vecvec.LAPACK         (Strided(..))
import Vecvec.LAPACK         qualified as VV
import Vecvec.LAPACK.Internal.Matrix.Dense



tests :: TestTree
tests = testGroup "classes"
  [ testGroup "Vector spaces"
    [ props_inner_space @VV.Vec @Float
    , props_inner_space @VV.Vec @Double
    , props_inner_space @VV.Vec @(Complex Float)
    , props_inner_space @VV.Vec @(Complex Double)
      -- Matrix
    , props_vector_space @Matrix @Double
      -- Vector instances
    , props_inner_space @V.Vector  @Double
    , props_inner_space @VU.Vector @Double
    , props_inner_space @VS.Vector @Double
    ]
  ]

-- Tests for vector space implementation
props_inner_space
  :: forall v a. ( IsModel v a, InnerSpace (v a), InnerSpace (Model v a)
                 , Scalar (v a) ~ a, Scalar (Model v a) ~ a
                 , Eq (R a), Show (R a)
                 )
  => TestTree
props_inner_space = testGroup (prop_name @v @a)
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
  :: forall v a. ( IsModel v a, VectorSpace (v a), VectorSpace (Model v a)
                 , Scalar (v a) ~ a, Scalar (Model v a) ~ a
                 , Eq (R a), Show (R a)
                 )
  => TestTree
props_vector_space = testGroup (prop_name @v @a)
  [ prop_addition_correct    @v @a
  , prop_subtraction_correct @v @a
  , prop_negation_correct    @v @a
  , prop_lmul_scalar         @v @a
  , prop_rmul_scalar         @v @a
  ]


prop_name :: forall (v :: Type -> Type) a. (Typeable v, Typeable a) => String
prop_name = tyConModule con <> "." <> tyConName con <> " " <> show tyA
  where
    tyA = typeRep (Proxy @a)
    tyV = typeRep (Proxy @v)
    (con,_) = splitTyConApp tyV


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
  fromModel (Model (ModelVec stride xs))
    = slice ((0,End) `Strided` stride) $ VG.fromList $ replicate stride =<< xs

instance (Typeable a, Show a, Eq a, ScalarModel a) => IsModel V.Vector a where
  type ModelRepr V.Vector = ModelVec
  fromModel = VG.fromList . unModelVec . unModel

instance (Typeable a, Show a, Eq a, VU.Unbox a, ScalarModel a) => IsModel VU.Vector a where
  type ModelRepr VU.Vector = ModelVec
  fromModel = VG.fromList . unModelVec . unModel

instance (Typeable a, Show a, Eq a, Storable a, ScalarModel a) => IsModel VS.Vector a where
  type ModelRepr VS.Vector = ModelVec
  fromModel = VG.fromList . unModelVec . unModel

instance (Typeable a, Show a, Eq a, Storable a, ScalarModel a) => IsModel Matrix a where
  type ModelRepr Matrix = ModelMat
  fromModel = fromRowsFF . unModelMat . unModel

----------------------------------------------------------------
-- Model for vectors
----------------------------------------------------------------

-- | We use lists as model for vectors.
data ModelVec a = ModelVec { modelVecStride :: !Int
                           , unModelVec     :: [a]
                           }
  deriving stock (Show)

instance ScalarModel a => Arbitrary (ModelVec a) where
  arbitrary = ModelVec <$> choose (1,4) <*> listOf genScalar
  shrink (ModelVec n xs) = do
    n' <- case n of 1 -> [1]
                    _ -> [1,n]
    x  <- shrinkList (const []) xs
    return $ ModelVec n' x

instance ScalarModel a => GenSameSize (ModelVec a) where
  sameSize (ModelVec _ xs) = ModelVec <$> choose (1,4) <*> sequence (genScalar <$ xs)

-- FIXME: We need implement checks than values have same size

instance Num a => AdditiveSemigroup (ModelVec a) where
  a .+. b = ModelVec 1 $ (coerce (zipWith ((+) @a)) `on` unModelVec) a b

instance Num a => AdditiveQuasigroup (ModelVec a) where
  a .-. b = ModelVec 1 $ (coerce (zipWith ((-) @a)) `on` unModelVec) a b
  negateV = ModelVec 1 . coerce (map (negate @a)) . unModelVec

instance Num a => VectorSpace (ModelVec a) where
  type Scalar (ModelVec a) = a
  a *. ModelVec _ xs = ModelVec 1 $ fmap (a*) xs
  ModelVec _ xs .* a = ModelVec 1 $ fmap (*a) xs

instance NormedScalar a => InnerSpace (ModelVec a) where
  ModelVec _ xs <.> ModelVec _ ys = sum $ zipWith (\x y -> conjugate x * y) xs ys
  magnitudeSq (ModelVec _ xs) = sum $ scalarNormSq <$> xs



----------------------------------------------------------------
-- Model for dense matrices
----------------------------------------------------------------

newtype ModelMat a = ModelMat { unModelMat :: [[a]] }
  deriving stock (Show,Eq)

instance ScalarModel a => Arbitrary (ModelMat a) where
  arbitrary = do
    cols <- arbitrary
    row  <- mapM (const genScalar) (() : cols)
    rows <- listOf $ unModelVec <$> sameSize (ModelVec 1 row)
    pure $ ModelMat (row:rows)

instance ScalarModel a => GenSameSize (ModelMat a) where
  sameSize (ModelMat xs) = ModelMat <$> (traverse . traverse) (const genScalar) xs


instance Num a => AdditiveSemigroup (ModelMat a) where
  (.+.) = coerce ((zipWith . zipWith) ((+) @a))

instance Num a => AdditiveQuasigroup (ModelMat a) where
  (.-.)   = coerce ((zipWith . zipWith) ((-) @a))
  negateV = coerce ((map . map) (negate @a))

instance Num a => VectorSpace (ModelMat a) where
  type Scalar (ModelMat a) = a
  a *. ModelMat xs = ModelMat $ (fmap . fmap) (a*) xs
  ModelMat xs .* a = ModelMat $ (fmap . fmap) (*a) xs


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
