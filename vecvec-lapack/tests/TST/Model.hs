{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost        #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RecordWildCards            #-}
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
import Data.Kind             (Type)
import Data.Function         (on)
import Data.Typeable
import Data.List             (intercalate)
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
import TST.Orphanage ()


tests :: TestTree
tests = testGroup "classes"
  [ testGroup "Vector space instances"
    [ props_inner_space @(VV.Vec Float)
    , props_inner_space @(VV.Vec Double)
    , props_inner_space @(VV.Vec (Complex Float))
    , props_inner_space @(VV.Vec (Complex Double))
      -- Matrix
    , props_vector_space @(Matrix Double)
      -- Vector instances
    , props_inner_space @(V.Vector  Double)
    , props_inner_space @(VU.Vector Double)
    , props_inner_space @(VS.Vector Double)
    ]
    ]
  ]

-- Tests for vector space implementation
props_inner_space
  :: forall v a. ( IsModel v, InnerSpace v, InnerSpace (Model v)
                 , Scalar v ~ a, Scalar (Model v) ~ a
                 , Eq (R a), Show (R a), ScalarModel a, Show a, Eq a
                 )
  => TestTree
props_inner_space = testGroup (prop_name @v)
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
  :: forall v a. ( IsModel v, VectorSpace v, VectorSpace (Model v)
                 , Scalar v ~ a, Scalar (Model v) ~ a
                 , ScalarModel a, Show a
                 )
  => TestTree
props_vector_space = testGroup (prop_name @v)
  [ prop_addition_correct    @v
  , prop_subtraction_correct @v
  , prop_negation_correct    @v
  , prop_lmul_scalar         @v
  , prop_rmul_scalar         @v
  ]


prop_name :: forall v. (Typeable v) => String
prop_name = intercalate " "
          $ tyConModule con <> "." <> tyConName con
          : map show par
  where
    tyV = typeRep (Proxy @v)
    (con,par) = splitTyConApp tyV


-- Model evaluate addition in the same way as implementation
prop_addition_correct
  :: forall v. ( IsModel v, AdditiveSemigroup v, AdditiveSemigroup (Model v))
  => TestTree
prop_addition_correct
  = testProperty "Addition"
  $ \(Pair m1 m2) ->
      let v1 = fromModel m1 :: v
          v2 = fromModel m2 :: v
          m  = m1 .+. m2
          v  = v1 .+. v2
      in id $ counterexample ("MODEL = " ++ show m)
            $ counterexample ("IMPL  = " ++ show v)
            $ v == fromModel m

-- Model evaluate subtraction in the same way as implementation
prop_subtraction_correct
  :: forall v. ( IsModel v, AdditiveQuasigroup v, AdditiveQuasigroup (Model v))
  => TestTree
prop_subtraction_correct
  = testProperty "Subtraction"
  $ \(Pair m1 m2) -> let v1 = fromModel m1 :: v
                         v2 = fromModel m2 :: v
                         m  = m1 .-. m2
                         v  = v1 .-. v2
                     in v == fromModel m

-- Model evaluate negation in the same way as implementation
prop_negation_correct
  :: forall v. ( IsModel v, AdditiveQuasigroup v, AdditiveQuasigroup (Model v))
  => TestTree
prop_negation_correct
  = testProperty "Negation"
  $ \m1 -> let v1 = fromModel m1 :: v
               m  = negateV m1
               v  = negateV v1
           in v == fromModel m

-- Model evaluates multiplication by scalar on the left
prop_lmul_scalar
  :: forall v a. ( IsModel v, VectorSpace v, VectorSpace (Model v)
                 , Scalar v ~ a, Scalar (Model v) ~ a
                 , ScalarModel a, Show a
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
  :: forall v a. ( IsModel v, VectorSpace v, VectorSpace (Model v)
                 , Scalar v ~ a, Scalar (Model v) ~ a
                 , ScalarModel a, Show a
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
  :: forall v a. ( IsModel v, InnerSpace v, InnerSpace (Model v)
                 , a ~ Scalar v
                 , a ~ Scalar (Model v)
                 , Eq a)
  => TestTree
prop_scalar_product
  = testProperty "Scalar product"
  $ \(Pair m1 m2) -> let v1 = fromModel m1 :: v
                         v2 = fromModel m2 :: v
                         rV = v1 <.> v2
                         rM = (m1 <.> m2)
                     in rV == rM

-- Model evaluates magnitude in the same way
prop_magnitude
  :: forall v a. ( IsModel v, InnerSpace v, InnerSpace (Model v)
                 , a ~ Scalar v
                 , a ~ Scalar (Model v)
                 , Eq (R a), Show (R a))
  => TestTree
prop_magnitude
  = testProperty "Magnitude"
  $ \m -> let v = fromModel m :: v
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
newtype Model v = Model { unModel :: ModelRepr v }

deriving newtype instance Show               (ModelRepr v) => Show               (Model v)
deriving newtype instance Arbitrary          (ModelRepr v) => Arbitrary          (Model v)
deriving newtype instance GenSameSize        (ModelRepr v) => GenSameSize        (Model v)
deriving newtype instance AdditiveSemigroup  (ModelRepr v) => AdditiveSemigroup  (Model v)
deriving newtype instance AdditiveQuasigroup (ModelRepr v) => AdditiveQuasigroup (Model v)
deriving newtype instance VectorSpace        (ModelRepr v) => VectorSpace        (Model v)
deriving newtype instance InnerSpace         (ModelRepr v) => InnerSpace         (Model v)



-- | Type class which
class ( GenSameSize (ModelRepr v)
      , Typeable v
      , Eq v
      , Show v
      , Show (ModelRepr v)
      ) => IsModel v where
  type ModelRepr v :: Type
  fromModel :: Model v -> v

instance IsModel Double where
  type ModelRepr Double = Double
  fromModel = coerce

instance (Storable a, Show a, Eq a, Typeable a, ScalarModel a) => IsModel (VV.Vec a) where
  type ModelRepr (VV.Vec a) = (ModelVec a)
  fromModel (Model (ModelVec stride xs))
    = slice ((0,End) `Strided` stride) $ VG.fromList $ replicate stride =<< xs

instance (Typeable a, Show a, Eq a, ScalarModel a) => IsModel (V.Vector a) where
  type ModelRepr (V.Vector a) = ModelVec a
  fromModel = VG.fromList . unModelVec . unModel

instance (Typeable a, Show a, Eq a, VU.Unbox a, ScalarModel a) => IsModel (VU.Vector a) where
  type ModelRepr (VU.Vector a) = ModelVec a
  fromModel = VG.fromList . unModelVec . unModel

instance (Typeable a, Show a, Eq a, Storable a, ScalarModel a) => IsModel (VS.Vector a) where
  type ModelRepr (VS.Vector a) = ModelVec a
  fromModel = VG.fromList . unModelVec . unModel

instance (Typeable a, Show a, Eq a, Storable a, ScalarModel a, Num a) => IsModel (Matrix a) where
  type ModelRepr (Matrix a) = ModelMat a
  fromModel (Model ModelMat{unModelMat=mat, ..})
    = slice ((padRows,End), (padCols,End))
    $ fromRowsFF
    $ replicate padRows (replicate (nC + padCols) 0)
   ++ map (replicate padCols 0 ++) mat
    where
      nC = case mat of []    -> 0
                       (r:_) -> length r


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
  a .+. b = ModelVec 1 $ (zipWith (+) `on` unModelVec) a b

instance Num a => AdditiveQuasigroup (ModelVec a) where
  a .-. b = ModelVec 1 $ (zipWith (-) `on` unModelVec) a b
  negateV = ModelVec 1 . map negate . unModelVec

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

data ModelMat a = ModelMat
  { padRows    :: !Int
  , padCols    :: !Int
  , unModelMat :: [[a]]
  }
  deriving stock (Show,Eq)

instance ScalarModel a => Arbitrary (ModelMat a) where
  arbitrary = do
    cols <- arbitrary
    row  <- mapM (const genScalar) (() : cols)
    rows <- listOf $ unModelVec <$> sameSize (ModelVec 1 row)
    r    <- choose (0, 3)
    c    <- choose (0, 3)
    pure $ ModelMat r c (row:rows)
  shrink ModelMat{..} = do
    p_r <- case padRows of 0 -> [0]; _ -> [0,padRows]
    p_c <- case padCols of 0 -> [0]; _ -> [0,padCols]
    [ModelMat{ padRows = p_r, padCols = p_c, .. }]

instance ScalarModel a => GenSameSize (ModelMat a) where
  sameSize (ModelMat r c xs) = ModelMat r c <$> (traverse . traverse) (const genScalar) xs


instance Num a => AdditiveSemigroup (ModelMat a) where
  a .+. b = ModelMat 0 0 $ ((zipWith . zipWith) (+) `on` unModelMat) a b

instance Num a => AdditiveQuasigroup (ModelMat a) where
  a .-. b = ModelMat 0 0 $ ((zipWith . zipWith) (-) `on` unModelMat) a b
  negateV = ModelMat 0 0 . ((map . map) negate) . unModelMat

instance Num a => VectorSpace (ModelMat a) where
  type Scalar (ModelMat a) = a
  a *. ModelMat _ _ xs = ModelMat 0 0 $ (fmap . fmap) (a*) xs
  ModelMat _ _ xs .* a = ModelMat 0 0 $ (fmap . fmap) (*a) xs


----------------------------------------------------------------
-- Helpers
----------------------------------------------------------------

-- | Pair of models with same size
data Pair v = Pair (Model v) (Model v)

deriving stock instance Show (Model v) => Show (Pair v)

instance IsModel v => Arbitrary (Pair v) where
  arbitrary = do m1 <- arbitrary
                 m2 <- sameSize m1
                 pure $ Pair m1 m2

-- | Scalar which uses @ScalarModel@ for generation
newtype X a = X a
  deriving newtype Show

instance ScalarModel a => Arbitrary (X a) where
  arbitrary = X <$> genScalar
