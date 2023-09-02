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
module TST.Model (tests) where

import Control.Monad
import Data.Complex          (Complex(..))
import Data.Kind             (Type)
import Data.Function         (on)
import Data.Typeable
import Data.List             (intercalate,transpose)
import Foreign.Storable      (Storable)
import Linear.Matrix         ((!*), (!*!))
import Test.Tasty
import Test.Tasty.QuickCheck

import Data.Vector.Fixed.Cont qualified as FC
import Data.Vector.Generic   qualified as VG
import Data.Vector           qualified as V
import Data.Vector.Unboxed   qualified as VU
import Data.Vector.Storable  qualified as VS

import Vecvec.Classes
import Vecvec.Classes.NDArray
import Vecvec.LAPACK                      (Strided(..))
import Vecvec.LAPACK                       qualified as VV
import Vecvec.LAPACK.Internal.Matrix.Dense (Matrix, fromRowsFF)
import Vecvec.LAPACK.FFI                   (S,D,C,Z)
import TST.Orphanage ()


tests :: TestTree
tests = testGroup "classes"
  [ testGroup "Vector space instances"
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
  , testGroup "MatMul"
    [
      -- Matrix-vector
      prop_matmul @(Matrix S)        @(VV.Vec S) unMV
    , prop_matmul @(Matrix D)        @(VV.Vec D) unMV
    , prop_matmul @(Matrix C)        @(VV.Vec C) unMV
    , prop_matmul @(Matrix Z)        @(VV.Vec Z) unMV
    , prop_matmul @(Tr (Matrix S))   @(VV.Vec S) unMV
    , prop_matmul @(Tr (Matrix D))   @(VV.Vec D) unMV
    , prop_matmul @(Tr (Matrix C))   @(VV.Vec C) unMV
    , prop_matmul @(Tr (Matrix Z))   @(VV.Vec Z) unMV
    , prop_matmul @(Conj (Matrix S)) @(VV.Vec S) unMV
    , prop_matmul @(Conj (Matrix D)) @(VV.Vec D) unMV
    , prop_matmul @(Conj (Matrix C)) @(VV.Vec C) unMV
    , prop_matmul @(Conj (Matrix Z)) @(VV.Vec Z) unMV
      -- Matrix-matrix
      -- 1.
    , prop_matmul @(Matrix S)        @(Matrix S) unMM
    , prop_matmul @(Matrix D)        @(Matrix D) unMM
    , prop_matmul @(Matrix C)        @(Matrix C) unMM
    , prop_matmul @(Matrix Z)        @(Matrix Z) unMM
    , prop_matmul @(Tr (Matrix S))   @(Matrix S) unMM
    , prop_matmul @(Tr (Matrix D))   @(Matrix D) unMM
    , prop_matmul @(Tr (Matrix C))   @(Matrix C) unMM
    , prop_matmul @(Tr (Matrix Z))   @(Matrix Z) unMM
    , prop_matmul @(Conj (Matrix S)) @(Matrix S) unMM
    , prop_matmul @(Conj (Matrix D)) @(Matrix D) unMM
    , prop_matmul @(Conj (Matrix C)) @(Matrix C) unMM
    , prop_matmul @(Conj (Matrix Z)) @(Matrix Z) unMM
      -- 2.
    , prop_matmul @(Matrix S)        @(Tr (Matrix S)) unMM
    , prop_matmul @(Matrix D)        @(Tr (Matrix D)) unMM
    , prop_matmul @(Matrix C)        @(Tr (Matrix C)) unMM
    , prop_matmul @(Matrix Z)        @(Tr (Matrix Z)) unMM
    , prop_matmul @(Tr (Matrix S))   @(Tr (Matrix S)) unMM
    , prop_matmul @(Tr (Matrix D))   @(Tr (Matrix D)) unMM
    , prop_matmul @(Tr (Matrix C))   @(Tr (Matrix C)) unMM
    , prop_matmul @(Tr (Matrix Z))   @(Tr (Matrix Z)) unMM
    , prop_matmul @(Conj (Matrix S)) @(Tr (Matrix S)) unMM
    , prop_matmul @(Conj (Matrix D)) @(Tr (Matrix D)) unMM
    , prop_matmul @(Conj (Matrix C)) @(Tr (Matrix C)) unMM
    , prop_matmul @(Conj (Matrix Z)) @(Tr (Matrix Z)) unMM
      -- 3.
    , prop_matmul @(Matrix S)        @(Conj (Matrix S)) unMM
    , prop_matmul @(Matrix D)        @(Conj (Matrix D)) unMM
    , prop_matmul @(Matrix C)        @(Conj (Matrix C)) unMM
    , prop_matmul @(Matrix Z)        @(Conj (Matrix Z)) unMM
    , prop_matmul @(Tr (Matrix S))   @(Conj (Matrix S)) unMM
    , prop_matmul @(Tr (Matrix D))   @(Conj (Matrix D)) unMM
    , prop_matmul @(Tr (Matrix C))   @(Conj (Matrix C)) unMM
    , prop_matmul @(Tr (Matrix Z))   @(Conj (Matrix Z)) unMM
    , prop_matmul @(Conj (Matrix S)) @(Conj (Matrix S)) unMM
    , prop_matmul @(Conj (Matrix D)) @(Conj (Matrix D)) unMM
    , prop_matmul @(Conj (Matrix C)) @(Conj (Matrix C)) unMM
    , prop_matmul @(Conj (Matrix Z)) @(Conj (Matrix Z)) unMM
    ]
  ]


----------------------------------------------------------------
--
---------------------------------------------------------------

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
          : map showParam par
  where
    tyV = typeRep (Proxy @v)
    (con,par) = splitTyConApp tyV
    showParam p = case show p of
      s | ' ' `elem` s -> "("++s++")"
        | otherwise    -> s


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


-- Test for generalized matrix-vector multiplication.
prop_matmul
  :: forall v1 v2 vR a.
     ( IsModel v1
     , IsModel v2
     , IsModel vR
     , MatMul (ModelRepr v1) (ModelRepr v2) (ModelRepr vR)
     , MatMul v1 v2 vR
     , Arbitrary a, Show a
     )
  => (a -> (ModelRepr v1, ModelRepr v2))
  -> TestTree
prop_matmul to_pair
  = testProperty (prop_name @v1 ++ " x " ++ prop_name @v2)
  $ \(to_pair -> (mr1, mr2)) ->
      let m1 = Model mr1    :: Model v1
          m2 = Model mr2    :: Model v2
          v1 = fromModel m1 :: v1
          v2 = fromModel m2 :: v2
          m  = mr1 @@ mr2
          v  = v1  @@ v2
      in id $ counterexample ("MODEL = " ++ show m)
            $ counterexample ("IMPL  = " ++ show v)
            $ v == fromModel (Model m)



----------------------------------------------------------------
-- Models
----------------------------------------------------------------

genSize :: forall shape n. (FC.Arity n, IsShape shape n) => Gen shape
genSize = shapeFromCVec <$> FC.replicateM @n (choose (1,10))

-- | Generate stride for vectors
genStride :: Gen Int
genStride = choose (1,3)

-- | Generate offset for matrices
genOffset :: Gen Int
genOffset = choose (0,3)

-- | Generate value with same size
class (Arbitrary a, HasShape a) => ArbitraryShape a where
  arbitraryShape :: (IsShape shape (NDim a)) => shape -> Gen a

-- | We want to test that multiplication return correct result without
--   going into dark forest of floating point. This is achieved by
--   exploiting fact that multiplication and addition small integers
--   are exact in floating point.
class Arbitrary a => ScalarModel a where
  genScalar    :: Gen a
  shrinkScalar :: a -> [a]

instance ScalarModel Float where
  genScalar = fromIntegral <$> choose @Int (-10, 10)
  shrinkScalar = \case
    0 -> []
    1 -> [0]
    _ -> [0,1]
instance ScalarModel Double where
  genScalar = fromIntegral <$> choose @Int (-10, 10)
  shrinkScalar = \case
    0 -> []
    1 -> [0]
    _ -> [0,1]
instance (RealFloat a, ScalarModel a) => ScalarModel (Complex a) where
  genScalar = (:+) <$> genScalar <*> genScalar
  shrinkScalar = \case
    0      -> []
    1      -> [0, 0:+1]
    0 :+ 1 -> [0, 1]
    _      -> [0, 1, 0:+1]

-- | New type for model.
newtype Model v = Model { unModel :: ModelRepr v }

deriving newtype instance Show               (ModelRepr v) => Show               (Model v)
deriving newtype instance Arbitrary          (ModelRepr v) => Arbitrary          (Model v)
deriving newtype instance HasShape           (ModelRepr v) => HasShape           (Model v)
deriving newtype instance ArbitraryShape     (ModelRepr v) => ArbitraryShape     (Model v)
deriving newtype instance AdditiveSemigroup  (ModelRepr v) => AdditiveSemigroup  (Model v)
deriving newtype instance AdditiveQuasigroup (ModelRepr v) => AdditiveQuasigroup (Model v)
deriving newtype instance VectorSpace        (ModelRepr v) => VectorSpace        (Model v)
deriving newtype instance InnerSpace         (ModelRepr v) => InnerSpace         (Model v)

-- | Type class for models
class ( ArbitraryShape (ModelRepr v)
      , Typeable v
      , Eq v
      , Show v
      , Show (ModelRepr v)
      ) => IsModel v where
  type ModelRepr v :: Type
  fromModel :: Model v -> v

instance (Storable a, Num a, Show a, Eq a, Typeable a, ScalarModel a) => IsModel (VV.Vec a) where
  type ModelRepr (VV.Vec a) = (ModelVec a)
  fromModel (Model (ModelVec stride xs))
    = slice ((0,End) `Strided` stride)
    $ VG.fromList
    $ (\n -> n : replicate (stride-1) 0) =<< xs

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
  fromModel (Model m@ModelMat{unModelMat=mat, ..})
    = slice ((padRows,End), (padCols,End))
    $ fromRowsFF
    $ replicate padRows (replicate (nC + padCols) 0)
   ++ map (replicate padCols 0 ++) mat
    where
      nC = modelMatCols m


instance (NDim a ~ 2, ArbitraryShape a) => ArbitraryShape (Tr a) where
  arbitraryShape (N2 n k) = Tr <$> arbitraryShape (k,n)
instance (NDim a ~ 2, ArbitraryShape a) => ArbitraryShape (Conj a) where
  arbitraryShape (N2 n k) = Conj <$> arbitraryShape (k,n)

instance (NDim (ModelRepr a) ~ 2, IsModel a) => IsModel (Tr a) where
  type ModelRepr (Tr a) = Tr (ModelRepr a)
  fromModel (Model (Tr a)) = Tr $ fromModel (Model a)

instance (NDim (ModelRepr a) ~ 2, IsModel a) => IsModel (Conj a) where
  type ModelRepr (Conj a) = Conj (ModelRepr a)
  fromModel (Model (Conj a)) = Conj $ fromModel (Model a)


----------------------------------------------------------------
-- Model for vectors
----------------------------------------------------------------

-- | We use lists as model for vectors.
data ModelVec a = ModelVec { modelVecStride :: !Int
                           , unModelVec     :: [a]
                           }
  deriving stock (Show)

instance HasShape (ModelVec a) where
  type NDim (ModelVec a) = 1
  shapeAsCVec = FC.mk1 . length . unModelVec

instance ScalarModel a => Arbitrary (ModelVec a) where
  arbitrary = arbitraryShape =<< genSize @Int
  shrink (ModelVec n xs) = do
    n' <- case n of 1 -> [1]
                    _ -> [1,n]
    x  <- shrinkList (const []) xs
    return $ ModelVec n' x

instance ScalarModel a => ArbitraryShape (ModelVec a) where
  arbitraryShape (N1 n) = ModelVec <$> genStride <*> replicateM n genScalar

-- FIXME: We need to implement checks than values have same size
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
-- Model for matrix/vector multiplication
----------------------------------------------------------------

-- | Type class for values that could be represented as matrices
class IsModelMat m a where
  toModelMat :: m -> ModelMat a

instance a ~ a' => IsModelMat (ModelMat a) a' where
  toModelMat = id
instance a ~ a' => IsModelMat (Tr (ModelMat a)) a' where
  toModelMat (Tr m) = (ModelMat 0 0 . transpose . unModelMat) m
instance (NormedScalar a, a ~ a') => IsModelMat (Conj (ModelMat a)) a' where
  toModelMat (Conj m) = (ModelMat 0 0 . (fmap . fmap) conjugate . transpose . unModelMat) m



instance (Num a) => MatMul (ModelMat a) (ModelVec a) (ModelVec a) where
  m @@ v = ModelVec 1 $ unModelMat m !* unModelVec v

instance (Num a) => MatMul (Tr (ModelMat a)) (ModelVec a) (ModelVec a) where
  m @@ v = ModelVec 1 $ (unModelMat . toModelMat) m !* unModelVec v

instance (NormedScalar a) => MatMul (Conj (ModelMat a)) (ModelVec a) (ModelVec a) where
  m @@ v = ModelVec 1 $ (unModelMat . toModelMat) m !* unModelVec v


instance (NormedScalar a) => MatMul       (ModelMat a)        (ModelMat a)  (ModelMat a) where (@@) = defaultMulMM
instance (NormedScalar a) => MatMul (Tr   (ModelMat a))       (ModelMat a)  (ModelMat a) where (@@) = defaultMulMM
instance (NormedScalar a) => MatMul (Conj (ModelMat a))       (ModelMat a)  (ModelMat a) where (@@) = defaultMulMM
instance (NormedScalar a) => MatMul       (ModelMat a)  (Tr   (ModelMat a)) (ModelMat a) where (@@) = defaultMulMM
instance (NormedScalar a) => MatMul (Tr   (ModelMat a)) (Tr   (ModelMat a)) (ModelMat a) where (@@) = defaultMulMM
instance (NormedScalar a) => MatMul (Conj (ModelMat a)) (Tr   (ModelMat a)) (ModelMat a) where (@@) = defaultMulMM
instance (NormedScalar a) => MatMul       (ModelMat a)  (Conj (ModelMat a)) (ModelMat a) where (@@) = defaultMulMM
instance (NormedScalar a) => MatMul (Tr   (ModelMat a)) (Conj (ModelMat a)) (ModelMat a) where (@@) = defaultMulMM
instance (NormedScalar a) => MatMul (Conj (ModelMat a)) (Conj (ModelMat a)) (ModelMat a) where (@@) = defaultMulMM

-- | Default model implementation of matrix-matrix multiplication
defaultMulMM :: (Num a, IsModelMat m1 a, IsModelMat m2 a) => m1 -> m2 -> ModelMat a
defaultMulMM m1 m2 = ModelMat 0 0 $ unModelMat (toModelMat m1) !*! unModelMat (toModelMat m2)


----------------------------------------------------------------
-- Model for dense matrices
----------------------------------------------------------------

data ModelMat a = ModelMat
  { padRows    :: !Int
  , padCols    :: !Int
  , unModelMat :: [[a]]
  }
  deriving stock (Show,Eq)

modelMatCols :: ModelMat a -> Int
modelMatCols m = case unModelMat m of
  []  -> 0
  x:_ -> length x

instance HasShape (ModelMat a) where
  type NDim (ModelMat a) = 2
  shapeAsCVec ModelMat{unModelMat=mat} = FC.mk2 (length mat) (length (head mat))

instance (ScalarModel a, Eq a) => Arbitrary (ModelMat a) where
  arbitrary = arbitraryShape =<< genSize @(Int,Int)
  shrink mat0@ModelMat{..} = do
    p_r <- case padRows of 0 -> [0]; _ -> [0,padRows]
    p_c <- case padCols of 0 -> [0]; _ -> [0,padCols]
    p_elem <- (traverse . traverse) shrinkScalar unModelMat
    let mat = ModelMat p_r p_c p_elem
    guard (mat /= mat0)
    return mat

instance (Eq a, ScalarModel a) => ArbitraryShape (ModelMat a) where
  arbitraryShape (N2 m n)
     = ModelMat
    <$> genOffset
    <*> genOffset
    <*> replicateM m (replicateM n genScalar)

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

instance (IsModel v) => Arbitrary (Pair v) where
  arbitrary = do
    sz <- genSize @(FC.ContVec _ _)
    Pair <$> arbitraryShape sz <*> arbitraryShape sz


-- | Scalar which uses @ScalarModel@ for generation
newtype X a = X a
  deriving newtype Show

instance ScalarModel a => Arbitrary (X a) where
  arbitrary = X <$> genScalar


-- | Generate matrices with correct size for multiplication
newtype MM a b = MM { unMM :: (a,b) }

instance (Show a, Show b) => Show (MM a b) where
  show (MM (a,b)) = show a ++ "\n" ++ show b

instance (NDim a ~ 2, NDim b ~ 2, ArbitraryShape a, ArbitraryShape b) => Arbitrary (MM a b) where
  arbitrary = do
    (n,k,m) <- genSize
    MM <$> ((,) <$> arbitraryShape (n,k) <*> arbitraryShape (k,m))

-- | Generate matrix and vector with correct size for multiplication
newtype MV a b = MV { unMV :: (a,b) }

instance (Show a, Show b) => Show (MV a b) where
  show (MV (a,b)) = show a ++ "\n" ++ show b

instance (NDim a ~ 2, NDim b ~ 1, ArbitraryShape a, ArbitraryShape b) => Arbitrary (MV a b) where
  arbitrary = do
    (n,k) <- genSize
    MV <$> ((,) <$> arbitraryShape (n,k) <*> arbitraryShape k)
