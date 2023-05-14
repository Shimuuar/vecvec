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

import Control.Monad
import Data.Coerce
import Data.Complex          (Complex(..))
import Data.Kind             (Type)
import Data.Function         (on)
import Data.Typeable
import Data.List             (intercalate,transpose)
import Foreign.Storable      (Storable)
import Linear.Matrix         ((!*), (!*!))
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
import Vecvec.LAPACK.FFI     (S,D,C,Z)
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
    [ prop_matmul_scal @(Tr (V.Vector  Double)) @(V.Vector  Double)
    , prop_matmul_scal @(Tr (VS.Vector Double)) @(VS.Vector Double)
    , prop_matmul_scal @(Tr (VU.Vector Double)) @(VU.Vector Double)
    , prop_matmul_scal @(Conj (V.Vector  Z)) @(V.Vector  Z)
    , prop_matmul_scal @(Conj (VS.Vector Z)) @(VS.Vector Z)
    , prop_matmul_scal @(Conj (VU.Vector Z)) @(VU.Vector Z)
      -- BLAS vectors
    , prop_matmul_scal @(Tr   (VV.Vec S)) @(VV.Vec S)
    , prop_matmul_scal @(Tr   (VV.Vec D)) @(VV.Vec D)
    , prop_matmul_scal @(Tr   (VV.Vec C)) @(VV.Vec C)
    , prop_matmul_scal @(Tr   (VV.Vec Z)) @(VV.Vec Z)
    , prop_matmul_scal @(Conj (VV.Vec S)) @(VV.Vec S)
    , prop_matmul_scal @(Conj (VV.Vec D)) @(VV.Vec D)
    , prop_matmul_scal @(Conj (VV.Vec C)) @(VV.Vec C)
    , prop_matmul_scal @(Conj (VV.Vec Z)) @(VV.Vec Z)
      -- Matrix-vector
    , prop_matmul @(Matrix S)        @(VV.Vec S)
    , prop_matmul @(Matrix D)        @(VV.Vec D)
    , prop_matmul @(Matrix C)        @(VV.Vec C)
    , prop_matmul @(Matrix Z)        @(VV.Vec Z)
    , prop_matmul @(Tr (Matrix S))   @(VV.Vec S)
    , prop_matmul @(Tr (Matrix D))   @(VV.Vec D)
    , prop_matmul @(Tr (Matrix C))   @(VV.Vec C)
    , prop_matmul @(Tr (Matrix Z))   @(VV.Vec Z)
    , prop_matmul @(Conj (Matrix S)) @(VV.Vec S)
    , prop_matmul @(Conj (Matrix D)) @(VV.Vec D)
    , prop_matmul @(Conj (Matrix C)) @(VV.Vec C)
    , prop_matmul @(Conj (Matrix Z)) @(VV.Vec Z)
      -- Matrix-matrix
    , prop_matmul @(Matrix S)        @(Matrix S)
    , prop_matmul @(Matrix D)        @(Matrix D)
    , prop_matmul @(Matrix C)        @(Matrix C)
    , prop_matmul @(Matrix Z)        @(Matrix Z)
    , prop_matmul @(Tr (Matrix S))   @(Matrix S)
    , prop_matmul @(Tr (Matrix D))   @(Matrix D)
    , prop_matmul @(Tr (Matrix C))   @(Matrix C)
    , prop_matmul @(Tr (Matrix Z))   @(Matrix Z)
    , prop_matmul @(Conj (Matrix S)) @(Matrix S)
    , prop_matmul @(Conj (Matrix D)) @(Matrix D)
    , prop_matmul @(Conj (Matrix C)) @(Matrix C)
    , prop_matmul @(Conj (Matrix Z)) @(Matrix Z)
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


-- Test for generalized matrix-vector multiplication.
prop_matmul
  :: forall v1 v2 vR.
     ( IsModel v1
     , IsModel v2
     , IsModel vR
     , MatMul (ModelRepr v1) (ModelRepr v2) (ModelRepr vR)
     , MatMul v1 v2 vR
     , Arbitrary (MM (ModelRepr v1) (ModelRepr v2))
     )
  => TestTree
prop_matmul
  = testProperty (prop_name @v1 ++ " " ++ prop_name @v2)
  $ \(MM mr1 mr2) ->
      let m1 = Model mr1    :: Model v1
          m2 = Model mr2    :: Model v2
          v1 = fromModel m1 :: v1
          v2 = fromModel m2 :: v2
          m  = mr1 @@ mr2
          v  = v1  @@ v2
      in id $ counterexample ("MODEL = " ++ show m)
            $ counterexample ("IMPL  = " ++ show v)
            $ v == fromModel (Model m)

-- Test for generalized matrix-vector multiplication when result is scalar
prop_matmul_scal
  :: forall v1 v2 r.
     ( IsModel v1
     , IsModel v2
     , MatMul (ModelRepr v1) (ModelRepr v2) r
     , MatMul v1 v2 r
     , Arbitrary (MM (ModelRepr v1) (ModelRepr v2))
     , Eq r
     )
  => TestTree
prop_matmul_scal
  = testProperty (prop_name @v1 ++ " " ++ prop_name @v2)
  $ \(MM mr1 mr2) ->
      let m1 = Model mr1    :: Model v1
          m2 = Model mr2    :: Model v2
          v1 = fromModel m1 :: v1
          v2 = fromModel m2 :: v2
          m  = mr1 @@ mr2
          v  = v1  @@ v2
      in v == m


----------------------------------------------------------------
-- Models
----------------------------------------------------------------

-- | Generate stride for vectors
genStride :: Gen Int
genStride = choose (1,3)

-- | Generate offset for matrices
genOffset :: Gen Int
genOffset = choose (0,3)

-- | Generate value with same size
class Arbitrary a => GenSameSize a where
  type Shape a
  shape :: a -> Shape a
  withSize :: Shape a -> Gen a

sameSize :: GenSameSize a => a -> Gen a
sameSize = withSize . shape


instance GenSameSize Float  where
  type Shape Float = ()
  shape    _ = ()
  withSize _ = genScalar
instance GenSameSize Double where
  type Shape Double = ()
  shape    _ = ()
  withSize _ = genScalar
instance ScalarModel a => GenSameSize (Complex a) where
  type Shape (Complex a) = ()
  shape    _ = ()
  withSize _ = genScalar


deriving newtype instance GenSameSize a => GenSameSize (Tr   a)
deriving newtype instance GenSameSize a => GenSameSize (Conj a)

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
  fromModel (Model m@ModelMat{unModelMat=mat, ..})
    = slice ((padRows,End), (padCols,End))
    $ fromRowsFF
    $ replicate padRows (replicate (nC + padCols) 0)
   ++ map (replicate padCols 0 ++) mat
    where
      nC = modelMatCols m


instance IsModel a => IsModel (Tr a) where
  type ModelRepr (Tr a) = Tr (ModelRepr a)
  fromModel (Model (Tr a)) = Tr $ fromModel (Model a)

instance IsModel a => IsModel (Conj a) where
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

instance ScalarModel a => Arbitrary (ModelVec a) where
  arbitrary = ModelVec <$> genStride <*> listOf genScalar
  shrink (ModelVec n xs) = do
    n' <- case n of 1 -> [1]
                    _ -> [1,n]
    x  <- shrinkList (const []) xs
    return $ ModelVec n' x

instance ScalarModel a => GenSameSize (ModelVec a) where
  type Shape (ModelVec a) = Int
  shape = length . unModelVec
  withSize n = ModelVec <$> genStride <*> replicateM n genScalar

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

instance (a ~ Scalar a, VectorSpace a, Num a) => MatMul (Tr (ModelVec a)) (ModelVec a) a where
  Tr a @@ b = sum $ (zipWith (*) `on` unModelVec) a b

instance (a ~ Scalar a, VectorSpace a, NormedScalar a) => MatMul (Conj (ModelVec a)) (ModelVec a) a where
  Conj a @@ b = sum $ (zipWith (\x y -> conjugate x * y) `on` unModelVec) a b

instance (Num a) => MatMul (ModelMat a) (ModelVec a) (ModelVec a) where
  m @@ v = ModelVec 1 $ unModelMat m !* unModelVec v

instance (Num a) => MatMul (Tr (ModelMat a)) (ModelVec a) (ModelVec a) where
  Tr m @@ v = ModelVec 1 $ transpose (unModelMat m) !* unModelVec v

instance (NormedScalar a) => MatMul (Conj (ModelMat a)) (ModelVec a) (ModelVec a) where
  Conj m @@ v = ModelVec 1 $ m' !* unModelVec v
    where m' = (fmap . fmap) conjugate $ transpose $ unModelMat m


instance (Num a) => MatMul (ModelMat a) (ModelMat a) (ModelMat a) where
  m @@ v = ModelMat 0 0 $ unModelMat m !*! unModelMat v

instance (Num a) => MatMul (Tr (ModelMat a)) (ModelMat a) (ModelMat a) where
  Tr m @@ v = ModelMat 0 0 $ m' !*! unModelMat v
    where m' = transpose $ unModelMat m

instance (NormedScalar a) => MatMul (Conj (ModelMat a)) (ModelMat a) (ModelMat a) where
  Conj m @@ v = ModelMat 0 0 $ m' !*! unModelMat v
    where
      m' = (fmap . fmap) conjugate $ transpose $ unModelMat m


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

modelMatRows :: ModelMat a -> Int
modelMatRows = length . unModelMat

instance ScalarModel a => Arbitrary (ModelMat a) where
  arbitrary = do
    cols <- arbitrary
    row  <- mapM (const genScalar) (() : cols)
    rows <- listOf $ unModelVec <$> sameSize (ModelVec 1 row)
    r    <- genOffset
    c    <- genOffset
    pure $ ModelMat r c (row:rows)
  shrink ModelMat{..} = do
    p_r <- case padRows of 0 -> [0]; _ -> [0,padRows]
    p_c <- case padCols of 0 -> [0]; _ -> [0,padCols]
    [ModelMat{ padRows = p_r, padCols = p_c, .. }]

instance ScalarModel a => GenSameSize (ModelMat a) where
  type Shape (ModelMat a) = (Int,Int)
  shape m = (modelMatRows m, modelMatCols m)
  withSize (m,n) = ModelMat
    <$> genOffset <*> genOffset
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

instance IsModel v => Arbitrary (Pair v) where
  arbitrary = do m1 <- arbitrary
                 m2 <- sameSize m1
                 pure $ Pair m1 m2

-- | Scalar which uses @ScalarModel@ for generation
newtype X a = X a
  deriving newtype Show

instance ScalarModel a => Arbitrary (X a) where
  arbitrary = X <$> genScalar


data MM a b = MM a b

instance (Show a, Show b) => Show (MM a b) where
  show (MM a b) = show a ++ "\n" ++ show b

instance (ScalarModel a) => Arbitrary (MM (Tr (ModelVec a)) (ModelVec a)) where
  arbitrary = do
    Tr a <- arbitrary
    b    <- sameSize a
    pure $ MM (Tr a) b

instance (ScalarModel a) => Arbitrary (MM (Conj (ModelVec a)) (ModelVec a)) where
  arbitrary = do
    Conj a <- arbitrary
    b      <- sameSize a
    pure $ MM (Conj a) b

instance (ScalarModel a) => Arbitrary (MM (ModelMat a) (ModelVec a)) where
  arbitrary = do
    m <- arbitrary
    v <- withSize (modelMatCols m)
    pure $ MM m v

instance (ScalarModel a) => Arbitrary (MM (Tr (ModelMat a)) (ModelVec a)) where
  arbitrary = do
    Tr m <- arbitrary
    v    <- withSize (modelMatRows m)
    pure $ MM (Tr m) v

instance (ScalarModel a) => Arbitrary (MM (Conj (ModelMat a)) (ModelVec a)) where
  arbitrary = do
    Conj m <- arbitrary
    v      <- withSize (modelMatRows m)
    pure $ MM (Conj m) v

instance (ScalarModel a) => Arbitrary (MM (ModelMat a) (ModelMat a)) where
  arbitrary = do
    mA <- arbitrary
    k  <- sized $ \n -> chooseInt (1, 1+n)
    mB <- withSize (modelMatCols mA, k)
    pure $ MM mA mB

instance (ScalarModel a) => Arbitrary (MM (Tr (ModelMat a)) (ModelMat a)) where
  arbitrary = do
    mA <- arbitrary
    k  <- sized $ \n -> chooseInt (1, 1+n)
    mB <- withSize (modelMatRows mA, k)
    pure $ MM (Tr mA) mB

instance (ScalarModel a) => Arbitrary (MM (Conj (ModelMat a)) (ModelMat a)) where
  arbitrary = do
    mA <- arbitrary
    k  <- sized $ \n -> chooseInt (1, 1+n)
    mB <- withSize (modelMatRows mA, k)
    pure $ MM (Conj mA) mB
