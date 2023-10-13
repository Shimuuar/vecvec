{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DerivingVia                #-}
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
{-# OPTIONS_GHC -Wno-orphans #-}
-- |
-- Tools for writing tests for linear algebra.
module TST.Tools.MatModel
  ( -- * Arbitrary extensions
    -- ** Scalars
    SmallScalar(..)
  , X(..)
    -- ** ND-arrays
  , ArbitraryShape(..)
  , genSize
    -- ** Matrices
  , Nonsingular(..)
  , genNonsingularMatrix
    -- * Models
  , TestMatrix
  , ModelM
  , fromModel
  , eq
  , eqV
  , mdl
  , val
  , ModelVec(..)
  , ModelMat(..)
    -- * Helpers
  , Pair(..)
  ) where

import Control.Monad
import Data.Complex          (Complex(..))
import Data.Function         (on)
import Data.Typeable
import Data.List             (transpose)
import Foreign.Storable      (Storable)
import Linear.Matrix         ((!*), (!*!))
import Test.Tasty.QuickCheck

import Data.Vector.Fixed.Cont qualified as FC
import Data.Vector.Generic    qualified as VG
import Data.Vector            qualified as V
import Data.Vector.Unboxed    qualified as VU
import Data.Vector.Storable   qualified as VS

import Vecvec.Classes
import Vecvec.Classes.NDArray
import Vecvec.LAPACK                      (Strided(..))
import Vecvec.LAPACK                       qualified as VV
import Vecvec.LAPACK.Internal.Matrix.Dense (Matrix, fromRowsFF)
import Vecvec.LAPACK.Internal.Matrix.Dense qualified as Mat
import TST.Tools.Util
import TST.Tools.Orphanage ()
import TST.Tools.Model

----------------------------------------------------------------
-- Scalar-related utils
----------------------------------------------------------------

-- | We want to test that multiplication return correct result without
--   going into dark forest of floating point. This is achieved by
--   exploiting fact that multiplication and addition small integers
--   are exact in floating point.
class Arbitrary a => SmallScalar a where
  genScalar    :: Gen a
  shrinkScalar :: a -> [a]

instance SmallScalar Float where
  genScalar = fromIntegral <$> choose @Int (-maxGenScacar, maxGenScacar)
  shrinkScalar = \case
    0 -> []
    1 -> [0]
    _ -> [0,1]

instance SmallScalar Double where
  genScalar = fromIntegral <$> choose @Int (-maxGenScacar, maxGenScacar)
  shrinkScalar = \case
    0 -> []
    1 -> [0]
    _ -> [0,1]

instance (RealFloat a, SmallScalar a) => SmallScalar (Complex a) where
  genScalar = (:+) <$> genScalar <*> genScalar
  shrinkScalar = \case
    0      -> []
    1      -> [0, 0:+1]
    0 :+ 1 -> [0, 1]
    _      -> [0, 1, 0:+1]

maxGenScacar :: Num a => a
maxGenScacar = 10

-- | Scalar which uses @SmallScalar@ for generation
newtype X a = X a
  deriving newtype (Show, TestEquiv)

deriving via ModelSelf (X a) instance TestData t (X a)
instance SmallScalar a => Arbitrary (X a) where
  arbitrary = X <$> genScalar



----------------------------------------------------------------
-- ND-arrays
----------------------------------------------------------------

-- | Generate random NDarray with specified shape
class (Arbitrary a, HasShape a) => ArbitraryShape a where
  arbitraryShape :: (IsShape shape (NDim a)) => shape -> Gen a

-- | Generate size for N-dimensional array
genSize :: forall shape n. (FC.Arity n, IsShape shape n) => Gen shape
genSize = shapeFromCVec <$> FC.replicateM @n (choose (1,10))

-- | Generate stride for vectors
genStride :: Gen Int
genStride = choose (1,3)

-- | Generate offset for matrices
genOffset :: Gen Int
genOffset = choose (0,3)


----------------------------------------------------------------
-- Matrices
----------------------------------------------------------------

newtype Nonsingular a = Nonsingular { getNonsingular :: Matrix a }
  deriving newtype Show

instance (SmallScalar a, VV.LAPACKy a, Typeable a, Show a, Eq a) => Arbitrary (Nonsingular a) where
  arbitrary = Nonsingular <$> (genNonsingularMatrix =<< genSize)

-- | Generate nonsingular square matrix. In order to ensure
--   nonsingularity we generate matrix with diagonal dominance
genNonsingularMatrix
  :: (SmallScalar a, VV.LAPACKy a, Eq a)
  => Int -> Gen (Matrix a)
genNonsingularMatrix sz = do
  mat <- arbitraryShape (sz,sz)
  pure $  (2 * maxGenScacar * fromIntegral sz) *. Mat.eye sz
      .+. mat


----------------------------------------------------------------
-- Models
----------------------------------------------------------------

eq :: (TestEquiv a, TestMatrix a) => a -> ModelM a -> Property
eq a m = property $ equiv a (fromModel m)

eqV :: (TestEquiv a, TestMatrix a, Show a, Show (ModelM a)) => a -> ModelM a -> Property
eqV a m
  = counterexample ("MODEL = " ++ show m)
  $ counterexample ("IMPL  = " ++ show a)
  $ property $ equiv a (fromModel m)

mdl :: forall a b c r. (TestMatrix a)
    => (b -> c -> r) -> (a -> b) -> (ModelM a -> c) -> (ModelM a -> r)
mdl eqv f g = \m -> f (fromModel m) `eqv` g m

val :: forall a b c r. (TestMatrix a)
    => (b -> c -> r) -> (a -> b) -> (ModelM a -> c) -> (a -> r)
val eqv f g = \a -> f a `eqv` g (model TagMat a)

-- | Tag for testing @vecvec@ routines
data TagMat = TagMat

type TestMatrix = TestData TagMat

type ModelM a = Model TagMat a

fromModel :: TestData TagMat a => Model TagMat a -> a
fromModel = unmodel TagMat


instance (Storable a, Num a) => TestData TagMat (VV.Vec a) where
  type Model TagMat (VV.Vec a) = ModelVec a
  unmodel _ (ModelVec stride xs)
    = slice ((0,End) `Strided` stride)
    $ VG.fromList
    $ (\n -> n : replicate (stride-1) 0) =<< xs
  model _ xs = ModelVec { modelVecStride = 1
                        , unModelVec     = VG.toList xs
                        }

instance TestData TagMat (V.Vector a) where
  type Model TagMat (V.Vector a) = ModelVec a
  unmodel _ = VG.fromList . unModelVec
  model _ xs = ModelVec { modelVecStride = 1
                        , unModelVec     = VG.toList xs
                        }

instance (VU.Unbox a) => TestData TagMat (VU.Vector a) where
  type Model TagMat (VU.Vector a) = ModelVec a
  unmodel _ = VG.fromList . unModelVec
  model _ xs = ModelVec { modelVecStride = 1
                        , unModelVec     = VG.toList xs
                        }

instance (Storable a) => TestData TagMat (VS.Vector a) where
  type Model TagMat (VS.Vector a) = ModelVec a
  unmodel _ = VG.fromList . unModelVec
  model _ xs = ModelVec { modelVecStride = 1
                        , unModelVec     = VG.toList xs
                        }

instance (Storable a, Num a) => TestData TagMat (Matrix a) where
  type Model TagMat (Matrix a) = ModelMat a
  unmodel _ m@ModelMat{unModelMat=mat, ..}
    = slice ((padRows,End), (padCols,End))
    $ fromRowsFF
    $ replicate padRows (replicate (nC + padCols) 0)
   ++ map (replicate padCols 0 ++) mat
    where
      nC = nCols m
  model _ m = ModelMat
    { padRows    = 0
    , padCols    = 0
    , unModelMat = VG.toList <$> Mat.toRowList m
    }

instance (Storable a, Eq a) => TestEquiv (Matrix a) where
  equiv = (==)

----------------------------------------------------------------
-- Model for vectors
----------------------------------------------------------------

-- | We use lists as model for vectors.
data ModelVec a = ModelVec
  { modelVecStride :: !Int -- ^ Stride for vector. Ignored if not supported
  , unModelVec     :: [a]  -- ^ Data for vectors
  }
  deriving stock (Show)

instance HasShape (ModelVec a) where
  type NDim (ModelVec a) = 1
  shapeAsCVec = FC.mk1 . length . unModelVec

instance Num a => AdditiveSemigroup (ModelVec a) where
  a .+. b = ModelVec 1 $ (zipWithX (+) `on` unModelVec) a b

instance Num a => AdditiveQuasigroup (ModelVec a) where
  a .-. b = ModelVec 1 $ (zipWithX (-) `on` unModelVec) a b
  negateV = ModelVec 1 . map negate . unModelVec

instance Num a => VectorSpace (ModelVec a) where
  type Scalar (ModelVec a) = a
  a *. ModelVec _ xs = ModelVec 1 $ fmap (a*) xs
  ModelVec _ xs .* a = ModelVec 1 $ fmap (*a) xs

instance NormedScalar a => InnerSpace (ModelVec a) where
  ModelVec _ xs <.> ModelVec _ ys = sum $ zipWithX (\x y -> conjugate x * y) xs ys
  magnitudeSq (ModelVec _ xs) = sum $ scalarNormSq <$> xs



----------------------------------------------------------------
-- Model for general dense matrix
----------------------------------------------------------------

-- | Model for general dense matrices
data ModelMat a = ModelMat
  { padRows    :: !Int  -- ^ Extra padding for rows
  , padCols    :: !Int  -- ^ Extra padding for columns
  , unModelMat :: [[a]]
  }
  deriving stock (Show,Eq)

instance HasShape (ModelMat a) where
  type NDim (ModelMat a) = 2
  shapeAsCVec ModelMat{unModelMat=mat} = FC.mk2 (length mat) (length (head mat))

instance Num a => AdditiveSemigroup (ModelMat a) where
  a .+. b = ModelMat 0 0 $ ((zipWithX . zipWithX) (+) `on` unModelMat) a b

instance Num a => AdditiveQuasigroup (ModelMat a) where
  a .-. b = ModelMat 0 0 $ ((zipWithX . zipWithX) (-) `on` unModelMat) a b
  negateV = ModelMat 0 0 . ((map . map) negate) . unModelMat

instance Num a => VectorSpace (ModelMat a) where
  type Scalar (ModelMat a) = a
  a *. ModelMat _ _ xs = ModelMat 0 0 $ (fmap . fmap) (a*) xs
  ModelMat _ _ xs .* a = ModelMat 0 0 $ (fmap . fmap) (*a) xs


-- | Data types which could be converted to matrix model
class IsModelMat m a where
  toModelMat :: m -> ModelMat a

instance a ~ a' => IsModelMat (ModelMat a) a' where
  toModelMat = id
instance a ~ a' => IsModelMat (Tr (ModelMat a)) a' where
  toModelMat (Tr m) = (ModelMat 0 0 . transpose . unModelMat) m
instance (NormedScalar a, a ~ a') => IsModelMat (Conj (ModelMat a)) a' where
  toModelMat (Conj m) = (ModelMat 0 0 . (fmap . fmap) conjugate . transpose . unModelMat) m


instance (NormedScalar a) => MatMul       (ModelMat a)  (ModelVec a) (ModelVec a) where (@@) = defaultMulMV
instance (NormedScalar a) => MatMul (Tr   (ModelMat a)) (ModelVec a) (ModelVec a) where (@@) = defaultMulMV
instance (NormedScalar a) => MatMul (Conj (ModelMat a)) (ModelVec a) (ModelVec a) where (@@) = defaultMulMV

instance (NormedScalar a) => MatMul       (ModelMat a)        (ModelMat a)  (ModelMat a) where (@@) = defaultMulMM
instance (NormedScalar a) => MatMul (Tr   (ModelMat a))       (ModelMat a)  (ModelMat a) where (@@) = defaultMulMM
instance (NormedScalar a) => MatMul (Conj (ModelMat a))       (ModelMat a)  (ModelMat a) where (@@) = defaultMulMM
instance (NormedScalar a) => MatMul       (ModelMat a)  (Tr   (ModelMat a)) (ModelMat a) where (@@) = defaultMulMM
instance (NormedScalar a) => MatMul (Tr   (ModelMat a)) (Tr   (ModelMat a)) (ModelMat a) where (@@) = defaultMulMM
instance (NormedScalar a) => MatMul (Conj (ModelMat a)) (Tr   (ModelMat a)) (ModelMat a) where (@@) = defaultMulMM
instance (NormedScalar a) => MatMul       (ModelMat a)  (Conj (ModelMat a)) (ModelMat a) where (@@) = defaultMulMM
instance (NormedScalar a) => MatMul (Tr   (ModelMat a)) (Conj (ModelMat a)) (ModelMat a) where (@@) = defaultMulMM
instance (NormedScalar a) => MatMul (Conj (ModelMat a)) (Conj (ModelMat a)) (ModelMat a) where (@@) = defaultMulMM

-- Default model implementation of matrix-matrix multiplication
defaultMulMM :: (Num a, IsModelMat m1 a, IsModelMat m2 a) => m1 -> m2 -> ModelMat a
defaultMulMM m1 m2 = ModelMat 0 0 $ unModelMat (toModelMat m1) !*! unModelMat (toModelMat m2)

-- Default model implementation of matrix-vector multiplication
defaultMulMV :: (Num a, IsModelMat m a) => m -> ModelVec a -> ModelVec a
defaultMulMV m v = ModelVec 1 $ (unModelMat . toModelMat) m !* unModelVec v


----------------------------------------------------------------
-- Helpers
----------------------------------------------------------------

-- | Pair of models with same size
data Pair v = Pair v v
  deriving stock    (Show,Functor)

deriving via ModelFunctor Pair a instance TestData t a => TestData t (Pair a)

instance (ArbitraryShape v) => Arbitrary (Pair v) where
  arbitrary = do
    sz <- genSize @(FC.ContVec _ _)
    Pair <$> arbitraryShape sz <*> arbitraryShape sz


----------------------------------------------------------------
-- Orphans & Arbitrary
----------------------------------------------------------------

instance (NDim a ~ 2, ArbitraryShape a) => Arbitrary (Tr a) where
  arbitrary = arbitraryShape =<< genSize @(Int,Int)

instance (NDim a ~ 2, ArbitraryShape a) => Arbitrary (Conj a) where
  arbitrary = arbitraryShape =<< genSize @(Int,Int)

instance (NDim a ~ 2, ArbitraryShape a) => ArbitraryShape (Tr a) where
  arbitraryShape (N2 n k) = Tr <$> arbitraryShape (k,n)

instance (NDim a ~ 2, ArbitraryShape a) => ArbitraryShape (Conj a) where
  arbitraryShape (N2 n k) = Conj <$> arbitraryShape (k,n)


----------------------------------------
-- Matrix

instance (SmallScalar a, Eq a) => Arbitrary (ModelMat a) where
  arbitrary = arbitraryShape =<< genSize @(Int,Int)
  shrink mat0@ModelMat{..} = do
    p_r <- case padRows of 0 -> [0]; _ -> [0,padRows]
    p_c <- case padCols of 0 -> [0]; _ -> [0,padCols]
    p_elem <- (traverse . traverse) shrinkScalar unModelMat
    let mat = ModelMat p_r p_c p_elem
    guard (mat /= mat0)
    return mat

instance (Eq a, SmallScalar a) => ArbitraryShape (ModelMat a) where
  arbitraryShape (N2 m n)
     =  ModelMat
    <$> genOffset
    <*> genOffset
    <*> replicateM m (replicateM n genScalar)

instance (SmallScalar a, Storable a, Num a, Eq a
         ) => Arbitrary (Matrix a) where
  arbitrary = arbitraryShape =<< genSize @(Int,Int)

instance (SmallScalar a, Storable a, Num a, Eq a
         ) => ArbitraryShape (Matrix a) where
  arbitraryShape sz = fromModel <$> arbitraryShape sz


----------------------------------------
-- Vectors

instance SmallScalar a => Arbitrary (ModelVec a) where
  arbitrary = arbitraryShape =<< genSize @Int
  shrink (ModelVec n xs) = do
    n' <- case n of 1 -> [1]
                    _ -> [1,n]
    x  <- shrinkList (const []) xs
    return $ ModelVec n' x

instance SmallScalar a => ArbitraryShape (ModelVec a) where
  arbitraryShape (N1 n) = ModelVec <$> genStride <*> replicateM n genScalar
