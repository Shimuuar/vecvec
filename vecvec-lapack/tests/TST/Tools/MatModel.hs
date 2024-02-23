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
  , Size2D(..)
  , genSize
    -- ** Matrices
  , Nonsingular(..)
  , genNonsingularMatrix
    -- * Models
  , TestMatrix
  , TestMatrix1
  , ModelM
  , Model1M
  , TagMat(..)
  , fromModel
  , plainEq
  , val
  , eq1
  , eq1V
  , mdl1
  , (=~=)
  , ModelVec(..)
  , ModelMat(..)
  , ModelSym(..)
  , mkMat
    -- * Helpers
  , Pair(..)
  , Pair1(..)
  ) where

import Control.Monad
import Data.Complex          (Complex(..))
import Data.Function         (on)
import Data.Typeable
import Data.List             (transpose)
import Foreign.Storable      (Storable)
import Linear.Matrix         ((!*), (!*!))
import Test.Tasty.QuickCheck
import GHC.TypeLits          (Nat)

import Data.Vector.Fixed.Cont qualified as FC
import Data.Vector.Generic    qualified as VG
import Data.Vector            qualified as V
import Data.Vector.Unboxed    qualified as VU
import Data.Vector.Storable   qualified as VS

import Vecvec.Classes
import Vecvec.Classes.NDArray
import Vecvec.LAPACK                           (Strided(..))
import Vecvec.LAPACK                           qualified as VV
import Vecvec.LAPACK.Internal.Matrix.Dense     (Matrix, fromRowsFF)
import Vecvec.LAPACK.Internal.Matrix.Dense     qualified as Mat
import Vecvec.LAPACK.Internal.Matrix.Symmetric (Symmetric)
import Vecvec.LAPACK.Internal.Matrix.Symmetric qualified as Sym
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
class (Arbitrary a, FC.Arity (CreationRank arr)) => ArbitraryShape arr a where
  type CreationRank arr :: Nat
  type CreationRank arr = Rank arr
  arbitraryShape :: (IsShape shape (CreationRank arr)) => shape -> Gen (arr a)

newtype Size2D = Size2D { getSize2D :: (Int,Int) }
  deriving stock Show

deriving via ModelSelf Size2D instance TestData t Size2D
instance Arbitrary Size2D where
  arbitrary = Size2D <$> ((,) <$> genSize <*> genSize)

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
  :: (SmallScalar a, VV.LAPACKy a)
  => Int -> Gen (Matrix a)
genNonsingularMatrix sz = do
  mat <- arbitraryShape (sz,sz)
  pure $  (2 * maxGenScacar * fromIntegral sz) *. Mat.eye sz
      .+. mat


----------------------------------------------------------------
-- Models
----------------------------------------------------------------

-- | Compare two values using equivalence
plainEq :: (TestEquiv a) => a -> a -> Property
plainEq a b = property $ equiv a b

-- | Compare value and its model using equivalence
eq1 :: forall v a. (TestEquiv (v a), TestMatrix1 v a)
    => v a         -- ^ Implementation
    -> Model1M v a -- ^ Model
    -> Property
eq1 a m = property $ equiv a (liftUnmodel TagMat id m)

-- | Verbose variant of 'eq1'
eq1V :: forall v a. (TestEquiv (v a), TestMatrix1 v a, Show (v a), Show (Model1M v a))
     => v a         -- ^ Implementation
     -> Model1M v a -- ^ Model
     -> Property
eq1V a m
  = counterexample ("MODEL = " ++ show m)
  $ counterexample ("IMPL  = " ++ show a)
  $ property $ equiv a (liftUnmodel TagMat id m)

-- | Create compare function where both implementation under test and
--   model take same arguments
val :: forall a impl model. (Arbitrary a, Show a)
    => (impl -> model -> Property) -- ^ Rest of comparator
    -> (a -> impl)                 -- ^ Implementation
    -> (a -> model)                -- ^ Model
    -> Property
val eqv f g = property $ \a -> f a `eqv` g a

-- | Create comparator for implementation and model where we use input
--   for model to generate samples
mdl1 :: forall v a impl model. (TestMatrix1 v a, Arbitrary (Model1M v a), Show (Model1M v a))
     => (impl -> model -> Property) -- ^ Rest of the comparator
     -> (v a -> impl)               -- ^ Implementation
     -> (Model1M v a -> model)      -- ^ Model
     -> Property
mdl1 eqv f g = property $ \m -> f (liftUnmodel TagMat id m) `eqv` g m


(=~=) :: LiftTestEq TagMat a => a -> ModelM a -> P a
(=~=) = equivalent TagMat
infix 4 =~=

-- | Tag for testing @vecvec@ routines
data TagMat = TagMat

type TestMatrix  = TestData TagMat
type TestMatrix1 = TestData1 TagMat

type ModelM a = Model TagMat a

type Model1M v = Model1 TagMat v

fromModel :: TestData TagMat a => Model TagMat a -> a
fromModel = unmodel TagMat


type instance Model1 TagMat VV.Vec    = ModelVec
type instance Model1 TagMat V.Vector  = ModelVec
type instance Model1 TagMat VS.Vector = ModelVec
type instance Model1 TagMat VU.Vector = ModelVec

instance (Storable a, Num a) => TestData1 TagMat VV.Vec a where
  liftUnmodel _ f (ModelVec stride xs)
    = slice ((0,End) `Strided` stride)
    $ VG.fromList
    $ (\n -> n : replicate (stride-1) 0) =<< (f <$> xs)
  liftModel _ f xs = ModelVec { modelVecStride = 1
                              , unModelVec     = f <$> VG.toList xs
                              }

instance TestData1 TagMat V.Vector a where
  liftUnmodel _ f  = VG.fromList . map f . unModelVec
  liftModel _ f xs = ModelVec { modelVecStride = 1
                              , unModelVec     = f <$> VG.toList xs
                              }

instance (VU.Unbox a) => TestData1 TagMat VU.Vector a where
  liftUnmodel _ f  = VG.fromList . map f .unModelVec
  liftModel _ f xs = ModelVec { modelVecStride = 1
                              , unModelVec     = f <$> VG.toList xs
                              }

instance (Storable a) => TestData1 TagMat VS.Vector a where
  liftUnmodel _ f  = VG.fromList . map f .unModelVec
  liftModel _ f xs = ModelVec { modelVecStride = 1
                              , unModelVec     = f <$> VG.toList xs
                              }





instance (Storable a, Num a) => TestData TagMat (VV.Vec a) where
  type Model TagMat (VV.Vec a) = ModelVec a
  unmodel t = liftUnmodel t id
  model   t = liftModel   t id

instance TestData TagMat (V.Vector a) where
  type Model TagMat (V.Vector a) = ModelVec a
  unmodel t = liftUnmodel t id
  model   t = liftModel   t id

instance (VU.Unbox a) => TestData TagMat (VU.Vector a) where
  type Model TagMat (VU.Vector a) = ModelVec a
  unmodel t = liftUnmodel t id
  model   t = liftModel   t id

instance (Storable a) => TestData TagMat (VS.Vector a) where
  type Model TagMat (VS.Vector a) = ModelVec a
  unmodel t = liftUnmodel t id
  model   t = liftModel   t id


type instance Model1 TagMat Matrix = ModelMat

instance (Storable a, Num a) => TestData1 TagMat Matrix a where
  liftUnmodel _ f m@ModelMat{unModelMat=mat, ..}
    = slice ((padRows,End), (padCols,End))
    $ fromRowsFF
    $ replicate padRows (replicate (nC + padCols) 0)
   ++ map (replicate padCols 0 ++) ((fmap . fmap) f mat)
    where
      nC = nCols m
  liftModel _ f m = ModelMat
    { padRows    = 0
    , padCols    = 0
    , unModelMat = (fmap . fmap) f $ fmap VG.toList $ Mat.toRowList m
    }

instance (Storable a, Num a) => TestData TagMat (Matrix a) where
  type Model TagMat (Matrix a) = ModelMat a
  unmodel t = liftUnmodel t id
  model   t = liftModel   t id

instance (Storable a, Eq a) => TestEquiv (Matrix a) where
  equiv = (==)


type instance Model1 TagMat Symmetric = ModelSym

instance (Storable a{-, Num a-}) => TestData1 TagMat Symmetric a where
  liftModel _ f m = ModelSym
    { pad = 0
    , unModelSym = [ [ f (m ! (i,j)) | j <- [i .. n]]
                   | i <- [0 .. n-1]
                   ]
    } where (n,_) = shape m
  -- FIXME: add padding
  liftUnmodel _ f (ModelSym _pad xs)
    = Sym.fromRowsFF
    $ (fmap . fmap) f xs

instance (Storable a, Num a) => TestData TagMat (Symmetric a) where
  type Model TagMat (Symmetric a) = ModelSym a
  unmodel t = liftUnmodel t id
  model   t = liftModel   t id

instance (Storable a, Eq a) => TestEquiv (Symmetric a) where
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

type instance Rank ModelVec = 1

instance HasShape ModelVec a where
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

mkMat :: [[a]] -> ModelMat a
mkMat = ModelMat 0 0

-- | Model for general dense matrices
data ModelMat a = ModelMat
  { padRows    :: !Int  -- ^ Extra padding for rows
  , padCols    :: !Int  -- ^ Extra padding for columns
  , unModelMat :: [[a]]
  }
  deriving stock (Show,Eq)

type instance Rank ModelMat = 2

instance HasShape ModelMat a where
  shapeAsCVec ModelMat{unModelMat=mat} = FC.mk2 (length mat) (length (head mat))

instance Num a => AdditiveSemigroup (ModelMat a) where
  a .+. b = ModelMat 0 0 $ ((zipWithX . zipWithX) (+) `on` unModelMat) a b

instance Num a => AdditiveQuasigroup (ModelMat a) where
  a .-. b = ModelMat 0 0 $ ((zipWithX . zipWithX) (-) `on` unModelMat) a b
  negateV = ModelMat 0 0 . ((map . map) negate) . unModelMat

instance Num a => VectorSpace (ModelMat a) where
  type Scalar (ModelMat a) = a
  a *. ModelMat _ _ xs = ModelMat 0 0 $ (fmap . fmap) (a*) xs
  (.*) = flip (*.)


-- | Model for symmetric matrix
data ModelSym a = ModelSym
  { pad        :: !Int  -- ^ Padding
  , unModelSym :: [[a]] -- ^ Rows above diagonal
  }
  deriving stock (Show, Eq)

type instance Rank ModelSym = 2

instance HasShape ModelSym a where
  shapeAsCVec ModelSym{unModelSym=mat} = let n = length mat in FC.mk2 n n

instance Num a => AdditiveSemigroup (ModelSym a) where
  a .+. b = ModelSym 0 $ ((zipWithX . zipWithX) (+) `on` unModelSym) a b

instance Num a => AdditiveQuasigroup (ModelSym a) where
  a .-. b = ModelSym 0 $ ((zipWithX . zipWithX) (-) `on` unModelSym) a b
  negateV = ModelSym 0 . ((map . map) negate) . unModelSym

instance Num a => VectorSpace (ModelSym a) where
  type Scalar (ModelSym a) = a
  a *. ModelSym _ xs = ModelSym 0 $ (fmap . fmap) (a*) xs
  ModelSym _ xs .* a = ModelSym 0 $ (fmap . fmap) (*a) xs


-- | Data types which could be converted to matrix model
class IsModelMat m a where
  toModelMat :: m -> ModelMat a

instance a ~ a' => IsModelMat (ModelMat a) a' where
  toModelMat = id
instance a ~ a' => IsModelMat (Tr ModelMat a) a' where
  toModelMat (Tr m) = (ModelMat 0 0 . transpose . unModelMat) m
instance (NormedScalar a, a ~ a') => IsModelMat (Conj ModelMat a) a' where
  toModelMat (Conj m) = (ModelMat 0 0 . (fmap . fmap) conjugate . transpose . unModelMat) m

instance (a ~ a') => IsModelMat (ModelSym a) a' where
  toModelMat (ModelSym _ xs) = ModelMat 0 0 $ zipWith (++) sym xs where
    sym     = zipWith row [0..] xs
    row n _ =[ (xs !! i) !! (n-i) | i <- [0 .. n-1]]

instance (a ~ a') => IsModelMat (Tr ModelSym a) a' where
  toModelMat (Tr m) = toModelMat m

instance (NormedScalar a) => MatMul      (ModelMat a) (ModelVec a) (ModelVec a) where (@@) = defaultMulMV
instance (NormedScalar a) => MatMul (Tr   ModelMat a) (ModelVec a) (ModelVec a) where (@@) = defaultMulMV
instance (NormedScalar a) => MatMul (Conj ModelMat a) (ModelVec a) (ModelVec a) where (@@) = defaultMulMV

instance (NormedScalar a) => MatMul      (ModelMat a)      (ModelMat a) (ModelMat a) where (@@) = defaultMulMM
instance (NormedScalar a) => MatMul (Tr   ModelMat a)      (ModelMat a) (ModelMat a) where (@@) = defaultMulMM
instance (NormedScalar a) => MatMul (Conj ModelMat a)      (ModelMat a) (ModelMat a) where (@@) = defaultMulMM
instance (NormedScalar a) => MatMul      (ModelMat a) (Tr   ModelMat a) (ModelMat a) where (@@) = defaultMulMM
instance (NormedScalar a) => MatMul (Tr   ModelMat a) (Tr   ModelMat a) (ModelMat a) where (@@) = defaultMulMM
instance (NormedScalar a) => MatMul (Conj ModelMat a) (Tr   ModelMat a) (ModelMat a) where (@@) = defaultMulMM
instance (NormedScalar a) => MatMul      (ModelMat a) (Conj ModelMat a) (ModelMat a) where (@@) = defaultMulMM
instance (NormedScalar a) => MatMul (Tr   ModelMat a) (Conj ModelMat a) (ModelMat a) where (@@) = defaultMulMM
instance (NormedScalar a) => MatMul (Conj ModelMat a) (Conj ModelMat a) (ModelMat a) where (@@) = defaultMulMM

instance (NormedScalar a) => MatMul (ModelSym a) (ModelVec a) (ModelVec a) where (@@) = defaultMulMV

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

instance (ArbitraryShape v a) => Arbitrary (Pair (v a)) where
  arbitrary = do
    sz <- genSize @(FC.ContVec _ _)
    Pair <$> arbitraryShape sz <*> arbitraryShape sz

-- | Pair of models with same size
data Pair1 v a = Pair1 (v a) (v a)
  deriving stock (Show)

type instance Model1 TagMat (Pair1 v) = Pair1 (Model1 TagMat v)
instance TestData1 TagMat v a => TestData1 TagMat (Pair1 v) a where
  liftModel   t f (Pair1 a b) = Pair1 (liftModel   t f a) (liftModel   t f b)
  liftUnmodel t f (Pair1 a b) = Pair1 (liftUnmodel t f a) (liftUnmodel t f b)

instance (ArbitraryShape v a) => Arbitrary (Pair1 v a) where
  arbitrary = do
    sz <- genSize @(FC.ContVec _ _)
    Pair1 <$> arbitraryShape sz <*> arbitraryShape sz


----------------------------------------------------------------
-- Orphans & Arbitrary
----------------------------------------------------------------

instance (Rank arr ~ 2, CreationRank arr ~ 2, ArbitraryShape arr a
         ) => Arbitrary (Tr arr a) where
  arbitrary = arbitraryShape =<< genSize @(Int,Int)

instance (Rank arr ~ 2, CreationRank arr ~ 2, ArbitraryShape arr a
         ) => Arbitrary (Conj arr a) where
  arbitrary = arbitraryShape =<< genSize @(Int,Int)

instance (Rank arr ~ 2, CreationRank arr ~ 2, ArbitraryShape arr a) => ArbitraryShape (Tr arr) a where
  arbitraryShape (N2 n k) = Tr <$> arbitraryShape (k,n)

instance (Rank arr ~ 2, CreationRank arr ~ 2, ArbitraryShape arr a
         ) => ArbitraryShape (Conj arr) a where
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

instance (SmallScalar a) => ArbitraryShape ModelMat a where
  arbitraryShape (N2 m n)
     =  ModelMat
    <$> genOffset
    <*> genOffset
    <*> replicateM m (replicateM n genScalar)

instance (SmallScalar a) => Arbitrary (ModelSym a) where
  arbitrary = arbitraryShape =<< genSize @Int
  
instance (SmallScalar a) => ArbitraryShape ModelSym a where
  type CreationRank ModelSym = 1
  arbitraryShape (N1 n)
    =  ModelSym
   <$> genOffset
   <*> sequence [ sequence [genScalar | _ <- [i .. n-1]]
                | i <- [0 .. n-1]
                ]

instance (SmallScalar a, Storable a, Num a
         ) => Arbitrary (Matrix a) where
  arbitrary = arbitraryShape =<< genSize @(Int,Int)

instance (SmallScalar a, Storable a, Num a
         ) => ArbitraryShape Matrix a where
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

instance SmallScalar a => ArbitraryShape ModelVec a where
  arbitraryShape (N1 n) = ModelVec <$> genStride <*> replicateM n genScalar



----------------------------------------
-- Vecvec


type instance Model1 TagMat (Tr   v) = Tr   (Model1 TagMat v)
type instance Model1 TagMat (Conj v) = Conj (Model1 TagMat v)

instance (TestMatrix1 v a) => TestData1 TagMat (Tr v) a where
  liftUnmodel t f (Tr v) = Tr $ liftUnmodel t f v
  liftModel   t f (Tr v) = Tr $ liftModel   t f v

instance (TestMatrix1 v a) => TestData1 TagMat (Conj v) a where
  liftUnmodel t f (Conj v) = Conj $ liftUnmodel t f v
  liftModel   t f (Conj v) = Conj $ liftModel   t f v

deriving newtype instance TestEquiv (v a) => TestEquiv (Tr   v a)
deriving newtype instance TestEquiv (v a) => TestEquiv (Conj v a)
