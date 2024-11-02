{-# LANGUAGE NoFieldSelectors     #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}
-- |
-- Tools for writing tests for linear algebra.
module TST.Tools.MatModel
  ( -- * Arbitrary extensions
    SmallScalar(..)
  , ArbitraryShape(..)
  , genSize
  , genNonsingularMatrix
  , genNonsingularSymmetric
  , genNonsingularHermitian
    -- ** Newtypes for selecting instances
  , X(..)
  , Size2D(..)
  , Nonsingular(..)
  , Square(..)
  , Pair1(..)
    -- * Models
  , TestMat(..)
  , HasModel
  , M(..)
  , pattern (:>:)
    -- * Concrete models    
  , ModelMat(..)
  , pattern ModelMat'
  , ModelSym(..)
  , ModelHer(..)
  ) where

import Control.Monad
import Data.Complex          (Complex(..))
import Data.Function         (on)
import Data.Typeable
import Data.Kind
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
import Vecvec.LAPACK.Vector             (Vec,LAPACKy)
import Vecvec.LAPACK.Matrix    (Matrix, fromRowsFF)
import Vecvec.LAPACK.Matrix    qualified as Mat
import Vecvec.LAPACK.Hermitian (Hermitian,multipleByReal)
import Vecvec.LAPACK.Hermitian qualified as Sym
import Vecvec.LAPACK.Symmetric (Symmetric)
import Vecvec.LAPACK.Symmetric qualified as TSym
import TST.Tools.Util
import TST.Tools.Orphanage ()


----------------------------------------------------------------
-- QuickCheck-related utils
----------------------------------------------------------------

-- | We want to test that multiplication return correct result without
--   going into dark forest of floating point. This is achieved by
--   exploiting fact that multiplication and addition small integers
--   are exact in floating point.
class Arbitrary a => SmallScalar a where
  genScalar     :: Gen a
  genRealScalar :: Gen a
  shrinkScalar  :: a -> [a]

instance SmallScalar Float where
  genScalar     = fromIntegral <$> choose @Int (-maxGenScacar, maxGenScacar)
  genRealScalar = genScalar
  shrinkScalar  = \case
    0 -> []
    1 -> [0]
    _ -> [0,1]

instance SmallScalar Double where
  genScalar     = fromIntegral <$> choose @Int (-maxGenScacar, maxGenScacar)
  genRealScalar = genScalar
  shrinkScalar  = \case
    0 -> []
    1 -> [0]
    _ -> [0,1]

instance (RealFloat a, SmallScalar a) => SmallScalar (Complex a) where
  genScalar     = (:+) <$> genScalar <*> genScalar
  genRealScalar = (:+ 0) <$> genScalar
  shrinkScalar  = \case
    0      -> []
    1      -> [0, 0:+1]
    0 :+ 1 -> [0, 1]
    _      -> [0, 1, 0:+1]

maxGenScacar :: Num a => a
maxGenScacar = 10



-- | Generate random NDarray with specified shape
class (Arbitrary a, FC.Arity (CreationRank arr)) => ArbitraryShape arr a where
  type CreationRank arr :: Nat
  type CreationRank arr = Rank arr
  -- | Generate array with given shape
  arbitraryShape :: (IsShape shape (CreationRank arr)) => shape -> Gen (arr a)
  -- | Generate matrix with given number of columns
  arbitraryNCols :: Int -> Gen (arr a)
  -- | Generate matrix with given number of rows
  arbitraryNRows :: Int -> Gen (arr a)


newtype Size2D = Size2D { getSize2D :: (Int,Int) }
  deriving stock Show

instance Arbitrary Size2D where
  arbitrary = Size2D <$> ((,) <$> genSize <*> genSize)



----------------------------------------------------------------
-- Newtypes for selecting Arbitrary
----------------------------------------------------------------

-- | Generate size for N-dimensional array
genSize :: forall shape n. (FC.Arity n, IsShape shape n) => Gen shape
genSize = shapeFromCVec <$> FC.replicateM @n (choose (1,2))

-- | Generate stride for vectors
genStride :: Gen Int
genStride = choose (1,3)

-- | Generate offset for matrices
genOffset :: Gen Int
genOffset = choose (0,3)


-- | Scalar which uses @SmallScalar@ for generation
newtype X a = X a
  deriving newtype (Show)

instance SmallScalar a => Arbitrary (X a) where
  arbitrary = X <$> genScalar


-- | Newtype wrapper for generation of nonsingular matrix.  In order
--   to ensure nonsingularity we generate matrix with diagonal
--   dominance.
newtype Nonsingular m a = Nonsingular { get :: m a }
  deriving newtype Show


instance (SmallScalar a, LAPACKy a, Typeable a, Show a, Eq a
         ) => Arbitrary (Nonsingular Matrix a) where
  arbitrary = Nonsingular <$> (genNonsingularMatrix =<< genSize)

instance (SmallScalar a, LAPACKy a, Typeable a, Show a, Eq a
         ) => Arbitrary (Nonsingular Symmetric a) where
  arbitrary = Nonsingular <$> (genNonsingularSymmetric =<< genSize)

instance (SmallScalar a, LAPACKy a, Typeable a, Show a, Eq a
         ) => Arbitrary (Nonsingular Hermitian a) where
  arbitrary = Nonsingular <$> (genNonsingularHermitian =<< genSize)


genNonsingularMatrix
  :: (SmallScalar a, LAPACKy a)
  => Int -> Gen (Matrix a)
genNonsingularMatrix sz = do
  mat <- arbitraryShape (sz,sz)
  pure $  (2 * maxGenScacar * fromIntegral sz) *. Mat.eye sz
      .+. mat

genNonsingularSymmetric
  :: (SmallScalar a, LAPACKy a)
  => Int -> Gen (Symmetric a)
genNonsingularSymmetric sz = do
  mat <- arbitraryShape (sz)
  pure $  (2 * maxGenScacar * fromIntegral sz) *. TSym.eye sz
      .+. mat

genNonsingularHermitian
  :: (SmallScalar a, LAPACKy a)
  => Int -> Gen (Hermitian a)
genNonsingularHermitian sz = do
  mat <- arbitraryShape (sz)
  pure $  multipleByReal (2 * maxGenScacar * fromIntegral sz) (Sym.eye sz)
      .+. mat

-- | Generate square matrix
newtype Square a = Square (Matrix a)
  deriving stock Show

instance (SmallScalar a, Num a, Storable a) => Arbitrary (Square a) where
  arbitrary = do
    sz <- genSize @Int
    Square <$> arbitraryShape (sz,sz)

-- | Pair of values with same size
data Pair1 v a = Pair1 (v a) (v a)
  deriving stock (Show)

instance (ArbitraryShape v a) => Arbitrary (Pair1 v a) where
  arbitrary = do
    sz <- genSize @(FC.ContVec _ _)
    Pair1 <$> arbitraryShape sz <*> arbitraryShape sz


----------------------------------------------------------------
-- Model type class and instances
----------------------------------------------------------------

-- | Type class which establish isomorphism and its model which is
--   used in reference implementations
class TestMat v a where
  type Model1M v :: Type -> Type
  unmodelMat :: Model1M v a -> v a
  modelMat   :: v a -> Model1M v a

type HasModel v a = ( TestMat v a
                    , a ~ Scalar (v a)
                    , a ~ Scalar (Model1M v a)
                    , Show a
                    , Show (v a)
                    , Show (Model1M v a)
                    )

-- | Newtype wrapper which provides injectivity
newtype M v a = M (Model1M v a)

deriving newtype instance Show      (Model1M v a) => Show      (M v a)
deriving newtype instance Arbitrary (Model1M v a) => Arbitrary (M v a)
instance ( ArbitraryShape (Model1M v) a
         , Arbitrary a
         ) => ArbitraryShape (M v) a where
  type CreationRank (M v) = CreationRank (Model1M v)
  arbitraryShape = fmap M . arbitraryShape
  arbitraryNCols = fmap M . arbitraryNCols
  arbitraryNRows = fmap M . arbitraryNRows

pattern (:>:) :: TestMat v a => v a -> Model1M v a -> M v a
pattern v :>: m <- ((\(M x) -> (unmodelMat x, x)) -> (v,m))
{-# COMPLETE (:>:) #-}



instance (Storable a, Num a) => TestMat Vec a where
  type Model1M Vec = ModelVec
  unmodelMat (ModelVec stride xs)
    = slice ((0,End) `Strided` stride)
    $ VG.fromList
    $ (\n -> n : replicate (stride-1) 0) =<< xs
  modelMat xs = ModelVec { stride = 1
                         , vec    = VG.toList xs
                         }

instance TestMat V.Vector a where
  type Model1M V.Vector = ModelVec
  unmodelMat = VG.fromList . (.vec)
  modelMat   = ModelVec' . VG.toList

instance (VU.Unbox a) => TestMat VU.Vector a where
  type Model1M VU.Vector = ModelVec
  unmodelMat = VG.fromList . (.vec)
  modelMat   = ModelVec' . VG.toList

instance (Storable a) => TestMat VS.Vector a where
  type Model1M VS.Vector = ModelVec
  unmodelMat = VG.fromList . (.vec)
  modelMat   = ModelVec' . VG.toList

instance (TestMat v a) => TestMat (Tr v) a where
  type Model1M (Tr v) = Tr (Model1M v)
  unmodelMat (Tr v) = Tr $ unmodelMat v
  modelMat   (Tr v) = Tr $ modelMat v

instance (TestMat v a) => TestMat (Conj v) a where
  type Model1M (Conj v) = Conj (Model1M v)
  unmodelMat (Conj v) = Conj $ unmodelMat v
  modelMat   (Conj v) = Conj $ modelMat v

instance (Storable a, Num a) => TestMat Matrix a where
  type Model1M Matrix = ModelMat
  unmodelMat m
    = slice ((m.padRows,End), (m.padCols,End))
    $ fromRowsFF
    $ replicate m.padRows (replicate (nC + m.padCols) 0)
   ++ map (replicate m.padCols 0 ++) m.mat
    where
      nC = nCols m
  modelMat m = ModelMat
    { padRows = 0
    , padCols = 0
    , mat     = fmap VG.toList $ Mat.toRowList m
    }

instance (Storable a) => TestMat Symmetric a where
  type Model1M Symmetric = ModelSym
  -- FIXME: add padding
  unmodelMat (ModelSym _pad xs)
    = TSym.fromRowsFF xs
  modelMat m = ModelSym
    { pad = 0
    , mat = [ [ m ! (i,j) | j <- [i .. n-1]]
              | i <- [0 .. n-1]
              ]
    } where (n,_) = shape m

instance (Storable a, NormedScalar a) => TestMat Hermitian a where
  type Model1M Hermitian = ModelHer
  -- FIXME: add padding
  unmodelMat (ModelHer _pad xs)
    = Sym.fromRowsFF xs
  modelMat m = ModelHer
    { pad = 0
    , mat = [ [ m ! (i,j) | j <- [i .. n-1]]
            | i <- [0 .. n-1]
            ]
    } where (n,_) = shape m


----------------------------------------------------------------
-- Model implementations
----------------------------------------------------------------

-- | We use lists as model for vectors.
data ModelVec a = ModelVec
  { stride :: !Int -- ^ Stride for vector. Ignored if not supported
  , vec    :: [a]  -- ^ Data for vectors
  }
  deriving stock (Show)

pattern ModelVec' :: [a] -> ModelVec a
pattern ModelVec' xs = ModelVec 1 xs

type instance Rank ModelVec = 1

instance HasShape ModelVec a where
  shapeAsCVec = FC.mk1 . length . (.vec)

instance Num a => AdditiveSemigroup (ModelVec a) where
  a .+. b = ModelVec 1 $ (zipWithX (+) `on` (.vec)) a b

instance Num a => AdditiveQuasigroup (ModelVec a) where
  a .-. b = ModelVec 1 $ (zipWithX (-) `on` (.vec)) a b
  negateV = ModelVec 1 . map negate . (.vec)

instance Num a => VectorSpace (ModelVec a) where
  type Scalar (ModelVec a) = a
  a *. ModelVec _ xs = ModelVec 1 $ fmap (a*) xs
  ModelVec _ xs .* a = ModelVec 1 $ fmap (*a) xs

instance NormedScalar a => InnerSpace (ModelVec a) where
  ModelVec _ xs <.> ModelVec _ ys = sum $ zipWithX (\x y -> conjugate x * y) xs ys
  magnitudeSq (ModelVec _ xs) = sum $ scalarNormSq <$> xs

instance SmallScalar a => Arbitrary (ModelVec a) where
  arbitrary = arbitraryShape =<< genSize @Int
  shrink (ModelVec n xs) = do
    n' <- case n of 1 -> [1]
                    _ -> [1,n]
    x  <- shrinkList (const []) xs
    return $ ModelVec n' x

instance SmallScalar a => ArbitraryShape ModelVec a where
  arbitraryShape (N1 n) = ModelVec <$> genStride <*> replicateM n genScalar
  arbitraryNRows = arbitraryShape
  arbitraryNCols = error "arbitraryNCols is not defined for ModelVec"


----------------------------------------------------------------

-- | Model for general dense matrices
data ModelMat a = ModelMat
  { padRows :: !Int  -- ^ Extra padding for rows
  , padCols :: !Int  -- ^ Extra padding for columns
  , mat     :: [[a]]
  }
  deriving stock (Show,Eq)

pattern ModelMat' :: [[a]] -> ModelMat a
pattern ModelMat' xs = ModelMat 0 0 xs

type instance Rank ModelMat = 2

instance HasShape ModelMat a where
  shapeAsCVec ModelMat{mat} = FC.mk2 (length mat) (length (head mat))

instance Num a => AdditiveSemigroup (ModelMat a) where
  a .+. b = ModelMat 0 0 $ ((zipWithX . zipWithX) (+) `on` (.mat)) a b

instance Num a => AdditiveQuasigroup (ModelMat a) where
  a .-. b = ModelMat 0 0 $ ((zipWithX . zipWithX) (-) `on` (.mat)) a b
  negateV = ModelMat 0 0 . ((map . map) negate) . (.mat)

instance Num a => VectorSpace (ModelMat a) where
  type Scalar (ModelMat a) = a
  a *. ModelMat _ _ xs = ModelMat 0 0 $ (fmap . fmap) (a*) xs
  (.*) = flip (*.)

instance (SmallScalar a, Eq a) => Arbitrary (ModelMat a) where
  arbitrary = arbitraryShape =<< genSize @(Int,Int)
  shrink mat0 = do
    p_r <- case mat0.padRows of 0 -> [0]; _ -> [0,mat0.padRows]
    p_c <- case mat0.padCols of 0 -> [0]; _ -> [0,mat0.padCols]
    p_elem <- (traverse . traverse) shrinkScalar mat0.mat
    let mat = ModelMat p_r p_c p_elem
    guard (mat /= mat0)
    return mat

instance (SmallScalar a) => ArbitraryShape ModelMat a where
  arbitraryShape (N2 m n)
     =  ModelMat
    <$> genOffset
    <*> genOffset
    <*> replicateM m (replicateM n genScalar)
  arbitraryNRows n = do k <- genSize
                        arbitraryShape (n,k)
  arbitraryNCols k = do n <- genSize
                        arbitraryShape (n,k)


----------------------------------------------------------------

-- | Model for symmetric matrix
data ModelHer a = ModelHer
  { pad :: !Int  -- ^ Padding
  , mat :: [[a]] -- ^ Rows above diagonal
  }
  deriving stock (Show, Eq)

type instance Rank ModelHer = 2

instance HasShape ModelHer a where
  shapeAsCVec m = let n = length m.mat in FC.mk2 n n

instance Num a => AdditiveSemigroup (ModelHer a) where
  a .+. b = ModelHer 0 $ ((zipWithX . zipWithX) (+) `on` (.mat)) a b

instance Num a => AdditiveQuasigroup (ModelHer a) where
  a .-. b = ModelHer 0 $ ((zipWithX . zipWithX) (-) `on` (.mat)) a b
  negateV = ModelHer 0 . ((map . map) negate) . (.mat)

instance Num a => VectorSpace (ModelHer a) where
  type Scalar (ModelHer a) = a
  a *. ModelHer _ xs = ModelHer 0 $ (fmap . fmap) (a*) xs
  ModelHer _ xs .* a = ModelHer 0 $ (fmap . fmap) (*a) xs

instance (SmallScalar a) => Arbitrary (ModelHer a) where
  arbitrary = arbitraryShape =<< genSize @Int
  
instance (SmallScalar a) => ArbitraryShape ModelHer a where
  type CreationRank ModelHer = 1
  arbitraryShape (N1 n)
    =  ModelHer
   <$> genOffset
   <*> sequence [ sequence [ if i == j then genRealScalar else genScalar
                           | j <- [i .. n-1]]
                | i <- [0 .. n-1]
                ]
  arbitraryNRows = arbitraryShape
  arbitraryNCols = arbitraryShape


----------------------------------------------------------------

-- | Model for symmetric matrix
data ModelSym a = ModelSym
  { pad :: !Int  -- ^ Padding
  , mat :: [[a]] -- ^ Rows above diagonal
  }
  deriving stock (Show, Eq)

type instance Rank ModelSym = 2

instance HasShape ModelSym a where
  shapeAsCVec m = let n = length m.mat in FC.mk2 n n

instance Num a => AdditiveSemigroup (ModelSym a) where
  a .+. b = ModelSym 0 $ ((zipWithX . zipWithX) (+) `on` (.mat)) a b

instance Num a => AdditiveQuasigroup (ModelSym a) where
  a .-. b = ModelSym 0 $ ((zipWithX . zipWithX) (-) `on` (.mat)) a b
  negateV = ModelSym 0 . ((map . map) negate) . (.mat)

instance Num a => VectorSpace (ModelSym a) where
  type Scalar (ModelSym a) = a
  a *. ModelSym _ xs = ModelSym 0 $ (fmap . fmap) (a*) xs
  ModelSym _ xs .* a = ModelSym 0 $ (fmap . fmap) (*a) xs

instance (SmallScalar a) => Arbitrary (ModelSym a) where
  arbitrary = arbitraryShape =<< genSize @Int
  
instance (SmallScalar a) => ArbitraryShape ModelSym a where
  type CreationRank ModelSym = 1
  arbitraryShape (N1 n)
    =  ModelSym
   <$> genOffset
   <*> sequence [ sequence [ genScalar | _ <- [i .. n-1]]
                | i <- [0 .. n-1]
                ]
  arbitraryNRows = arbitraryShape
  arbitraryNCols = arbitraryShape


----------------------------------------------------------------
-- Matrix multiplication for models
----------------------------------------------------------------

-- | Data types which could be converted to matrix model. All
--   computation with models of specialized matrices are done using
--   'ModelMat'
class IsModelMat m a where
  toModelMat :: m a -> ModelMat a


instance IsModelMat m a => IsModelMat (Tr m) a where
  toModelMat (Tr m) = ModelMat' $ transpose $ (toModelMat m).mat

instance (NormedScalar a, IsModelMat m a) => IsModelMat (Conj m) a where
  toModelMat (Conj m) = ModelMat' $ (fmap . fmap) conjugate
                                  $ transpose
                                  $ (toModelMat m).mat

instance IsModelMat ModelMat a where
  toModelMat = id

instance NormedScalar a => IsModelMat ModelHer a where
  toModelMat (ModelHer _ xs) = ModelMat 0 0 $ zipWith (++) sym xs where
    sym     = zipWith row [0..] xs
    row n _ = [ (if i < n then conjugate else id) $ (xs !! i) !! (n-i) | i <- [0 .. n-1]]

instance IsModelMat ModelSym a where
  toModelMat (ModelSym _ xs) = ModelMat 0 0 $ zipWith (++) sym xs where
    sym     = zipWith row [0..] xs
    row n _ = [ (xs !! i) !! (n-i) | i <- [0 .. n-1]]


-- Matrix × Vector
instance (NormedScalar a) => MatMul a (     ModelMat) ModelVec ModelVec where (@@) = defaultMulMV
instance (NormedScalar a) => MatMul a (Tr   ModelMat) ModelVec ModelVec where (@@) = defaultMulMV
instance (NormedScalar a) => MatMul a (Conj ModelMat) ModelVec ModelVec where (@@) = defaultMulMV
instance (NormedScalar a) => MatMul a (     ModelSym) ModelVec ModelVec where (@@) = defaultMulMV
instance (NormedScalar a) => MatMul a (Tr   ModelSym) ModelVec ModelVec where (@@) = defaultMulMV
instance (NormedScalar a) => MatMul a (     ModelHer) ModelVec ModelVec where (@@) = defaultMulMV
instance (NormedScalar a) => MatMul a (Conj ModelHer) ModelVec ModelVec where (@@) = defaultMulMV

-- Matrix × Matrix
instance (NormedScalar a) => MatMul a       ModelMat        ModelMat  ModelMat where (@@) = defaultMulMM
instance (NormedScalar a) => MatMul a (Tr   ModelMat)       ModelMat  ModelMat where (@@) = defaultMulMM
instance (NormedScalar a) => MatMul a (Conj ModelMat)       ModelMat  ModelMat where (@@) = defaultMulMM
instance (NormedScalar a) => MatMul a       ModelMat  (Tr   ModelMat) ModelMat where (@@) = defaultMulMM
instance (NormedScalar a) => MatMul a (Tr   ModelMat) (Tr   ModelMat) ModelMat where (@@) = defaultMulMM
instance (NormedScalar a) => MatMul a (Conj ModelMat) (Tr   ModelMat) ModelMat where (@@) = defaultMulMM
instance (NormedScalar a) => MatMul a       ModelMat  (Conj ModelMat) ModelMat where (@@) = defaultMulMM
instance (NormedScalar a) => MatMul a (Tr   ModelMat) (Conj ModelMat) ModelMat where (@@) = defaultMulMM
instance (NormedScalar a) => MatMul a (Conj ModelMat) (Conj ModelMat) ModelMat where (@@) = defaultMulMM
--
instance (NormedScalar a) => MatMul a ModelSym ModelMat ModelMat where (@@) = defaultMulMM
instance (NormedScalar a) => MatMul a ModelMat ModelSym ModelMat where (@@) = defaultMulMM
instance (NormedScalar a) => MatMul a ModelSym ModelSym ModelMat where (@@) = defaultMulMM
--
instance (NormedScalar a) => MatMul a ModelHer ModelMat ModelMat where (@@) = defaultMulMM
instance (NormedScalar a) => MatMul a ModelMat ModelHer ModelMat where (@@) = defaultMulMM
instance (NormedScalar a) => MatMul a ModelHer ModelHer ModelMat where (@@) = defaultMulMM

-- Default model implementation of matrix-matrix multiplication
defaultMulMM :: (Num a, IsModelMat m1 a, IsModelMat m2 a) => m1 a -> m2 a -> ModelMat a
defaultMulMM m1 m2 = ModelMat 0 0 $ (toModelMat m1).mat !*! (toModelMat m2).mat

-- Default model implementation of matrix-vector multiplication
defaultMulMV :: (Num a, IsModelMat m a) => m a -> ModelVec a -> ModelVec a
defaultMulMV m v = ModelVec 1 $ ((.mat) . toModelMat) m !* v.vec



----------------------------------------------------------------
-- Arbitrary instances
----------------------------------------------------------------

class FC.Arity n => TransposeShape n where
  transposeShape :: FC.ContVec n a -> FC.ContVec n a

instance TransposeShape 1 where
  transposeShape = id
  {-# INLINE transposeShape #-}
instance TransposeShape 2 where
  transposeShape (FC.ContVec cont) = FC.ContVec $ \(FC.Fun f) -> cont (FC.Fun $ flip f)
  {-# INLINE transposeShape #-}


instance ( Rank arr ~ 2
         , TransposeShape (CreationRank arr)
         , ArbitraryShape arr a
         ) => Arbitrary (Tr arr a) where
  arbitrary = arbitraryShape =<< genSize @(FC.ContVec (CreationRank arr) Int)

instance ( Rank arr ~ 2
         , TransposeShape (CreationRank arr)
         , ArbitraryShape arr a
         ) => Arbitrary (Conj arr a) where
  arbitrary = arbitraryShape =<< genSize @(FC.ContVec (CreationRank arr) Int)


instance ( Rank arr ~ 2
         , TransposeShape (CreationRank arr)
         , ArbitraryShape arr a
         ) => ArbitraryShape (Tr arr) a where
  type CreationRank (Tr arr) = CreationRank arr
  arbitraryShape = fmap Tr . arbitraryShape . transposeShape . shapeToCVec
  arbitraryNCols = fmap Tr . arbitraryNRows
  arbitraryNRows = fmap Tr . arbitraryNCols

instance ( Rank arr ~ 2
         , TransposeShape (CreationRank arr)
         , ArbitraryShape arr a
         ) => ArbitraryShape (Conj arr) a where
  type CreationRank (Conj arr) = CreationRank arr
  arbitraryShape = fmap Conj . arbitraryShape . transposeShape . shapeToCVec
  arbitraryNCols = fmap Conj . arbitraryNRows
  arbitraryNRows = fmap Conj . arbitraryNCols


instance (SmallScalar a, Storable a, Num a
         ) => ArbitraryShape Matrix a where
  arbitraryShape = fmap unmodelMat . arbitraryShape
  arbitraryNCols = fmap unmodelMat . arbitraryNCols
  arbitraryNRows = fmap unmodelMat . arbitraryNRows

instance (SmallScalar a, Storable a
         ) => ArbitraryShape Symmetric a where
  type CreationRank Symmetric = 1
  arbitraryShape = fmap unmodelMat . arbitraryShape
  arbitraryNCols = fmap unmodelMat . arbitraryNCols
  arbitraryNRows = fmap unmodelMat . arbitraryNRows

instance (SmallScalar a, Storable a, NormedScalar a
         ) => ArbitraryShape Hermitian a where
  type CreationRank Hermitian = 1
  arbitraryShape = fmap unmodelMat . arbitraryShape
  arbitraryNCols = fmap unmodelMat . arbitraryNCols
  arbitraryNRows = fmap unmodelMat . arbitraryNRows


instance (SmallScalar a, Storable a, Num a
         ) => Arbitrary (Matrix a) where
  arbitrary = arbitraryShape =<< genSize @(Int,Int)

instance (SmallScalar a, Storable a, NormedScalar a) => Arbitrary (Symmetric a) where
  arbitrary = arbitraryShape =<< genSize @Int

instance (SmallScalar a, Storable a, NormedScalar a) => Arbitrary (Hermitian a) where
  arbitrary = arbitraryShape =<< genSize @Int
