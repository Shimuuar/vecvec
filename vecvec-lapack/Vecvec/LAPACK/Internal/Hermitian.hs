{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost        #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NegativeLiterals           #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
-- |
module Vecvec.LAPACK.Internal.Hermitian
  ( -- * Immutable matrix
    Hermitian(..)
    -- * Operations
    -- ** Conversion to\/from mutable
  , unsafeFreeze
  , freeze
  , thaw
  , toDense
    -- ** Access
  , reallyUnsafeIndex
    -- ** Creation
  , zeros
  , eye
  , fromRowsFF
  , fromRowsFV
  , diag
  , diagF
  , replicate
  , generate
  ) where

import Control.Monad
import Control.Monad.Primitive
import Control.Monad.ST
import Data.Vector.Generic         qualified as VG
import Data.Vector.Generic.Mutable qualified as MVG
import Data.Vector.Fixed.Cont      qualified as FC
import Foreign.Storable
import Foreign.Marshal.Array
import System.IO.Unsafe
import Prelude hiding (replicate)

import Vecvec.Classes
import Vecvec.Classes.NDArray
import Vecvec.LAPACK.Internal.Matrix.Mutable     qualified as MMat
import Vecvec.LAPACK.Internal.Matrix             qualified as Mat
import Vecvec.LAPACK.Internal.Matrix             (Matrix)
import Vecvec.LAPACK.Internal.Hermitian.Mutable  qualified as MSym
import Vecvec.LAPACK.Internal.Compat
import Vecvec.LAPACK.Internal.Vector
import Vecvec.LAPACK.Internal.Vector.Mutable
import Vecvec.LAPACK.FFI                           qualified as C
import Vecvec.LAPACK.Utils

----------------------------------------------------------------
--
----------------------------------------------------------------

-- | Hermitian matrix
data Hermitian a = Hermitian () (MSym.MSymView a)

instance (Slice1D i, Storable a) => Slice i (Hermitian a) where
  {-# INLINE sliceMaybe #-}
  sliceMaybe i (Hermitian flag view) = do
    view' <- sliceMaybe i view
    pure $ Hermitian flag view'

instance (Show a, Storable a) => Show (Hermitian a) where
  show = show . toDense

instance MSym.InHermitian s Hermitian where
  {-# INLINE symmetricRepr #-}
  symmetricRepr (Hermitian _ mat) = pure mat

type instance Rank Hermitian = 2

instance HasShape Hermitian a where
  shapeAsCVec (Hermitian _ MSym.MSymView{..}) = FC.mk2 size size
  {-# INLINE shapeAsCVec #-}

instance (NormedScalar a, Storable a) => NDArray Hermitian a where
  basicUnsafeIndex mat (N2 i j)
    | j >= i    = reallyUnsafeIndex mat (i,j)
    | otherwise = conjugate $ reallyUnsafeIndex mat (j,i)

instance (NormedScalar a, Storable a, Eq a) => Eq (Hermitian a) where
  a == b
    | n /= nCols b = False
    | otherwise    = and [ a ! (i,j)  == b ! (i,j)
                         | i <- [0 .. n-1]
                         , j <- [i .. n-1]
                         ]
    where n = nCols a

unsafeFreeze :: (Storable a, NormedScalar a, PrimMonad m, s ~ PrimState m)
             => MSym.MHermitian s a -> m (Hermitian a)
unsafeFreeze (MSym.MHermitian view)
  = pure $ Hermitian flag view
  where
    flag = unsafePerformIO $ () <$ MSym.symmetrizeMSymView view


freeze :: (Storable a, NormedScalar a, PrimMonad m, s ~ PrimState m)
       => MSym.MHermitian s a -> m (Hermitian a)
freeze = unsafeFreeze <=< MSym.clone

thaw :: (Storable a, PrimMonad m, s ~ PrimState m)
     => Hermitian a -> m (MSym.MHermitian s a)
thaw = MSym.clone

toDense :: Hermitian a -> Mat.Matrix a
toDense (Hermitian () MSym.MSymView{..}) =
  Mat.Matrix MMat.MView
    { nrows      = size
    , ncols      = size
    , leadingDim = leadingDim
    , buffer     = buffer
    }

----------------------------------------------------------------
-- Access
----------------------------------------------------------------

-- | Read value at given index. Index must be in range and reference
--   item on or above diagonal. Content of items below diagonal is
--   undefined.
--
-- __UNSAFE__: this function does not any range checks.
reallyUnsafeIndex
  :: (Storable a) => Hermitian a -> (Int, Int) -> a
{-# INLINE reallyUnsafeIndex #-}
reallyUnsafeIndex (Hermitian _ MSym.MSymView{..}) (i,j)
  = unsafePerformIO
  $ unsafeWithForeignPtr buffer $ \p -> do
    peekElemOff p (i * leadingDim + j)


----------------------------------------------------------------
-- Creation
----------------------------------------------------------------

-- | Create matrix from list of rows.
fromRowsFF :: (Storable a, NormedScalar a, Foldable f, Foldable g)
           => f (g a) -> Hermitian a
fromRowsFF dat = runST $ unsafeFreeze =<< MSym.fromRowsFF dat

-- | Create matrix from list of rows.
fromRowsFV :: (Storable a, NormedScalar a, Foldable f, VG.Vector v a)
           => f (v a) -> Hermitian a
{-# INLINE fromRowsFV #-}
fromRowsFV dat = runST $ unsafeFreeze =<< MSym.fromRowsFV dat

-- | Create matrix filled with zeros. It's more efficient than using
--   'replicate'.
--
-- ==== __Examples__
--
-- >>> zeros 2 :: Hermitian Double
-- [ [0.0,0.0]
-- , [0.0,0.0]]
zeros :: (LAPACKy a)
      => Int          -- ^ Matrix size
      -> Hermitian a
zeros sz = runST $ unsafeFreeze =<< MSym.zeros sz

-- | Create identity matrix
eye :: (LAPACKy a, Num a) => Int -> Hermitian a
eye n = runST $ unsafeFreeze =<< MSym.eye n


-- | Fill matrix of given size with provided value.
--
-- ==== __Examples__
--
-- >>> replicate 2 (42::Double)
-- [ [42.0,42.0]
-- , [42.0,42.0]]
replicate :: (Storable a, NormedScalar a)
          => Int  -- ^ Matrix size
          -> a    -- ^ Element
          -> Hermitian a
replicate sz a = runST $ unsafeFreeze =<< MSym.replicate sz a

-- | Fill matrix of given size using function from indices to element.
--
-- ==== __Examples__
--
-- >>> generate 3 (\i j -> 100*i + j)
-- [ [0,1,2]
-- , [1,101,102]
-- , [2,102,202]]
generate :: (Storable a, NormedScalar a)
         => Int               -- ^ Matrix size
         -> (Int -> Int -> a) -- ^ Function that takes \(N_{row}\) and \(N_{column}\) as input
         -> Hermitian a
generate sz f = runST $ unsafeFreeze =<< MSym.generate sz f

-- | Create diagonal matrix. Diagonal elements are stored in list-like
--   container.
diagF :: (LAPACKy a, Foldable f) => f a -> Hermitian a
diagF xs = runST $ unsafeFreeze =<< MSym.diagF xs

-- | Create diagonal matrix. Diagonal elements are stored in vector
diag :: (LAPACKy a, VG.Vector v a) => v a -> Hermitian a
{-# INLINE diag #-}
diag xs = runST $ unsafeFreeze =<< MSym.diag xs

----------------------------------------------------------------
-- Matrix-Vector
----------------------------------------------------------------

unsafeColumnPart :: Storable a => MSym.MSymView a -> Int -> VecRepr a
unsafeColumnPart MSym.MSymView{..} n = VecRepr
  { vecSize   = size - n
  , vecBuffer = updPtr (`advancePtr` (n * leadingDim + n)) buffer
  , vecStride = 1
  }

instance (C.LAPACKy a) => AdditiveSemigroup (Hermitian a) where
  m1 .+. m2@(Hermitian _ mat2)
    | nRows m2 /= n = error "Size mismatch"
    | otherwise     = runST $ do
        -- FIXME: This could be optimized. We're making a lot of BLAS
        --        calls.  Maybe it would be better to write C kernel
        --        which will be quite a bit faster.
        r@(MSym.MHermitian mat1) <- MSym.clone m1
        loop0_ n $ \i -> do
          unsafeBlasAxpy 1 (Vec  $ unsafeColumnPart mat2 i)
                           (MVec $ unsafeColumnPart mat1 i)
        unsafeFreeze r
    where
      n = nRows m1

instance C.LAPACKy a => AdditiveQuasigroup (Hermitian a) where
  m1 .-. m2@(Hermitian _ mat2)
    | nRows m1 /= nRows m2 = error "Size mismatch"
    | otherwise = runST $ do
        r@(MSym.MHermitian mat1) <- MSym.clone m1
        loop0_ n $ \i -> do
          unsafeBlasAxpy -1 (Vec  $ unsafeColumnPart mat2 i)
                            (MVec $ unsafeColumnPart mat1 i)
        unsafeFreeze r
    where
      n = nRows m1
  negateV m = -1 *. m

instance C.LAPACKy a => VectorSpace (Hermitian a) where
  type Scalar (Hermitian a) = a
  a *. m@(Hermitian _ mat) = runST $ do
    r@(MSym.MHermitian res) <- MSym.new n
    loop0_ n $ \i -> do
          unsafeBlasAxpy a (Vec  $ unsafeColumnPart mat i)
                           (MVec $ unsafeColumnPart res i)
    unsafeFreeze r
    where
      n = nRows m
  (.*) = flip (*.)

instance (C.LAPACKy a, a ~ a') => MatMul (Hermitian a) (Vec a') (Vec a) where
  m @@ v
    | nCols m /= VG.length v = error "matrix size mismatch"
  mat @@ vecX = unsafePerformIO $ do
    vecY <- MVG.new (nRows mat)
    MSym.unsafeBlasHemv 1 mat vecX 0 vecY
    VG.unsafeFreeze vecY

instance (C.LAPACKy a, a ~ a') => MatMul (Tr Hermitian a) (Vec a') (Vec a) where
  Tr m @@ v = m @@ v


instance (C.LAPACKy a, a ~ a') => MatMul (Hermitian a) (Matrix a') (Matrix a) where
  matA @@ matB
    | nCols matA /= n = error "matrix size mismatch"
    | otherwise       = unsafePerformIO $ do
        matC <- MMat.new (n,k)
        MSym.unsafeBlasHemmL 1 matA matB 0 matC
        Mat.unsafeFreeze matC
    where
      (n,k) = shape matB

instance (C.LAPACKy a, a ~ a') => MatMul (Matrix a) (Hermitian a') (Matrix a) where
  matB @@ matA
    | nCols matA /= n = error "matrix size mismatch"
    | otherwise       = unsafePerformIO $ do
        matC <- MMat.new (k,n)
        MSym.unsafeBlasHemmR 1 matB matA 0 matC
        Mat.unsafeFreeze matC
    where
      (k,n) = shape matB

instance (C.LAPACKy a, a ~ a') => MatMul (Hermitian a) (Hermitian a') (Matrix a) where
  matA @@ matB
    | n /= nCols matB = error "matrix size mismatch"
    | otherwise       = unsafePerformIO $ do
        matC  <- MMat.new (n,n)
        MSym.unsafeBlasHemmL 1 matA (toDense matB) 0 matC
        Mat.unsafeFreeze matC
    where
      n = nCols matA


