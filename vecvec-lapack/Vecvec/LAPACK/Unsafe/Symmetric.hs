{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies    #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- |
module Vecvec.LAPACK.Unsafe.Symmetric
  ( -- * Immutable matrix
    Symmetric(..)
  , LAPACKy
    -- * Operations
    -- ** Conversion to\/from mutable
  , unsafeFreeze
  , freeze
  , thaw
  , toDense
  , asHermitian
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
import Foreign.Storable
import Foreign.Marshal.Array
import System.IO.Unsafe
import Prelude hiding (replicate)

import Vecvec.Classes
import Vecvec.Classes.NDArray
import Vecvec.LAPACK.Unsafe.Matrix.Mutable    qualified as MMat
import Vecvec.LAPACK.Unsafe.Matrix            qualified as Mat
import Vecvec.LAPACK.Unsafe.Matrix            (Matrix)
import Vecvec.LAPACK.Unsafe.Symmetric.Mutable qualified as MTSym
import Vecvec.LAPACK.Unsafe.Symmetric.Types
import Vecvec.LAPACK.Unsafe.Compat
import Vecvec.LAPACK.Unsafe.Vector
import Vecvec.LAPACK.Unsafe.Vector.Mutable
import Vecvec.LAPACK.FFI                        qualified as C
import Vecvec.LAPACK.Utils

----------------------------------------------------------------
--
----------------------------------------------------------------

instance (Show a, Storable a) => Show (Symmetric a) where
  show = show . toDense

instance MTSym.InSymmetric s Symmetric where
  {-# INLINE symmetricRepr #-}
  symmetricRepr (Symmetric _ mat) = pure mat

instance Storable a => NDArray Symmetric a where
  basicUnsafeIndex mat (N2 i j)
    | j >= i    = reallyUnsafeIndex mat (i,j)
    | otherwise = reallyUnsafeIndex mat (j,i)

instance (Storable a, Eq a) => Eq (Symmetric a) where
  a == b
    | n /= nCols b = False
    | otherwise    = and [ a ! (i,j)  == b ! (i,j)
                         | i <- [0 .. n-1]
                         , j <- [i .. n-1]
                         ]
    where n = nCols a

unsafeFreeze :: (Storable a, PrimMonad m, s ~ PrimState m)
             => MSymmetric s a -> m (Symmetric a)
unsafeFreeze (MTSym.MSymmetric view)
  = pure $ Symmetric flag view
  where
    flag = unsafePerformIO $ () <$ MTSym.symmetrizeMSymView view


freeze :: (Storable a, PrimMonad m, s ~ PrimState m)
       => MTSym.MSymmetric s a -> m (Symmetric a)
freeze = unsafeFreeze <=< MTSym.clone

thaw :: (Storable a, PrimMonad m, s ~ PrimState m)
     => Symmetric a -> m (MTSym.MSymmetric s a)
thaw = MTSym.clone

toDense :: Symmetric a -> Mat.Matrix a
toDense (Symmetric () MTSym.MSymView{..}) =
  Mat.Matrix MMat.MView
    { nrows      = size
    , ncols      = size
    , leadingDim = leadingDim
    , buffer     = buffer
    }

-- | /O(1)/ cast hermitian matrix to symmetric if its elements are
--   real.
asHermitian :: (R a ~ a) => Symmetric a -> Hermitian a
asHermitian (Symmetric tag repr) = Hermitian tag repr


----------------------------------------------------------------
-- Access
----------------------------------------------------------------

-- | Read value at given index. Index must be in range and reference
--   item on or above diagonal. Content of items below diagonal is
--   undefined.
--
-- __UNSAFE__: this function does not any range checks.
reallyUnsafeIndex
  :: (Storable a) => Symmetric a -> (Int, Int) -> a
{-# INLINE reallyUnsafeIndex #-}
reallyUnsafeIndex (Symmetric _ MTSym.MSymView{..}) (i,j)
  = unsafePerformIO
  $ unsafeWithForeignPtr buffer $ \p -> do
    peekElemOff p (i * leadingDim + j)


----------------------------------------------------------------
-- Creation
----------------------------------------------------------------

-- | Create matrix from list of rows.
fromRowsFF :: (Storable a, Foldable f, Foldable g)
           => f (g a) -> Symmetric a
fromRowsFF dat = runST $ unsafeFreeze =<< MTSym.fromRowsFF dat

-- | Create matrix from list of rows.
fromRowsFV :: (Storable a, Foldable f, VG.Vector v a)
           => f (v a) -> Symmetric a
{-# INLINE fromRowsFV #-}
fromRowsFV dat = runST $ unsafeFreeze =<< MTSym.fromRowsFV dat

-- | Create matrix filled with zeros. It's more efficient than using
--   'replicate'.
--
-- ==== __Examples__
--
-- >>> zeros 2 :: Symmetric Double
-- [ [0.0,0.0]
-- , [0.0,0.0]]
zeros :: (LAPACKy a)
      => Int          -- ^ Matrix size
      -> Symmetric a
zeros sz = runST $ unsafeFreeze =<< MTSym.zeros sz

-- | Create identity matrix
eye :: (LAPACKy a, Num a) => Int -> Symmetric a
eye n = runST $ unsafeFreeze =<< MTSym.eye n


-- | Fill matrix of given size with provided value.
--
-- ==== __Examples__
--
-- >>> replicate 2 (42::Double)
-- [ [42.0,42.0]
-- , [42.0,42.0]]
replicate :: (Storable a)
          => Int  -- ^ Matrix size
          -> a    -- ^ Element
          -> Symmetric a
replicate sz a = runST $ unsafeFreeze =<< MTSym.replicate sz a

-- | Fill matrix of given size using function from indices to element.
--
-- ==== __Examples__
--
-- >>> generate 3 (\i j -> 100*i + j)
-- [ [0,1,2]
-- , [1,101,102]
-- , [2,102,202]]
generate :: (Storable a)
         => Int               -- ^ Matrix size
         -> (Int -> Int -> a) -- ^ Function that takes \(N_{row}\) and \(N_{column}\) as input
         -> Symmetric a
generate sz f = runST $ unsafeFreeze =<< MTSym.generate sz f

-- | Create diagonal matrix. Diagonal elements are stored in list-like
--   container.
diagF :: (LAPACKy a, Foldable f) => f a -> Symmetric a
diagF xs = runST $ unsafeFreeze =<< MTSym.diagF xs

-- | Create diagonal matrix. Diagonal elements are stored in vector
diag :: (LAPACKy a, VG.Vector v a) => v a -> Symmetric a
{-# INLINE diag #-}
diag xs = runST $ unsafeFreeze =<< MTSym.diag xs

----------------------------------------------------------------
-- Matrix-Vector
----------------------------------------------------------------

unsafeColumnPart :: Storable a => MTSym.MSymView a -> Int -> VecRepr a
unsafeColumnPart MTSym.MSymView{..} n = VecRepr
  { vecSize   = size - n
  , vecBuffer = updPtr (`advancePtr` (n * leadingDim + n)) buffer
  , vecStride = 1
  }

instance (C.LAPACKy a) => AdditiveSemigroup (Symmetric a) where
  m1 .+. m2@(Symmetric _ mat2)
    | nRows m2 /= n = error "Size mismatch"
    | otherwise     = runST $ do
        -- FIXME: This could be optimized. We're making a lot of BLAS
        --        calls.  Maybe it would be better to write C kernel
        --        which will be quite a bit faster.
        r@(MTSym.MSymmetric mat1) <- MTSym.clone m1
        loop0_ n $ \i -> do
          unsafeBlasAxpy 1 (Vec  $ unsafeColumnPart mat2 i)
                           (MVec $ unsafeColumnPart mat1 i)
        unsafeFreeze r
    where
      n = nRows m1

instance C.LAPACKy a => AdditiveQuasigroup (Symmetric a) where
  m1 .-. m2@(Symmetric _ mat2)
    | nRows m1 /= nRows m2 = error "Size mismatch"
    | otherwise = runST $ do
        r@(MTSym.MSymmetric mat1) <- MTSym.clone m1
        loop0_ n $ \i -> do
          unsafeBlasAxpy -1 (Vec  $ unsafeColumnPart mat2 i)
                            (MVec $ unsafeColumnPart mat1 i)
        unsafeFreeze r
    where
      n = nRows m1
  negateV m = -1 *. m

instance C.LAPACKy a => VectorSpace (Symmetric a) where
  type Scalar (Symmetric a) = a
  a *. m@(Symmetric _ mat) = runST $ do
    r@(MTSym.MSymmetric res) <- MTSym.new n
    loop0_ n $ \i -> do
          unsafeBlasAxpy a (Vec  $ unsafeColumnPart mat i)
                           (MVec $ unsafeColumnPart res i)
    unsafeFreeze r
    where
      n = nRows m
  (.*) = flip (*.)

instance (C.LAPACKy a) => MatMul a Symmetric Vec Vec where
  m @@ v
    | nCols m /= VG.length v = error "matrix size mismatch"
  mat @@ vecX = unsafePerformIO $ do
    vecY <- MVG.new (nRows mat)
    MTSym.unsafeBlasSymv 1 mat vecX 0 vecY
    VG.unsafeFreeze vecY



instance (C.LAPACKy a) => MatMul a Symmetric Matrix Matrix where
  matA @@ matB
    | nCols matA /= n = error "matrix size mismatch"
    | otherwise       = unsafePerformIO $ do
        matC <- MMat.new (n,k)
        MTSym.unsafeBlasSymmL 1 matA matB 0 matC
        Mat.unsafeFreeze matC
    where
      (n,k) = shape matB

instance (C.LAPACKy a) => MatMul a Matrix Symmetric Matrix where
  matB @@ matA
    | nCols matA /= n = error "matrix size mismatch"
    | otherwise       = unsafePerformIO $ do
        matC <- MMat.new (k,n)
        MTSym.unsafeBlasSymmR 1 matB matA 0 matC
        Mat.unsafeFreeze matC
    where
      (k,n) = shape matB

instance (C.LAPACKy a) => MatMul a Symmetric Symmetric Matrix where
  matA @@ matB
    | n /= nCols matB = error "matrix size mismatch"
    | otherwise       = unsafePerformIO $ do
        matC  <- MMat.new (n,n)
        MTSym.unsafeBlasSymmL 1 matA (toDense matB) 0 matC
        Mat.unsafeFreeze matC
    where
      n = nCols matA


