{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies    #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- |
-- Symmetric matrices.
module Vecvec.LAPACK.Unsafe.Symmetric.Mutable
  ( -- * Data types
    MSymmetric(..)
  , MSymView(..)
  , InSymmetric(..)
  , LAPACKy
  , symmetrizeMSymView
  , asMHermitian
    -- * Operations
    -- ** Creation
  , clone
  , new
  , unsafeNew
  , fromRowsFF
  , fromRowsFV
  , zeros
  , replicate
  , replicateM
  , generate
  , generateM
  , eye
  , diag
  , diagF

    -- * Unsafe functions
  , unsafeCast
  , unsafeToDense
    -- ** BLAS wrappers
  , unsafeBlasSymv
  , unsafeBlasSymmL
  , unsafeBlasSymmR
  ) where

import Control.Monad.Primitive
import Control.Monad.ST
import Data.Coerce
import Data.Foldable
import Data.Vector.Generic.Mutable           qualified as MVG
import Data.Vector.Generic                   qualified as VG
import Foreign.Storable
import Foreign.ForeignPtr
import Foreign.Marshal.Array

import Prelude hiding (read,replicate)

import Vecvec.Classes.NDMutable
import Vecvec.Classes
import Vecvec.LAPACK.Utils
import Vecvec.LAPACK.Unsafe.Compat
import Vecvec.LAPACK.Unsafe.Vector.Mutable hiding (clone)
import Vecvec.LAPACK.Unsafe.Matrix.Mutable qualified as MMat
import Vecvec.LAPACK.Unsafe.Matrix.Mutable (MMatrix(..), MView(..), InMatrix(..))
import Vecvec.LAPACK.Unsafe.Symmetric.Types
import Vecvec.LAPACK.FFI                     qualified as C


----------------------------------------------------------------
-- RO access to representation
----------------------------------------------------------------

-- | This type class allows to use both mutable and immutable vector
--   as input parameters to functions operating in 'PrimMonad' with
--   state token @s@.
class InSymmetric s m where
  -- | Expose internal representation of a type. Expected to be /O(1)/
  --   and very cheap.
  symmetricRepr :: m a -> ST s (MSymView a)

instance s ~ s' => InSymmetric s (MSymmetric s') where
  {-# INLINE symmetricRepr #-}
  symmetricRepr = pure . coerce

----------------------------------------------------------------
-- Conversions
----------------------------------------------------------------

-- | Symmetrises matrix by copying value from above diagonal below.
symmetrizeMSymView :: Storable a => MSymView a -> IO ()
symmetrizeMSymView view@MSymView{..} = do
  loopUpD_ size $ \i j -> do
    reallyUnsafeWrite (MSymmetric view) (j,i) =<< reallyUnsafeRead (MSymmetric view) (i,j)

-- | Convert matrix to dense matrix. Resulting matrix will share
--   underlying buffer with symmetric matrix. All function that modify
--   symmetric matrix will only element on diagonal and above unless
--   noted otherwise.
unsafeToDense
  :: forall a m s. (Storable a, PrimMonad m, s ~ PrimState m)
  => MSymmetric s a -> m (MMatrix s a)
unsafeToDense (MSymmetric view@MSymView{..}) = unsafeIOToPrim $ do
  symmetrizeMSymView view
  pure $ MMat.unsafeCast $ MMatrix MView
    { nrows      = size
    , ncols      = size
    , leadingDim = leadingDim
    , buffer     = buffer
    }

-- | /O(1)/ Cast symmetric matrix to hermitian one if its elements
--   are real.
asMHermitian :: (R a ~ a) => MSymmetric s a -> MSymmetric s a
asMHermitian = coerce
{-# INLINE asMHermitian #-}

----------------------------------------------------------------
--
----------------------------------------------------------------

instance Storable a => NDMutable MSymmetric a where
  basicUnsafeReadArr mat (D2 i j)
    | j >= i    = reallyUnsafeRead mat (i,j)
    | otherwise = reallyUnsafeRead mat (j,i)
  basicUnsafeWriteArr mat (D2 i j)
    | j >= i    = reallyUnsafeWrite mat (i,j)
    | otherwise = reallyUnsafeWrite mat (j,i)

unsafeCast :: MSymmetric s a -> MSymmetric s' a
unsafeCast = coerce
{-# INLINE unsafeCast #-}


 
----------------------------------------------------------------
-- Mutation
----------------------------------------------------------------

-- | Read value at given index. Index must be in range and reference
--   item on or above diagonal. Content of items below diagonal is
--   undefined.
--
-- __UNSAFE__: this function does not any range checks.
reallyUnsafeRead
  :: forall a m s. (Storable a, PrimMonad m, s ~ PrimState m)
  => MSymmetric s a -> (Int, Int) -> m a
{-# INLINE reallyUnsafeRead #-}
reallyUnsafeRead (MSymmetric MSymView{..}) (i,j)
  = unsafePrimToPrim
  $ unsafeWithForeignPtr buffer $ \p -> do
    peekElemOff p (i * leadingDim + j)

-- | Write value at given index. Index must be in range and reference
--   item on or above diagonal. Writes below diagonal will have not
--   effect.
--
-- __UNSAFE__: this function does not any range checks.
reallyUnsafeWrite
  :: (Storable a, PrimMonad m, s ~ PrimState m)
  => MSymmetric s a -> (Int, Int) -> a -> m ()
{-# INLINE reallyUnsafeWrite #-}
reallyUnsafeWrite (MSymmetric MSymView{..}) (i,j) a
  = unsafePrimToPrim
  $ unsafeWithForeignPtr buffer $ \p -> do
    pokeElemOff p (i * leadingDim + j) a



----------------------------------------------------------------
-- Creation
----------------------------------------------------------------

-- | Create copy of mutable matrix
clone
  :: forall a m mat s. (Storable a, PrimMonad m, s ~ PrimState m, InSymmetric s mat)
  => mat a -> m (MSymmetric s a)
{-# INLINE clone #-}
clone mat = stToPrim $ do
  MSymView{..} <- symmetricRepr mat
  unsafeIOToPrim $ do
    buf <- mallocForeignPtrArray (size * size)
    unsafeWithForeignPtr buffer $ \src ->
      unsafeWithForeignPtr buf $ \dst ->
      -- FIXME: We copy all elements in matrix
      if-- Source buffer is dense. We can copy in one go
        | size == leadingDim -> copyArray dst src (size*size)
        -- We have to copy row by row
        | otherwise -> let loop !d !s i
                             | i >= size = return ()
                             | otherwise  = do
                                 copyArray d s size
                                 loop (advancePtr d size) (advancePtr s leadingDim) (i+1)
                       in loop dst src 0
    pure $ MSymmetric MSymView { size       = size
                               , leadingDim = size
                               , buffer     = buf
                               }

-- | Create matrix from list of rows. Each row contains elements
--   starting from diagonal.
fromRowsFF :: (Storable a, Foldable f, Foldable g, PrimMonad m, s ~ PrimState m)
           => f (g a) -> m (MSymmetric s a)
fromRowsFF dat
  | otherwise = unsafeIOToPrim $ do
      mat <- unsafeNew n
      let loop !_ [] = pure ()
          loop i  (row:rest)
            | n /= (length row + i) = error "Row has incorrect length"
            | otherwise             = do fillRow i (toList row)
                                         loop (i+1) rest
            where
              fillRow !_ []     = pure ()
              fillRow  j (x:xs) = reallyUnsafeWrite mat (i,j) x >> fillRow (j+1) xs
      loop 0 (toList dat)
      pure $ unsafeCast mat
  where
    n = length dat

-- | Create matrix from list of rows. Each row contains elements
--   starting from diagonal.
fromRowsFV :: (Storable a, Foldable f, VG.Vector v a, PrimMonad m, s ~ PrimState m)
           => f (v a) -> m (MSymmetric s a)
{-# INLINE fromRowsFV #-}
fromRowsFV dat
  | otherwise = unsafeIOToPrim $ do
      mat <- unsafeNew n
      let loop !_ [] = pure ()
          loop i  (row:rest)
            | n /= (VG.length row + i) = error "Row has incorrect length"
            | otherwise                = do
                VG.imapM_ (\j x -> reallyUnsafeWrite mat (i,i+j) x) row
                loop (i+1) rest
      loop 0 (toList dat)
      pure $ unsafeCast mat
  where
    n = length dat

-- | Allocate new matrix. Content of buffer zeroed out.
new :: (Storable a, PrimMonad m, s ~ PrimState m)
    => Int -- ^ Matrix size
    -> m (MSymmetric s a)
new n = do
  MVec buffer <- MVG.new (n * n)
  pure $ MSymmetric MSymView { size       = n
                             , leadingDim = n
                             , buffer     = buffer.vecBuffer
                             }

-- | Allocate new matrix. Content of buffer if not touched and may
--   contain remnants of some earlier data
unsafeNew
  :: (Storable a, PrimMonad m, s ~ PrimState m)
  => Int -- ^ Matrix size
  -> m (MSymmetric s a)
unsafeNew n = do
  MVec buffer <- MVG.unsafeNew (n * n)
  pure $ MSymmetric MSymView { size       = n
                                 , leadingDim = n
                                 , buffer     = buffer.vecBuffer
                                 }

-- | Fill matrix of given size with provided value.
replicate
  :: (Storable a, PrimMonad m, s ~ PrimState m)
  => Int -- ^ Size of matrix
  -> a   -- ^ Element to replicate
  -> m (MSymmetric s a)
replicate n a = stToPrim $ do
  mat <- unsafeNew n
  loopUp_ n $ \i j ->
    reallyUnsafeWrite mat (i,j) a
  pure mat

-- | Fill matrix of given size using provided monadic action.
replicateM :: (Storable a, PrimMonad m, s ~ PrimState m)
           => Int -- ^ Matrix size
           -> m a
           -> m (MSymmetric s a)
replicateM n action = do
  mat <- unsafeNew n
  loopUp_ n $ \i j -> do
    a <- action
    reallyUnsafeWrite mat (i,j) a
  pure mat

-- | Create matrix filled with zeros. It's more efficient than using
--   'replicate'.
zeros :: (LAPACKy a, PrimMonad m, s ~ PrimState m)
      => Int -- ^ Size of a matrix
      -> m (MSymmetric s a)
zeros n = stToPrim $ do
  mat@(MSymmetric MSymView{..}) <- unsafeNew n
  unsafeIOToPrim $ unsafeWithForeignPtr buffer $ \p -> C.fillZeros p (n*n)
  pure mat

-- | Fill matrix of given size using function from indices to element.
generate
  :: (Storable a, PrimMonad m, s ~ PrimState m)
  => Int               -- ^ Size of matrix
  -> (Int -> Int -> a) -- ^ Function that takes \(N_{row}\) and \(N_{column}\) as input
  -> m (MSymmetric s a)
generate n a = stToPrim $ do
  mat <- unsafeNew n
  loopUp_ n $ \i j ->
    reallyUnsafeWrite mat (i,j) (a i j)
  pure mat

-- | Fill matrix of given size using monadic function from indices to element.
generateM
  :: (Storable a, PrimMonad m, s ~ PrimState m)
  => Int -- ^ Matrix size
  -> (Int -> Int -> m a)
  -> m (MSymmetric s a)
generateM n action = do
  mat <- unsafeNew n
  loopUp_ n $ \i j -> do
    a <- action i j
    reallyUnsafeWrite mat (i,j) a
  pure mat


-- | Create identity matrix
eye :: (LAPACKy a, Num a, PrimMonad m, s ~ PrimState m)
    => Int -- ^ Matrix size
    -> m (MSymmetric s a)
eye n = stToPrim $ do
  mat <- zeros n
  loop0_ n $ \i -> unsafeWriteArr mat (i,i) 1
  pure mat

-- | Create diagonal matrix. Diagonal elements are stored in vector.
diag :: (LAPACKy a, VG.Vector v a, PrimMonad m, s ~ PrimState m)
     => v a
     -> m (MSymmetric s a)
{-# INLINE diag #-}
diag xs = stToPrim $ do
  mat <- zeros n
  VG.iforM_ xs $ \i x -> unsafeWriteArr mat (i,i) x
  pure mat
  where
    n = VG.length xs

-- | Create diagonal matrix. Diagonal elements are stored in list-like
--   container.
diagF :: (LAPACKy a, Foldable f, PrimMonad m, s ~ PrimState m)
      => f a
      -> m (MSymmetric s a)
diagF xs = stToPrim $ do
  mat <- zeros n
  -- FIXME: is build/foldr fusion reliable here?
  forM_ ([0..] `zip` toList xs) $ \(i,x) -> unsafeWriteArr mat (i,i) x
  pure mat
  where
    n = length xs



----------------------------------------------------------------
-- BLAS wrappers
----------------------------------------------------------------

-- | matrix-vector multiplication by symmetric matrix
--
-- > y := αAx + βy
unsafeBlasSymv
  :: forall a m mat vec s.
     (C.LAPACKy a, PrimMonad m, s ~ PrimState m, InSymmetric s mat, InVector s vec)
  => a               -- ^ Scalar @α@
  -> mat a           -- ^ Matrix @A@
  -> vec a           -- ^ Vector @x@
  -> a               -- ^ Scalar @β@
  -> MVec s a        -- ^ Vector @y@
  -> m ()
{-# INLINE unsafeBlasSymv #-}
unsafeBlasSymv α mat vecX β (MVec (VecRepr _ incY fpY)) = stToPrim $ do
  MSymView{..}           <- symmetricRepr mat
  VecRepr _lenX incX fpX <- vectorRepr    vecX
  unsafeIOToPrim $
    unsafeWithForeignPtr buffer $ \p_A ->
    unsafeWithForeignPtr fpX    $ \p_x ->
    unsafeWithForeignPtr fpY    $ \p_y ->
      C.symv C.RowMajor C.UP
        (C.toB size) α p_A (C.toB leadingDim)
        p_x (C.toB incX)
        β p_y (C.toB incY)

-- | Multiplication of general matrix @B@ by symmetric matrix @A@ on the left
--
-- > C := αAB + βC
unsafeBlasSymmL
  :: forall a m matA matB s.
     ( C.LAPACKy a, PrimMonad m, s ~ PrimState m
     , InSymmetric s matA, InMatrix s matB
     )
  => a               -- ^ Scalar @α@
  -> matA a          -- ^ Matrix @A@
  -> matB a          -- ^ Matrix @a@
  -> a               -- ^ Scalar @β@
  -> MMatrix s a     -- ^ Vector @y@
  -> m ()
{-# INLINE unsafeBlasSymmL #-}
unsafeBlasSymmL α matA matB β (MMatrix mC) = stToPrim $ do
  mA <- symmetricRepr matA
  mB <- matrixRepr    matB
  unsafeIOToPrim $
    unsafeWithForeignPtr mA.buffer $ \p_A ->
    unsafeWithForeignPtr mB.buffer $ \p_B ->
    unsafeWithForeignPtr mC.buffer $ \p_C -> do
      C.symm C.RowMajor C.LeftSide C.UP
        (C.toB mC.nrows) (C.toB mC.ncols)
        α p_A (C.toB mA.leadingDim)
          p_B (C.toB mB.leadingDim)
        β p_C (C.toB mC.leadingDim)

-- | Multiplication of general matrix @B@ by symmetric matrix @A@ on the right
--
-- > C := αBA + βC
unsafeBlasSymmR
  :: forall a m matA matB s.
     ( C.LAPACKy a, PrimMonad m, s ~ PrimState m
     , InSymmetric s matA, InMatrix s matB
     )
  => a               -- ^ Scalar @α@
  -> matB a          -- ^ Matrix @B@
  -> matA a          -- ^ Matrix @A@
  -> a               -- ^ Scalar @β@
  -> MMatrix s a     -- ^ Vector @y@
  -> m ()
{-# INLINE unsafeBlasSymmR #-}
unsafeBlasSymmR α matB matA β (MMatrix mC) = stToPrim $ do
  mB <- matrixRepr    matB
  mA <- symmetricRepr matA
  unsafeIOToPrim $
    unsafeWithForeignPtr mA.buffer $ \p_A ->
    unsafeWithForeignPtr mB.buffer $ \p_B ->
    unsafeWithForeignPtr mC.buffer $ \p_C -> do
      C.symm C.RowMajor C.RightSide C.UP
        (C.toB mC.nrows) (C.toB mC.ncols)
        α p_A (C.toB mA.leadingDim)
          p_B (C.toB mB.leadingDim)
        β p_C (C.toB mC.leadingDim)
