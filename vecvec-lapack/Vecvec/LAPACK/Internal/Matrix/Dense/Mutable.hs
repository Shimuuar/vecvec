{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost        #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}
-- |
module Vecvec.LAPACK.Internal.Matrix.Dense.Mutable
  ( -- * Data types
    MMatrix(..)
  , MView(..)
  , AsMInput(..)
  , pattern AsMVec
    -- * Operations
    -- ** Creation
  , clone
  , new
  , unsafeNew
  , fromRowsFF
  , fromRowsFV
  , fromColsFF
  , fromColsFV
  , replicate
  , replicateM
  , generate
  , generateM
  , zeros
  , eye
  , diagF
  , diag
  , gdiagF
  , gdiag
    -- ** Access
  , read
  , write
  , getCol
  , getRow
    -- * BLAS wrappers
  , MatrixTranspose(..)
  , unsafeBlasGemv
  , unsafeBlasGemm
    -- * Unsafe functions
  , unsafeRead
  , unsafeWrite
  , unsafeGetCol
  , unsafeGetRow
  ) where

import Control.Monad           (foldM)
import Control.Monad.Primitive
import Data.Coerce
import Data.Foldable
import Data.Vector.Generic.Mutable           qualified as MVG
import Data.Vector.Fixed.Cont                qualified as FC
import Data.Vector.Generic                   qualified as VG
import Foreign.ForeignPtr
import Foreign.Storable
import Foreign.Marshal.Array

import Prelude hiding (read,replicate)

import Vecvec.Classes.NDArray
import Vecvec.Classes.Util
import Vecvec.LAPACK.Internal.Compat
import Vecvec.LAPACK.Internal.Vector.Mutable hiding (clone)
import Vecvec.LAPACK.FFI                     qualified as C
import Vecvec.LAPACK.FFI                     (MatrixTranspose(..))


----------------------------------------------------------------
-- Internal memory representation
----------------------------------------------------------------

-- | In-memory representation of mutable and immutable matrices.
data MView a = MView
  { nrows      :: !Int            -- ^ Number of rows
  , ncols      :: !Int            -- ^ Number of columns
  , leadingDim :: !Int            -- ^ Leading dimension size. Matrix
                                  --   is stored in row-major order
                                  --   so it's row size.
  , buffer     :: !(ForeignPtr a) -- ^ Underlying buffer
  }
  deriving Show

instance (Slice1D i, Slice1D j, Storable a) => Slice (i,j) (MView a) where
  {-# INLINE sliceMaybe #-}
  sliceMaybe (idxI,idxJ) MView{..} = do
    (i,lenI) <- computeSlice1D nrows idxI
    (j,lenJ) <- computeSlice1D ncols idxJ
    return MView { nrows      = lenI
                 , ncols      = lenJ
                 , leadingDim = leadingDim
                 , buffer     = updPtr (`advancePtr` (leadingDim * i + j)) buffer
                 }

-- | Values that could be used as read-only dense matrix parameter.
class AsMInput s m where
  asMInput :: m a -> MView a

instance s ~ s' => AsMInput s' (MMatrix s) where
  {-# INLINE asMInput #-}
  asMInput = coerce


----------------------------------------------------------------
-- Mutable matrices
----------------------------------------------------------------

-- | Mutable generic dense matrix.
newtype MMatrix s a = MMatrix (MView a)

deriving newtype instance (Slice1D i, Slice1D j, Storable a) => Slice (i,j) (MMatrix s a)

instance HasShape (MMatrix s a) where
  type NDim (MMatrix s a) = 2
  shapeAsCVec (MMatrix MView{..}) = FC.mk2 nrows ncols
  {-# INLINE shapeAsCVec #-}


-- | Pattern which is used to check whether matrix is represented by
--   contiguous vector
pattern AsMVec :: MVec s a -> MMatrix s a
pattern AsMVec v <- (tryMVec -> Just v)

tryMVec :: MMatrix s a -> Maybe (MVec s a)
{-# INLINE tryMVec #-}
tryMVec (MMatrix MView{..})
  | ncols /= leadingDim = Nothing
  | otherwise           = Just (MVec (VecRepr (ncols * nrows) 1 buffer))



-- | Get nth row of matrix.
--
-- __UNSAFE__: this function does not any range checks.
unsafeGetRow :: (Storable a) => MMatrix s a -> Int -> MVec s a
unsafeGetRow (MMatrix MView{..}) i =
   MVec (VecRepr ncols 1 (updPtr (`advancePtr` (leadingDim * i)) buffer))

-- | Get nth column of matrix.
--
-- __UNSAFE__: this function does not any range checks.
unsafeGetCol :: (Storable a) => MMatrix s a -> Int -> MVec s a
unsafeGetCol (MMatrix MView{..}) i =
  MVec (VecRepr nrows leadingDim (updPtr (`advancePtr` i) buffer))

-- | Read value at given index
--
-- __UNSAFE__: this function does not any range checks.
unsafeRead :: forall a m mat s. (Storable a, PrimMonad m, s ~ PrimState m, AsMInput s mat)
           => mat a -> (Int, Int) -> m a
{-# INLINE unsafeRead #-}
unsafeRead (asMInput @s -> MView{..}) (i,j)
  = unsafePrimToPrim
  $ unsafeWithForeignPtr buffer $ \p -> do
    peekElemOff p (i * leadingDim + j)

-- | Write value at given index.
--
-- __UNSAFE__: this function does not any range checks.
unsafeWrite :: (Storable a, PrimMonad m, s ~ PrimState m)
            => MMatrix s a -> (Int, Int) -> a -> m ()
{-# INLINE unsafeWrite #-}
unsafeWrite (MMatrix MView{..}) (i,j) a
  = unsafePrimToPrim
  $ unsafeWithForeignPtr buffer $ \p -> do
    pokeElemOff p (i * leadingDim + j) a


-- | Get nth row of matrix.
getRow :: (Storable a) => MMatrix s a -> Int -> MVec s a
getRow m@(MMatrix MView{..}) i
  | i < 0 || i >= nrows = error "Out of range"
  | otherwise           = unsafeGetRow m i

-- | Get nth column of matrix.
getCol :: (Storable a) => MMatrix s a -> Int -> MVec s a
getCol m@(MMatrix MView{..}) i
  | i < 0 || i >= ncols = error "Out of range"
  | otherwise           = unsafeGetCol m i

-- | Read value at given index
read :: forall a m mat s. (Storable a, PrimMonad m, s ~ PrimState m, AsMInput s mat)
     => mat a -> (Int, Int) -> m a
{-# INLINE read #-}
read m@(asMInput @s -> MView{..}) (i,j)
  | i < 0 || i >= nrows = error "Out of range"
  | j < 0 || i >= ncols = error "Out of range"
  | otherwise           = unsafeRead m (i,j)

-- | Write value at given index.
write :: (Storable a, PrimMonad m, s ~ PrimState m)
      => MMatrix s a -> (Int, Int) -> a -> m ()
{-# INLINE write #-}
write m@(MMatrix MView{..}) (i,j) a
  | i < 0 || i >= nrows = error "Out of range"
  | j < 0 || i >= ncols = error "Out of range"
  | otherwise           = unsafeWrite m (i,j) a


----------------------------------------------------------------
-- Constructors
----------------------------------------------------------------

-- | Create copy of mutable matrix
clone :: forall a m mat s. (Storable a, PrimMonad m, s ~ PrimState m, AsMInput s mat)
      => mat a -> m (MMatrix s a)
clone (asMInput @s -> MView{..}) = unsafePrimToPrim $ do
  buf <- mallocForeignPtrArray n_elt
  unsafeWithForeignPtr buffer $ \src ->
    unsafeWithForeignPtr buf $ \dst ->
    if-- Source buffer is dense. We can copy in one go
      | ncols == leadingDim -> copyArray dst src n_elt
      -- We have to copy row by row
      | otherwise -> let loop !d !s i
                           | i >= nrows = return ()
                           | otherwise  = do
                               copyArray d s ncols
                               loop (advancePtr d ncols) (advancePtr s leadingDim) (i+1)
                     in loop dst src 0
  pure $ MMatrix MView { buffer     = buf
                       , leadingDim = ncols
                       , ..
                       }
  where
    n_elt = ncols * nrows

-- | Create matrix from list of rows.
fromRowsFF :: (Storable a, Foldable f, Foldable g, PrimMonad m, s ~ PrimState m)
           => f (g a) -> m (MMatrix s a)
fromRowsFF dat
  | ncols == 0 = error "Number of columns is zero"
  | nrows == 0 = error "Number of rows is zero"
  | otherwise = unsafePrimToPrim $ do
      buffer <- mallocForeignPtrArray (ncols * nrows)
      let step p row
            | length row /= ncols = error "Row has different length"
            | otherwise           = do
                pokeArray p (toList row)
                pure $! advancePtr p ncols
      _ <- unsafeWithForeignPtr buffer $ \p -> foldM step p dat
      pure $ MMatrix MView { leadingDim = ncols
                           , ..
                           }
  where
    nrows = length dat
    ncols = length $ head $ toList dat

-- | Create matrix from list of rows.
fromRowsFV :: (Storable a, Foldable f, VG.Vector v a, PrimMonad m, s ~ PrimState m)
           => f (v a) -> m (MMatrix s a)
{-# INLINE fromRowsFV #-}
fromRowsFV dat
  | ncols == 0 = error "Number of columns is zero"
  | nrows == 0 = error "Number of rows is zero"
  | otherwise = unsafePrimToPrim $ do
      buffer <- mallocForeignPtrArray (ncols * nrows)
      let step ptr0 row
            | VG.length row /= ncols = error "Row has different length"
            | otherwise              = VG.foldM
                (\ptr a -> advancePtr ptr 1 <$ poke ptr a)
                ptr0 row
      _ <- unsafeWithForeignPtr buffer $ \p -> foldM step p dat
      pure $ MMatrix MView { leadingDim = ncols
                           , ..
                           }
  where
    nrows = length dat
    ncols = VG.length $ head $ toList dat

-- | Create matrix from list of columns.
fromColsFF :: (Storable a, Foldable f, Foldable g, PrimMonad m, s ~ PrimState m)
           => f (g a) -> m (MMatrix s a)
fromColsFF dat
  | ncols == 0 = error "Number of columns is zero"
  | nrows == 0 = error "Number of rows is zero"
  | otherwise = unsafePrimToPrim $ do
      buffer <- mallocForeignPtrArray (ncols * nrows)
      let step ptr0 col
            | length col /= nrows = error "Columns has different length"
            | otherwise           = do
                _ <- foldM (\ptr a -> advancePtr ptr ncols <$ poke ptr a) ptr0 col
                pure $! advancePtr ptr0 1
      _ <- unsafeWithForeignPtr buffer $ \p -> foldM step p dat
      pure $ MMatrix MView { leadingDim = ncols
                           , ..
                           }
  where
    ncols = length dat
    nrows = length $ head $ toList dat

-- | Create matrix from list of columns.
fromColsFV :: (Storable a, Foldable f, VG.Vector v a, PrimMonad m, s ~ PrimState m)
           => f (v a) -> m (MMatrix s a)
{-# INLINE fromColsFV #-}
fromColsFV dat
  | ncols == 0 = error "Number of columns is zero"
  | nrows == 0 = error "Number of rows is zero"
  | otherwise = unsafePrimToPrim $ do
      buffer <- mallocForeignPtrArray (ncols * nrows)
      let step ptr0 col
            | VG.length col /= nrows = error "Columns has different length"
            | otherwise              = do
                VG.foldM_ (\ptr a -> advancePtr ptr ncols <$ poke ptr a) ptr0 col
                pure $! advancePtr ptr0 1
      _ <- unsafeWithForeignPtr buffer $ \p -> foldM step p dat
      pure $ MMatrix MView { leadingDim = ncols
                           , ..
                           }
  where
    ncols = length dat
    nrows = VG.length $ head $ toList dat


-- | Allocate new matrix. Content of buffer if not touched and may
--   contain remnants of some earlier data
unsafeNew :: (Storable a, PrimMonad m, s ~ PrimState m)
          => (Int,Int) -> m (MMatrix s a)
unsafeNew (n,k) = do
  MVec buffer <- MVG.unsafeNew (n * k)
  pure $ MMatrix MView { nrows      = n
                       , ncols      = k
                       , leadingDim = k
                       , buffer     = vecBuffer buffer
                       }

-- | Allocate new matrix. Content of buffer zeroed out.
new :: (Storable a, PrimMonad m, s ~ PrimState m)
    => (Int,Int) -- ^ Tuple (\(N_{rows}\), \(N_{columns}\))
    -> m (MMatrix s a)
new (n,k) = do
  MVec buffer <- MVG.new (n * k)
  pure $ MMatrix MView { nrows      = n
                       , ncols      = k
                       , leadingDim = k
                       , buffer     = vecBuffer buffer
                       }

-- | Fill matrix of given size with provided value.
replicate :: (Storable a, PrimMonad m, s ~ PrimState m)
          => (Int,Int) -- ^ Tuple (\(N_{rows}\), \(N_{columns}\))
          -> a
          -> m (MMatrix s a)
replicate (n,k) a = unsafeIOToPrim $ do
  MMatrix mat@MView{..} <- unsafeNew (n,k)
  unsafeWithForeignPtr buffer $ \ptr -> do
    forM_ [0 .. (n*k) - 1] $ \i -> pokeElemOff ptr i a
  pure (MMatrix mat)

-- | Fill matrix of given size using provided monadic action.
replicateM :: (Storable a, PrimMonad m, s ~ PrimState m)
           => (Int,Int) -- ^ Tuple (\(N_{rows}\), \(N_{columns}\))
           -> m a
           -> m (MMatrix s a)
replicateM (n,k) action = do
  mat@(MMatrix MView{..}) <- unsafeNew (n,k)
  forM_ [0 .. (n*k) - 1] $ \i -> do
    a <- action
    unsafeIOToPrim $ unsafeWithForeignPtr buffer $ \ptr -> pokeElemOff ptr i a
  pure mat

-- | Fill matrix of given size using function from indices to element.
generate :: (Storable a, PrimMonad m, s ~ PrimState m)
         => (Int,Int)         -- ^ Tuple (\(N_{rows}\), \(N_{columns}\))
         -> (Int -> Int -> a) -- ^ Function that takes \(N_{row}\) and \(N_{column}\) as input
         -> m (MMatrix s a)
generate (n,k) fun = stToPrim $ do
  mat <- unsafeNew (n,k)
  forM_ [0 .. n-1] $ \i ->
    forM_ [0 .. k-1] $ \j ->
      unsafeWrite mat (i,j) (fun i j)
  pure mat

-- | Fill matrix of given size using monadic function from indices to element.
generateM :: (Storable a, PrimMonad m, s ~ PrimState m)
          => (Int,Int)           -- ^ Tuple (\(N_{rows}\), \(N_{columns}\))
          -> (Int -> Int -> m a) -- ^ Function that takes \(N_{row}\) and \(N_{column}\) as input
          -> m (MMatrix s a)
generateM (n,k) fun = do
  mat <- unsafeNew (n,k)
  forM_ [0 .. n-1] $ \i ->
    forM_ [0 .. k-1] $ \j -> do
      unsafeWrite mat (i,j) =<< fun i j
  pure mat


-- | Create matrix filled with zeros. It's more efficient than using
--   'replicate'.
zeros :: (StorableZero a, PrimMonad m, s ~ PrimState m)
      => (Int,Int) -- ^ Tuple (\(N_{rows}\), \(N_{columns}\))
      -> m (MMatrix s a)
zeros (n,k) = unsafeIOToPrim $ do
  (MMatrix mat@MView{..}) <- unsafeNew (n,k)
  unsafeWithForeignPtr buffer $ \p -> zeroOutBuffer p (n*k)
  pure (MMatrix mat)

-- | Create identity matrix
eye :: (StorableZero a, Num a, PrimMonad m, s ~ PrimState m)
    => Int -- ^ Matrix size
    -> m (MMatrix s a)
eye n = stToPrim $ do
  mat <- zeros (n,n)
  -- FIXME: is build/foldr fusion reliable here?
  forM_ [0..n-1] $ \i -> unsafeWrite mat (i,i) 1
  pure mat

-- | Create diagonal matrix. Diagonal elements are stored in list-like
--   container.
diagF :: (StorableZero a, Foldable f, PrimMonad m, s ~ PrimState m)
      => f a
      -> m (MMatrix s a)
diagF xs = stToPrim $ do
  mat <- zeros (n,n)
  -- FIXME: is build/foldr fusion reliable here?
  forM_ ([0..] `zip` toList xs) $ \(i,x) -> unsafeWrite mat (i,i) x
  pure mat
  where
    n = length xs

-- | Create diagonal matrix. Diagonal elements are stored in vector.
diag :: (StorableZero a, VG.Vector v a, PrimMonad m, s ~ PrimState m)
     => v a
     -> m (MMatrix s a)
{-# INLINE diag #-}
diag xs = stToPrim $ do
  mat <- zeros (n,n)
  VG.iforM_ xs $ \i x -> unsafeWrite mat (i,i) x
  pure mat
  where
    n = VG.length xs

-- | Create general diagonal matrix. Diagonal elements are stored in vector.
gdiagF :: (StorableZero a, Foldable f, PrimMonad m, s ~ PrimState m)
       => (Int,Int)
       -> f a
       -> m (MMatrix s a)
{-# INLINE gdiagF #-}
gdiagF (n,k) xs
  | len > min n k = error "Diagonal is too long"
  | otherwise     = stToPrim $ do
      mat <- zeros (n,k)
      -- FIXME: is build/foldr fusion reliable here?
      forM_ ([0..] `zip` toList xs) $ \(i,x) -> unsafeWrite mat (i,i) x
      pure mat
  where
    len = length xs

-- | Create general diagonal matrix. Diagonal elements are stored in vector.
gdiag :: (StorableZero a, VG.Vector v a, PrimMonad m, s ~ PrimState m)
      => (Int,Int)
      -> v a
      -> m (MMatrix s a)
{-# INLINE gdiag #-}
gdiag (n,k) xs
  | len > min n k = error "Diagonal is too long"
  | otherwise     = stToPrim $ do
      mat <- zeros (n,k)
      VG.iforM_ xs $ \i x -> unsafeWrite mat (i,i) x
      pure mat
  where
    len = VG.length xs



----------------------------------------------------------------
-- BLAS wrappers
----------------------------------------------------------------

-- | General matrix-vector multiplication
--
-- > y := αAx + βy
unsafeBlasGemv
  :: forall a m mat vec s. (C.LAPACKy a, PrimMonad m, s ~ PrimState m, AsMInput s mat, AsInput s vec)
  => MatrixTranspose -- ^ Matrix transformation
  -> a        -- ^ Scalar @α@
  -> mat a    -- ^ Matrix @A@
  -> vec a    -- ^ Vector @x@
  -> a        -- ^ Scalar @β@
  -> MVec s a -- ^ Vector @y@
  -> m ()
{-# INLINE unsafeBlasGemv #-}
unsafeBlasGemv tr α (asMInput @s -> MView{..}) vecX β (MVec (VecRepr _ incY fpY))
  = unsafePrimToPrim
  $ do VecRepr _lenX incX fpX <- unsafePrimToPrim $ asInput @s vecX
       id $ unsafeWithForeignPtr buffer $ \p_A ->
            unsafeWithForeignPtr fpX    $ \p_x ->
            unsafeWithForeignPtr fpY    $ \p_y ->
              C.gemv (C.toCEnum C.RowMajor) (C.toCEnum tr)
                (fromIntegral nrows) (fromIntegral ncols) α p_A (fromIntegral leadingDim)
                p_x (fromIntegral incX)
                β p_y (fromIntegral incY)

-- | General matrix-matrix multiplication
--
-- > C := α·op(A)·op(B) + β·C
unsafeBlasGemm
  :: forall a m matA matB s. (C.LAPACKy a, PrimMonad m, s ~ PrimState m, AsMInput s matA, AsMInput s matB)
  => a               -- ^ Scalar @α@
  -> MatrixTranspose -- ^ Transformation for @A@
  -> matA a          -- ^ Matrix @A@
  -> MatrixTranspose -- ^ Transformation for @B@
  -> matB a          -- ^ Matrix @B@
  -> a               -- ^ Scalar @β@
  -> MMatrix s a     -- ^ Matrix @C@
  -> m ()
unsafeBlasGemm α trA (asMInput @s -> matA) trB (asMInput @s -> matB) β (MMatrix matC)
  = unsafePrimToPrim
  $ unsafeWithForeignPtr (buffer matA) $ \p_A ->
    unsafeWithForeignPtr (buffer matB) $ \p_B ->
    unsafeWithForeignPtr (buffer matC) $ \p_C ->
      C.gemm (C.toCEnum C.RowMajor)
        (C.toCEnum trA) (C.toCEnum trB)
        (fromIntegral $ if trA == C.NoTrans then nrows matA else ncols matA)
        (fromIntegral $ if trB == C.NoTrans then ncols matB else nrows matB)
        (fromIntegral $ if trB == C.NoTrans then nrows matB else ncols matB)
        α p_A (fromIntegral $ leadingDim matA)
          p_B (fromIntegral $ leadingDim matB)
        β p_C (fromIntegral $ leadingDim matC)
