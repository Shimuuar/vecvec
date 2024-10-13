{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies    #-}
-- |
module Vecvec.LAPACK.Unsafe.Matrix.Mutable
  ( -- * Data types
    MMatrix(..)
  , MView(..)
  , InMatrix(..)
  , pattern AsMVec
  , LAPACKy
    -- * Operations
    -- ** Creation
  , clone
  , copy
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
  , getCol
  , getRow
    -- * BLAS wrappers
  , MatrixTranspose(..)
  , unsafeBlasGemv
  , unsafeBlasGemm
    -- * Unsafe functions
  , unsafeGetCol
  , unsafeGetRow
  , unsafeCast
  ) where

import Control.Monad           (foldM,when)
import Control.Monad.Primitive
import Control.Monad.ST
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
import Vecvec.Classes.NDMutable
import Vecvec.Classes.Deriving
import Vecvec.LAPACK.Utils
import Vecvec.LAPACK.Unsafe.Compat
import Vecvec.LAPACK.Unsafe.Vector.Mutable hiding (clone)
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

type instance Rank MView = 2

instance HasShape MView a where
  shapeAsCVec MView{..} = FC.mk2 nrows ncols
  {-# INLINE shapeAsCVec #-}


-- | This type class allows to use both mutable and immutable vector
--   as input parameters to functions operating in 'PrimMonad' with
--   state token @s@.
class InMatrix s m where
  -- | Expose internal representation of a type. Expected to be /O(1)/
  --   and very cheap.
  matrixRepr :: m a -> ST s (MView a)

instance s ~ s' => InMatrix s' (MMatrix s) where
  {-# INLINE matrixRepr #-}
  matrixRepr = pure . coerce


----------------------------------------------------------------
-- Mutable matrices
----------------------------------------------------------------

-- | Mutable generic dense matrix.
newtype MMatrix s a = MMatrix (MView a)

deriving newtype instance (Slice1D i, Slice1D j, Storable a) => Slice (i,j) (MMatrix s a)

type instance Rank (MMatrix s) = 2

instance HasShape (MMatrix s) a where
  shapeAsCVec (MMatrix MView{..}) = FC.mk2 nrows ncols
  {-# INLINE shapeAsCVec #-}

instance Storable a => NDMutable MMatrix a where
  basicUnsafeReadArr (MMatrix MView{..}) (FC.ContVec idx)
    = unsafeIOToPrim
    $ idx $ FC.Fun $ \i j ->
      unsafeWithForeignPtr buffer $ \p ->
        peekElemOff p (i * leadingDim + j)
  basicUnsafeWriteArr (MMatrix MView{..}) (FC.ContVec idx) a
    = unsafeIOToPrim
    $ idx $ FC.Fun $ \i j ->
      unsafeWithForeignPtr buffer $ \p ->
        pokeElemOff p (i * leadingDim + j) a
  {-# INLINE basicUnsafeReadArr  #-}
  {-# INLINE basicUnsafeWriteArr #-}


unsafeCast :: MMatrix s a -> MMatrix s' a
unsafeCast = coerce

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


-- | Get nth row of matrix.
getRow :: (Storable a) => MMatrix s a -> Int -> MVec s a
getRow m@(MMatrix MView{..}) i
  | inRange i nrows = unsafeGetRow m i
  | otherwise       = error "Out of range"

-- | Get nth column of matrix.
getCol :: (Storable a) => MMatrix s a -> Int -> MVec s a
getCol m@(MMatrix MView{..}) i
  | inRange i ncols = unsafeGetCol m i
  | otherwise       = error "Out of range"


----------------------------------------------------------------
-- Constructors
----------------------------------------------------------------

-- | Create copy of mutable matrix
clone :: forall a m mat s. (Storable a, PrimMonad m, s ~ PrimState m, InMatrix s mat)
      => mat a -> m (MMatrix s a)
clone mat = stToPrim $ do
  MView{..} <- matrixRepr mat
  let n_elt = ncols * nrows
  unsafeIOToPrim $ do
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


-- | Copy matrix to another matrix. They must have same dimensions
copy :: forall a m mat s. (Storable a, PrimMonad m, s ~ PrimState m, InMatrix s mat)
     => mat a       -- ^ Source
     -> MMatrix s a -- ^ Destination
     -> m ()
copy src_mat (MMatrix dst) = stToPrim $ do
  src <- matrixRepr src_mat
  -- FIXME: We need Eq for ContVec
  when (shape src /= (shape dst :: (Int,Int))) $
    error "copy: Dimensions do not match"
  --
  unsafeIOToPrim $
    unsafeWithForeignPtr src.buffer $ \p_src ->
    unsafeWithForeignPtr dst.buffer $ \p_dst ->
         -- Both source and destination are dense. We can copy in one go
      if | src.ncols == src.leadingDim
         , dst.ncols == dst.leadingDim
         -> copyArray p_dst p_src (src.ncols * src.nrows)
         | otherwise
         -> loop0_ src.nrows $ \i -> do
             copyArray
               (p_dst `advancePtr` (i * dst.leadingDim))
               (p_src `advancePtr` (i * src.leadingDim))
               src.ncols


-- | Create matrix from list of rows.
fromRowsFF :: (Storable a, Foldable f, Foldable g, PrimMonad m, s ~ PrimState m)
           => f (g a) -> m (MMatrix s a)
fromRowsFF dat
  | ncols == 0 = error "Number of columns is zero"
  | nrows == 0 = error "Number of rows is zero"
  | otherwise = unsafeIOToPrim $ do
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
    loop0_ (n*k) $ \i -> pokeElemOff ptr i a
  pure (MMatrix mat)

-- | Fill matrix of given size using provided monadic action.
replicateM :: (Storable a, PrimMonad m, s ~ PrimState m)
           => (Int,Int) -- ^ Tuple (\(N_{rows}\), \(N_{columns}\))
           -> m a
           -> m (MMatrix s a)
replicateM (n,k) action = do
  mat@(MMatrix MView{..}) <- unsafeNew (n,k)
  loop0_ (n*k) $ \i -> do
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
  loop0_ n $ \i ->
    loop0_ k $ \j ->
      unsafeWriteArr mat (i,j) (fun i j)
  pure mat

-- | Fill matrix of given size using monadic function from indices to element.
generateM :: (Storable a, PrimMonad m, s ~ PrimState m)
          => (Int,Int)           -- ^ Tuple (\(N_{rows}\), \(N_{columns}\))
          -> (Int -> Int -> m a) -- ^ Function that takes \(N_{row}\) and \(N_{column}\) as input
          -> m (MMatrix s a)
generateM (n,k) fun = do
  mat <- unsafeNew (n,k)
  loop0_ n $ \i ->
    loop0_ k $ \j -> do
      unsafeWriteArr mat (i,j) =<< fun i j
  pure mat


-- | Create matrix filled with zeros. It's more efficient than using
--   'replicate'.
zeros :: (LAPACKy a, PrimMonad m, s ~ PrimState m)
      => (Int,Int) -- ^ Tuple (\(N_{rows}\), \(N_{columns}\))
      -> m (MMatrix s a)
zeros (n,k) = stToPrim $ do
  mat@(MMatrix MView{..}) <- unsafeNew (n,k)
  unsafeIOToPrim $
    unsafeWithForeignPtr buffer $ \p -> C.fillZeros p (n*k)
  pure mat

-- | Create identity matrix
eye :: (LAPACKy a, Num a, PrimMonad m, s ~ PrimState m)
    => Int -- ^ Matrix size
    -> m (MMatrix s a)
eye n = stToPrim $ do
  mat <- zeros (n,n)
  loop0_ n $ \i -> unsafeWriteArr mat (i,i) 1
  pure mat

-- | Create diagonal matrix. Diagonal elements are stored in list-like
--   container.
diagF :: (LAPACKy a, Foldable f, PrimMonad m, s ~ PrimState m)
      => f a
      -> m (MMatrix s a)
diagF xs = stToPrim $ do
  mat <- zeros (n,n)
  -- FIXME: is build/foldr fusion reliable here?
  forM_ ([0..] `zip` toList xs) $ \(i,x) -> unsafeWriteArr mat (i,i) x
  pure mat
  where
    n = length xs

-- | Create diagonal matrix. Diagonal elements are stored in vector.
diag :: (LAPACKy a, VG.Vector v a, PrimMonad m, s ~ PrimState m)
     => v a
     -> m (MMatrix s a)
{-# INLINE diag #-}
diag xs = stToPrim $ do
  mat <- zeros (n,n)
  VG.iforM_ xs $ \i x -> unsafeWriteArr mat (i,i) x
  pure mat
  where
    n = VG.length xs

-- | Create general diagonal matrix. Diagonal elements are stored in vector.
gdiagF :: (LAPACKy a, Foldable f, PrimMonad m, s ~ PrimState m)
       => (Int,Int)
       -> f a
       -> m (MMatrix s a)
{-# INLINE gdiagF #-}
gdiagF (n,k) xs
  | len > min n k = error "Diagonal is too long"
  | otherwise     = stToPrim $ do
      mat <- zeros (n,k)
      -- FIXME: is build/foldr fusion reliable here?
      forM_ ([0..] `zip` toList xs) $ \(i,x) -> unsafeWriteArr mat (i,i) x
      pure mat
  where
    len = length xs

-- | Create general diagonal matrix. Diagonal elements are stored in vector.
gdiag :: (LAPACKy a, VG.Vector v a, PrimMonad m, s ~ PrimState m)
      => (Int,Int)
      -> v a
      -> m (MMatrix s a)
{-# INLINE gdiag #-}
gdiag (n,k) xs
  | len > min n k = error "Diagonal is too long"
  | otherwise     = stToPrim $ do
      mat <- zeros (n,k)
      VG.iforM_ xs $ \i x -> unsafeWriteArr mat (i,i) x
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
  :: forall a m mat vec s.
     (C.LAPACKy a, PrimMonad m, s ~ PrimState m, InMatrix s mat, InVector s vec)
  => MatrixTranspose -- ^ Matrix transformation
  -> a        -- ^ Scalar @α@
  -> mat a    -- ^ Matrix @A@
  -> vec a    -- ^ Vector @x@
  -> a        -- ^ Scalar @β@
  -> MVec s a -- ^ Vector @y@
  -> m ()
{-# INLINE unsafeBlasGemv #-}
unsafeBlasGemv tr α mat vecX β (MVec (VecRepr _ incY fpY)) = stToPrim $ do
  MView{..}              <- matrixRepr mat
  VecRepr _lenX incX fpX <- vectorRepr vecX
  unsafeIOToPrim $
    unsafeWithForeignPtr buffer $ \p_A ->
    unsafeWithForeignPtr fpX    $ \p_x ->
    unsafeWithForeignPtr fpY    $ \p_y ->
      C.gemv C.RowMajor tr
        (C.toB nrows) (C.toB ncols) α p_A (C.toB leadingDim)
        p_x (C.toB incX)
        β p_y (C.toB incY)

-- | General matrix-matrix multiplication
--
-- > C := α·op(A)·op(B) + β·C
unsafeBlasGemm
  :: forall a m matA matB s. (C.LAPACKy a, PrimMonad m, s ~ PrimState m, InMatrix s matA, InMatrix s matB)
  => a               -- ^ Scalar @α@
  -> MatrixTranspose -- ^ Transformation for @A@
  -> matA a          -- ^ Matrix @A@
  -> MatrixTranspose -- ^ Transformation for @B@
  -> matB a          -- ^ Matrix @B@
  -> a               -- ^ Scalar @β@
  -> MMatrix s a     -- ^ Matrix @C@
  -> m ()
unsafeBlasGemm α trA matA trB matB β (MMatrix mC) = stToPrim $ do
  mA <- matrixRepr matA
  mB <- matrixRepr matB
  unsafeIOToPrim $
    unsafeWithForeignPtr (buffer mA) $ \p_A ->
    unsafeWithForeignPtr (buffer mB) $ \p_B ->
    unsafeWithForeignPtr (buffer mC) $ \p_C ->
      C.gemm C.RowMajor trA trB
        (C.toB $ if trA == C.NoTrans then nrows mA else ncols mA)
        (C.toB $ if trB == C.NoTrans then ncols mB else nrows mB)
        (C.toB $ if trB == C.NoTrans then nrows mB else ncols mB)
        α p_A (C.toB $ leadingDim mA)
          p_B (C.toB $ leadingDim mB)
        β p_C (C.toB $ leadingDim mC)
