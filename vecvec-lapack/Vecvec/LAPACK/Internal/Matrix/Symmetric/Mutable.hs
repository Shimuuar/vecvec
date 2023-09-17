{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost        #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}
-- |
-- Symmetric matrices.
module Vecvec.LAPACK.Internal.Matrix.Symmetric.Mutable
  ( -- * Data types
    MSymmetric(..)
  , MSymView(..)
  , AsSymInput(..)
  , symmetrizeMSymView
    -- * Operations
    -- ** Creation
  , clone
  , new
  , unsafeNew
  , fromRowsFF
  , fromRowsFV
  , replicate
  , replicateM
  , generate
  , generateM
    -- ** Conversion
  -- , toDense
    -- * Unsafe functions
  , unsafeCast
    -- * Internals

  ) where

import Control.Monad.Primitive
import Data.Coerce
import Data.Foldable
import Data.Vector.Fixed.Cont                qualified as FC
import Data.Vector.Generic.Mutable           qualified as MVG
import Data.Vector.Generic                   qualified as VG
import Foreign.Storable
import Foreign.ForeignPtr
import Foreign.Marshal.Array

import Prelude hiding (read,replicate)

import Vecvec.Classes.NDMutable
-- import Vecvec.Classes.Deriving
import Vecvec.LAPACK.Utils
import Vecvec.LAPACK.Internal.Compat
import Vecvec.LAPACK.Internal.Vector.Mutable       hiding (clone)
import Vecvec.LAPACK.Internal.Matrix.Dense.Mutable qualified as MMat



----------------------------------------------------------------
-- View
----------------------------------------------------------------

-- | View on symmetric matrix. It's distinct from 'MMat.MView' because
--   symmetric matrix is always square and we want to preserve
--   original pointer for conversion of immutable symmetric matrices
--   to general.
data MSymView a = MSymView
  { size       :: !Int            -- ^ Number of row\/columns
  , leadingDim :: !Int            -- ^ Leading dimension
  , buffer     :: !(ForeignPtr a) -- ^ Underlying buffer
  }
  deriving Show

instance (Slice1D i, Storable a) => Slice i (MSymView a) where
  {-# INLINE sliceMaybe #-}
  sliceMaybe idx MSymView{..} = do
    (i,len) <- computeSlice1D size idx
    return MSymView { size       = len
                    , leadingDim = leadingDim
                    , buffer     = updPtr (`advancePtr` (i * (leadingDim + 1))) buffer
                    }

-- | Symmetrises matrix by copying value from above diagonal below.
symmetrizeMSymView :: Storable a => MSymView a -> IO ()
symmetrizeMSymView view@MSymView{..} = do
  loopUp_ size $ \i j -> do
    reallyUnsafeWrite (MSymmetric view) (j,i) =<< reallyUnsafeRead (MSymmetric view) (i,j)

-- | Values that could be used as read-only dense matrix parameter.
class AsSymInput s m where
  asSymInput :: m a -> MSymView a

instance s ~ s' => AsSymInput s (MSymmetric s') where
  {-# INLINE asSymInput #-}
  asSymInput = coerce

----------------------------------------------------------------
--
----------------------------------------------------------------

-- | Symmetric matrix. It uses same memory layout as general dense
--   matrix but only diagonal and elements above it are referenced.
newtype MSymmetric s a = MSymmetric (MSymView a)

deriving newtype instance (Slice1D i, Storable a) => Slice i (MSymmetric s a)

type instance Rank (MSymmetric s) = 2

instance HasShape (MSymmetric s) a where
  shapeAsCVec (MSymmetric MSymView{..}) = FC.mk2 size size
  {-# INLINE shapeAsCVec #-}

instance Storable a => NDMutable MSymmetric a where
  basicUnsafeReadArr mat (N2 i j)
    | j >= i    = reallyUnsafeRead mat (i,j)
    | otherwise = reallyUnsafeRead mat (j,i)
  basicUnsafeWriteArr mat (N2 i j)
    | j >= i    = reallyUnsafeWrite mat (i,j)
    | otherwise = reallyUnsafeWrite mat (j,i)

unsafeCast :: MSymmetric s a -> MSymmetric s' a
unsafeCast = coerce
{-# INLINE unsafeCast #-}

-- -- | Convert matrix to dense matrix. Resulting matrix will share
-- --   underlying buffer with symmetric matrix. All function that modify
-- --   symmetric matrix will only element on diagonal and above unless
-- --   noted otherwise.
-- toDense
--   :: forall a m s. (Storable a, PrimMonad m, s ~ PrimState m)
--   => MSymmetric s a -> m (MMat.MMatrix s a)
-- toDense (MSymmetric view@MSymView{..}) = unsafeIOToPrim $ do
--   internalSymmetrize view
--   pure $ MMat.unsafeCast $ MMat.MMatrix MMat.MView
--     { nrows      = size
--     , ncols      = size
--     , leadingDim = leadingDim
--     , buffer     = buffer
--     }


 
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
  :: forall a m mat s. (Storable a, PrimMonad m, s ~ PrimState m, AsSymInput s mat)
  => mat a -> m (MSymmetric s a)
{-# INLINE clone #-}
clone (asSymInput @s -> MSymView{..}) = unsafePrimToPrim $ do
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
                             , buffer     = vecBuffer buffer
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
                             , buffer     = vecBuffer buffer
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
