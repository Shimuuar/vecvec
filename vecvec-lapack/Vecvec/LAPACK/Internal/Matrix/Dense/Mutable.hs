{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE MultiWayIf      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies    #-}
-- |
module Vecvec.LAPACK.Internal.Matrix.Dense.Mutable
  ( MMatrix(..)
  , unsafeRead
  , unsafeWrite
    -- * Matrix creation
  , clone
  , fromRowsFF
  ) where

import Control.Monad
import Control.Monad.Primitive
import Data.Foldable
import Foreign.ForeignPtr
import Foreign.Storable
import Foreign.Marshal.Array

import Vecvec.LAPACK.Internal.Compat


-- | Mutable matrix.
data MMatrix s a = MMatrix
  { nrowsM      :: !Int            -- ^ Number of rows
  , ncolsM      :: !Int            -- ^ Number of columns
  , leadingDimM :: !Int            -- ^ Leading dimension size. Matrix
                                   --   is stored in row-major order
                                   --   so it's row size.
  , bufferM     :: !(ForeignPtr a) -- ^ Underlying buffer
  }


-- | Create copy of mutable matrix
clone :: (Storable a, PrimMonad m, s ~ PrimState m)
      => MMatrix s a -> m (MMatrix s a)
clone MMatrix{..} = unsafePrimToPrim $ do
  buf <- mallocForeignPtrArray n_elt
  unsafeWithForeignPtr bufferM $ \src ->
    unsafeWithForeignPtr buf $ \dst ->
    if-- Source buffer is dense. We can copy in one go
      | ncolsM == leadingDimM -> copyArray dst src n_elt
      -- We have to copy row by row
      | otherwise -> let loop !d !s i
                           | i >= nrowsM = return ()
                           | otherwise   = do
                               copyArray d s ncolsM
                               loop (advancePtr d nrowsM) (advancePtr s leadingDimM) (i+1)
                     in loop dst src 0
  pure MMatrix { bufferM = buf
               , ..
               }
  where
    n_elt = ncolsM * nrowsM


unsafeRead :: (Storable a, PrimMonad m, s ~ PrimState m)
           => MMatrix s a -> (Int, Int) -> m a
{-# INLINE unsafeRead #-}
unsafeRead MMatrix{..} (i,j)
  = unsafePrimToPrim
  $ unsafeWithForeignPtr bufferM $ \p -> do
    peekElemOff p (i * leadingDimM + j)

unsafeWrite :: (Storable a, PrimMonad m, s ~ PrimState m)
           => MMatrix s a -> (Int, Int) -> a -> m ()
{-# INLINE unsafeWrite #-}
unsafeWrite MMatrix{..} (i,j) a
  = unsafePrimToPrim
  $ unsafeWithForeignPtr bufferM $ \p -> do
    pokeElemOff p (i * leadingDimM + j) a

fromRowsFF :: (Storable a, Foldable f, Foldable g, PrimMonad m, s ~ PrimState m)
           => f (g a) -> m (MMatrix s a)
fromRowsFF dat
  | ncolsM == 0 = error "Number of columns is zero"
  | nrowsM == 0 = error "Number of rows is zero"
  | otherwise = unsafePrimToPrim $ do
      bufferM <- mallocForeignPtrArray (ncolsM * nrowsM)
      let step p row
            | length row /= ncolsM = error "Row has different length"
            | otherwise            = do
                pokeArray p (toList row)
                pure $! advancePtr p ncolsM
      _ <- unsafeWithForeignPtr bufferM $ \p -> foldM step p dat
      pure MMatrix { leadingDimM = ncolsM
                   , ..
                   }
  where
    nrowsM = length dat
    ncolsM = length $ head $ toList dat
