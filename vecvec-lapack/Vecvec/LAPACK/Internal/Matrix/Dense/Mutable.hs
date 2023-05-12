{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE BangPatterns               #-}
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
  , Vecvec.LAPACK.Internal.Matrix.Dense.Mutable.clone -- FIXME: Name
  , fromRowsFF
    -- ** Access
  , unsafeRead
  , unsafeWrite
  , unsafeCol
  , unsafeRow
    -- * BLAS wrappers
  , MatrixTranspose(..)
  , unsafeBlasGemv
  ) where

import Control.Monad
import Control.Monad.Primitive
import Data.Coerce
import Data.Foldable
import Foreign.ForeignPtr
import Foreign.Storable
import Foreign.Marshal.Array

import Vecvec.Classes.Slice
import Vecvec.LAPACK.Internal.Compat
import Vecvec.LAPACK.Internal.Vector.Mutable
import Vecvec.LAPACK.FFI                     qualified as C
import Vecvec.LAPACK.FFI                     (MatrixTranspose(..))

-- | Matrix
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

-- | Mutable matrix.
newtype MMatrix s a = MMatrix (MView a)

class AsMInput s m where
  asMInput :: m a -> MView a

instance s ~ s' => AsMInput s' (MMatrix s) where
  {-# INLINE asMInput #-}
  asMInput = coerce

deriving newtype instance (Slice1D i, Slice1D j, Storable a) => Slice (i,j) (MMatrix s a)


pattern AsMVec :: MVec s a -> MMatrix s a
pattern AsMVec v <- (tryMVec -> Just v)

tryMVec :: MMatrix s a -> Maybe (MVec s a)
{-# INLINE tryMVec #-}
tryMVec (MMatrix MView{..})
  | ncols /= leadingDim = Nothing
  | otherwise           = Just (MVec (VecRepr (ncols * nrows) 1 buffer))



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

unsafeRow :: (Storable a) => MMatrix s a -> Int -> MVec s a
unsafeRow (MMatrix MView{..}) i =
   MVec (VecRepr ncols 1 (updPtr (`advancePtr` (leadingDim * i)) buffer))

unsafeCol :: (Storable a) => MMatrix s a -> Int -> MVec s a
unsafeCol (MMatrix MView{..}) i =
  MVec (VecRepr nrows leadingDim (updPtr (`advancePtr` i) buffer))

unsafeRead :: forall a m mat s. (Storable a, PrimMonad m, s ~ PrimState m, AsMInput s mat)
           => mat a -> (Int, Int) -> m a
{-# INLINE unsafeRead #-}
unsafeRead (asMInput @s -> MView{..}) (i,j)
  = unsafePrimToPrim
  $ unsafeWithForeignPtr buffer $ \p -> do
    peekElemOff p (i * leadingDim + j)

unsafeWrite :: (Storable a, PrimMonad m, s ~ PrimState m)
            => MMatrix s a -> (Int, Int) -> a -> m ()
{-# INLINE unsafeWrite #-}
unsafeWrite (MMatrix MView{..}) (i,j) a
  = unsafePrimToPrim
  $ unsafeWithForeignPtr buffer $ \p -> do
    pokeElemOff p (i * leadingDim + j) a

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
  $ do VecRepr lenX incX fpX <- unsafePrimToPrim $ asInput @s vecX
       id $ unsafeWithForeignPtr buffer $ \p_A ->
            unsafeWithForeignPtr fpX    $ \p_x ->
            unsafeWithForeignPtr fpY    $ \p_y ->
              C.gemv (C.toCEnum C.RowMajor) (C.toCEnum tr)
                (fromIntegral nrows) (fromIntegral ncols) α p_A (fromIntegral leadingDim)
                p_x (fromIntegral incX)
                β p_y (fromIntegral incY)
