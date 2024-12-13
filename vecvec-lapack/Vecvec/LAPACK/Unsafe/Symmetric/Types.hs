{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies    #-}
-- |
-- Data types used for both symmetric and hermitian matrices
module Vecvec.LAPACK.Unsafe.Symmetric.Types
  ( MSymView(..)
  , MHermitian(..)
  , Hermitian(..)
  , MSymmetric(..)
  , Symmetric(..)
  , asSymmetric
  , asHermitian
  ) where

import Data.Vector.Fixed.Cont qualified as FC
import Foreign.ForeignPtr
import Foreign.Storable
import Foreign.Marshal.Array

import Vecvec.Classes
import Vecvec.Classes.NDMutable
import Vecvec.LAPACK.Unsafe.Compat


----------------------------------------------------------------
-- Representation
----------------------------------------------------------------

-- | Internal representation of symmetric or hermitian matrix. It's
--   distinct from 'MMat.MView' because symmetric matrix is always
--   square and we want to preserve original pointer for conversion of
--   immutable symmetric matrices to general.
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


----------------------------------------------------------------
-- Hermitian matrices
----------------------------------------------------------------

-- | Hermitian matrix. It uses same memory layout as general dense
--   matrix but only diagonal and elements above it are referenced.
newtype MHermitian s a = MHermitian (MSymView a)

deriving newtype instance (Slice1D i, Storable a) => Slice i (MHermitian s a)

type instance Rank (MHermitian s) = FC.N2

instance HasShape (MHermitian s) a where
  shapeAsCVec (MHermitian MSymView{..}) = FC.mk2 size size
  {-# INLINE shapeAsCVec #-}


-- | Hermitian matrix
data Hermitian a = Hermitian () (MSymView a)

instance (Slice1D i, Storable a) => Slice i (Hermitian a) where
  {-# INLINE sliceMaybe #-}
  sliceMaybe i (Hermitian flag view) = do
    view' <- sliceMaybe i view
    pure $ Hermitian flag view'

type instance Rank Hermitian = FC.N2

instance HasShape Hermitian a where
  shapeAsCVec (Hermitian _ MSymView{..}) = FC.mk2 size size
  {-# INLINE shapeAsCVec #-}

-- | /O(1)/ cast hermitian matrix to symmetric if its elements are
--   real.
asSymmetric :: (R a ~ a) => Hermitian a -> Symmetric a
asSymmetric (Hermitian tag repr) = Symmetric tag repr

-- | /O(1)/ cast hermitian matrix to symmetric if its elements are
--   real.
asHermitian :: (R a ~ a) => Symmetric a -> Hermitian a
asHermitian (Symmetric tag repr) = Hermitian tag repr



----------------------------------------------------------------
-- Symmetric matrices
----------------------------------------------------------------x

-- | Symmetric matrix. It uses same memory layout as general dense
--   matrix but only diagonal and elements above it are referenced.
newtype MSymmetric s a = MSymmetric (MSymView a)

deriving newtype instance (Slice1D i, Storable a) => Slice i (MSymmetric s a)

type instance Rank (MSymmetric s) = FC.N2

instance HasShape (MSymmetric s) a where
  shapeAsCVec (MSymmetric MSymView{..}) = FC.mk2 size size
  {-# INLINE shapeAsCVec #-}


-- | Symmetric matrix
data Symmetric a = Symmetric () (MSymView a)

instance (Slice1D i, Storable a) => Slice i (Symmetric a) where
  {-# INLINE sliceMaybe #-}
  sliceMaybe i (Symmetric flag view) = do
    view' <- sliceMaybe i view
    pure $ Symmetric flag view'

type instance Rank Symmetric = FC.N2

instance HasShape Symmetric a where
  shapeAsCVec (Symmetric _ MSymView{..}) = FC.mk2 size size
  {-# INLINE shapeAsCVec #-}
