{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
-- |
-- Data types used for both symmetric and hermitian matrices
module Vecvec.LAPACK.Internal.Symmetric.Types where

import Foreign.ForeignPtr
import Foreign.Storable
import Foreign.Marshal.Array

import Vecvec.Classes
import Vecvec.Classes.NDMutable
import Vecvec.LAPACK.Internal.Compat

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
