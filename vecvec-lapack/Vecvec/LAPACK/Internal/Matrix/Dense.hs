{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE TypeFamilies        #-}
-- |
module Vecvec.LAPACK.Internal.Matrix.Dense
  ( -- * Immutable matrix
    Matrix(..)
  , nRows, nCols
  , unsafeRead
  , unsafeRow
  , unsafeCol
    -- * Creation of matrices
  , fromRowsFF
    -- * Conversion to\/from mutable
  , unsafeFreeze
  , freeze
  , thaw
  ) where


import Control.Monad.Primitive
import Control.Monad.ST
import Data.List               (intercalate)
import Foreign.ForeignPtr
import Foreign.Storable
import Foreign.Marshal.Array

import Vecvec.LAPACK.Internal.Matrix.Dense.Mutable qualified as M
import Vecvec.LAPACK.Internal.Compat
import Vecvec.LAPACK.Internal.Vector

-- | Immutable matrix
data Matrix a = Matrix
  { nrows      :: !Int
  , ncols      :: !Int
  , leadingDim :: !Int
  , buffer     :: !(ForeignPtr a)
  }

nRows :: Matrix a -> Int
nRows = nrows

nCols :: Matrix a -> Int
nCols = ncols

instance (Show a, Storable a) => Show (Matrix a) where
  show m = "[ " ++ intercalate "\n, " [ show (unsafeRow m i) | i <- [0 .. nRows m - 1]] ++ "]"


unsafeFreeze :: (Storable a, PrimMonad m, s ~ PrimState m)
             => M.MMatrix s a -> m (Matrix a)
unsafeFreeze M.MMatrix{..} = pure $ do
  Matrix { nrows      = nrowsM
         , ncols      = ncolsM
         , leadingDim = leadingDimM
         , buffer     = bufferM
         }

freeze :: (Storable a, PrimMonad m, s ~ PrimState m)
       => M.MMatrix s a -> m (Matrix a)
freeze M.MMatrix{..} = do
  -- FIXME: We need to allocate new buffer. Note that ncols /=
  --        leadingDim in general case. We ma need to copy row by row
  pure Matrix { nrows      = nrowsM
              , ncols      = ncolsM
              , leadingDim = ncolsM
              , buffer     = undefined
              }

thaw :: (Storable a, PrimMonad m, s ~ PrimState m)
     => Matrix a -> m (M.MMatrix s a)
thaw Matrix{..} = do
  -- FIXME: We need to allocate new buffer. Note that ncols /=
  --        leadingDim in general case. We ma need to copy row by row
  pure M.MMatrix { nrowsM      = nrows
                 , ncolsM      = ncols
                 , leadingDimM = ncols
                 , bufferM     = undefined
                 }

unsafeRead :: (Storable a) => Matrix a -> (Int, Int) -> a
{-# INLINE unsafeRead #-}
unsafeRead Matrix{..} (i,j)
  = unsafeInlineIO
  $ unsafeWithForeignPtr buffer $ \p -> do
    peekElemOff p (i * leadingDim + j)



unsafeRow :: (Storable a) => Matrix a -> Int -> Vec a
unsafeRow Matrix{..} i =
  Vec ncols 1 (updPtr (`advancePtr` (leadingDim * i)) buffer)

unsafeCol :: (Storable a) => Matrix a -> Int -> Vec a
unsafeCol Matrix{..} i =
  Vec nrows leadingDim (updPtr (`advancePtr` i) buffer)

fromRowsFF :: (Storable a, Foldable f, Foldable g)
           => f (g a) -> Matrix a
fromRowsFF dat = runST $ unsafeFreeze =<< M.fromRowsFF dat
