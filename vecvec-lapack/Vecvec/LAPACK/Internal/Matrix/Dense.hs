{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE ImportQualifiedPost   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeFamilies          #-}
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

import Control.Monad
import Control.Monad.Primitive
import Control.Monad.ST
import Data.Coerce
import Data.List                    (intercalate)
import Data.Vector.Generic.Mutable  qualified as MVG
import Data.Vector.Generic          qualified as VG
import Foreign.ForeignPtr
import Foreign.Storable
import Foreign.Marshal.Array
import System.IO.Unsafe

import Vecvec.Classes
import Vecvec.LAPACK.Internal.Matrix.Dense.Mutable qualified as M
import Vecvec.LAPACK.Internal.Compat
import Vecvec.LAPACK.Internal.Vector
import Vecvec.LAPACK.Vector.Mutable
import Vecvec.LAPACK.FFI                           qualified as C

-- | Immutable matrix
newtype Matrix a = Matrix (M.MView M.Immut a)


nRows :: Matrix a -> Int
nRows (Matrix m) = M.nrows m

nCols :: Matrix a -> Int
nCols (Matrix m) = M.ncols m

instance M.AsMInput s Matrix where
  {-# INLINE asMInput #-}
  asMInput = coerce

instance (Show a, Storable a) => Show (Matrix a) where
  show m = "[ " ++ intercalate "\n, " [ show (unsafeRow m i) | i <- [0 .. nRows m - 1]] ++ "]"

instance C.LAPACKy a => AdditiveSemigroup (Matrix a) where
  -- FIXME: implement method

instance C.LAPACKy a => AdditiveQuasigroup (Matrix a) where
  -- FIXME: implement method

instance C.LAPACKy a => VectorSpace (Matrix a) where
  type Scalar (Matrix a) = a
  (.*) = flip (*.)
  -- FIXME: implement method

instance (C.LAPACKy a, a ~ a') => MatMul (Matrix a) (Vec a') (Vec a) where
  m @@ v
    | nCols m /= VG.length v = error "matrix size mismatch"
  mat @@ vecX = unsafePerformIO $ do
    vecY <- MVG.new (nRows mat)
    M.unsafeBlasGemv 1 mat vecX 0 vecY
    VG.unsafeFreeze vecY

unsafeFreeze :: (Storable a, PrimMonad m, s ~ PrimState m)
             => M.MMatrix s a -> m (Matrix a)
unsafeFreeze = pure . coerce


freeze :: (Storable a, PrimMonad m, s ~ PrimState m)
       => M.MMatrix s a -> m (Matrix a)
freeze = unsafeFreeze <=< M.clone

thaw :: (Storable a, PrimMonad m, s ~ PrimState m)
     => Matrix a -> m (M.MMatrix s a)
thaw = M.clone


unsafeRead :: (Storable a) => Matrix a -> (Int, Int) -> a
{-# INLINE unsafeRead #-}
unsafeRead (Matrix M.MView{..}) (i,j)
  = unsafeInlineIO
  $ unsafeWithForeignPtr buffer $ \p -> do
    peekElemOff p (i * leadingDim + j)



unsafeRow :: (Storable a) => Matrix a -> Int -> Vec a
unsafeRow (Matrix M.MView{..}) i =
  Vec ncols 1 (updPtr (`advancePtr` (leadingDim * i)) buffer)

unsafeCol :: (Storable a) => Matrix a -> Int -> Vec a
unsafeCol (Matrix M.MView{..}) i =
  Vec nrows leadingDim (updPtr (`advancePtr` i) buffer)

fromRowsFF :: (Storable a, Foldable f, Foldable g)
           => f (g a) -> Matrix a
fromRowsFF dat = runST $ unsafeFreeze =<< M.fromRowsFF dat
