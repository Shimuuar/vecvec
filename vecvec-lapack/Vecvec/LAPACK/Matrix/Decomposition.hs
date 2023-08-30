{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RecordWildCards     #-}
-- |
module Vecvec.LAPACK.Matrix.Decomposition where

import Foreign.Storable
import Data.Char
import Vecvec.LAPACK.Internal.Compat
import Vecvec.LAPACK.Internal.Matrix.Dense
import Vecvec.LAPACK.Internal.Matrix.Dense.Mutable qualified as MM
import Vecvec.LAPACK.Internal.Matrix.Dense.Mutable (MMatrix(..), MView(..))
import Vecvec.LAPACK.Internal.Vector
import Vecvec.LAPACK.Internal.Vector.Mutable
import Data.Vector.Generic.Mutable qualified as MVG
import Vecvec.LAPACK.FFI
import Vecvec.Classes

import System.IO.Unsafe

-- | Perform SVD decomposition of arbitrary matrix \(A\):
--
--  \[A = U\Sigma V\]
--
--   where matrices \(U\) and \(V\) are orthogonal\/unitary.
decomposeSVD
  :: (LAPACKy a, Storable (R a))
  => Matrix a
  -> (Matrix a, Vec (R a), Matrix a)
decomposeSVD a = unsafePerformIO $ do
  -- We need to clone matrix A since is get destroyed when computing
  -- SVD. We need to allocate buffers for products
  MMatrix mat_A  <- MM.clone a
  let n_col = ncols mat_A
      n_row = nrows mat_A
  MMatrix mat_U   <- MM.unsafeNew   (n_row, n_row)
  MVec    vec_Sig <- MVG.unsafeNew (min n_row n_col)
  MMatrix mat_VT  <- MM.unsafeNew   (n_col, n_col)
  -- Run SVD
  info <-
    unsafeWithForeignPtr (buffer mat_A)      $ \ptr_A ->
    unsafeWithForeignPtr (buffer mat_U)      $ \ptr_U ->
    unsafeWithForeignPtr (vecBuffer vec_Sig) $ \ptr_Sig ->
    unsafeWithForeignPtr (buffer mat_VT)     $ \ptr_VT ->
      gesdd (toCEnum RowMajor) (fromIntegral $ ord 'A')
            (fromIntegral n_row) (fromIntegral n_col)
            ptr_A (fromIntegral (leadingDim mat_A))
            ptr_Sig
            ptr_U  (fromIntegral (leadingDim mat_U))
            ptr_VT (fromIntegral (leadingDim mat_VT))
  case info of
    0 -> pure ( Matrix mat_U
              , Vec    vec_Sig
              , Matrix mat_VT
              )
    _ -> error "SVD failed"



    
