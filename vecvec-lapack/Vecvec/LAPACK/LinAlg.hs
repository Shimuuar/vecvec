{-# LANGUAGE RecordWildCards #-}
-- |
-- Linear algebra routines.
module Vecvec.LAPACK.LinAlg
  ( -- * Linear systems
    -- $linear_eq
    -- ** Type classes
    LinearEq(..)
  , LinearEqRHS(..)
    -- ** Solvers
  , solveLinEq
  , solveLinEqSym
    -- * Matrix inversion
  , invertMatrix
    -- * Matrix decomposition
  , decomposeSVD
  ) where

import Control.Monad
import Control.Monad.ST
import Control.Monad.Primitive
import Foreign.Storable
import Foreign.Marshal.Array
import Data.Char
import Data.Vector               qualified as V
import Data.Vector.Unboxed       qualified as VU
import Data.Vector.Storable      qualified as VS
import Data.Vector.Primitive     qualified as VP
import Data.Vector.Generic       qualified as VG
import Vecvec.LAPACK.Internal.Compat
import Vecvec.LAPACK.Internal.Matrix
import Vecvec.LAPACK.Internal.Symmetric         (Symmetric)
import Vecvec.LAPACK.Internal.Matrix.Mutable    qualified as MMat
import Vecvec.LAPACK.Internal.Matrix.Mutable    (MMatrix(..), MView(..))
import Vecvec.LAPACK.Internal.Symmetric.Mutable qualified as MSym
import Vecvec.LAPACK.Internal.Symmetric.Mutable (MSymmetric(..), MSymView(..))
import Vecvec.LAPACK.Internal.Vector
import Vecvec.LAPACK.Internal.Vector.Mutable
import Data.Vector.Generic.Mutable qualified as MVG
import Vecvec.LAPACK.FFI
import Vecvec.Classes
import Vecvec.Classes.NDArray

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
  MMatrix mat_A  <- MMat.clone a
  let n_col = ncols mat_A
      n_row = nrows mat_A
  MMatrix mat_U   <- MMat.unsafeNew   (n_row, n_row)
  MVec    vec_Sig <- MVG.unsafeNew (min n_row n_col)
  MMatrix mat_VT  <- MMat.unsafeNew   (n_col, n_col)
  -- Run SVD
  info <-
    unsafeWithForeignPtr (mat_A.buffer)      $ \ptr_A ->
    unsafeWithForeignPtr (mat_U.buffer)      $ \ptr_U ->
    unsafeWithForeignPtr (vecBuffer vec_Sig) $ \ptr_Sig ->
    unsafeWithForeignPtr (mat_VT.buffer)     $ \ptr_VT ->
      gesdd (toCEnum RowMajor) (fromIntegral $ ord 'A')
            (toL n_row) (toL n_col)
            ptr_A (toL mat_A.leadingDim)
            ptr_Sig
            ptr_U  (toL mat_U.leadingDim)
            ptr_VT (toL mat_VT.leadingDim)
  case info of
    LAPACK0 -> pure ( Matrix mat_U
                    , Vec    vec_Sig
                    , Matrix mat_VT
                    )
    _ -> error "SVD failed"


----------------------------------------------------------------
-- Linear equation
----------------------------------------------------------------

-- | Compute inverse of square matrix @A@
invertMatrix :: LAPACKy a => Matrix a -> Matrix a
invertMatrix m
  | nCols m /= nRows m = error "Matrix must be square"
  | otherwise          = unsafePerformIO $ do
      MMatrix inv@MView{..} <- MMat.clone m
      id $
        unsafeWithForeignPtr buffer $ \ptr_a    ->
        allocaArray ncols           $ \ptr_ipiv -> do
          info_trf <- getrf
            (toCEnum RowMajor) (toL ncols) (toL ncols)
            ptr_a (toL leadingDim) ptr_ipiv
          case info_trf of LAPACK0 -> pure ()
                           _       -> error "invertMatrix failed (GETRF)"
          info_tri <- getri
            (toCEnum RowMajor) (toL ncols)
            ptr_a (toL leadingDim) ptr_ipiv
          case info_tri of LAPACK0 -> pure ()
                           _       -> error "invertMatrix failed (GETRF)"
      --
      pure $ Matrix inv


-- $linear_eq
--
-- Placeholder for documentation for systems of linear equations

-- | Standard solver for linear equation. This type class picks
--   default algorithm for solving linear equations
class LinearEq m a where
  -- | Solve linear equation \(Ax=b\)
  (\\\) :: (LinearEqRHS rhs a) => m a -> rhs a -> rhs a

-- | See 'solveLinEq'
instance LAPACKy a => LinearEq Matrix a where
  (\\\) = solveLinEq


-- | When solving linear equations like \(Ax=b\) most of the work is
--   spent on factoring matrix. Thus it's computationally advantageous
--   to batch right hand sides of an equation. This type class exists
--   in order to built such batches in form of matrices from haskell
--   data structures
--
--   Type class is traversal like and should obey following law:
--
--   > rhsGetSolutions rhs (runST (rhsToMatrix rhs >>= unsafeFreeze))
--   >   == rhs
class LinearEqRHS rhs a where
  -- | Convert right hand of equation to matrix where each \(b\) is
  --   arranged as column. We need to create mutable matrix in order
  --   to ensure that fresh buffer is allocated since LAPACK routines
  --   frequently reuse storage.
  rhsToMatrix     :: Storable a => rhs a -> ST s (MMatrix s a)
  -- | Extract solutions from matrix. First argument is used to retain
  --   information which isn't right hand sides.
  rhsGetSolutions :: Storable a => rhs a -> Matrix a -> rhs a


instance LinearEqRHS Matrix a where
  rhsToMatrix     = MMat.clone
  rhsGetSolutions = const id

instance LinearEqRHS [] a where
  rhsToMatrix v = MMat.fromColsFF [v]
  rhsGetSolutions _ m = VG.toList $ getCol m 0

instance LinearEqRHS Vec a where
  rhsToMatrix v = MMat.fromColsFV [v]
  rhsGetSolutions _ m = getCol m 0

instance LinearEqRHS V.Vector a where
  rhsToMatrix v = MMat.fromColsFV [v]
  rhsGetSolutions _ m = VG.convert $ getCol m 0

instance VS.Storable a => LinearEqRHS VS.Vector a where
  rhsToMatrix v = MMat.fromColsFV [v]
  rhsGetSolutions _ m = VG.convert $ getCol m 0

instance VP.Prim a => LinearEqRHS VP.Vector a where
  rhsToMatrix v = MMat.fromColsFV [v]
  rhsGetSolutions _ m = VG.convert $ getCol m 0

instance VU.Unbox a => LinearEqRHS VU.Vector a where
  rhsToMatrix v = MMat.fromColsFV [v]
  rhsGetSolutions _ m = VG.convert $ getCol m 0


-- | Simple solver for linear equation of the form \(Ax=b\).
--
--   Note that this function does not check whether matrix is
--   ill-conditioned and may return nonsensical answers in this case.
--
--   /Uses _GESV LAPACK routine internally/
solveLinEq
  :: (LinearEqRHS rhs a, LAPACKy a)
  => Matrix a -- ^ Matrix \(A\)
  -> rhs a    -- ^ Right hand side(s) \(b\)
  -> rhs a
solveLinEq a _
  | nRows a /= nCols a = error "Matrix A is not square"
solveLinEq a0 rhs = unsafePerformIO $ do
  -- Prepare right hand side and check sizes. We also need to clone
  -- A. It gets destroyed during solution
  MMatrix a <- MMat.clone a0
  let n = ncols a
  MMatrix b <- stToPrim $ rhsToMatrix rhs
  when (nrows b /= n) $ error "Right hand dimensions don't match"
  -- Solve equation
  info <-
    unsafeWithForeignPtr a.buffer $ \ptr_a    ->
    unsafeWithForeignPtr b.buffer $ \ptr_b    ->
    allocaArray n                 $ \ptr_ipiv ->
      gesv (toCEnum RowMajor)
        (toL n) (toL (ncols b))
        ptr_a (toL a.leadingDim)
        ptr_ipiv
        ptr_b (toL b.leadingDim)
  case info of
    LAPACK0 -> pure $ rhsGetSolutions rhs (Matrix b)
    _       -> error "solveLinEq failed"


-- | Simple solver for linear equation of the form \(Ax=b\) where
--   \(A\) is symmetric matrix.
--
--   Note that this function does not check whether matrix is
--   ill-conditioned and may return nonsensical answers in this case.
--
--   /Uses _SYSV LAPACK routine internally/
solveLinEqSym
  :: (LinearEqRHS rhs a, LAPACKy a)
  => Symmetric a -- ^ Matrix \(A\)
  -> rhs a       -- ^ Right hand side(s) \(b\)
  -> rhs a
solveLinEqSym a0 rhs = unsafePerformIO $ do
  -- Clone A it gets destroyed during solution
  MSymmetric a <- MSym.clone a0
  -- Prepare right hand side and check sizes. We also need to clone
  let n = a.size
  MMatrix b <- stToPrim $ rhsToMatrix rhs
  when (nrows b /= n) $ error "Right hand dimensions don't match"
  -- Solve equation
  info <-
    unsafeWithForeignPtr (a.buffer) $ \ptr_a    ->
    unsafeWithForeignPtr (b.buffer) $ \ptr_b    ->
    allocaArray n                   $ \ptr_ipiv ->
      sysv (toCEnum RowMajor) (toCEnum FortranUP)
        (toL n) (toL (ncols b))
        ptr_a (toL a.leadingDim)
        ptr_ipiv
        ptr_b (toL b.leadingDim)
  case info of
    LAPACK0 -> pure $ rhsGetSolutions rhs (Matrix b)
    _       -> error "solveLinEqSym failed"
