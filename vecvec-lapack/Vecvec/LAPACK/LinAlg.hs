-- |
-- Linear algebra routines.
module Vecvec.LAPACK.LinAlg
  ( -- * Linear systems
    -- $linear_eq
    -- ** Type classes
    LinearEq(..)
  , EquationRHS
  , rhsToMatrix
  , rhsGetSolutions
    -- ** Solvers
  , solveLinEq
  , solveLinEqSym
  , solveLinEqHer
    -- * Matrix inversion
  , invertMatrix
    -- * Matrix decomposition
  , decomposeSVD
  ) where

import Control.Monad.ST
import Control.Monad.Primitive
import Foreign.Storable
import Foreign.Marshal.Array
import Foreign.ForeignPtr
import Foreign.Ptr
import Data.Complex
import Vecvec.LAPACK.Unsafe.Compat
import Vecvec.LAPACK.Unsafe.Matrix
import Vecvec.LAPACK.Unsafe.Symmetric         (Symmetric)
import Vecvec.LAPACK.Unsafe.Hermitian         (Hermitian)
import Vecvec.LAPACK.Unsafe.Matrix.Mutable    qualified as MMat
import Vecvec.LAPACK.Unsafe.Matrix.Mutable    (MMatrix(..), MView(..))
import Vecvec.LAPACK.Unsafe.Symmetric.Mutable qualified as MSym
import Vecvec.LAPACK.Unsafe.Symmetric.Mutable (MSymmetric(..), MSymView(..))
import Vecvec.LAPACK.Unsafe.Hermitian.Mutable qualified as MHer
import Vecvec.LAPACK.Unsafe.Hermitian.Mutable (MHermitian(..))
import Vecvec.LAPACK.Unsafe.Vector
import Vecvec.LAPACK.Unsafe.Vector.Mutable
import Data.Vector.Generic.Mutable qualified as MVG
import Vecvec.LAPACK.FFI
import Vecvec.LAPACK.LinAlg.Types
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
  let n_col = mat_A.ncols
      n_row = mat_A.nrows
  MMatrix mat_U   <- MMat.unsafeNew (n_row, n_row)
  MVec    vec_Sig <- MVG.unsafeNew  (min n_row n_col)
  MMatrix mat_VT  <- MMat.unsafeNew (n_col, n_col)
  -- Run SVD
  info <-
    unsafeWithForeignPtr mat_A.buffer      $ \ptr_A ->
    unsafeWithForeignPtr mat_U.buffer      $ \ptr_U ->
    unsafeWithForeignPtr vec_Sig.vecBuffer $ \ptr_Sig ->
    unsafeWithForeignPtr mat_VT.buffer     $ \ptr_VT ->
      gesdd RowMajor SvdA
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
      MMatrix inv <- MMat.clone m
      id $
        unsafeWithForeignPtr inv.buffer $ \ptr_a    ->
        allocaArray inv.ncols           $ \ptr_ipiv -> do
          let n   = toL inv.ncols
              lda = toL inv.leadingDim
          info_trf <- getrf RowMajor n n ptr_a lda ptr_ipiv
          case info_trf of LAPACK0 -> pure ()
                           _       -> error "invertMatrix failed (GETRF)"
          info_tri <- getri RowMajor n ptr_a lda ptr_ipiv
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
  (\\\) :: (EquationRHS rhs a) => m a -> rhs -> rhs

-- | See 'solveLinEq'
instance LAPACKy a => LinearEq Matrix a where
  (\\\) = solveLinEq

-- | See 'solveLinEqSym'
instance LAPACKy a => LinearEq Symmetric a where
  (\\\) = solveLinEqSym

-- | See 'solveLinEqHer'
instance LAPACKy a => LinearEq Hermitian a where
  (\\\) = solveLinEqHer



-- | Simple solver for linear equation of the form \(Ax=b\).
--
--   Note that this function does not check whether matrix is
--   ill-conditioned and may return nonsensical answers in this case.
--
--   /Uses _GESV LAPACK routine internally/
solveLinEq
  :: forall a rhs. (EquationRHS rhs a, LAPACKy a)
  => Matrix a -- ^ Matrix \(A\)
  -> rhs      -- ^ Right hand side(s) \(b\)
  -> rhs
solveLinEq a _
  | nRows a /= nCols a = error "Matrix A is not square"
solveLinEq a0 rhs = runST $ do
  rhsToMatrix rhs >>= \case
    EmptyRHS     -> pure $ rhsGetSolutions rhs (nullMatrix @a)
    InvalidRHS{} -> error "Vectors on right size have different dimensions"
    PreparedRHS (MMatrix b)
      | n /= nRows b -> error "Right hand dimensions don't match"
      | otherwise    -> do
          -- Prepare right hand side and check sizes. We also need to clone
          -- A. It gets destroyed during solution
          MMatrix a <- MMat.clone a0
          info <-
            unsafeIOToPrim                $
            unsafeWithForeignPtr a.buffer $ \ptr_a    ->
            unsafeWithForeignPtr b.buffer $ \ptr_b    ->
            allocaArray n                 $ \ptr_ipiv ->
              gesv RowMajor
                (toL n) (toL b.ncols)
                ptr_a (toL a.leadingDim)
                ptr_ipiv
                ptr_b (toL b.leadingDim)
          case info of
            LAPACK0 -> pure $ rhsGetSolutions rhs (Matrix b)
            _       -> error "solveLinEq failed"
  where
    n = nCols a0

-- | Simple solver for linear equation of the form \(Ax=b\) where
--   \(A\) is symmetric matrix.
--
--   Note that this function does not check whether matrix is
--   ill-conditioned and may return nonsensical answers in this case.
--
--   /Uses _SYSV LAPACK routine internally/
solveLinEqSym
  :: forall a rhs. (EquationRHS rhs a, LAPACKy a)
  => Symmetric a -- ^ Matrix \(A\)
  -> rhs         -- ^ Right hand side(s) \(b\)
  -> rhs
solveLinEqSym a0 rhs = runST $ do
  rhsToMatrix rhs >>= \case
    EmptyRHS     -> pure $ rhsGetSolutions rhs (nullMatrix @a)
    InvalidRHS{} -> error "Vectors on right size have different dimensions"
    PreparedRHS (MMatrix b)
      | n /= nRows b -> error "Right hand dimensions don't match"
      | otherwise    -> do
          -- Clone A it gets destroyed during solution
          MSymmetric a <- MSym.clone a0
          info <-
            unsafeIOToPrim                $
            unsafeWithForeignPtr a.buffer $ \ptr_a    ->
            unsafeWithForeignPtr b.buffer $ \ptr_b    ->
            allocaArray n                 $ \ptr_ipiv ->
              sysv RowMajor FortranUP
                (toL n) (toL b.ncols)
                ptr_a (toL a.leadingDim)
                ptr_ipiv
                ptr_b (toL b.leadingDim)
          case info of
            LAPACK0 -> pure $ rhsGetSolutions rhs (Matrix b)
            _       -> error "solveLinEqSym failed"
  where
    n = nCols a0

-- | Simple solver for linear equation of the form \(Ax=b\) where
--   \(A\) is hermitian matrix
--
--   Note that this function does not check whether matrix is
--   ill-conditioned and may return nonsensical answers in this case.
--
--   /Uses _HESV LAPACK routine internally/
solveLinEqHer
  :: forall a rhs. (EquationRHS rhs a, LAPACKy a)
  => Hermitian a -- ^ Matrix \(A\)
  -> rhs         -- ^ Right hand side(s) \(b\)
  -> rhs
solveLinEqHer a0 rhs = runST $ do
  rhsToMatrix rhs >>= \case
    EmptyRHS     -> pure $ rhsGetSolutions rhs (nullMatrix @a)
    InvalidRHS{} -> error "Vectors on right size have different dimensions"
    PreparedRHS (MMatrix b)
      | n /= nRows b -> error "Right hand dimensions don't match"
      | otherwise    -> do
          -- Clone A it gets destroyed during solution
          MHermitian a <- MHer.clone a0
          info <-
            unsafeIOToPrim                $
            unsafeWithForeignPtr a.buffer $ \ptr_a    ->
            unsafeWithForeignPtr b.buffer $ \ptr_b    ->
            allocaArray n                 $ \ptr_ipiv ->
              hesv RowMajor FortranUP
                (toL n) (toL b.ncols)
                ptr_a (toL a.leadingDim)
                ptr_ipiv
                ptr_b (toL b.leadingDim)
          case info of
            LAPACK0 -> pure $ rhsGetSolutions rhs (Matrix b)
            _       -> error "solveLinEqSym failed"
  where
    n = nCols a0


-- Empty matrix used as a placeholder value.
nullMatrix :: Matrix a
{-# NOINLINE nullMatrix #-}
nullMatrix = unsafePerformIO $ do
  buf <- newForeignPtr_ nullPtr
  pure $ Matrix MView
    { nrows      = 0
    , ncols      = 0
    , leadingDim = 1
    , buffer     = buf
    }
