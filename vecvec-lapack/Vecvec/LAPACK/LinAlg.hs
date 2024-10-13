{-# LANGUAGE RecordWildCards #-}
-- |
-- Linear algebra routines.
module Vecvec.LAPACK.LinAlg
  ( -- * Linear systems
    -- $linear_eq
    -- ** Type classes
    LinearEq(..)
  , LinearEqRHS(..)
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

import Control.Monad
import Control.Monad.ST
import Control.Monad.Primitive
import Foreign.Storable
import Foreign.Marshal.Array
import Data.Char
import Data.Proxy
import Data.Vector               qualified as V
import Data.Vector.Unboxed       qualified as VU
import Data.Vector.Storable      qualified as VS
import Data.Vector.Primitive     qualified as VP
import Data.Vector.Generic       qualified as VG
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
import Vecvec.LAPACK.Utils
import Vecvec.Classes
import Vecvec.Classes.NDArray
import Vecvec.Classes.NDMutable

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
  (\\\) :: (LinearEqRHS rhs a) => m a -> rhs -> rhs

-- | See 'solveLinEq'
instance LAPACKy a => LinearEq Matrix a where
  (\\\) = solveLinEq

-- | See 'solveLinEqSym'
instance LAPACKy a => LinearEq Symmetric a where
  (\\\) = solveLinEqSym

-- | See 'solveLinEqHer'
instance LAPACKy a => LinearEq Hermitian a where
  (\\\) = solveLinEqHer

-- | When solving linear equations like \(Ax=b\) most of the work is
--   spent on factoring matrix. Thus it's computationally advantageous
--   to batch right hand sides of an equation. This type class exists
--   in order to built such batches in form of matrices from haskell
--   data structures
class LinearEqRHS rhs a where
  -- | Number of right hand sides in a container.
  numberOfRhs :: Proxy a -> rhs -> Int
  -- | Compute dimension of right hand side. Should return @Nothing@
  --   if it's not possible to compute one or right sides have different sizes
  dimensionOfRhs :: Proxy a -> rhs -> Maybe Int
  -- | Store RHS to a matrix as columns.
  storeRhsToMatrix :: Storable a
                   => Int         -- ^ Offset in buffer
                   -> rhs         -- ^ Right hand side of equation
                   -> MMatrix s a -- ^ Output buffer
                   -> ST s ()
  -- | Read solution from a matrix. Original right hand side provides
  --   information about shape of RHS.
  loadRhsFromMatrix
    :: Storable a
    => rhs      -- ^ Original RHS
    -> Int      -- ^ Offset in a buffer
    -> Matrix a -- ^ Solution
    -> rhs


-- | Convert right hand of equation to matrix where each \(b\) is
--   arranged as column. We need to create mutable matrix in order
--   to ensure that fresh buffer is allocated since LAPACK routines
--   frequently reuse storage.
rhsToMatrix
  :: forall a rhs s. (LinearEqRHS rhs a, Storable a)
  => rhs -> ST s (MMatrix s a)
rhsToMatrix rhs = do
  mat <- MMat.new (k, n)
  storeRhsToMatrix 0 rhs mat
  return mat
  where
    n = numberOfRhs    (Proxy @a) rhs
    k = maybe (error "No RHS dimension") id
      $ dimensionOfRhs (Proxy @a) rhs

-- | Extract solutions from matrix. First argument is used to retain
--   information which isn't right hand sides.
rhsGetSolutions
  :: forall a rhs. (LinearEqRHS rhs a, Storable a)
  => rhs -> Matrix a -> rhs
rhsGetSolutions rhs solution = loadRhsFromMatrix rhs 0 solution



instance (LinearEqRHS r1 a, LinearEqRHS r2 a) => LinearEqRHS (r1,r2) a where
  numberOfRhs    p (r1,r2) = numberOfRhs p r1 + numberOfRhs p r2
  dimensionOfRhs p (r1,r2)
    | Just n1 <- dimensionOfRhs p r1
    , Just n2 <- dimensionOfRhs p r2
    , n1 == n2
      = Just n1
    | otherwise = Nothing
  --
  storeRhsToMatrix i (r1,r2) dst = do
    storeRhsToMatrix i        r1 dst
    storeRhsToMatrix (i + n1) r2 dst
    where
      n1 = numberOfRhs (Proxy @a) r1
  --
  loadRhsFromMatrix (r1,r2) i res =
    ( loadRhsFromMatrix r1  i       res
    , loadRhsFromMatrix r2 (i + n1) res
    ) where
    n1 = numberOfRhs (Proxy @a) r1



instance (a ~ a', Storable a) => LinearEqRHS (Matrix a) a' where
  numberOfRhs    _ = nCols
  dimensionOfRhs _ = Just . nRows
  --
  storeRhsToMatrix  i src dst = do
    MMat.copy src
              (((0,End), (i, Length (nCols src))) `slice` dst)
  --
  loadRhsFromMatrix rhs i = slice ((0,End), (i, Length (nCols rhs)))


instance (a ~ a', Storable a) => LinearEqRHS (Vec a) a' where
  numberOfRhs    _ _ = 1
  dimensionOfRhs _   = Just . VG.length
  storeRhsToMatrix i v dst = do
    loop0_ (VG.length v) $ \j -> writeArr dst (j,i) (v ! j)
  loadRhsFromMatrix _ i res = getCol res i

instance (a ~ a', Storable a) => LinearEqRHS (V.Vector a) a' where
  numberOfRhs    _ _ = 1
  dimensionOfRhs _   = Just . VG.length
  storeRhsToMatrix i v dst = do
    loop0_ (VG.length v) $ \j -> writeArr dst (j,i) (v ! j)
  loadRhsFromMatrix _ i res = VG.convert $ getCol res i

instance (a ~ a', Storable a) => LinearEqRHS (VS.Vector a) a' where
  numberOfRhs    _ _ = 1
  dimensionOfRhs _   = Just . VG.length
  storeRhsToMatrix i v dst = do
    loop0_ (VG.length v) $ \j -> writeArr dst (j,i) (v ! j)
  loadRhsFromMatrix _ i res = VG.convert $ getCol res i

instance (a ~ a', Storable a, VU.Unbox a) => LinearEqRHS (VU.Vector a) a' where
  numberOfRhs    _ _ = 1
  dimensionOfRhs _   = Just . VG.length
  storeRhsToMatrix i v dst = do
    loop0_ (VG.length v) $ \j -> writeArr dst (j,i) (v ! j)
  loadRhsFromMatrix _ i res = VG.convert $ getCol res i

instance (a ~ a', Storable a, VP.Prim a) => LinearEqRHS (VP.Vector a) a' where
  numberOfRhs    _ _ = 1
  dimensionOfRhs _   = Just . VG.length
  storeRhsToMatrix i v dst = do
    loop0_ (VG.length v) $ \j -> writeArr dst (j,i) (v ! j)
  loadRhsFromMatrix _ i res = VG.convert $ getCol res i




-- | Simple solver for linear equation of the form \(Ax=b\).
--
--   Note that this function does not check whether matrix is
--   ill-conditioned and may return nonsensical answers in this case.
--
--   /Uses _GESV LAPACK routine internally/
solveLinEq
  :: (LinearEqRHS rhs a, LAPACKy a)
  => Matrix a -- ^ Matrix \(A\)
  -> rhs      -- ^ Right hand side(s) \(b\)
  -> rhs
solveLinEq a _
  | nRows a /= nCols a = error "Matrix A is not square"
solveLinEq a0 rhs = runST $ do
  -- Prepare right hand side and check sizes. We also need to clone
  -- A. It gets destroyed during solution
  MMatrix a <- MMat.clone a0
  let n = ncols a
  MMatrix b <- rhsToMatrix rhs
  when (nrows b /= n) $ error "Right hand dimensions don't match"
  -- Solve equation
  info <-
    unsafeIOToPrim                $
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
  -> rhs         -- ^ Right hand side(s) \(b\)
  -> rhs
solveLinEqSym a0 rhs = runST $ do
  -- Clone A it gets destroyed during solution
  MSymmetric a <- MSym.clone a0
  -- Prepare right hand side and check sizes. We also need to clone
  let n = a.size
  MMatrix b <- rhsToMatrix rhs
  when (nrows b /= n) $ error "Right hand dimensions don't match"
  -- Solve equation
  info <-
    unsafeIOToPrim                $
    unsafeWithForeignPtr a.buffer $ \ptr_a    ->
    unsafeWithForeignPtr b.buffer $ \ptr_b    ->
    allocaArray n                 $ \ptr_ipiv ->
      sysv (toCEnum RowMajor) (toCEnum FortranUP)
        (toL n) (toL (ncols b))
        ptr_a (toL a.leadingDim)
        ptr_ipiv
        ptr_b (toL b.leadingDim)
  case info of
    LAPACK0 -> pure $ rhsGetSolutions rhs (Matrix b)
    _       -> error "solveLinEqSym failed"

-- | Simple solver for linear equation of the form \(Ax=b\) where
--   \(A\) is hermitian matrix
--
--   Note that this function does not check whether matrix is
--   ill-conditioned and may return nonsensical answers in this case.
--
--   /Uses _HESV LAPACK routine internally/
solveLinEqHer
  :: (LinearEqRHS rhs a, LAPACKy a)
  => Hermitian a -- ^ Matrix \(A\)
  -> rhs         -- ^ Right hand side(s) \(b\)
  -> rhs
solveLinEqHer a0 rhs = runST $ do
  -- Clone A it gets destroyed during solution
  MHermitian a <- MHer.clone a0
  -- Prepare right hand side and check sizes. We also need to clone
  let n = a.size
  MMatrix b <- rhsToMatrix rhs
  when (nrows b /= n) $ error "Right hand dimensions don't match"
  -- Solve equation
  info <-
    unsafeIOToPrim                $
    unsafeWithForeignPtr a.buffer $ \ptr_a    ->
    unsafeWithForeignPtr b.buffer $ \ptr_b    ->
    allocaArray n                 $ \ptr_ipiv ->
      hesv (toCEnum RowMajor) (toCEnum FortranUP)
        (toL n) (toL (ncols b))
        ptr_a (toL a.leadingDim)
        ptr_ipiv
        ptr_b (toL b.leadingDim)
  case info of
    LAPACK0 -> pure $ rhsGetSolutions rhs (Matrix b)
    _       -> error "solveLinEqSym failed"
