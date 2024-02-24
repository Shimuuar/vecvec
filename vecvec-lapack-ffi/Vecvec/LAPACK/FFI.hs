{-# LANGUAGE CApiFFI                    #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE ForeignFunctionInterface   #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
-- |
module Vecvec.LAPACK.FFI
  ( LAPACKy(..)
    -- * Type synonyms
  , S
  , D
  , C
  , Z
    -- * Enumeration wrappers
  , CEnum(..)
  , MatrixLayout(..)
  , MatrixTranspose(..)
  , UpLo(..)
  , Side(..)
  ) where

import Data.Complex
import Data.Primitive.Ptr
import Foreign.C
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal
import Foreign.Storable.Complex ()
import Vecvec.Classes           (NormedScalar(..))

-- We want to be able to easily switch how we do foreign calls. ccall
-- is slightly faster while capi allows to check
#define CCALL capi


----------------------------------------------------------------
-- Type synonyms
----------------------------------------------------------------

type S = Float
type D = Double
type C = Complex Float
type Z = Complex Double

type ARR a r = Ptr a -> CInt -> r

----------------------------------------------------------------
-- Enumerations
----------------------------------------------------------------

-- | Type class for conversion of haskell representation of values and
--   constants to representation used in FFI calls
class CEnum a where
  data CRepr a
  toCEnum :: a -> CRepr a

-- | Layout of matrix. Could be row or column major.
data MatrixLayout
  = RowMajor
  | ColMajor
  deriving stock (Show,Eq)

instance CEnum MatrixLayout where
  newtype CRepr MatrixLayout = CMatrixLayout CInt
  {-# INLINE toCEnum #-}
  toCEnum = \case
    RowMajor -> c_BLAS_ROW_MAJOR
    ColMajor -> c_BLAS_COL_MAJOR

-- | Whether upper or lower part of symmetric\/hermitian matrix is
--   referenced.
data UpLo
  = UP
  | LO
  deriving stock (Show,Eq)

instance CEnum UpLo where
  newtype CRepr UpLo = CUpLo CInt
  {-# INLINE toCEnum #-}
  toCEnum = \case
    UP -> c_UP
    LO -> c_LO

-- | Which side i
data Side
  = LeftSide
  | RightSide
  deriving stock (Show,Eq)

instance CEnum Side where
  newtype CRepr Side = CSide CInt
  {-# INLINE toCEnum #-}
  toCEnum = \case
    LeftSide  -> c_LEFT
    RightSide -> c_RIGHT


deriving newtype instance Storable (CRepr UpLo)
deriving newtype instance Storable (CRepr MatrixLayout)
deriving newtype instance Storable (CRepr Side)

foreign import capi "cblas.h value CblasRowMajor" c_BLAS_ROW_MAJOR :: CRepr MatrixLayout
foreign import capi "cblas.h value CblasColMajor" c_BLAS_COL_MAJOR :: CRepr MatrixLayout
foreign import capi "cblas.h value CblasUpper"    c_UP             :: CRepr UpLo
foreign import capi "cblas.h value CblasLower"    c_LO             :: CRepr UpLo
foreign import capi "cblas.h value CblasLeft"     c_LEFT           :: CRepr Side
foreign import capi "cblas.h value CblasRight"    c_RIGHT          :: CRepr Side

-- | Whether matrix should be transposed, transposed and conjugater
data MatrixTranspose
  = NoTrans
  | Trans
  | ConjTrans
  | ConjNoTrans
  deriving stock (Show,Eq)

instance CEnum MatrixTranspose where
  newtype CRepr MatrixTranspose = CMatrixTranspose CInt
  {-# INLINE toCEnum #-}
  toCEnum = \case
    NoTrans     -> c_NO_TRANS
    Trans       -> c_TRANS
    ConjTrans   -> c_CONJ_TRANS
    ConjNoTrans -> c_CONJ_NO_TRANS

foreign import capi "cblas.h value CblasNoTrans"     c_NO_TRANS      :: CRepr MatrixTranspose
foreign import capi "cblas.h value CblasTrans"       c_TRANS         :: CRepr MatrixTranspose
foreign import capi "cblas.h value CblasConjTrans"   c_CONJ_TRANS    :: CRepr MatrixTranspose
foreign import capi "cblas.h value CblasConjNoTrans" c_CONJ_NO_TRANS :: CRepr MatrixTranspose


----------------------------------------------------------------
-- Overload of BLAS functions
----------------------------------------------------------------

-- | LAPACK provides function for working with single and double
--   precision numbers and corresponding complex numbers. We use this
--   type class to provide overloading.
--
--   There're only 4 instances excluding newtypes.
class (NormedScalar a, Storable a) => LAPACKy a where
  -- | Fill buffer with zeros
  fillZeros :: Ptr a -- ^ Pointer to buffer
            -> Int   -- ^ Size of buffer in elements
            -> IO ()


  -- | Computes a vector-scalar product and adds the result to a vector.
  --
  -- > y := a*x + y
  axpy :: CInt      -- ^ Number of elements
       -> a         -- ^ Scalar @a@
       -> Ptr a     -- ^ [in] array @x@
       -> CInt      -- ^ increment for elements of @x@
       -> Ptr a     -- ^ [in,out] array @y@
       -> CInt      -- ^ increment for elements of @y@
       -> IO ()

  copy :: CInt
       -> Ptr a -> CInt -- ^ Source vector
       -> Ptr a -> CInt -- ^ Target vector
       -> IO ()

  scal :: CInt
       -> a             -- ^ Constant to scale vector
       -> Ptr a -> CInt -- ^ Vector to modify
       -> IO ()

  dot  :: CInt
       -> Ptr a -> CInt
       -> Ptr a -> CInt
       -> IO a

  dotc :: CInt
       -> Ptr a -> CInt
       -> Ptr a -> CInt
       -> IO a

  nrm2 :: CInt
       -> Ptr a -> CInt
       -> IO (R a)

  -- | Matrix-vector multiplication. Compute one of:
  --
  -- > y := α·A·x       + β·y
  -- > y := α·tr(A)·x   + β·y
  -- > y := α·conj(A)·x + β·y
  gemv
    :: MatrixLayout    -- ^ Matrix layout
    -> MatrixTranspose -- ^ Whether matrix should be transposed
    -> CInt            -- ^ Number of rows
    -> CInt            -- ^ Number of columns
    -> a               -- ^ Scalar @α@
    -> Ptr a           -- ^ Pointer to matrix data @A@
    -> CInt            -- ^ Leading dimension size
    -> Ptr a           -- ^ Buffer for vector @x@
    -> CInt            -- ^ Stride of vector @x@
    -> a               -- ^ Scalar β
    -> Ptr a           -- ^ Buffer for vector @y@
    -> CInt            -- ^ Stride for vector @y@
    -> IO ()

  -- | Symmetric-vector multiplication. Compute one of:
  --
  -- > y := conj(α·A·x) + β·y
  symv
    :: MatrixLayout -- ^ Matrix layout
    -> UpLo         -- ^ Whether upper or lower part of matrix should be referenced
    -> CInt         -- ^ Size of matrix
    -> a            -- ^ Scalar @α@
    -> Ptr a        -- ^ Pointer to matrix data @A@
    -> CInt         -- ^ Leading dimension size
    -> Ptr a        -- ^ Buffer for vector @x@
    -> CInt         -- ^ Stride of vector @x@
    -> a            -- ^ Scalar β
    -> Ptr a        -- ^ Buffer for vector @y@
    -> CInt         -- ^ Stride for vector @y@
    -> IO ()


  -- | Matrix-matrix multiplication.
  --
  -- > C := α·op(A)·op(B) + β·C
  --
  -- * @op(A)@: m×k matrix
  -- * @op(B)@: k×n matrix
  -- * @C    @: m×n matrix
  gemm
    :: MatrixLayout    -- ^ Matrix layout
    -> MatrixTranspose -- ^ Operation applied to matrix @A@
    -> MatrixTranspose -- ^ Operation applied to matrix @B@
    -> CInt            -- ^ @m@ — number of rows in A and C
    -> CInt            -- ^ @n@ — number of columns in B and C
    -> CInt            -- ^ @k@ — number of columns in A and rows in B
    -> a               -- ^ Scalar @α@
    -> Ptr a           -- ^ Buffer for matrix @A@
    -> CInt            -- ^ Leading dimension for @A@
    -> Ptr a           -- ^ Buffer for matrix @B@
    -> CInt            -- ^ Leading dimension for @B@
    -> a               -- ^ Scalar @β@
    -> Ptr a           -- ^ Buffer for matrix @C@
    -> CInt            -- ^ Leading dimension for @C@
    -> IO ()

  -- | Multiplication of symmetric and general matrix. It evaluates one of:
  --
  -- > C := α·A·B + β·C   -- LeftSide
  -- > C := α·B·A + β·C   -- RightSide
  --
  -- Where @A@ is symmetric matrix and @B@ and @C@ are general n×m
  -- matrices.
  symm
    :: MatrixLayout -- ^ Layout of matrix
    -> Side         -- ^ On which side symmetric matrix go
    -> UpLo         -- ^ Which part of symmetric matrix is referenced
    -> CInt         -- ^ @M@ number of rows of matrix @C@
    -> CInt         -- ^ @N@ number of columns of matrix @C@
    -> a            -- ^ @α@ constant
    -> Ptr a        -- ^ Buffer for matrix @A@
    -> CInt         -- ^ Leading dimension for @A@
    -> Ptr a        -- ^ Buffer for matrix @B@
    -> CInt         -- ^ Leading dimension for @B@
    -> a            -- ^ @β@ constant
    -> Ptr a        -- ^ Buffer for matrix @C@
    -> CInt         -- ^ Leading dimension for @C@
    -> IO ()

  -- | LAPACK driver routine for computing SVD decomposition
  --   \(A=U\Sigma{}V^T\).
  gesdd
    :: CRepr MatrixLayout -- ^ Matrix layout
    -> CChar              -- ^ Job variant
    -> CInt               -- ^ @m@ number of rows of @A@
    -> CInt               -- ^ @n@ number of columns of @A@
    -> Ptr a              -- ^ Matrix @A@
    -> CInt               -- ^ Leading dimension size of @A@
    -> Ptr (R a)          -- ^ Vector of singular values @min(m,n)@
    -> Ptr a              -- ^ Buffer for matrix @U@
    -> CInt               -- ^ Leading dimension size of @U@
    -> Ptr a              -- ^ Buffer for matrix @tr(V)@
    -> CInt               -- ^ Leading dimension of @tr(V)@
    -> IO CInt

  -- | Solve linear system \(Ax=B\) where B could have multiple right
  -- sides where A is square matrix of dimension @N×N@
  --
  -- The LU decomposition with partial pivoting and row interchanges
  -- is used to factor A as \(A = PLU\) where P is a permutation
  -- matrix, L is unit lower triangular, and U is upper triangular.
  -- The factored form of A is then used to solve the system of
  -- equations A * X = B.
  gesv
    :: CRepr MatrixLayout -- ^ Matrix layout
    -> CInt               -- ^ Size of square matrix A
    -> CInt               -- ^ Number of right sides
    -> Ptr a              -- ^ Buffer of @A@. Upon exit factor @L@ and
                          --   @U@ are stored there.
    -> CInt               -- ^ Leading dimension size of @A@
    -> Ptr CInt           -- ^ Integer array of size @N@. Upon exit
                          --   contains pivot indices that define the
                          --   permutation matrix P; row i of the matrix
                          --   was interchanged with row IPIV[i].
    -> Ptr a              -- ^ Right side of equations @B@. On exit
                          --   contains solutions to equation
    -> CInt               -- ^ Leading dimension size of @B@
    -> IO CInt
  -- NOTE: *getrs solves using transposition/conjugation

  -- | Compute inverse of square matrix @A@ using the LU factorization
  --   computed by 'getrf' routine.
  getri
    :: CRepr MatrixLayout -- ^ Matrix layout
    -> CInt               -- ^ Matrix size @N@
    -> Ptr a              -- ^ LU decomposition of @A@ matrix.
    -> CInt               -- ^ Leading dimension of @A@
    -> Ptr CInt           -- ^ Buffer of length @N@ for permutation matrix
    -> IO CInt

  -- | Compute LU factorization of a general M-by-N matrix A using
  --   partial pivoting with row interchanges. The factorization has
  --   the form
  --
  --  \[ A = P L U \]
  --
  -- where P is a permutation matrix, L is lower triangular with unit
  -- diagonal elements (lower trapezoidal if m > n), and U is upper
  -- triangular (upper trapezoidal if m < n).
  getrf
    :: CRepr MatrixLayout -- ^ Matrix layout
    -> CInt               -- ^ @M@: Number of rows of matrix @A@
    -> CInt               -- ^ @N@: Number of columns of matrix @A@
    -> Ptr a              -- ^ Matrix @A@. Overwritten with factors @L@ and @U@.
    -> CInt               -- ^ Leading dimension of @A@
    -> Ptr CInt           -- ^ Integer array @IPIV@, dimension
                          --   @min(M,N)@. Row i of the matrix was
                          --   interchanged with row IPIV(i).
    -> IO CInt


instance LAPACKy Float where
  fillZeros ptr n = setPtr ptr n 0
  axpy = s_axpy
  copy = s_copy
  scal = s_scal
  dot  = s_dot
  dotc = s_dot
  nrm2 = s_nrm2
  gemv layout op = s_gemv (toCEnum layout) (toCEnum op)
  {-# INLINE gemv #-}
  symv layout uplo = s_symv (toCEnum layout) (toCEnum uplo)
  {-# INLINE symv #-}
  gemm layout opA opB = s_gemm (toCEnum layout) (toCEnum opA) (toCEnum opB)
  {-# INLINE gemm #-}
  symm layout side uplo = s_symm (toCEnum layout) (toCEnum side) (toCEnum uplo)
  {-# INLINE symm #-}
  -- LAPACK
  gesdd = c_sgesdd
  gesv  = c_sgesv
  getri = c_sgetri
  getrf = c_sgetrf

instance LAPACKy Double where
  fillZeros ptr n = setPtr ptr n 0
  axpy = d_axpy
  copy = d_copy
  scal = d_scal
  dot  = d_dot
  dotc = d_dot
  nrm2 = d_nrm2
  gemv layout op = d_gemv (toCEnum layout) (toCEnum op)
  {-# INLINE gemv #-}
  symv layout uplo = d_symv (toCEnum layout) (toCEnum uplo)
  {-# INLINE symv #-}
  gemm layout opA opB = d_gemm (toCEnum layout) (toCEnum opA) (toCEnum opB)
  {-# INLINE gemm #-}
  symm layout side uplo = d_symm (toCEnum layout) (toCEnum side) (toCEnum uplo)
  {-# INLINE symm #-}
  -- LAPACK
  gesdd = c_dgesdd
  gesv  = c_dgesv
  getri = c_dgetri
  getrf = c_dgetrf

instance LAPACKy (Complex Float) where
  fillZeros ptr n = fillZeros (castPtr @_ @Double ptr) n
  copy = c_copy
  nrm2 = c_nrm2
  --
  {-# INLINE axpy #-}
  axpy n a x incX y incY = alloca $ \p_a -> do
    poke p_a a
    c_axpy n p_a x incX y incY
  --
  {-# INLINE scal #-}
  scal n a x incX = alloca $ \p_a -> do
    poke p_a a
    c_scal n p_a x incX
  --
  {-# INLINE dot #-}
  dot n x incX y incY = alloca $ \p_a -> do
    c_dotu n x incX y incY p_a >> peek p_a
  {-# INLINE dotc #-}
  dotc n x incX y incY = alloca $ \p_a -> do
    c_dotc n x incX y incY p_a >> peek p_a
  --
  -- FIXME: we should coalesce two alloca
  {-# INLINE gemv #-}
  gemv layout tr
    n_r n_c α p_A ldA
    p_x incX β p_y incY
    = alloca $ \p_α -> alloca $ \p_β -> do
        poke p_α α
        poke p_β β
        c_gemv (toCEnum layout) (toCEnum tr)
          n_r n_c p_α p_A ldA
          p_x incX p_β p_y incY
  {-# INLINE symv #-}
  symv layout uplo sz α p_A ldA p_x incX β p_y incY
    = alloca $ \p_uplo   ->
      alloca $ \p_sz     ->
      alloca $ \p_α      ->
      alloca $ \p_lda    ->
      alloca $ \p_incX   ->
      alloca $ \p_β      ->
      alloca $ \p_incY   -> do
        poke p_uplo $ case layout of
          RowMajor -> case uplo of UP -> CUpLo 76 -- 'L'
                                   LO -> CUpLo 85 -- 'U'
          ColMajor -> case uplo of UP -> CUpLo 85 -- 'U'
                                   LO -> CUpLo 76 -- 'L'
        poke p_sz   sz
        poke p_α    α
        poke p_lda  ldA
        poke p_incX incX
        poke p_β    β
        poke p_incY incY
        c_symv p_uplo p_sz p_α p_A p_lda p_x p_incX p_β p_y p_incY

  {-# INLINE gemm #-}
  gemm layout opA opB m n k α bufA ldaA bufB ldaB β bufC ldaC
    = alloca $ \p_α -> alloca $ \p_β -> do
        poke p_α α
        poke p_β β
        c_gemm (toCEnum layout) (toCEnum opA) (toCEnum opB) m n k p_α bufA ldaA bufB ldaB p_β bufC ldaC
  {-# INLINE symm #-}
  symm layout side uplo m n α bufA lda bufB ldb β bufC ldC
    = alloca $ \p_α -> alloca $ \p_β -> do
        poke p_α α
        poke p_β β
        c_symm (toCEnum layout) (toCEnum side) (toCEnum uplo)
          m n p_α bufA lda bufB ldb p_β bufC ldC
  -- LAPACK
  gesdd = c_cgesdd
  gesv  = c_cgesv
  getri = c_cgetri
  getrf = c_cgetrf

instance LAPACKy (Complex Double) where
  fillZeros ptr n = fillZeros (castPtr @_ @Double ptr) (2*n)
  copy = z_copy
  nrm2 = z_nrm2
  --
  {-# INLINE axpy #-}
  axpy n a x incX y incY = alloca $ \p_a -> do
    poke p_a a
    z_axpy n p_a x incX y incY
  --
  {-# INLINE scal #-}
  scal n a x incX = alloca $ \p_a -> do
    poke p_a a
    z_scal n p_a x incX
  --
  {-# INLINE dot #-}
  dot n x incX y incY = alloca $ \p_a -> do
    z_dotu n x incX y incY p_a >> peek p_a
  {-# INLINE dotc #-}
  dotc n x incX y incY = alloca $ \p_a -> do
    z_dotc n x incX y incY p_a >> peek p_a
  --
  -- FIXME: we should coalesce two alloca
  {-# INLINE gemv #-}
  gemv layout tr
    n_r n_c α p_A ldA
    p_x incX β p_y incY
    = alloca $ \p_α -> alloca $ \p_β -> do
        poke p_α α
        poke p_β β
        z_gemv (toCEnum layout) (toCEnum tr)
          n_r n_c p_α p_A ldA
          p_x incX p_β p_y incY
  {-# INLINE symv #-}
  symv layout uplo sz α p_A ldA p_x incX β p_y incY
    = alloca $ \p_uplo   ->
      alloca $ \p_sz     ->
      alloca $ \p_α      ->
      alloca $ \p_lda    ->
      alloca $ \p_incX   ->
      alloca $ \p_β      ->
      alloca $ \p_incY   -> do
        poke p_uplo $ case layout of
          RowMajor -> case uplo of UP -> CUpLo 76 -- 'L'
                                   LO -> CUpLo 85 -- 'U'
          ColMajor -> case uplo of UP -> CUpLo 85 -- 'U'
                                   LO -> CUpLo 76 -- 'L'
        poke p_sz   sz
        poke p_α    α
        poke p_lda  ldA
        poke p_incX incX
        poke p_β    β
        poke p_incY incY
        z_symv p_uplo p_sz p_α p_A p_lda p_x p_incX p_β p_y p_incY
  {-# INLINE gemm #-}
  gemm layout opA opB m n k α bufA ldaA bufB ldaB β bufC ldaC
    = alloca $ \p_α -> alloca $ \p_β -> do
        poke p_α α
        poke p_β β
        z_gemm (toCEnum layout) (toCEnum opA) (toCEnum opB) m n k p_α bufA ldaA bufB ldaB p_β bufC ldaC
  {-# INLINE symm #-}
  symm layout side uplo m n α bufA lda bufB ldb β bufC ldC
    = alloca $ \p_α -> alloca $ \p_β -> do
        poke p_α α
        poke p_β β
        z_symm (toCEnum layout) (toCEnum side) (toCEnum uplo)
          m n p_α bufA lda bufB ldb p_β bufC ldC  -- LAPACK
  gesdd = c_zgesdd
  gesv  = c_zgesv
  getri = c_zgetri
  getrf = c_zgetrf

----------------------------------------------------------------
-- BLAS FFI
----------------------------------------------------------------

foreign import CCALL unsafe "cblas.h cblas_saxpy" s_axpy :: CInt -> S     -> ARR S (ARR S (IO ()))
foreign import CCALL unsafe "cblas.h cblas_daxpy" d_axpy :: CInt -> D     -> ARR D (ARR D (IO ()))
foreign import CCALL unsafe "cblas.h cblas_caxpy" c_axpy :: CInt -> Ptr C -> ARR C (ARR C (IO ()))
foreign import CCALL unsafe "cblas.h cblas_zaxpy" z_axpy :: CInt -> Ptr Z -> ARR Z (ARR Z (IO ()))

foreign import CCALL unsafe "cblas.h cblas_scopy" s_copy :: CInt -> ARR S (ARR S (IO ()))
foreign import CCALL unsafe "cblas.h cblas_dcopy" d_copy :: CInt -> ARR D (ARR D (IO ()))
foreign import CCALL unsafe "cblas.h cblas_ccopy" c_copy :: CInt -> ARR C (ARR C (IO ()))
foreign import CCALL unsafe "cblas.h cblas_zcopy" z_copy :: CInt -> ARR Z (ARR Z (IO ()))

foreign import CCALL unsafe "cblas.h cblas_sscal" s_scal :: CInt -> S     -> ARR S (IO ())
foreign import CCALL unsafe "cblas.h cblas_dscal" d_scal :: CInt -> D     -> ARR D (IO ())
foreign import CCALL unsafe "cblas.h cblas_cscal" c_scal :: CInt -> Ptr C -> ARR C (IO ())
foreign import CCALL unsafe "cblas.h cblas_zscal" z_scal :: CInt -> Ptr Z -> ARR Z (IO ())

foreign import CCALL unsafe "cblas.h cblas_sdot"      s_dot  :: CInt -> ARR S (ARR S (IO S))
foreign import CCALL unsafe "cblas.h cblas_ddot"      d_dot  :: CInt -> ARR D (ARR D (IO D))
foreign import CCALL unsafe "cblas.h cblas_cdotu_sub" c_dotu :: CInt -> ARR C (ARR C (Ptr C -> IO ()))
foreign import CCALL unsafe "cblas.h cblas_zdotu_sub" z_dotu :: CInt -> ARR Z (ARR Z (Ptr Z -> IO ()))
foreign import CCALL unsafe "cblas.h cblas_cdotc_sub" c_dotc :: CInt -> ARR C (ARR C (Ptr C -> IO ()))
foreign import CCALL unsafe "cblas.h cblas_zdotc_sub" z_dotc :: CInt -> ARR Z (ARR Z (Ptr Z -> IO ()))

foreign import CCALL unsafe "cblas.h cblas_snrm2"  s_nrm2 :: CInt -> ARR S (IO S)
foreign import CCALL unsafe "cblas.h cblas_dnrm2"  d_nrm2 :: CInt -> ARR D (IO D)
foreign import CCALL unsafe "cblas.h cblas_scnrm2" c_nrm2 :: CInt -> ARR C (IO S)
foreign import CCALL unsafe "cblas.h cblas_dznrm2" z_nrm2 :: CInt -> ARR Z (IO D)


foreign import CCALL unsafe "cblas.h cblas_sgemv" s_gemv
  :: CRepr MatrixLayout
  -> CRepr MatrixTranspose
  -> CInt -> CInt -> S -> Ptr S -> CInt
  -> ARR S (S -> ARR S (IO ()))
foreign import CCALL unsafe "cblas.h cblas_dgemv" d_gemv
  :: CRepr MatrixLayout
  -> CRepr MatrixTranspose
  -> CInt -> CInt -> D -> Ptr D -> CInt
  -> ARR D (D -> ARR D (IO ()))
foreign import CCALL unsafe "cblas.h cblas_cgemv" c_gemv
  :: CRepr MatrixLayout
  -> CRepr MatrixTranspose
  -> CInt -> CInt -> Ptr C -> Ptr C -> CInt
  -> ARR C (Ptr C -> ARR C (IO ()))
foreign import CCALL unsafe "cblas.h cblas_zgemv" z_gemv
  :: CRepr MatrixLayout
  -> CRepr MatrixTranspose
  -> CInt -> CInt -> Ptr Z -> Ptr Z -> CInt
  -> ARR Z (Ptr Z -> ARR Z (IO ()))

foreign import CCALL unsafe "cblas.h cblas_ssymv" s_symv
  :: CRepr MatrixLayout
  -> CRepr UpLo
  -> CInt
  -> Float -> Ptr Float -> CInt
  -> Ptr Float -> CInt
  -> Float
  -> Ptr Float -> CInt
  -> IO ()
foreign import CCALL unsafe "cblas.h cblas_dsymv" d_symv
  :: CRepr MatrixLayout
  -> CRepr UpLo
  -> CInt
  -> Double -> Ptr Double -> CInt
  -> Ptr Double -> CInt
  -> Double
  -> Ptr Double -> CInt
  -> IO ()

-- NOTE: there's no cblas wrapper for CSYMV and ZSYMV!
--
-- We have to call FORTRAN versions directly.
foreign import ccall unsafe "csymv_" c_symv
  :: Ptr (CRepr UpLo)    -- Upper/lower part should be referenced
  -> Ptr CInt            -- Size of matrix/vector
  -> Ptr (Complex Float) -- alpha
  -> Ptr (Complex Float) -- Matrix buffer
  -> Ptr CInt            -- LDA
  -> Ptr (Complex Float) -- Vector buffer
  -> Ptr CInt            -- Vector stride
  -> Ptr (Complex Float) -- beta
  -> Ptr (Complex Float) -- Output vector buffer
  -> Ptr CInt            -- Output vector stride
  -> IO ()

foreign import ccall unsafe "zsymv_" z_symv
  :: Ptr (CRepr UpLo)     -- Upper/lower part should be referenced
  -> Ptr CInt             -- Size of matrix/vector
  -> Ptr (Complex Double) -- alpha
  -> Ptr (Complex Double) -- Matrix buffer
  -> Ptr CInt             -- LDA
  -> Ptr (Complex Double) -- Vector buffer
  -> Ptr CInt             -- Vector stride
  -> Ptr (Complex Double) -- beta
  -> Ptr (Complex Double) -- Output vector buffer
  -> Ptr CInt             -- Output vector stride
  -> IO ()


foreign import CCALL unsafe "cblas.h cblas_sgemm" s_gemm
  :: CRepr MatrixLayout
  -> CRepr MatrixTranspose
  -> CRepr MatrixTranspose
  -> CInt
  -> CInt
  -> CInt
  -> S
  -> Ptr S -> CInt
  -> Ptr S -> CInt
  -> S
  -> Ptr S -> CInt
  -> IO ()
foreign import CCALL unsafe "cblas.h cblas_dgemm" d_gemm
  :: CRepr MatrixLayout
  -> CRepr MatrixTranspose
  -> CRepr MatrixTranspose
  -> CInt
  -> CInt
  -> CInt
  -> D
  -> Ptr D -> CInt
  -> Ptr D -> CInt
  -> D
  -> Ptr D -> CInt
  -> IO ()
foreign import CCALL unsafe "cblas.h cblas_cgemm" c_gemm
  :: CRepr MatrixLayout
  -> CRepr MatrixTranspose
  -> CRepr MatrixTranspose
  -> CInt
  -> CInt
  -> CInt
  -> Ptr C
  -> Ptr C -> CInt
  -> Ptr C -> CInt
  -> Ptr C
  -> Ptr C -> CInt
  -> IO ()
foreign import CCALL unsafe "cblas.h cblas_zgemm" z_gemm
  :: CRepr MatrixLayout
  -> CRepr MatrixTranspose
  -> CRepr MatrixTranspose
  -> CInt
  -> CInt
  -> CInt
  -> Ptr Z
  -> Ptr Z -> CInt
  -> Ptr Z -> CInt
  -> Ptr Z
  -> Ptr Z -> CInt
  -> IO ()


foreign import CCALL unsafe "cblas.h cblas_ssymm" s_symm
  :: CRepr MatrixLayout
  -> CRepr Side
  -> CRepr UpLo
  -> CInt      -- M
  -> CInt      -- N
  -> Float     -- alpha
  -> Ptr Float -- A
  -> CInt      -- lda
  -> Ptr Float -- B
  -> CInt      -- ldb
  -> Float     -- beta
  -> Ptr Float -- C
  -> CInt      -- ldc
  -> IO ()
foreign import CCALL unsafe "cblas.h cblas_dsymm" d_symm
  :: CRepr MatrixLayout
  -> CRepr Side
  -> CRepr UpLo
  -> CInt       -- M
  -> CInt       -- N
  -> Double     -- alpha
  -> Ptr Double -- A
  -> CInt       -- lda
  -> Ptr Double -- B
  -> CInt       -- ldb
  -> Double     -- beta
  -> Ptr Double -- C
  -> CInt       -- ldc
  -> IO ()
foreign import CCALL unsafe "cblas.h cblas_csymm" c_symm
  :: CRepr MatrixLayout
  -> CRepr Side
  -> CRepr UpLo
  -> CInt                -- M
  -> CInt                -- N
  -> Ptr (Complex Float) -- alpha
  -> Ptr (Complex Float) -- A
  -> CInt                -- lda
  -> Ptr (Complex Float) -- B
  -> CInt                -- ldb
  -> Ptr (Complex Float) -- beta
  -> Ptr (Complex Float) -- C
  -> CInt                -- ldc
  -> IO ()
foreign import CCALL unsafe "cblas.h cblas_zsymm" z_symm
  :: CRepr MatrixLayout
  -> CRepr Side
  -> CRepr UpLo
  -> CInt                 -- M
  -> CInt                 -- N
  -> Ptr (Complex Double) -- alpha
  -> Ptr (Complex Double) -- A
  -> CInt                 -- lda
  -> Ptr (Complex Double) -- B
  -> CInt                 -- ldb
  -> Ptr (Complex Double) -- beta
  -> Ptr (Complex Double) -- C
  -> CInt                 -- ldc
  -> IO ()




----------------------------------------------------------------
-- LAPACK FFI
----------------------------------------------------------------

-- We have to use ccall. GHC cannot compile capi wrappers


foreign import ccall unsafe "lapacke.h LAPACKE_sgesdd" c_sgesdd
  :: CRepr MatrixLayout -> CChar
  -> CInt -> CInt
  -> Ptr Float -> CInt
  -> Ptr Float
  -> Ptr Float -> CInt
  -> Ptr Float -> CInt
  -> IO CInt

foreign import ccall unsafe "lapacke.h LAPACKE_dgesdd" c_dgesdd
  :: CRepr MatrixLayout -> CChar
  -> CInt -> CInt
  -> Ptr Double -> CInt
  -> Ptr Double
  -> Ptr Double -> CInt
  -> Ptr Double -> CInt
  -> IO CInt

foreign import ccall unsafe "lapacke.h LAPACKE_cgesdd" c_cgesdd
  :: CRepr MatrixLayout -> CChar
  -> CInt -> CInt
  -> Ptr (Complex Float) -> CInt
  -> Ptr Float
  -> Ptr (Complex Float) -> CInt
  -> Ptr (Complex Float) -> CInt
  -> IO CInt

foreign import ccall unsafe "lapacke.h LAPACKE_zgesdd" c_zgesdd
  :: CRepr MatrixLayout -> CChar
  -> CInt -> CInt
  -> Ptr (Complex Double) -> CInt
  -> Ptr Double
  -> Ptr (Complex Double) -> CInt
  -> Ptr (Complex Double) -> CInt
  -> IO CInt



foreign import ccall unsafe "lapacke.h LAPACKE_sgesv" c_sgesv
  :: CRepr MatrixLayout
  -> CInt -> CInt
  -> Ptr Float -> CInt
  -> Ptr CInt
  -> Ptr Float -> CInt
  -> IO CInt

foreign import ccall unsafe "lapacke.h LAPACKE_dgesv" c_dgesv
  :: CRepr MatrixLayout
  -> CInt -> CInt
  -> Ptr Double -> CInt
  -> Ptr CInt
  -> Ptr Double -> CInt
  -> IO CInt

foreign import ccall unsafe "lapacke.h LAPACKE_cgesv" c_cgesv
  :: CRepr MatrixLayout
  -> CInt -> CInt
  -> Ptr (Complex Float) -> CInt
  -> Ptr CInt
  -> Ptr (Complex Float) -> CInt
  -> IO CInt

foreign import ccall unsafe "lapacke.h LAPACKE_zgesv" c_zgesv
  :: CRepr MatrixLayout
  -> CInt -> CInt
  -> Ptr (Complex Double) -> CInt
  -> Ptr CInt
  -> Ptr (Complex Double) -> CInt
  -> IO CInt

foreign import ccall unsafe "lapacke.h LAPACKE_sgetri" c_sgetri
  :: CRepr MatrixLayout -> CInt -> Ptr Float -> CInt -> Ptr CInt -> IO CInt
foreign import ccall unsafe "lapacke.h LAPACKE_dgetri" c_dgetri
  :: CRepr MatrixLayout -> CInt -> Ptr Double -> CInt -> Ptr CInt -> IO CInt
foreign import ccall unsafe "lapacke.h LAPACKE_cgetri" c_cgetri
  :: CRepr MatrixLayout -> CInt -> Ptr (Complex Float) -> CInt -> Ptr CInt -> IO CInt
foreign import ccall unsafe "lapacke.h LAPACKE_zgetri" c_zgetri
  :: CRepr MatrixLayout -> CInt -> Ptr (Complex Double) -> CInt -> Ptr CInt -> IO CInt

foreign import ccall unsafe "lapacke.h LAPACKE_sgetrf" c_sgetrf
  :: CRepr MatrixLayout -> CInt -> CInt -> Ptr Float -> CInt -> Ptr CInt -> IO CInt
foreign import ccall unsafe "lapacke.h LAPACKE_dgetrf" c_dgetrf
  :: CRepr MatrixLayout -> CInt -> CInt -> Ptr Double -> CInt -> Ptr CInt -> IO CInt
foreign import ccall unsafe "lapacke.h LAPACKE_cgetrf" c_cgetrf
  :: CRepr MatrixLayout -> CInt -> CInt -> Ptr (Complex Float) -> CInt -> Ptr CInt -> IO CInt
foreign import ccall unsafe "lapacke.h LAPACKE_zgetrf" c_zgetrf
  :: CRepr MatrixLayout -> CInt -> CInt -> Ptr (Complex Double) -> CInt -> Ptr CInt -> IO CInt
