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
    -- * Int types
    -- $BLAS_int
  , BLASInt
  , toB
  , LAPACKInt
  , toL
  , pattern LAPACK0
    -- * Enumeration wrappers
  , CEnum(..)
  , MatrixLayout(..)
  , MatrixTranspose(..)
  , UpLo(..)
  , Side(..)
  ) where

import Data.Complex
import Data.Primitive.Ptr
#ifdef VECVEC_BLAS64
import Data.Int
#endif
import Foreign.C
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal
import Foreign.Storable.Complex ()
import Vecvec.Classes           (NormedScalar(..))

-- We want to be able to easily switch how we do foreign calls. ccall
-- is slightly faster while capi allows to check that implementation
-- is correct
#define CCALL capi


-- $BLAS_int
--
-- BLAS could use ints of different width for indexing (32 or
-- 64bits). We want to support both options which are selected at
-- build time. So representation of 'BLASInt' and 'LAPACKInt' depends
-- on build options.

-- | Integer type used by BLAS
newtype BLASInt = BLASInt
#ifdef VECVEC_BLAS64
    Int64
#else
    CInt
#endif
  deriving newtype Storable

-- | Integer type used by LAPACK
newtype LAPACKInt = LAPACKInt
#ifdef VECVEC_LAPACK64
    Int64
#else
    CInt
#endif
  deriving newtype Storable

-- FIXME: We should really trap overflows. But...

toB :: Int -> BLASInt
toB = BLASInt . fromIntegral

toL :: Int -> LAPACKInt
toL = LAPACKInt . fromIntegral

-- | Zero (used in error checking)
pattern LAPACK0 :: LAPACKInt
pattern LAPACK0 = LAPACKInt 0
{-# INLINE LAPACK0 #-}

----------------------------------------------------------------
-- Type synonyms
----------------------------------------------------------------

type S = Float
type D = Double
type C = Complex Float
type Z = Complex Double

type ARR a r = Ptr a -> BLASInt -> r

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
  axpy :: BLASInt   -- ^ Number of elements
       -> a         -- ^ Scalar @a@
       -> Ptr a     -- ^ [in] array @x@
       -> BLASInt   -- ^ increment for elements of @x@
       -> Ptr a     -- ^ [in,out] array @y@
       -> BLASInt   -- ^ increment for elements of @y@
       -> IO ()

  copy :: BLASInt
       -> Ptr a -> BLASInt -- ^ Source vector
       -> Ptr a -> BLASInt -- ^ Target vector
       -> IO ()

  scal :: BLASInt
       -> a                -- ^ Constant to scale vector
       -> Ptr a -> BLASInt -- ^ Vector to modify
       -> IO ()

  dot  :: BLASInt
       -> Ptr a -> BLASInt
       -> Ptr a -> BLASInt
       -> IO a

  dotc :: BLASInt
       -> Ptr a -> BLASInt
       -> Ptr a -> BLASInt
       -> IO a

  nrm2 :: BLASInt
       -> Ptr a -> BLASInt
       -> IO (R a)

  -- | Matrix-vector multiplication. Compute one of:
  --
  -- > y := α·A·x       + β·y
  -- > y := α·tr(A)·x   + β·y
  -- > y := α·conj(A)·x + β·y
  gemv
    :: MatrixLayout    -- ^ Matrix layout
    -> MatrixTranspose -- ^ Whether matrix should be transposed
    -> BLASInt         -- ^ Number of rows
    -> BLASInt         -- ^ Number of columns
    -> a               -- ^ Scalar @α@
    -> Ptr a           -- ^ Pointer to matrix data @A@
    -> BLASInt         -- ^ Leading dimension size
    -> Ptr a           -- ^ Buffer for vector @x@
    -> BLASInt         -- ^ Stride of vector @x@
    -> a               -- ^ Scalar β
    -> Ptr a           -- ^ Buffer for vector @y@
    -> BLASInt         -- ^ Stride for vector @y@
    -> IO ()

  -- | Symmetric-vector multiplication. Compute one of:
  --
  -- > y := conj(α·A·x) + β·y
  symv
    :: MatrixLayout -- ^ Matrix layout
    -> UpLo         -- ^ Whether upper or lower part of matrix should be referenced
    -> BLASInt      -- ^ Size of matrix
    -> a            -- ^ Scalar @α@
    -> Ptr a        -- ^ Pointer to matrix data @A@
    -> BLASInt      -- ^ Leading dimension size
    -> Ptr a        -- ^ Buffer for vector @x@
    -> BLASInt      -- ^ Stride of vector @x@
    -> a            -- ^ Scalar β
    -> Ptr a        -- ^ Buffer for vector @y@
    -> BLASInt      -- ^ Stride for vector @y@
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
    -> BLASInt         -- ^ @m@ — number of rows in A and C
    -> BLASInt         -- ^ @n@ — number of columns in B and C
    -> BLASInt         -- ^ @k@ — number of columns in A and rows in B
    -> a               -- ^ Scalar @α@
    -> Ptr a           -- ^ Buffer for matrix @A@
    -> BLASInt         -- ^ Leading dimension for @A@
    -> Ptr a           -- ^ Buffer for matrix @B@
    -> BLASInt         -- ^ Leading dimension for @B@
    -> a               -- ^ Scalar @β@
    -> Ptr a           -- ^ Buffer for matrix @C@
    -> BLASInt         -- ^ Leading dimension for @C@
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
    -> BLASInt      -- ^ @M@ number of rows of matrix @C@
    -> BLASInt      -- ^ @N@ number of columns of matrix @C@
    -> a            -- ^ @α@ constant
    -> Ptr a        -- ^ Buffer for matrix @A@
    -> BLASInt      -- ^ Leading dimension for @A@
    -> Ptr a        -- ^ Buffer for matrix @B@
    -> BLASInt      -- ^ Leading dimension for @B@
    -> a            -- ^ @β@ constant
    -> Ptr a        -- ^ Buffer for matrix @C@
    -> BLASInt      -- ^ Leading dimension for @C@
    -> IO ()

  -- | LAPACK driver routine for computing SVD decomposition
  --   \(A=U\Sigma{}V^T\).
  gesdd
    :: CRepr MatrixLayout -- ^ Matrix layout
    -> CChar              -- ^ Job variant
    -> LAPACKInt          -- ^ @m@ number of rows of @A@
    -> LAPACKInt          -- ^ @n@ number of columns of @A@
    -> Ptr a              -- ^ Matrix @A@
    -> LAPACKInt          -- ^ Leading dimension size of @A@
    -> Ptr (R a)          -- ^ Vector of singular values @min(m,n)@
    -> Ptr a              -- ^ Buffer for matrix @U@
    -> LAPACKInt          -- ^ Leading dimension size of @U@
    -> Ptr a              -- ^ Buffer for matrix @tr(V)@
    -> LAPACKInt          -- ^ Leading dimension of @tr(V)@
    -> IO LAPACKInt

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
    -> LAPACKInt          -- ^ Size of square matrix A
    -> LAPACKInt          -- ^ Number of right sides
    -> Ptr a              -- ^ Buffer of @A@. Upon exit factor @L@ and
                          --   @U@ are stored there.
    -> LAPACKInt          -- ^ Leading dimension size of @A@
    -> Ptr LAPACKInt      -- ^ Integer array of size @N@. Upon exit
                          --   contains pivot indices that define the
                          --   permutation matrix P; row i of the matrix
                          --   was interchanged with row IPIV[i].
    -> Ptr a              -- ^ Right side of equations @B@. On exit
                          --   contains solutions to equation
    -> LAPACKInt          -- ^ Leading dimension size of @B@
    -> IO LAPACKInt
  -- NOTE: *getrs solves using transposition/conjugation

  -- | Compute inverse of square matrix @A@ using the LU factorization
  --   computed by 'getrf' routine.
  getri
    :: CRepr MatrixLayout -- ^ Matrix layout
    -> LAPACKInt          -- ^ Matrix size @N@
    -> Ptr a              -- ^ LU decomposition of @A@ matrix.
    -> LAPACKInt          -- ^ Leading dimension of @A@
    -> Ptr LAPACKInt      -- ^ Buffer of length @N@ for permutation matrix
    -> IO LAPACKInt

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
    -> LAPACKInt          -- ^ @M@: Number of rows of matrix @A@
    -> LAPACKInt          -- ^ @N@: Number of columns of matrix @A@
    -> Ptr a              -- ^ Matrix @A@. Overwritten with factors @L@ and @U@.
    -> LAPACKInt          -- ^ Leading dimension of @A@
    -> Ptr LAPACKInt      -- ^ Integer array @IPIV@, dimension
                          --   @min(M,N)@. Row i of the matrix was
                          --   interchanged with row IPIV(i).
    -> IO LAPACKInt


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

foreign import CCALL unsafe "cblas.h cblas_saxpy" s_axpy :: BLASInt -> S     -> ARR S (ARR S (IO ()))
foreign import CCALL unsafe "cblas.h cblas_daxpy" d_axpy :: BLASInt -> D     -> ARR D (ARR D (IO ()))
foreign import CCALL unsafe "cblas.h cblas_caxpy" c_axpy :: BLASInt -> Ptr C -> ARR C (ARR C (IO ()))
foreign import CCALL unsafe "cblas.h cblas_zaxpy" z_axpy :: BLASInt -> Ptr Z -> ARR Z (ARR Z (IO ()))

foreign import CCALL unsafe "cblas.h cblas_scopy" s_copy :: BLASInt -> ARR S (ARR S (IO ()))
foreign import CCALL unsafe "cblas.h cblas_dcopy" d_copy :: BLASInt -> ARR D (ARR D (IO ()))
foreign import CCALL unsafe "cblas.h cblas_ccopy" c_copy :: BLASInt -> ARR C (ARR C (IO ()))
foreign import CCALL unsafe "cblas.h cblas_zcopy" z_copy :: BLASInt -> ARR Z (ARR Z (IO ()))

foreign import CCALL unsafe "cblas.h cblas_sscal" s_scal :: BLASInt -> S     -> ARR S (IO ())
foreign import CCALL unsafe "cblas.h cblas_dscal" d_scal :: BLASInt -> D     -> ARR D (IO ())
foreign import CCALL unsafe "cblas.h cblas_cscal" c_scal :: BLASInt -> Ptr C -> ARR C (IO ())
foreign import CCALL unsafe "cblas.h cblas_zscal" z_scal :: BLASInt -> Ptr Z -> ARR Z (IO ())

foreign import CCALL unsafe "cblas.h cblas_sdot"      s_dot  :: BLASInt -> ARR S (ARR S (IO S))
foreign import CCALL unsafe "cblas.h cblas_ddot"      d_dot  :: BLASInt -> ARR D (ARR D (IO D))
foreign import CCALL unsafe "cblas.h cblas_cdotu_sub" c_dotu :: BLASInt -> ARR C (ARR C (Ptr C -> IO ()))
foreign import CCALL unsafe "cblas.h cblas_zdotu_sub" z_dotu :: BLASInt -> ARR Z (ARR Z (Ptr Z -> IO ()))
foreign import CCALL unsafe "cblas.h cblas_cdotc_sub" c_dotc :: BLASInt -> ARR C (ARR C (Ptr C -> IO ()))
foreign import CCALL unsafe "cblas.h cblas_zdotc_sub" z_dotc :: BLASInt -> ARR Z (ARR Z (Ptr Z -> IO ()))

foreign import CCALL unsafe "cblas.h cblas_snrm2"  s_nrm2 :: BLASInt -> ARR S (IO S)
foreign import CCALL unsafe "cblas.h cblas_dnrm2"  d_nrm2 :: BLASInt -> ARR D (IO D)
foreign import CCALL unsafe "cblas.h cblas_scnrm2" c_nrm2 :: BLASInt -> ARR C (IO S)
foreign import CCALL unsafe "cblas.h cblas_dznrm2" z_nrm2 :: BLASInt -> ARR Z (IO D)


foreign import CCALL unsafe "cblas.h cblas_sgemv" s_gemv
  :: CRepr MatrixLayout
  -> CRepr MatrixTranspose
  -> BLASInt -> BLASInt -> S -> Ptr S -> BLASInt
  -> ARR S (S -> ARR S (IO ()))
foreign import CCALL unsafe "cblas.h cblas_dgemv" d_gemv
  :: CRepr MatrixLayout
  -> CRepr MatrixTranspose
  -> BLASInt -> BLASInt -> D -> Ptr D -> BLASInt
  -> ARR D (D -> ARR D (IO ()))
foreign import CCALL unsafe "cblas.h cblas_cgemv" c_gemv
  :: CRepr MatrixLayout
  -> CRepr MatrixTranspose
  -> BLASInt -> BLASInt -> Ptr C -> Ptr C -> BLASInt
  -> ARR C (Ptr C -> ARR C (IO ()))
foreign import CCALL unsafe "cblas.h cblas_zgemv" z_gemv
  :: CRepr MatrixLayout
  -> CRepr MatrixTranspose
  -> BLASInt -> BLASInt -> Ptr Z -> Ptr Z -> BLASInt
  -> ARR Z (Ptr Z -> ARR Z (IO ()))

foreign import CCALL unsafe "cblas.h cblas_ssymv" s_symv
  :: CRepr MatrixLayout
  -> CRepr UpLo
  -> BLASInt
  -> Float -> Ptr Float -> BLASInt
  -> Ptr Float -> BLASInt
  -> Float
  -> Ptr Float -> BLASInt
  -> IO ()
foreign import CCALL unsafe "cblas.h cblas_dsymv" d_symv
  :: CRepr MatrixLayout
  -> CRepr UpLo
  -> BLASInt
  -> Double -> Ptr Double -> BLASInt
  -> Ptr Double -> BLASInt
  -> Double
  -> Ptr Double -> BLASInt
  -> IO ()

-- NOTE: there's no cblas wrapper for CSYMV and ZSYMV!
--
-- We have to call FORTRAN versions directly.
foreign import ccall unsafe "csymv_" c_symv
  :: Ptr (CRepr UpLo)    -- Upper/lower part should be referenced
  -> Ptr BLASInt         -- Size of matrix/vector
  -> Ptr (Complex Float) -- alpha
  -> Ptr (Complex Float) -- Matrix buffer
  -> Ptr BLASInt         -- LDA
  -> Ptr (Complex Float) -- Vector buffer
  -> Ptr BLASInt         -- Vector stride
  -> Ptr (Complex Float) -- beta
  -> Ptr (Complex Float) -- Output vector buffer
  -> Ptr BLASInt         -- Output vector stride
  -> IO ()

foreign import ccall unsafe "zsymv_" z_symv
  :: Ptr (CRepr UpLo)     -- Upper/lower part should be referenced
  -> Ptr BLASInt          -- Size of matrix/vector
  -> Ptr (Complex Double) -- alpha
  -> Ptr (Complex Double) -- Matrix buffer
  -> Ptr BLASInt          -- LDA
  -> Ptr (Complex Double) -- Vector buffer
  -> Ptr BLASInt          -- Vector stride
  -> Ptr (Complex Double) -- beta
  -> Ptr (Complex Double) -- Output vector buffer
  -> Ptr BLASInt          -- Output vector stride
  -> IO ()


foreign import CCALL unsafe "cblas.h cblas_sgemm" s_gemm
  :: CRepr MatrixLayout
  -> CRepr MatrixTranspose
  -> CRepr MatrixTranspose
  -> BLASInt
  -> BLASInt
  -> BLASInt
  -> S
  -> Ptr S -> BLASInt
  -> Ptr S -> BLASInt
  -> S
  -> Ptr S -> BLASInt
  -> IO ()
foreign import CCALL unsafe "cblas.h cblas_dgemm" d_gemm
  :: CRepr MatrixLayout
  -> CRepr MatrixTranspose
  -> CRepr MatrixTranspose
  -> BLASInt
  -> BLASInt
  -> BLASInt
  -> D
  -> Ptr D -> BLASInt
  -> Ptr D -> BLASInt
  -> D
  -> Ptr D -> BLASInt
  -> IO ()
foreign import CCALL unsafe "cblas.h cblas_cgemm" c_gemm
  :: CRepr MatrixLayout
  -> CRepr MatrixTranspose
  -> CRepr MatrixTranspose
  -> BLASInt
  -> BLASInt
  -> BLASInt
  -> Ptr C
  -> Ptr C -> BLASInt
  -> Ptr C -> BLASInt
  -> Ptr C
  -> Ptr C -> BLASInt
  -> IO ()
foreign import CCALL unsafe "cblas.h cblas_zgemm" z_gemm
  :: CRepr MatrixLayout
  -> CRepr MatrixTranspose
  -> CRepr MatrixTranspose
  -> BLASInt
  -> BLASInt
  -> BLASInt
  -> Ptr Z
  -> Ptr Z -> BLASInt
  -> Ptr Z -> BLASInt
  -> Ptr Z
  -> Ptr Z -> BLASInt
  -> IO ()


foreign import CCALL unsafe "cblas.h cblas_ssymm" s_symm
  :: CRepr MatrixLayout
  -> CRepr Side
  -> CRepr UpLo
  -> BLASInt   -- M
  -> BLASInt   -- N
  -> Float     -- alpha
  -> Ptr Float -- A
  -> BLASInt   -- lda
  -> Ptr Float -- B
  -> BLASInt   -- ldb
  -> Float     -- beta
  -> Ptr Float -- C
  -> BLASInt   -- ldc
  -> IO ()
foreign import CCALL unsafe "cblas.h cblas_dsymm" d_symm
  :: CRepr MatrixLayout
  -> CRepr Side
  -> CRepr UpLo
  -> BLASInt    -- M
  -> BLASInt    -- N
  -> Double     -- alpha
  -> Ptr Double -- A
  -> BLASInt    -- lda
  -> Ptr Double -- B
  -> BLASInt    -- ldb
  -> Double     -- beta
  -> Ptr Double -- C
  -> BLASInt    -- ldc
  -> IO ()
foreign import CCALL unsafe "cblas.h cblas_csymm" c_symm
  :: CRepr MatrixLayout
  -> CRepr Side
  -> CRepr UpLo
  -> BLASInt             -- M
  -> BLASInt             -- N
  -> Ptr (Complex Float) -- alpha
  -> Ptr (Complex Float) -- A
  -> BLASInt             -- lda
  -> Ptr (Complex Float) -- B
  -> BLASInt             -- ldb
  -> Ptr (Complex Float) -- beta
  -> Ptr (Complex Float) -- C
  -> BLASInt             -- ldc
  -> IO ()
foreign import CCALL unsafe "cblas.h cblas_zsymm" z_symm
  :: CRepr MatrixLayout
  -> CRepr Side
  -> CRepr UpLo
  -> BLASInt              -- M
  -> BLASInt              -- N
  -> Ptr (Complex Double) -- alpha
  -> Ptr (Complex Double) -- A
  -> BLASInt              -- lda
  -> Ptr (Complex Double) -- B
  -> BLASInt              -- ldb
  -> Ptr (Complex Double) -- beta
  -> Ptr (Complex Double) -- C
  -> BLASInt              -- ldc
  -> IO ()




----------------------------------------------------------------
-- LAPACK FFI
----------------------------------------------------------------

-- We have to use ccall. GHC cannot compile capi wrappers


foreign import ccall unsafe "lapacke.h LAPACKE_sgesdd" c_sgesdd
  :: CRepr MatrixLayout -> CChar
  -> LAPACKInt -> LAPACKInt
  -> Ptr Float -> LAPACKInt
  -> Ptr Float
  -> Ptr Float -> LAPACKInt
  -> Ptr Float -> LAPACKInt
  -> IO LAPACKInt

foreign import ccall unsafe "lapacke.h LAPACKE_dgesdd" c_dgesdd
  :: CRepr MatrixLayout -> CChar
  -> LAPACKInt -> LAPACKInt
  -> Ptr Double -> LAPACKInt
  -> Ptr Double
  -> Ptr Double -> LAPACKInt
  -> Ptr Double -> LAPACKInt
  -> IO LAPACKInt

foreign import ccall unsafe "lapacke.h LAPACKE_cgesdd" c_cgesdd
  :: CRepr MatrixLayout -> CChar
  -> LAPACKInt -> LAPACKInt
  -> Ptr (Complex Float) -> LAPACKInt
  -> Ptr Float
  -> Ptr (Complex Float) -> LAPACKInt
  -> Ptr (Complex Float) -> LAPACKInt
  -> IO LAPACKInt

foreign import ccall unsafe "lapacke.h LAPACKE_zgesdd" c_zgesdd
  :: CRepr MatrixLayout -> CChar
  -> LAPACKInt -> LAPACKInt
  -> Ptr (Complex Double) -> LAPACKInt
  -> Ptr Double
  -> Ptr (Complex Double) -> LAPACKInt
  -> Ptr (Complex Double) -> LAPACKInt
  -> IO LAPACKInt



foreign import ccall unsafe "lapacke.h LAPACKE_sgesv" c_sgesv
  :: CRepr MatrixLayout
  -> LAPACKInt -> LAPACKInt
  -> Ptr Float -> LAPACKInt
  -> Ptr LAPACKInt
  -> Ptr Float -> LAPACKInt
  -> IO LAPACKInt

foreign import ccall unsafe "lapacke.h LAPACKE_dgesv" c_dgesv
  :: CRepr MatrixLayout
  -> LAPACKInt -> LAPACKInt
  -> Ptr Double -> LAPACKInt
  -> Ptr LAPACKInt
  -> Ptr Double -> LAPACKInt
  -> IO LAPACKInt

foreign import ccall unsafe "lapacke.h LAPACKE_cgesv" c_cgesv
  :: CRepr MatrixLayout
  -> LAPACKInt -> LAPACKInt
  -> Ptr (Complex Float) -> LAPACKInt
  -> Ptr LAPACKInt
  -> Ptr (Complex Float) -> LAPACKInt
  -> IO LAPACKInt

foreign import ccall unsafe "lapacke.h LAPACKE_zgesv" c_zgesv
  :: CRepr MatrixLayout
  -> LAPACKInt -> LAPACKInt
  -> Ptr (Complex Double) -> LAPACKInt
  -> Ptr LAPACKInt
  -> Ptr (Complex Double) -> LAPACKInt
  -> IO LAPACKInt

foreign import ccall unsafe "lapacke.h LAPACKE_sgetri" c_sgetri
  :: CRepr MatrixLayout -> LAPACKInt -> Ptr Float -> LAPACKInt -> Ptr LAPACKInt -> IO LAPACKInt
foreign import ccall unsafe "lapacke.h LAPACKE_dgetri" c_dgetri
  :: CRepr MatrixLayout -> LAPACKInt -> Ptr Double -> LAPACKInt -> Ptr LAPACKInt -> IO LAPACKInt
foreign import ccall unsafe "lapacke.h LAPACKE_cgetri" c_cgetri
  :: CRepr MatrixLayout -> LAPACKInt -> Ptr (Complex Float) -> LAPACKInt -> Ptr LAPACKInt -> IO LAPACKInt
foreign import ccall unsafe "lapacke.h LAPACKE_zgetri" c_zgetri
  :: CRepr MatrixLayout -> LAPACKInt -> Ptr (Complex Double) -> LAPACKInt -> Ptr LAPACKInt -> IO LAPACKInt

foreign import ccall unsafe "lapacke.h LAPACKE_sgetrf" c_sgetrf
  :: CRepr MatrixLayout -> LAPACKInt -> LAPACKInt -> Ptr Float -> LAPACKInt -> Ptr LAPACKInt -> IO LAPACKInt
foreign import ccall unsafe "lapacke.h LAPACKE_dgetrf" c_dgetrf
  :: CRepr MatrixLayout -> LAPACKInt -> LAPACKInt -> Ptr Double -> LAPACKInt -> Ptr LAPACKInt -> IO LAPACKInt
foreign import ccall unsafe "lapacke.h LAPACKE_cgetrf" c_cgetrf
  :: CRepr MatrixLayout -> LAPACKInt -> LAPACKInt -> Ptr (Complex Float) -> LAPACKInt -> Ptr LAPACKInt -> IO LAPACKInt
foreign import ccall unsafe "lapacke.h LAPACKE_zgetrf" c_zgetrf
  :: CRepr MatrixLayout -> LAPACKInt -> LAPACKInt -> Ptr (Complex Double) -> LAPACKInt -> Ptr LAPACKInt -> IO LAPACKInt
