{-# LANGUAGE CApiFFI                    #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE ForeignFunctionInterface   #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
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
  , CRepr(..)
  , CEnum(..)
  , MatrixLayout(..)
  , MatrixTranspose(..)
  , UpLo(..)
  , FortranUpLo(..)
  , Side(..)
  , EigJob(..)
  , SvdJob(..)
  ) where

import Data.Complex
import Data.Primitive.Ptr       (setPtr)
import Data.Kind
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

-- | C representation of an enumeration
newtype CRepr a = CRepr (CRepresentation a)

deriving stock   instance Show     (CRepresentation a) => Show     (CRepr a)
deriving stock   instance Eq       (CRepresentation a) => Eq       (CRepr a)
deriving newtype instance Storable (CRepresentation a) => Storable (CRepr a)

-- | Type class for conversion of haskell representation of values and
--   constants to representation used in FFI calls
class CEnum a where
  type family CRepresentation a :: Type
  type CRepresentation a = CInt
  toCEnum :: a -> CRepr a

-- | Layout of matrix. Could be row or column major.
data MatrixLayout
  = RowMajor
  | ColMajor
  deriving stock (Show,Eq)

instance CEnum MatrixLayout where
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
  {-# INLINE toCEnum #-}
  toCEnum = \case
    UP -> c_UP
    LO -> c_LO

-- | Whether upper or lower part of symmetric\/hermitian matrix is
--   referenced. This is FORTRAN enumerations which is different from
--   CBLAS.
data FortranUpLo
  = FortranUP
  | FortranLO
  deriving stock (Show, Eq)

instance CEnum FortranUpLo where
  {-# INLINE toCEnum #-}
  toCEnum = \case
    FortranUP -> CRepr 85 -- 'U'
    FortranLO -> CRepr 76 -- 'L'

-- | Job specification for eigenvector computation
data EigJob
  = EigV -- ^ Compute eigenvectors
  | EigN -- ^ Do not compute eigenvectors
  deriving stock (Show, Eq)

instance CEnum EigJob where
  type CRepresentation EigJob = CChar
  {-# INLINE toCEnum #-}
  toCEnum = \case
    EigV -> CRepr 86 -- 'V'
    EigN -> CRepr 78 -- 'N'

-- | How SVD decomposition \(A = U\Sigma\V^T\) should be
--   computed. \(U\) is @M×M@ matrix, and \(V\) is @N×N@.
data SvdJob
  = SvdA -- ^ Matrices U and V are computed fully and returned in
         --   output arrays
  | SvdS -- ^ the first @min(M,N)@ columns of U and the first @min(M,N)@
         --   rows of @V^T@ are computed and returned in output arrays.
  | SvdO -- ^ If @M >= N@, the first N columns of U are overwritten on
         --   the array A and all rows of @V^T@ are returned in the
         --   array @VT@; otherwise, all columns of U are returned in
         --   the array U and the first M rows of @V^T@ are
         --   overwritten in the array A;
  | SvdN -- ^ No columns of U or rows of V^T are computed.
  deriving stock (Show, Eq)

instance CEnum SvdJob where
  type CRepresentation SvdJob = CChar
  {-# INLINE toCEnum #-}
  toCEnum = \case
    SvdA -> CRepr 65 -- 'A'
    SvdS -> CRepr 83 -- 'S'
    SvdO -> CRepr 79 -- 'O'
    SvdN -> CRepr 78 -- 'N'

-- | On which side of multiplication matrix appears
data Side
  = LeftSide
  | RightSide
  deriving stock (Show,Eq)

instance CEnum Side where
  {-# INLINE toCEnum #-}
  toCEnum = \case
    LeftSide  -> c_LEFT
    RightSide -> c_RIGHT

-- | Whether matrix should be transposed, transposed and conjugater
data MatrixTranspose
  = NoTrans
  | Trans
  | ConjTrans
  | ConjNoTrans
  deriving stock (Show,Eq)

instance CEnum MatrixTranspose where
  {-# INLINE toCEnum #-}
  toCEnum = \case
    NoTrans     -> c_NO_TRANS
    Trans       -> c_TRANS
    ConjTrans   -> c_CONJ_TRANS
    ConjNoTrans -> c_CONJ_NO_TRANS



foreign import capi "cblas.h value CblasRowMajor"    c_BLAS_ROW_MAJOR :: CRepr MatrixLayout
foreign import capi "cblas.h value CblasColMajor"    c_BLAS_COL_MAJOR :: CRepr MatrixLayout
foreign import capi "cblas.h value CblasUpper"       c_UP             :: CRepr UpLo
foreign import capi "cblas.h value CblasLower"       c_LO             :: CRepr UpLo
foreign import capi "cblas.h value CblasLeft"        c_LEFT           :: CRepr Side
foreign import capi "cblas.h value CblasRight"       c_RIGHT          :: CRepr Side
foreign import capi "cblas.h value CblasNoTrans"     c_NO_TRANS       :: CRepr MatrixTranspose
foreign import capi "cblas.h value CblasTrans"       c_TRANS          :: CRepr MatrixTranspose
foreign import capi "cblas.h value CblasConjTrans"   c_CONJ_TRANS     :: CRepr MatrixTranspose
foreign import capi "cblas.h value CblasConjNoTrans" c_CONJ_NO_TRANS  :: CRepr MatrixTranspose


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

  -- | Hermitian-vector multiplication. Compute one of:
  --
  -- > y := conj(α·A·x) + β·y
  hemv
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

  -- | Multiplication of hermitian and general matrix. It evaluates one of:
  --
  -- > C := α·A·B + β·C   -- LeftSide
  -- > C := α·B·A + β·C   -- RightSide
  --
  -- Where @A@ is hermitian matrix and @B@ and @C@ are general n×m
  -- matrices.
  hemm
    :: MatrixLayout -- ^ Layout of matrix
    -> Side         -- ^ On which side hermitian matrix go
    -> UpLo         -- ^ Which part of hermitian matrix is referenced
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
    :: MatrixLayout -- ^ Matrix layout
    -> SvdJob       -- ^ Job variant
    -> LAPACKInt    -- ^ @m@ number of rows of @A@
    -> LAPACKInt    -- ^ @n@ number of columns of @A@
    -> Ptr a        -- ^ Matrix @A@
    -> LAPACKInt    -- ^ Leading dimension size of @A@
    -> Ptr (R a)    -- ^ Vector of singular values @min(m,n)@
    -> Ptr a        -- ^ Buffer for matrix @U@
    -> LAPACKInt    -- ^ Leading dimension size of @U@
    -> Ptr a        -- ^ Buffer for matrix \(V^T\)
    -> LAPACKInt    -- ^ Leading dimension of \(V^T\)
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
    :: MatrixLayout  -- ^ Matrix layout
    -> LAPACKInt     -- ^ Size of square matrix A
    -> LAPACKInt     -- ^ Number of right sides
    -> Ptr a         -- ^ Buffer of @A@. Upon exit factor @L@ and
                     --   @U@ are stored there.
    -> LAPACKInt     -- ^ Leading dimension size of @A@
    -> Ptr LAPACKInt -- ^ Integer array of size @N@. Upon exit
                     --   contains pivot indices that define the
                     --   permutation matrix P; row i of the matrix
                     --   was interchanged with row IPIV[i].
    -> Ptr a         -- ^ Right side of equations @B@. On exit
                     --   contains solutions to equation
    -> LAPACKInt     -- ^ Leading dimension size of @B@
    -> IO LAPACKInt

  -- | Solve to a real system of linear equations \(Ax=B\), where @A@ is
  --   an N-by-N symmetric matrix and @X@ and @B@ are N-by-NRHS matrices.
  sysv
    :: MatrixLayout  -- ^ Matrix layout
    -> FortranUpLo   -- ^ Whether upper or lower part of matrix should be referenced
    -> LAPACKInt     -- ^ Matrix size
    -> LAPACKInt     -- ^ Number of right hand sizes
    -> Ptr a         -- ^ Buffer of @A@ matrix. On exit, overwritten if @INFO=0@
    -> LAPACKInt     -- ^ Leading dimension size of @A@.
    -> Ptr LAPACKInt -- ^ @[out]@ Integer array of size @N@
    -> Ptr a         -- ^ Buffer of matrix of right hand @B@
    -> LAPACKInt     -- ^ Leading dimension size of @B@.
    -> IO LAPACKInt  -- ^ @INFO@ Return parameter. If @INFO=-i@, the
                     --   @i@-th argument has invalid value. If @INFO=i@ block matrix is
                     --   exactly singular and solution could not be found

  -- | Solve to a real system of linear equations \(Ax=B\), where @A@ is
  --   an N-by-N hermitian matrix and @X@ and @B@ are N-by-NRHS matrices.
  hesv
    :: MatrixLayout  -- ^ Matrix layout
    -> FortranUpLo   -- ^ Whether upper or lower part of matrix should be referenced
    -> LAPACKInt     -- ^ Matrix size
    -> LAPACKInt     -- ^ Number of right hand sizes
    -> Ptr a         -- ^ Buffer of @A@ matrix. On exit, overwritten if @INFO=0@
    -> LAPACKInt     -- ^ Leading dimension size of @A@.
    -> Ptr LAPACKInt -- ^ @[out]@ Integer array of size @N@
    -> Ptr a         -- ^ Buffer of matrix of right hand @B@
    -> LAPACKInt     -- ^ Leading dimension size of @B@.
    -> IO LAPACKInt  -- ^ @INFO@ Return parameter. If @INFO=-i@, the
                     --   @i@-th argument has invalid value. If @INFO=i@ block matrix is
                     --   exactly singular and solution could not be found


  -- NOTE: *getrs solves using transposition/conjugation

  -- | Compute inverse of square matrix @A@ using the LU factorization
  --   computed by 'getrf' routine.
  getri
    :: MatrixLayout  -- ^ Matrix layout
    -> LAPACKInt     -- ^ Matrix size @N@
    -> Ptr a         -- ^ LU decomposition of @A@ matrix.
    -> LAPACKInt     -- ^ Leading dimension of @A@
    -> Ptr LAPACKInt -- ^ Buffer of length @N@ for permutation matrix
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
    :: MatrixLayout  -- ^ Matrix layout
    -> LAPACKInt     -- ^ @M@: Number of rows of matrix @A@
    -> LAPACKInt     -- ^ @N@: Number of columns of matrix @A@
    -> Ptr a         -- ^ Matrix @A@. Overwritten with factors @L@ and @U@.
    -> LAPACKInt     -- ^ Leading dimension of @A@
    -> Ptr LAPACKInt -- ^ Integer array @IPIV@, dimension @min(M,N)@. Row i
                     --   of the matrix was interchanged with row IPIV(i).
    -> IO LAPACKInt

  -- | Compute eigenvalues and (optionally) eigenvectors of a general matrix
  geev
    :: MatrixLayout        -- ^ Matrix layout
    -> EigJob              -- ^ Whether to compute left eigenvectors
    -> EigJob              -- ^ Whether to compute right eigenvectors
    -> LAPACKInt           -- ^ Size of a matrix
    -> Ptr a               -- ^ @[IN,OUT]@ Buffer of a matrix. It will
                           --   be overwritten on exit.
    -> LAPACKInt           -- ^ Leading dimension of a matrix
    -> Ptr (Complex (R a)) -- ^ @[OUT]@ buffer for eigenvalues
    -> Ptr a               -- ^ Buffer for left eigenvectors.
    -> LAPACKInt           -- ^ Leading dimension for left eigenvectors.
    -> Ptr a               -- ^ Buffer for right eigenvectors.
    -> LAPACKInt           -- ^ Leading dimension for right eigenvectors.
    -> IO LAPACKInt

  -- | Compute eigenvalues and (optionally) eigenvectors of a
  --   hermitian (symmetric for real @a@ matrix.
  heev
    :: MatrixLayout -- ^ Matrix layout
    -> EigJob       -- ^ Whether to compute eigenvalues
    -> FortranUpLo  -- ^ Whether upper or lower part of matrix is referenced
    -> LAPACKInt    -- ^ Size of a matrix
    -> Ptr a        -- ^ @[IN,OUT]@ Matrix A: N×N array.
    -> LAPACKInt    -- ^ Leading dimension for @A@
    -> Ptr (R a)    -- ^ @[OUT]@ eigenvalues of a matrix in ascending order
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
  hemv = symv
  gemm layout opA opB = s_gemm (toCEnum layout) (toCEnum opA) (toCEnum opB)
  {-# INLINE gemm #-}
  symm layout side uplo = s_symm (toCEnum layout) (toCEnum side) (toCEnum uplo)
  {-# INLINE symm #-}
  hemm = symm
  -- LAPACK
  gesdd layout job = c_sgesdd (toCEnum layout) (toCEnum job)
  {-# INLINE gesdd #-}
  gesv layout = c_sgesv (toCEnum layout)
  {-# INLINE gesv #-}
  sysv layout uplo = c_ssysv (toCEnum layout) (toCEnum uplo)
  {-# INLINE sysv #-}
  hesv = sysv
  {-# INLINE hesv #-}
  getri layout = c_sgetri (toCEnum layout)
  {-# INLINE getri #-}
  getrf layout = c_sgetrf (toCEnum layout)
  {-# INLINE getrf #-}
  geev layout jobL jobR sz@(LAPACKInt (fromIntegral -> sz_i)) ptrA lda w vL ldL vR ldR
    = allocaArray (2 * sz_i) $ \ptr_v -> do
        let ptr_re = ptr_v
            ptr_im = ptr_v `advancePtr` sz_i
        res <- c_sgeev (toCEnum layout) (toCEnum jobL) (toCEnum jobR)
          sz ptrA lda
          ptr_re ptr_im
          vL ldL vR ldR
        case res of
          LAPACK0 -> loop0 sz_i $ \i -> do re <- peekElemOff ptr_re i
                                           im <- peekElemOff ptr_im i
                                           pokeElemOff w i (re :+ im)
          _       -> pure ()
        return res
  {-# INLINE geev #-}
  heev layout job uplo = c_ssyev (toCEnum layout) (toCEnum job) (toCEnum uplo)
  {-# INLINE heev #-}

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
  hemv = symv
  gemm layout opA opB = d_gemm (toCEnum layout) (toCEnum opA) (toCEnum opB)
  {-# INLINE gemm #-}
  symm layout side uplo = d_symm (toCEnum layout) (toCEnum side) (toCEnum uplo)
  {-# INLINE symm #-}
  hemm = symm
  -- LAPACK
  gesdd layout job = c_dgesdd (toCEnum layout) (toCEnum job)
  {-# INLINE gesdd #-}
  gesv layout = c_dgesv (toCEnum layout)
  {-# INLINE gesv #-}
  sysv layout uplo = c_dsysv (toCEnum layout) (toCEnum uplo)
  {-# INLINE sysv #-}
  hesv = sysv
  {-# INLINE hesv #-}
  getri layout = c_dgetri (toCEnum layout)
  {-# INLINE getri #-}
  getrf layout = c_dgetrf (toCEnum layout)
  {-# INLINE getrf #-}
  geev layout jobL jobR sz@(LAPACKInt (fromIntegral -> sz_i)) ptrA lda w vL ldL vR ldR
    = allocaArray (2 * sz_i) $ \ptr_v -> do
        let ptr_re = ptr_v
            ptr_im = ptr_v `advancePtr` sz_i
        res <- c_dgeev (toCEnum layout) (toCEnum jobL) (toCEnum jobR)
          sz ptrA lda
          ptr_re ptr_im
          vL ldL vR ldR
        case res of
          LAPACK0 -> loop0 sz_i $ \i -> do re <- peekElemOff ptr_re i
                                           im <- peekElemOff ptr_im i
                                           pokeElemOff w i (re :+ im)
          _       -> pure ()
        return res
  {-# INLINE geev #-}
  heev layout job uplo = c_dsyev (toCEnum layout) (toCEnum job) (toCEnum uplo)
  {-# INLINE heev #-}

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
        poke p_uplo $ toCEnum $ case layout of
          RowMajor -> case uplo of UP -> FortranLO
                                   LO -> FortranUP
          ColMajor -> case uplo of UP -> FortranUP
                                   LO -> FortranLO
        poke p_sz   sz
        poke p_α    α
        poke p_lda  ldA
        poke p_incX incX
        poke p_β    β
        poke p_incY incY
        c_symv p_uplo p_sz p_α p_A p_lda p_x p_incX p_β p_y p_incY
  {-# INLINE hemv #-}
  hemv layout uplo sz α p_A ldA p_x incX β p_y incY
    = alloca $ \p_α ->
      alloca $ \p_β -> do
        poke p_α α
        poke p_β β
        c_hemv (toCEnum layout) (toCEnum uplo) sz p_α p_A ldA p_x incX p_β p_y incY

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
  {-# INLINE hemm #-}
  hemm layout side uplo m n α bufA lda bufB ldb β bufC ldC
    = alloca $ \p_α -> alloca $ \p_β -> do
        poke p_α α
        poke p_β β
        c_hemm (toCEnum layout) (toCEnum side) (toCEnum uplo)
          m n p_α bufA lda bufB ldb p_β bufC ldC
  -- LAPACK
  gesdd layout job = c_cgesdd (toCEnum layout) (toCEnum job)
  {-# INLINE gesdd #-}
  gesv layout = c_cgesv (toCEnum layout)
  {-# INLINE gesv #-}
  sysv layout uplo = c_csysv (toCEnum layout) (toCEnum uplo)
  {-# INLINE sysv #-}
  hesv layout uplo = c_chesv (toCEnum layout) (toCEnum uplo)
  {-# INLINE hesv #-}
  getri layout = c_cgetri (toCEnum layout)
  {-# INLINE getri #-}
  getrf layout = c_cgetrf (toCEnum layout)
  {-# INLINE getrf #-}
  geev layout jobL jobR = c_cgeev (toCEnum layout) (toCEnum jobL) (toCEnum jobR)
  {-# INLINE geev #-}
  heev layout job uplo = c_cheev (toCEnum layout) (toCEnum job) (toCEnum uplo)
  {-# INLINE heev #-}


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
        poke p_uplo $ toCEnum $ case layout of
          RowMajor -> case uplo of UP -> FortranLO
                                   LO -> FortranUP
          ColMajor -> case uplo of UP -> FortranUP
                                   LO -> FortranLO
        poke p_sz   sz
        poke p_α    α
        poke p_lda  ldA
        poke p_incX incX
        poke p_β    β
        poke p_incY incY
        z_symv p_uplo p_sz p_α p_A p_lda p_x p_incX p_β p_y p_incY
  {-# INLINE hemv #-}
  hemv layout uplo sz α p_A ldA p_x incX β p_y incY
    = alloca $ \p_α ->
      alloca $ \p_β -> do
        poke p_α α
        poke p_β β
        z_hemv (toCEnum layout) (toCEnum uplo) sz p_α p_A ldA p_x incX p_β p_y incY
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
  {-# INLINE hemm #-}
  hemm layout side uplo m n α bufA lda bufB ldb β bufC ldC
    = alloca $ \p_α -> alloca $ \p_β -> do
        poke p_α α
        poke p_β β
        z_hemm (toCEnum layout) (toCEnum side) (toCEnum uplo)
          m n p_α bufA lda bufB ldb p_β bufC ldC  -- LAPACK
  gesdd layout job = c_zgesdd (toCEnum layout) (toCEnum job)
  {-# INLINE gesdd #-}
  gesv layout = c_zgesv (toCEnum layout)
  {-# INLINE gesv #-}
  sysv layout uplo = c_zsysv (toCEnum layout) (toCEnum uplo)
  {-# INLINE sysv #-}
  hesv layout uplo = c_zhesv (toCEnum layout) (toCEnum uplo)
  {-# INLINE hesv #-}
  getri layout = c_zgetri (toCEnum layout)
  {-# INLINE getri #-}
  getrf layout = c_zgetrf (toCEnum layout)
  {-# INLINE getrf #-}
  geev layout jobL jobR = c_zgeev (toCEnum layout) (toCEnum jobL) (toCEnum jobR)
  {-# INLINE geev #-}
  heev layout job uplo = c_zheev (toCEnum layout) (toCEnum job) (toCEnum uplo)
  {-# INLINE heev #-}



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
  -> S -> Ptr S -> BLASInt
  -> Ptr S -> BLASInt
  -> S
  -> Ptr S -> BLASInt
  -> IO ()
foreign import CCALL unsafe "cblas.h cblas_dsymv" d_symv
  :: CRepr MatrixLayout
  -> CRepr UpLo
  -> BLASInt
  -> D -> Ptr D -> BLASInt
  -> Ptr D -> BLASInt
  -> D
  -> Ptr D -> BLASInt
  -> IO ()

foreign import CCALL unsafe "cblas.h cblas_chemv" c_hemv
  :: CRepr MatrixLayout
  -> CRepr UpLo
  -> BLASInt
  -> Ptr C -> Ptr C -> BLASInt
  -> Ptr C -> BLASInt
  -> Ptr C
  -> Ptr C -> BLASInt
  -> IO ()
foreign import CCALL unsafe "cblas.h cblas_zhemv" z_hemv
  :: CRepr MatrixLayout
  -> CRepr UpLo
  -> BLASInt
  -> Ptr Z -> Ptr Z -> BLASInt
  -> Ptr Z -> BLASInt
  -> Ptr Z
  -> Ptr Z -> BLASInt
  -> IO ()


-- NOTE: there's no cblas wrapper for CSYMV and ZSYMV!
--
-- We have to call FORTRAN versions directly.
foreign import ccall unsafe "csymv_" c_symv
  :: Ptr (CRepr FortranUpLo) -- Upper/lower part should be referenced
  -> Ptr BLASInt             -- Size of matrix/vector
  -> Ptr C                   -- alpha
  -> Ptr C                   -- Matrix buffer
  -> Ptr BLASInt             -- LDA
  -> Ptr C                   -- Vector buffer
  -> Ptr BLASInt             -- Vector stride
  -> Ptr C                   -- beta
  -> Ptr C                   -- Output vector buffer
  -> Ptr BLASInt             -- Output vector stride
  -> IO ()

foreign import ccall unsafe "zsymv_" z_symv
  :: Ptr (CRepr FortranUpLo) -- Upper/lower part should be referenced
  -> Ptr BLASInt             -- Size of matrix/vector
  -> Ptr Z                   -- alpha
  -> Ptr Z                   -- Matrix buffer
  -> Ptr BLASInt             -- LDA
  -> Ptr Z                   -- Vector buffer
  -> Ptr BLASInt             -- Vector stride
  -> Ptr Z                   -- beta
  -> Ptr Z                   -- Output vector buffer
  -> Ptr BLASInt             -- Output vector stride
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
  -> S         -- alpha
  -> Ptr S     -- A
  -> BLASInt   -- lda
  -> Ptr S     -- B
  -> BLASInt   -- ldb
  -> S         -- beta
  -> Ptr S     -- C
  -> BLASInt   -- ldc
  -> IO ()
foreign import CCALL unsafe "cblas.h cblas_dsymm" d_symm
  :: CRepr MatrixLayout
  -> CRepr Side
  -> CRepr UpLo
  -> BLASInt    -- M
  -> BLASInt    -- N
  -> D          -- alpha
  -> Ptr D      -- A
  -> BLASInt    -- lda
  -> Ptr D      -- B
  -> BLASInt    -- ldb
  -> D          -- beta
  -> Ptr D      -- C
  -> BLASInt    -- ldc
  -> IO ()
foreign import CCALL unsafe "cblas.h cblas_csymm" c_symm
  :: CRepr MatrixLayout
  -> CRepr Side
  -> CRepr UpLo
  -> BLASInt -- M
  -> BLASInt -- N
  -> Ptr C   -- alpha
  -> Ptr C   -- A
  -> BLASInt -- lda
  -> Ptr C   -- B
  -> BLASInt -- ldb
  -> Ptr C   -- beta
  -> Ptr C   -- C
  -> BLASInt -- ldc
  -> IO ()
foreign import CCALL unsafe "cblas.h cblas_zsymm" z_symm
  :: CRepr MatrixLayout
  -> CRepr Side
  -> CRepr UpLo
  -> BLASInt -- M
  -> BLASInt -- N
  -> Ptr Z   -- alpha
  -> Ptr Z   -- A
  -> BLASInt -- lda
  -> Ptr Z   -- B
  -> BLASInt -- ldb
  -> Ptr Z   -- beta
  -> Ptr Z   -- C
  -> BLASInt -- ldc
  -> IO ()

foreign import CCALL unsafe "cblas.h cblas_chemm" c_hemm
  :: CRepr MatrixLayout
  -> CRepr Side
  -> CRepr UpLo
  -> BLASInt -- M
  -> BLASInt -- N
  -> Ptr C   -- alpha
  -> Ptr C   -- A
  -> BLASInt -- lda
  -> Ptr C   -- B
  -> BLASInt -- ldb
  -> Ptr C   -- beta
  -> Ptr C   -- C
  -> BLASInt -- ldc
  -> IO ()
foreign import CCALL unsafe "cblas.h cblas_zhemm" z_hemm
  :: CRepr MatrixLayout
  -> CRepr Side
  -> CRepr UpLo
  -> BLASInt -- M
  -> BLASInt -- N
  -> Ptr Z   -- alpha
  -> Ptr Z   -- A
  -> BLASInt -- lda
  -> Ptr Z   -- B
  -> BLASInt -- ldb
  -> Ptr Z   -- beta
  -> Ptr Z   -- C
  -> BLASInt -- ldc
  -> IO ()



----------------------------------------------------------------
-- LAPACK FFI
----------------------------------------------------------------

-- We have to use ccall. GHC cannot compile capi wrappers


foreign import ccall unsafe "lapacke.h LAPACKE_sgesdd" c_sgesdd
  :: CRepr MatrixLayout -> CRepr SvdJob
  -> LAPACKInt -> LAPACKInt
  -> Ptr S -> LAPACKInt
  -> Ptr S
  -> Ptr S -> LAPACKInt
  -> Ptr S -> LAPACKInt
  -> IO LAPACKInt

foreign import ccall unsafe "lapacke.h LAPACKE_dgesdd" c_dgesdd
  :: CRepr MatrixLayout -> CRepr SvdJob
  -> LAPACKInt -> LAPACKInt
  -> Ptr D -> LAPACKInt
  -> Ptr D
  -> Ptr D -> LAPACKInt
  -> Ptr D -> LAPACKInt
  -> IO LAPACKInt

foreign import ccall unsafe "lapacke.h LAPACKE_cgesdd" c_cgesdd
  :: CRepr MatrixLayout -> CRepr SvdJob
  -> LAPACKInt -> LAPACKInt
  -> Ptr C -> LAPACKInt
  -> Ptr S
  -> Ptr C -> LAPACKInt
  -> Ptr C -> LAPACKInt
  -> IO LAPACKInt

foreign import ccall unsafe "lapacke.h LAPACKE_zgesdd" c_zgesdd
  :: CRepr MatrixLayout -> CRepr SvdJob
  -> LAPACKInt -> LAPACKInt
  -> Ptr Z -> LAPACKInt
  -> Ptr D
  -> Ptr Z -> LAPACKInt
  -> Ptr Z -> LAPACKInt
  -> IO LAPACKInt



foreign import ccall unsafe "lapacke.h LAPACKE_sgesv" c_sgesv
  :: CRepr MatrixLayout
  -> LAPACKInt -> LAPACKInt
  -> Ptr S -> LAPACKInt
  -> Ptr LAPACKInt
  -> Ptr S -> LAPACKInt
  -> IO LAPACKInt

foreign import ccall unsafe "lapacke.h LAPACKE_dgesv" c_dgesv
  :: CRepr MatrixLayout
  -> LAPACKInt -> LAPACKInt
  -> Ptr D -> LAPACKInt
  -> Ptr LAPACKInt
  -> Ptr D -> LAPACKInt
  -> IO LAPACKInt

foreign import ccall unsafe "lapacke.h LAPACKE_cgesv" c_cgesv
  :: CRepr MatrixLayout
  -> LAPACKInt -> LAPACKInt
  -> Ptr C -> LAPACKInt
  -> Ptr LAPACKInt
  -> Ptr C -> LAPACKInt
  -> IO LAPACKInt

foreign import ccall unsafe "lapacke.h LAPACKE_zgesv" c_zgesv
  :: CRepr MatrixLayout
  -> LAPACKInt -> LAPACKInt
  -> Ptr Z -> LAPACKInt
  -> Ptr LAPACKInt
  -> Ptr Z -> LAPACKInt
  -> IO LAPACKInt

foreign import ccall unsafe "lapacke.h LAPACKE_ssysv" c_ssysv
  :: CRepr MatrixLayout -> CRepr FortranUpLo
  -> LAPACKInt -> LAPACKInt
  -> Ptr S -> LAPACKInt
  -> Ptr LAPACKInt
  -> Ptr S -> LAPACKInt
  -> IO LAPACKInt

foreign import ccall unsafe "lapacke.h LAPACKE_dsysv" c_dsysv
  :: CRepr MatrixLayout -> CRepr FortranUpLo
  -> LAPACKInt -> LAPACKInt
  -> Ptr D -> LAPACKInt
  -> Ptr LAPACKInt
  -> Ptr D -> LAPACKInt
  -> IO LAPACKInt

foreign import ccall unsafe "lapacke.h LAPACKE_csysv" c_csysv
  :: CRepr MatrixLayout -> CRepr FortranUpLo
  -> LAPACKInt -> LAPACKInt
  -> Ptr C -> LAPACKInt
  -> Ptr LAPACKInt
  -> Ptr C -> LAPACKInt
  -> IO LAPACKInt

foreign import ccall unsafe "lapacke.h LAPACKE_zsysv" c_zsysv
  :: CRepr MatrixLayout -> CRepr FortranUpLo
  -> LAPACKInt -> LAPACKInt
  -> Ptr Z -> LAPACKInt
  -> Ptr LAPACKInt
  -> Ptr Z -> LAPACKInt
  -> IO LAPACKInt

foreign import ccall unsafe "lapacke.h LAPACKE_chesv" c_chesv
  :: CRepr MatrixLayout -> CRepr FortranUpLo
  -> LAPACKInt -> LAPACKInt
  -> Ptr C -> LAPACKInt
  -> Ptr LAPACKInt
  -> Ptr C -> LAPACKInt
  -> IO LAPACKInt

foreign import ccall unsafe "lapacke.h LAPACKE_zhesv" c_zhesv
  :: CRepr MatrixLayout -> CRepr FortranUpLo
  -> LAPACKInt -> LAPACKInt
  -> Ptr Z -> LAPACKInt
  -> Ptr LAPACKInt
  -> Ptr Z -> LAPACKInt
  -> IO LAPACKInt

foreign import ccall unsafe "lapacke.h LAPACKE_sgetri" c_sgetri
  :: CRepr MatrixLayout -> LAPACKInt -> Ptr S -> LAPACKInt -> Ptr LAPACKInt -> IO LAPACKInt
foreign import ccall unsafe "lapacke.h LAPACKE_dgetri" c_dgetri
  :: CRepr MatrixLayout -> LAPACKInt -> Ptr D -> LAPACKInt -> Ptr LAPACKInt -> IO LAPACKInt
foreign import ccall unsafe "lapacke.h LAPACKE_cgetri" c_cgetri
  :: CRepr MatrixLayout -> LAPACKInt -> Ptr C -> LAPACKInt -> Ptr LAPACKInt -> IO LAPACKInt
foreign import ccall unsafe "lapacke.h LAPACKE_zgetri" c_zgetri
  :: CRepr MatrixLayout -> LAPACKInt -> Ptr Z -> LAPACKInt -> Ptr LAPACKInt -> IO LAPACKInt

foreign import ccall unsafe "lapacke.h LAPACKE_sgetrf" c_sgetrf
  :: CRepr MatrixLayout -> LAPACKInt -> LAPACKInt -> Ptr S -> LAPACKInt -> Ptr LAPACKInt -> IO LAPACKInt
foreign import ccall unsafe "lapacke.h LAPACKE_dgetrf" c_dgetrf
  :: CRepr MatrixLayout -> LAPACKInt -> LAPACKInt -> Ptr D -> LAPACKInt -> Ptr LAPACKInt -> IO LAPACKInt
foreign import ccall unsafe "lapacke.h LAPACKE_cgetrf" c_cgetrf
  :: CRepr MatrixLayout -> LAPACKInt -> LAPACKInt -> Ptr C -> LAPACKInt -> Ptr LAPACKInt -> IO LAPACKInt
foreign import ccall unsafe "lapacke.h LAPACKE_zgetrf" c_zgetrf
  :: CRepr MatrixLayout -> LAPACKInt -> LAPACKInt -> Ptr Z -> LAPACKInt -> Ptr LAPACKInt -> IO LAPACKInt


foreign import ccall unsafe "lapacke.h LAPACKE_sgeev" c_sgeev
  :: CRepr MatrixLayout
  -> CRepr EigJob -> CRepr EigJob
  -> LAPACKInt -> Ptr S -> LAPACKInt
  -> Ptr S -> Ptr S
  -> Ptr S -> LAPACKInt
  -> Ptr S -> LAPACKInt
  -> IO LAPACKInt

foreign import ccall unsafe "lapacke.h LAPACKE_dgeev" c_dgeev
  :: CRepr MatrixLayout
  -> CRepr EigJob -> CRepr EigJob
  -> LAPACKInt -> Ptr D -> LAPACKInt
  -> Ptr D -> Ptr D
  -> Ptr D -> LAPACKInt
  -> Ptr D -> LAPACKInt
  -> IO LAPACKInt

foreign import ccall unsafe "lapacke.h LAPACKE_cgeev" c_cgeev
  :: CRepr MatrixLayout
  -> CRepr EigJob -> CRepr EigJob
  -> LAPACKInt -> Ptr C -> LAPACKInt
  -> Ptr C
  -> Ptr C -> LAPACKInt
  -> Ptr C -> LAPACKInt
  -> IO LAPACKInt

foreign import ccall unsafe "lapacke.h LAPACKE_zgeev" c_zgeev
  :: CRepr MatrixLayout
  -> CRepr EigJob -> CRepr EigJob
  -> LAPACKInt -> Ptr Z -> LAPACKInt
  -> Ptr Z
  -> Ptr Z -> LAPACKInt
  -> Ptr Z -> LAPACKInt
  -> IO LAPACKInt


foreign import ccall unsafe "lapacke.h LAPACKE_ssyev" c_ssyev
  :: CRepr MatrixLayout
  -> CRepr EigJob
  -> CRepr FortranUpLo
  -> LAPACKInt -> Ptr S -> LAPACKInt
  -> Ptr S -> IO LAPACKInt

foreign import ccall unsafe "lapacke.h LAPACKE_dsyev" c_dsyev
  :: CRepr MatrixLayout
  -> CRepr EigJob
  -> CRepr FortranUpLo
  -> LAPACKInt -> Ptr D -> LAPACKInt
  -> Ptr D -> IO LAPACKInt

foreign import ccall unsafe "lapacke.h LAPACKE_cheev" c_cheev
  :: CRepr MatrixLayout
  -> CRepr EigJob
  -> CRepr FortranUpLo
  -> LAPACKInt -> Ptr C -> LAPACKInt
  -> Ptr (R C) -> IO LAPACKInt

foreign import ccall unsafe "lapacke.h LAPACKE_zheev" c_zheev
  :: CRepr MatrixLayout
  -> CRepr EigJob
  -> CRepr FortranUpLo
  -> LAPACKInt -> Ptr Z -> LAPACKInt
  -> Ptr (R Z) -> IO LAPACKInt


loop0 :: Int -> (Int -> IO ()) -> IO ()
loop0 n action = go 0 where
  go i | i >= n = return ()
       | otherwise = action i >> go (i+1)
