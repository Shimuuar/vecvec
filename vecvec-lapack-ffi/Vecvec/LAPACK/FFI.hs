{-# LANGUAGE CApiFFI                    #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE ForeignFunctionInterface   #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE PatternSynonyms            #-}
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
  , MatrixOrder(..)
  , MatrixTranspose(..)
  ) where

import Data.Complex
import Foreign.C
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal
import Foreign.Storable.Complex ()
import Vecvec.Classes           (NormedScalar(..))

-- We want to be able to easily switch how we do foreign calls. ccall
-- is slightly faster while capi allows to check
#define CCALL capi


type S = Float
type D = Double
type C = Complex Float
type Z = Complex Double

type ARR a r = Ptr a -> CInt -> r

----------------------------------------------------------------
-- Enumerations
----------------------------------------------------------------

class CEnum a where
  toCEnum :: a -> CInt

data MatrixOrder
  = RowMajor
  | ColMajor
  deriving stock (Show,Eq)

instance CEnum MatrixOrder where
  {-# INLINE toCEnum #-}
  toCEnum = \case
    RowMajor -> c_BLAS_ROW_MAJOR
    ColMajor -> c_BLAS_COL_MAJOR

foreign import capi "cblas.h value CblasRowMajor" c_BLAS_ROW_MAJOR :: CInt
foreign import capi "cblas.h value CblasColMajor" c_BLAS_COL_MAJOR :: CInt

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

foreign import capi "cblas.h value CblasNoTrans"     c_NO_TRANS      :: CInt
foreign import capi "cblas.h value CblasTrans"       c_TRANS         :: CInt
foreign import capi "cblas.h value CblasConjTrans"   c_CONJ_TRANS    :: CInt
foreign import capi "cblas.h value CblasConjNoTrans" c_CONJ_NO_TRANS :: CInt


----------------------------------------------------------------
-- Overload of BLAS functions
----------------------------------------------------------------

-- | LAPACK provides function for working with single and double
--   precision numbers and corresponding complex numbers. We use this
--   type class to provide overloading.
--
--   There're only 4 instances excluding newtypes.
class (Num a, Storable a) => LAPACKy a where
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

  nrm2 :: CInt
       -> Ptr a -> CInt
       -> IO (R a)

instance LAPACKy Float where
  axpy = s_axpy
  copy = s_copy
  scal = s_scal
  dot  = s_dot
  nrm2 = s_nrm2

instance LAPACKy Double where
  axpy = d_axpy
  copy = d_copy
  scal = d_scal
  dot  = d_dot
  nrm2 = d_nrm2

instance LAPACKy (Complex Float) where
  copy = c_copy
  nrm2 = c_nrm2
  --
  axpy n a x incX y incY = alloca $ \p_a -> do
    poke p_a a
    c_axpy n p_a x incX y incY
  --
  scal n a x incX = alloca $ \p_a -> do
    poke p_a a
    c_scal n p_a x incX
  --
  dot n x incX y incY = alloca $ \p_a -> do
    c_dot n x incX y incY p_a >> peek p_a

instance LAPACKy (Complex Double) where
  copy = z_copy
  nrm2 = z_nrm2
  --
  axpy n a x incX y incY = alloca $ \p_a -> do
    poke p_a a
    z_axpy n p_a x incX y incY
  --
  scal n a x incX = alloca $ \p_a -> do
    poke p_a a
    z_scal n p_a x incX
  --
  dot n x incX y incY = alloca $ \p_a -> do
    z_dot n x incX y incY p_a >> peek p_a



----------------------------------------------------------------
-- FFI
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

foreign import CCALL unsafe "cblas.h cblas_sdot"      s_dot :: CInt -> ARR S (ARR S (IO S))
foreign import CCALL unsafe "cblas.h cblas_ddot"      d_dot :: CInt -> ARR D (ARR D (IO D))
foreign import CCALL unsafe "cblas.h cblas_cdotc_sub" c_dot :: CInt -> ARR C (ARR C (Ptr C -> IO ()))
foreign import CCALL unsafe "cblas.h cblas_zdotc_sub" z_dot :: CInt -> ARR Z (ARR Z (Ptr Z -> IO ()))

foreign import CCALL unsafe "cblas.h cblas_snrm2"  s_nrm2 :: CInt -> ARR S (IO S)
foreign import CCALL unsafe "cblas.h cblas_dnrm2"  d_nrm2 :: CInt -> ARR D (IO D)
foreign import CCALL unsafe "cblas.h cblas_scnrm2" c_nrm2 :: CInt -> ARR C (IO S)
foreign import CCALL unsafe "cblas.h cblas_dznrm2" z_nrm2 :: CInt -> ARR Z (IO D)
