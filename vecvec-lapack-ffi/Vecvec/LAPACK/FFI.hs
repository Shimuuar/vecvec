{-# LANGUAGE CApiFFI                  #-}
{-# LANGUAGE CPP                      #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeFamilies             #-}
-- |
module Vecvec.LAPACK.FFI
  ( LAPACKy(..)
  , S
  , D
  , C
  , Z
  ) where

import Data.Complex
import Foreign.C
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal
import Foreign.Storable.Complex ()

-- We want to be able to easily switch how we do foreign calls. ccall
-- is slightly faster while capi allows to check
#define CCALL capi


type S = Float
type D = Double
type C = Complex Float
type Z = Complex Double

type ARR a r = Ptr a -> CInt -> r

-- | LAPACK provides function for working with single and double
--   precision numbers and corresponding complex numbers. We use this
--   type class to provide overloading.
--
--   There're only 4 instances excluding newtypes.
class Storable a => LAPACKy a where
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
-- const MKL_INT n, const void *a, const void *x, const MKL_INT incx, void *y, const MKL_INT incy);

instance LAPACKy Float where
  axpy = s_axpy
  copy = s_copy

instance LAPACKy Double where
  axpy = d_axpy
  copy = d_copy

instance LAPACKy (Complex Float) where
  axpy n a x incX y incY = alloca $ \p_a -> do
    poke p_a a
    c_axpy n p_a x incX y incY
  copy = c_copy

instance LAPACKy (Complex Double) where
  axpy n a x incX y incY = alloca $ \p_a -> do
    poke p_a a
    z_axpy n p_a x incX y incY
  copy = z_copy

----------------------------------------------------------------
-- FFI
----------------------------------------------------------------

foreign import CCALL "cblas.h cblas_saxpy" s_axpy :: CInt -> S     -> ARR S (ARR S (IO ()))
foreign import CCALL "cblas.h cblas_daxpy" d_axpy :: CInt -> D     -> ARR D (ARR D (IO ()))
foreign import CCALL "cblas.h cblas_caxpy" c_axpy :: CInt -> Ptr C -> ARR C (ARR C (IO ()))
foreign import CCALL "cblas.h cblas_zaxpy" z_axpy :: CInt -> Ptr Z -> ARR Z (ARR Z (IO ()))

foreign import CCALL "cblas.h cblas_scopy" s_copy :: CInt -> ARR S (ARR S (IO ()))
foreign import CCALL "cblas.h cblas_dcopy" d_copy :: CInt -> ARR D (ARR D (IO ()))
foreign import CCALL "cblas.h cblas_ccopy" c_copy :: CInt -> ARR C (ARR C (IO ()))
foreign import CCALL "cblas.h cblas_zcopy" z_copy :: CInt -> ARR Z (ARR Z (IO ()))
