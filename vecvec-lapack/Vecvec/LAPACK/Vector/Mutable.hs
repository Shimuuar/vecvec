{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeFamilies        #-}
-- |
module Vecvec.LAPACK.Vector.Mutable
  ( clone
  , scal
  , axpy
  ) where

import Control.Monad.Primitive
import Data.Coerce
import Data.Vector.Generic.Mutable qualified as MVG
-- import Data.Vector.Generic         qualified as VG

import Vecvec.LAPACK.Internal.Vector
import Vecvec.LAPACK.Internal.Compat (unsafeWithForeignPtr)
import Vecvec.LAPACK.FFI             (LAPACKy)
import Vecvec.LAPACK.FFI             qualified as C

-- FIXME: What to do with size difference between CInt and Int??

----------------------------------------------------------------
-- Public API
----------------------------------------------------------------

-- | Create copy of a vector
clone :: (LAPACKy a, PrimMonad m, PrimState m ~ s)
      => MVec s a
      -> m (MVec s a)
clone (MVec len inc fp)
  = unsafePrimToPrim
  $ unsafeWithForeignPtr fp $ \p -> do
      vec@(MVec _ inc' fp') <- MVG.unsafeNew len
      unsafeWithForeignPtr fp' $ \p' -> do
        C.copy (fromIntegral len) p (fromIntegral inc) p' (fromIntegral inc')
      return $ unsafeCastMVec vec

-- | Compute vector-scalar product in place
--
-- > y := a*x + y
axpy
  :: (LAPACKy a, PrimMonad m, PrimState m ~ s)
  => a        -- ^ Scalar @a@
  -> MVec s a -- ^ Vector @x@
  -> MVec s a -- ^ Vector @y@
  -> m ()
axpy a (MVec lenX incX fpX) (MVec lenY incY fpY)
  | lenX /= lenY = error "Length mismatch"
  | otherwise    = unsafePrimToPrim
      $ unsafeWithForeignPtr fpX $ \pX ->
        unsafeWithForeignPtr fpY $ \pY ->
          C.axpy (fromIntegral lenX) a pX (fromIntegral incX)
                                       pY (fromIntegral incY)

-- | Multiply vector by scalar in place
--
-- > x := a*x
scal
  :: (LAPACKy a, PrimMonad m, PrimState m ~ s)
  => a        -- ^ Scalar @a@
  -> MVec s a -- ^ Vector @x@
  -> m ()
scal a (MVec lenX incX fpX)
  = unsafePrimToPrim
  $ unsafeWithForeignPtr fpX $ \pX ->
    C.scal (fromIntegral lenX) a pX (fromIntegral incX)


----------------------------------------------------------------
-- Helpers
----------------------------------------------------------------

unsafeCastMVec :: MVec s a -> MVec s' a
{-# INLINE unsafeCastMVec #-}
unsafeCastMVec = coerce
