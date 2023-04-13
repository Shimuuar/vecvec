{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NegativeLiterals    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE ViewPatterns        #-}
-- |
{-# OPTIONS_GHC -Wno-orphans #-}
module Vecvec.LAPACK.Vector.Mutable
  ( clone
  , scal
  , axpy
  , dot
  , nrm2
  ) where

import Control.Monad.Primitive
import Control.Monad.ST
import Data.Coerce
import Data.Vector.Generic.Mutable qualified as MVG
import Data.Vector.Generic         qualified as VG

import Vecvec.LAPACK.Internal.Vector
import Vecvec.LAPACK.Internal.Compat (unsafeWithForeignPtr)
import Vecvec.LAPACK.FFI             (LAPACKy)
import Vecvec.LAPACK.FFI             qualified as C
import Vecvec.Classes

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

-- | Compute scalar product of two vectors
dot
  :: (LAPACKy a, PrimMonad m, PrimState m ~ s)
  => MVec s a -- ^ Vector @x@
  -> MVec s a -- ^ Vector @y@
  -> m a
dot (MVec lenX incX fpX) (MVec lenY incY fpY)
  | lenX /= lenY = error "Length mismatch"
  | otherwise    = unsafePrimToPrim
    $ unsafeWithForeignPtr fpX $ \pX ->
      unsafeWithForeignPtr fpY $ \pY ->
      C.dot (fromIntegral lenX) pX (fromIntegral incX) pY (fromIntegral incY)

-- | Compute euclidean norm or two vectors
nrm2
  :: (LAPACKy a, PrimMonad m, PrimState m ~ s)
  => MVec s a -- ^ Vector @x@
  -> m (R a)
nrm2 (MVec lenX incX fpX)
  = unsafePrimToPrim
  $ unsafeWithForeignPtr fpX $ \pX ->
    C.nrm2 (fromIntegral lenX) pX (fromIntegral incX)

----------------------------------------------------------------
-- Instances
----------------------------------------------------------------

instance LAPACKy a => AdditiveSemigroup (Vec a) where
  v .+. u = runST $ do
    mv <- VG.unsafeThaw v
    mu <- VG.unsafeThaw u
    mr <- clone mv
    axpy 1 mu mr
    VG.unsafeFreeze mr

instance LAPACKy a => AdditiveQuasigroup (Vec a) where
  v .-. u = runST $ do
    mv <- VG.unsafeThaw v
    mu <- VG.unsafeThaw u
    mr <- clone mv
    axpy -1 mu mr
    VG.unsafeFreeze mr
  negateV v = runST $ do
    mv <- VG.unsafeThaw v
    mr <- clone mv
    scal -1 mr
    VG.unsafeFreeze mr

instance LAPACKy a => VectorSpace (Vec a) where
  type Scalar (Vec a) = a
  a *. v = runST $ do
    mv <- VG.unsafeThaw v
    mr <- clone mv
    scal a mr
    VG.unsafeFreeze mr
  (.*) = flip (*.)

instance (NormedScalar a, LAPACKy a) => InnerSpace (Vec a) where
  v <.> u = runST $ do
    mv <- VG.unsafeThaw v
    mu <- VG.unsafeThaw u
    dot mv mu
  magnitudeSq v = runST $ do
    mv <- VG.unsafeThaw v
    nrm2 mv


----------------------------------------------------------------
-- Helpers
----------------------------------------------------------------

unsafeCastMVec :: MVec s a -> MVec s' a
{-# INLINE unsafeCastMVec #-}
unsafeCastMVec = coerce
