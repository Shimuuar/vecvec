{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NegativeLiterals    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE ViewPatterns        #-}
-- |
{-# OPTIONS_GHC -Wno-orphans #-}
module Vecvec.LAPACK.Vector.Mutable
  ( MVec
  , clone
  , scal
  , axpy
  , unsafeBlasAxpy
  , dot
  , nrm2
  ) where

import Control.Monad.Primitive
import Control.Monad.ST
import Control.Monad
import Data.Coerce
import Data.Vector.Generic.Mutable qualified as MVG
import Data.Vector.Generic         qualified as VG

import Vecvec.LAPACK.Internal.Vector
import Vecvec.LAPACK.Internal.Vector.Mutable
import Vecvec.LAPACK.Internal.Compat (unsafeWithForeignPtr)
import Vecvec.LAPACK.FFI             (LAPACKy)
import Vecvec.LAPACK.FFI             qualified as C
import Vecvec.Classes

-- FIXME: What to do with size difference between CInt and Int??

----------------------------------------------------------------
-- Public API
----------------------------------------------------------------

-- | Create copy of a vector
clone :: forall a m inp s. (LAPACKy a, PrimMonad m, PrimState m ~ s, AsInput s inp)
      => inp a
      -> m (MVec s a)
clone vec
  = unsafePrimToPrim
  $ do VecRepr len inc fp <- unsafePrimToPrim $ asInput @s vec
       unsafeWithForeignPtr fp $ \p -> do
         vec@(MVec (VecRepr _ inc' fp')) <- MVG.unsafeNew len
         unsafeWithForeignPtr fp' $ \p' -> do
           C.copy (fromIntegral len) p (fromIntegral inc) p' (fromIntegral inc')
         return $ unsafeCastMVec vec

-- | Compute vector-scalar product in place
--
-- > y := a*x + y
axpy
  :: forall a m inp s. (LAPACKy a, PrimMonad m, PrimState m ~ s, AsInput s inp)
  => a        -- ^ Scalar @a@
  -> inp a    -- ^ Vector @x@
  -> MVec s a -- ^ Vector @y@
  -> m ()
axpy a vecX vecY = primToPrim $ do
  VecRepr lenX _ _ <- asInput @s vecX
  when (lenX /= MVG.length vecY) $ error "Length mismatch"
  unsafeBlasAxpy a vecX vecY

-- | Compute vector-scalar product in place
--
-- > y := a*x + y
unsafeBlasAxpy
  :: forall a m inp s. (LAPACKy a, PrimMonad m, PrimState m ~ s, AsInput s inp)
  => a        -- ^ Scalar @a@
  -> inp a    -- ^ Vector @x@
  -> MVec s a -- ^ Vector @y@
  -> m ()
{-# INLINE unsafeBlasAxpy #-}
unsafeBlasAxpy a vecX (MVec (VecRepr lenY incY fpY)) = unsafePrimToPrim $ do
  VecRepr lenX incX fpX <- unsafePrimToPrim $ asInput @s vecX
  id $ unsafeWithForeignPtr fpX $ \pX ->
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
scal a (MVec (VecRepr lenX incX fpX))
  = unsafePrimToPrim
  $ unsafeWithForeignPtr fpX $ \pX ->
    C.scal (fromIntegral lenX) a pX (fromIntegral incX)

-- | Compute scalar product of two vectors
dot
  :: forall a m inpX inpY s. (LAPACKy a, PrimMonad m, PrimState m ~ s, AsInput s inpX, AsInput s inpY)
  => inpX a -- ^ Vector @x@
  -> inpY a -- ^ Vector @y@
  -> m a
dot vecX vecY = unsafePrimToPrim $ do
  VecRepr lenX incX fpX <- unsafePrimToPrim $ asInput @s vecX
  VecRepr lenY incY fpY <- unsafePrimToPrim $ asInput @s vecY
  when (lenX /= lenY) $ error "Length mismatch"
  id $ unsafeWithForeignPtr fpX $ \pX ->
       unsafeWithForeignPtr fpY $ \pY ->
       C.dot (fromIntegral lenX) pX (fromIntegral incX) pY (fromIntegral incY)

-- | Compute euclidean norm or two vectors
nrm2
  :: forall a m inp s. (LAPACKy a, PrimMonad m, PrimState m ~ s, AsInput s inp)
  => inp a -- ^ Vector @x@
  -> m (R a)
nrm2 vec
  = unsafePrimToPrim
  $ do VecRepr lenX incX fpX <- unsafePrimToPrim $ asInput @s vec
       unsafeWithForeignPtr fpX $ \pX ->
         C.nrm2 (fromIntegral lenX) pX (fromIntegral incX)

----------------------------------------------------------------
-- Instances
----------------------------------------------------------------

instance LAPACKy a => AdditiveSemigroup (Vec a) where
  v .+. u = runST $ do
    mr <- clone v
    axpy 1 u mr
    VG.unsafeFreeze mr

instance LAPACKy a => AdditiveQuasigroup (Vec a) where
  v .-. u = runST $ do
    mr <- clone v
    axpy -1 u mr
    VG.unsafeFreeze mr
  negateV v = runST $ do
    mr <- clone v
    scal -1 mr
    VG.unsafeFreeze mr

instance LAPACKy a => VectorSpace (Vec a) where
  type Scalar (Vec a) = a
  a *. v = runST $ do
    mr <- clone v
    scal a mr
    VG.unsafeFreeze mr
  (.*) = flip (*.)

instance (NormedScalar a, LAPACKy a) => InnerSpace (Vec a) where
  v <.> u = runST $ dot v u
  -- nrm2 return _norm_ of vector not a norm squared. For now we
  -- revert to in-haskell implementation
  {-# INLINE magnitudeSq #-}
  magnitudeSq = coerce (magnitudeSq @(AsVector Vec a))


----------------------------------------------------------------
-- Helpers
----------------------------------------------------------------

unsafeCastMVec :: MVec s a -> MVec s' a
{-# INLINE unsafeCastMVec #-}
unsafeCastMVec = coerce
