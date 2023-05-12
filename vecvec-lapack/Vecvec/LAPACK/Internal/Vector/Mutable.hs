{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost        #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
-- |
-- Internal module with definition of strided storable vector.  It
-- exposes constuctors for mutable vector. They are unsafe. Use at you
-- own risk.
--
-- This module considered to bepart of public API.
module Vecvec.LAPACK.Internal.Vector.Mutable
  ( -- * Representation
    VecRepr(..)
  , AsInput(..)
    -- * Mutable vector
  , MVec(..)
  , Strided(..)
  , LAPACKy
  , fromMVector
    -- * Mutable BLAS wrappersx
    -- ** Checked varians
  , clone
  , blasAxpy
  , blasScal
  , blasDotu
  , blasDotc
  , blasNrm2
    -- ** Unchecked variants
  , unsafeBlasAxpy
  , unsafeBlasDotu
  , unsafeBlasDotc
  ) where

import Control.Monad
import Control.Monad.ST
import Control.Monad.Primitive
import Data.Primitive.Ptr      hiding (advancePtr)
import Data.Word
import Data.Coerce
import Foreign.Storable
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Marshal.Array

import Data.Vector.Storable         qualified as VS
import Data.Vector.Storable.Mutable qualified as MVS
import Data.Vector.Generic.Mutable  qualified as MVG

import Vecvec.Classes
import Vecvec.Classes.Slice
import Vecvec.LAPACK.FFI             (LAPACKy)
import Vecvec.LAPACK.FFI             qualified as C

import Vecvec.LAPACK.Internal.Compat


----------------------------------------------------------------
--
----------------------------------------------------------------

-- | Representation of mutable or immutable vector. Actual mutable
--   immutable vectors are newtype wrappers over this data type.
data VecRepr a = VecRepr
  { vecSize :: !Int
    -- ^ Size if vector
  , vecStride :: Int
    -- ^ Stride of vector
  , vecBuffer :: {-# UNPACK #-} !(ForeignPtr a)
    -- ^ Underlying buffer
  }
  deriving stock Show

instance (Slice1D i, Storable a) => Slice (Strided i) (VecRepr a) where
  {-# INLINE sliceMaybe #-}
  sliceMaybe (Strided idx stride) VecRepr{..} = do
    guard (stride > 0)
    (i,len) <- computeSlice1D vecSize idx
    Just $ VecRepr { vecSize   = adjustLen len
                   , vecStride = vecStride * stride
                   , vecBuffer = updPtr (`advancePtr` (vecStride*i)) vecBuffer
                   }
    where
      adjustLen n = case n `quotRem` stride of
        (k,0) -> k
        (k,_) -> k + 1



-- | Convenience type class which allows to use several data types as
--   read-only input for in-place operations with mutable vectors.
class AsInput s v where
  -- | Expected to be /O(1)/ and very cheap.
  asInput :: v a -> ST s (VecRepr a)

instance s ~ s => AsInput s (MVec s') where
  {-# INLINE asInput #-}
  asInput (MVec v) = pure v

instance s ~ s => AsInput s (MVS.MVector s') where
  {-# INLINE asInput #-}
  asInput (MVS.MVector n buf) = pure (VecRepr n 1 buf)

-- -- FIXME: We cannot define instance since we need Storable a for that
-- instance AsInput s VS.Vector where
--   {-# INLINE asInput #-}
--   asInput = asInput <=< VS.unsafeThaw




-- | Mutable stided storable vector. This means that elements are not
--   necessarily consecutive in memory. This is necessary in order to
--   have zero-copy rows and columns in matrices.
--
--   This data type is instance of 'MVG.Vector' and all function from
--   @vector@ works with it.
newtype MVec s a = MVec (VecRepr a)


-- | /O(1)/ Convert storable vector to strided storable vector. Vector
--   will use same buffer.
fromMVector :: MVS.MVector s a -> MVec s a
fromMVector (MVS.MVector len buf) = MVec (VecRepr len 1 buf)


-- | Data type which is used for slicing which changes stride of a
--   vector.
data Strided a = Strided a !Int

instance (i ~ Int, Storable a) => Slice (i, Length) (MVec s a) where
  {-# INLINE sliceMaybe #-}
  sliceMaybe = implSliceMVector

instance (i ~ Int, Storable a) => Slice (i, End) (MVec s a) where
  {-# INLINE sliceMaybe #-}
  sliceMaybe = implSliceMVector

instance (i ~ Int, Storable a) => Slice (Range i) (MVec s a) where
  {-# INLINE sliceMaybe #-}
  sliceMaybe = implSliceMVector

deriving newtype instance (Slice1D i, Storable a) => Slice (Strided i) (MVec s a)


instance VS.Storable a => MVG.MVector MVec a where
  {-# INLINE basicLength #-}
  basicLength (MVec v) = vecSize v
  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeSlice j m (MVec (VecRepr _ inc fp))
    = MVec (VecRepr m inc (updPtr (`advancePtr` (inc*j)) fp))
  {-# INLINE basicOverlaps #-}
  basicOverlaps (MVec (VecRepr lenA incA fpA)) (MVec (VecRepr lenB incB fpB))
    = between pA pB (pB `advancePtr` (lenB*incB)) || between pB pA (pA `advancePtr` (lenA*incA))
    where
      between x y z = x >= y && x < z
      pA = getPtr fpA
      pB = getPtr fpB
  {-# INLINE basicUnsafeNew #-}
  basicUnsafeNew = fmap fromMVector . MVG.basicUnsafeNew
  {-# INLINE basicInitialize #-}
  basicInitialize (MVec (VecRepr len 1   fp))
    = MVG.basicInitialize (MVS.MVector len fp)
  -- FIXME: Can we improve? Specialize for different sizes???
  basicInitialize (MVec (VecRepr len inc fp))
    = unsafePrimToPrim
    $ unsafeWithForeignPtr fp $ \p ->
      let loop i | i >= len  = return ()
                 | otherwise = do setPtr (castPtr $ p `advancePtr` (i*inc) :: Ptr Word8)
                                         (sizeOf (undefined :: a))
                                         0
                                  loop (i+1)
      in loop 0
  {-# INLINE basicUnsafeReplicate #-}
  basicUnsafeReplicate n a = fromMVector <$> MVG.basicUnsafeReplicate n a
  {-# INLINE basicUnsafeRead #-}
  basicUnsafeRead (MVec (VecRepr _ inc fp)) i
    = unsafePrimToPrim
    $ unsafeWithForeignPtr fp (\p -> peekElemOff p (i*inc))
  {-# INLINE basicUnsafeWrite #-}
  basicUnsafeWrite (MVec (VecRepr _ inc fp)) i a
    = unsafePrimToPrim
    $ unsafeWithForeignPtr fp (\p -> pokeElemOff p (i*inc) a)
  {-# INLINE basicSet #-}
  basicSet (MVec (VecRepr len inc fp)) a
    = unsafePrimToPrim
    $ unsafeWithForeignPtr fp
    $ \p -> forM_ [0 .. len-1] (\i -> pokeElemOff p (i*inc) a)
  {-# INLINE basicUnsafeCopy #-}
  basicUnsafeCopy (MVec (VecRepr len 1 fpA)) (MVec (VecRepr _ 1 fpB))
    = MVG.basicUnsafeCopy (MVS.MVector len fpA) (MVS.MVector len fpB)
  -- FIXME: We just fall back to elementwise copy via Storable. Can we do better?
  basicUnsafeCopy (MVec (VecRepr len incA fpA)) (MVec (VecRepr _ incB fpB))
    = unsafePrimToPrim
    $ unsafeWithForeignPtr fpA $ \pA ->
      unsafeWithForeignPtr fpB $ \pB ->
      let loop i | i >= len  = pure ()
                 | otherwise = do pokeElemOff pA (i * incA) =<< peekElemOff pB (i * incB)
                                  loop (i + 1)
      in loop 0
  {-# INLINE basicUnsafeMove #-}
  basicUnsafeMove (MVec (VecRepr len 1 fpA)) (MVec (VecRepr _ 1 fpB))
    = MVG.basicUnsafeMove (MVS.MVector len fpA) (MVS.MVector len fpB)
  -- FIXME: We don't handle possible overlap
  basicUnsafeMove (MVec (VecRepr len incA fpA)) (MVec (VecRepr _ incB fpB))
    = unsafePrimToPrim
    $ unsafeWithForeignPtr fpA $ \pA ->
      unsafeWithForeignPtr fpB $ \pB ->
      let loop i | i >= len  = pure ()
                 | otherwise = do pokeElemOff pA (i * incA) =<< peekElemOff pB (i * incB)
                                  loop (i + 1)
      in loop 0


----------------------------------------------------------------
-- BLAS overloads
----------------------------------------------------------------

-- | Create copy of a vector
clone :: forall a m inp s. (LAPACKy a, PrimMonad m, PrimState m ~ s, AsInput s inp)
      => inp a
      -> m (MVec s a)
clone vecIn
  = unsafePrimToPrim
  $ do VecRepr len inc fp <- unsafePrimToPrim $ asInput @s vecIn
       unsafeWithForeignPtr fp $ \p -> do
         vecOut@(MVec (VecRepr _ inc' fp')) <- MVG.unsafeNew len
         unsafeWithForeignPtr fp' $ \p' -> do
           C.copy (fromIntegral len) p (fromIntegral inc) p' (fromIntegral inc')
         return $ coerce vecOut


-- | Compute vector-scalar product in place
--
-- > y := a*x + y
blasAxpy
  :: forall a m inp s. (LAPACKy a, PrimMonad m, PrimState m ~ s, AsInput s inp)
  => a        -- ^ Scalar @a@
  -> inp a    -- ^ Vector @x@
  -> MVec s a -- ^ Vector @y@
  -> m ()
blasAxpy a vecX vecY = primToPrim $ do
  VecRepr lenX _ _ <- asInput @s vecX
  when (lenX /= MVG.length vecY) $ error "Length mismatch"
  unsafeBlasAxpy a vecX vecY

-- | See 'blasAxpy'.
unsafeBlasAxpy
  :: forall a m inp s. (LAPACKy a, PrimMonad m, PrimState m ~ s, AsInput s inp)
  => a        -- ^ Scalar @a@
  -> inp a    -- ^ Vector @x@
  -> MVec s a -- ^ Vector @y@
  -> m ()
{-# INLINE unsafeBlasAxpy #-}
unsafeBlasAxpy a vecX (MVec (VecRepr _ incY fpY)) = unsafePrimToPrim $ do
  VecRepr lenX incX fpX <- unsafePrimToPrim $ asInput @s vecX
  id $ unsafeWithForeignPtr fpX $ \pX ->
       unsafeWithForeignPtr fpY $ \pY ->
       C.axpy (fromIntegral lenX) a pX (fromIntegral incX)
                                    pY (fromIntegral incY)

-- | Multiply vector by scalar in place
--
-- > x := a*x
blasScal
  :: (LAPACKy a, PrimMonad m, PrimState m ~ s)
  => a        -- ^ Scalar @a@
  -> MVec s a -- ^ Vector @x@
  -> m ()
blasScal a (MVec (VecRepr lenX incX fpX))
  = unsafePrimToPrim
  $ unsafeWithForeignPtr fpX $ \pX ->
    C.scal (fromIntegral lenX) a pX (fromIntegral incX)



-- | Compute scalar product of two vectors without complex
--   conjugation. See 'blasDotc' for variant with conjugation.
--
-- \[ \operatorname{dotu}(\vec x,\vec y) = \sum_i x_i y_i \]
blasDotu
  :: forall a m inpX inpY s. (LAPACKy a, PrimMonad m, PrimState m ~ s, AsInput s inpX, AsInput s inpY)
  => inpX a -- ^ Vector @x@
  -> inpY a -- ^ Vector @y@
  -> m a
blasDotu vecX vecY = primToPrim $ do
  VecRepr lenX _ _ <- asInput vecX
  VecRepr lenY _ _ <- asInput vecY
  when (lenX /= lenY) $ error "Length mismatch"
  unsafeBlasDotu vecX vecY

-- | See 'blasDot'.
unsafeBlasDotu
  :: forall a m inpX inpY s. (LAPACKy a, PrimMonad m, PrimState m ~ s, AsInput s inpX, AsInput s inpY)
  => inpX a -- ^ Vector @x@
  -> inpY a -- ^ Vector @y@
  -> m a
unsafeBlasDotu vecX vecY = unsafePrimToPrim $ do
  VecRepr lenX incX fpX <- unsafePrimToPrim $ asInput @s vecX
  VecRepr _    incY fpY <- unsafePrimToPrim $ asInput @s vecY
  id $ unsafeWithForeignPtr fpX $ \pX ->
       unsafeWithForeignPtr fpY $ \pY ->
       C.dot (fromIntegral lenX) pX (fromIntegral incX) pY (fromIntegral incY)

-- | Compute scalar product of two vectors. First vector is complex
--   conjugated. See 'blasDotc' for variant without conjugation.
--
-- \[ \operatorname{dotc}(\vec x,\vec y) = \sum_i \bar{x_i} y_i \]
blasDotc
  :: forall a m inpX inpY s. (LAPACKy a, PrimMonad m, PrimState m ~ s, AsInput s inpX, AsInput s inpY)
  => inpX a -- ^ Vector @x@
  -> inpY a -- ^ Vector @y@
  -> m a
blasDotc vecX vecY = primToPrim $ do
  VecRepr lenX _ _ <- asInput vecX
  VecRepr lenY _ _ <- asInput vecY
  when (lenX /= lenY) $ error "Length mismatch"
  unsafeBlasDotc vecX vecY

-- | See 'blasDotc'.
unsafeBlasDotc
  :: forall a m inpX inpY s. (LAPACKy a, PrimMonad m, PrimState m ~ s, AsInput s inpX, AsInput s inpY)
  => inpX a -- ^ Vector @x@
  -> inpY a -- ^ Vector @y@
  -> m a
unsafeBlasDotc vecX vecY = unsafePrimToPrim $ do
  VecRepr lenX incX fpX <- unsafePrimToPrim $ asInput @s vecX
  VecRepr _    incY fpY <- unsafePrimToPrim $ asInput @s vecY
  id $ unsafeWithForeignPtr fpX $ \pX ->
       unsafeWithForeignPtr fpY $ \pY ->
       C.dotc (fromIntegral lenX) pX (fromIntegral incX) pY (fromIntegral incY)

-- | Compute euclidean norm or vector:
--
-- \[ \operatorname{nrm2}(\vec{x}) = \sqrt{\sum_i x_i^2} \]
blasNrm2
  :: forall a m inp s. (LAPACKy a, PrimMonad m, PrimState m ~ s, AsInput s inp)
  => inp a -- ^ Vector @x@
  -> m (R a)
blasNrm2 vec
  = unsafePrimToPrim
  $ do VecRepr lenX incX fpX <- unsafePrimToPrim $ asInput @s vec
       unsafeWithForeignPtr fpX $ \pX ->
         C.nrm2 (fromIntegral lenX) pX (fromIntegral incX)
