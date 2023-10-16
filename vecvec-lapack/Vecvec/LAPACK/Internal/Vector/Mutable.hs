{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost        #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
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
import Vecvec.Classes.NDArray
import Vecvec.Classes.Via
import Vecvec.LAPACK.FFI             (LAPACKy)
import Vecvec.LAPACK.FFI             qualified as C
import Vecvec.LAPACK.Utils

import Vecvec.LAPACK.Internal.Compat
import Debug.Trace


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

deriving via AsMVector MVec s a instance Storable a => HasShape (MVec s a)

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
    $ \p -> loop0_ len (\i -> pokeElemOff p (i*inc) a)
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
  --              target                        source
  -- TODO "inc" --> stride
  --      "A" -> target
  --      "B" -> source
  basicUnsafeMove (MVec (VecRepr len _    _))   _                           | len == 0                   = pure ()
  basicUnsafeMove (MVec (VecRepr _   incA fpA)) (MVec (VecRepr _ incB fpB)) | incA == incB && fpA == fpB = pure ()
  basicUnsafeMove (MVec (VecRepr len 1    fpA)) (MVec (VecRepr _ 1 fpB))
    = MVG.basicUnsafeMove (MVS.MVector len fpA) (MVS.MVector len fpB)
  basicUnsafeMove (MVec (VecRepr len incTarget fpTarget)) (MVec (VecRepr _ incSource fpSource)) = do
    -- TODO 1: use memmove/memcopy for incA, incB = 1
    -- TODO 2: what to do if **same** start, but different incA/incB?
    -- TODO 3: overlapping when different fpA,fpB; incA,incB -- but elements still overlapping sometimes.
    --
    -- TODO use basicOverlaps
    --
    case incSource - incTarget of
      0       -> if fpTarget < fpSource then move 0 len else move (len - 1) (-1)
      incDiff ->
        let diff = distancePtr (getPtr fpSource) (getPtr fpTarget)
            fpTargetEnd = updPtr (\p -> advancePtr p (len*incTarget)) fpTarget
        in
        -- случай наложения...
        case {-trace ("DIFF = " ++ show diff ++ ", incDiff = " ++ show incDiff) $ -} divMod diff incDiff of -- TODO заменить quotRem на просто quot
          (intersectPosition, r)
            | intersectPosition >= 0 && intersectPosition < len && (intersectPosition > 0 || r /= 0) -> do
                -- first, from `intersectPosition` to left, then from `intersectPosition` to right
                if fpTarget < fpSource then do
                    move 0         (intersectPosition + 1)
                    move (len - 1) intersectPosition
                else do
                    move intersectPosition (-1)
                    move (intersectPosition + 1) len
                pure ()
          _
            -- TODO
            | fpTarget == fpSource -> if incTarget < incSource then move 0 len else move (len - 1) (-1)
            | fpTarget < fpSource -> move 0         len
            | otherwise           -> move (len - 1) (-1)
   where
    move from to =
      let delta = if from < to then 1 else (-1)
      in unsafePrimToPrim
         $ unsafeWithForeignPtr fpSource $ \pSource ->
           unsafeWithForeignPtr fpTarget $ \pTarget ->
               let loop i | i == to = pure ()
                          | otherwise = do pokeElemOff pTarget (i * incTarget) =<< peekElemOff pSource (i * incSource)
                                           loop $ i + delta
               in loop from
  

    {-
    --
    --
    --
    -- simple solution for overlapping: just check pointer positions.
    -- this is not effective, but just to check conception
    = unsafePrimToPrim
    $ unsafeWithForeignPtr fpA $ \pA ->
      unsafeWithForeignPtr fpB $ \pB ->
      if pA < pB then
          let loop i | i >= len  = pure ()
                     | otherwise = do pokeElemOff pA (i * incA) =<< peekElemOff pB (i * incB)
                                      loop (i + 1)
          in loop 0
      else
          let loop i | i <= 0  = pure ()
                     | otherwise = do pokeElemOff pA (i * incA) =<< peekElemOff pB (i * incB)
                                      loop (i - 1)
          in loop len
          -}


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
