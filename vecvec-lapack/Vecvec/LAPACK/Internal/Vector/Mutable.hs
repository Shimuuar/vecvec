{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost        #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE MultiWayIf                 #-}
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

import Data.Vector.Fixed.Cont       qualified as FC
import Data.Vector.Fixed.Cont       (ContVec(..),Fun(..))
import Data.Vector.Storable         qualified as VS
import Data.Vector.Storable.Mutable qualified as MVS
import Data.Vector.Generic.Mutable  qualified as MVG

import Vecvec.Classes
import Vecvec.Classes.NDMutable
import Vecvec.Classes.Deriving
import Vecvec.LAPACK.FFI             (LAPACKy)
import Vecvec.LAPACK.FFI             qualified as C
import Vecvec.LAPACK.Utils

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

instance (Storable a) => NDMutable MVec a where
  basicUnsafeReadArr  v (ContVec idx)   = idx $ Fun $ MVG.unsafeRead v
  basicUnsafeWriteArr v (ContVec idx) a = idx $ Fun $ \i -> MVG.unsafeWrite v i a
  {-# INLINE basicUnsafeReadArr  #-}
  {-# INLINE basicUnsafeWriteArr #-}

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


type instance Rank (MVec s) = 1

instance Storable a => HasShape (MVec s) a where
  shapeAsCVec = FC.mk1 . MVG.length
  {-# INLINE shapeAsCVec #-}


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
  basicUnsafeMove (MVec (VecRepr len _    _))                _
    | len == 0 = pure ()
  basicUnsafeMove (MVec (VecRepr _   strideTarget fpTarget)) (MVec (VecRepr _ strideSource fpSource))
    | strideTarget == strideSource && fpTarget == fpSource = pure ()
  basicUnsafeMove (MVec (VecRepr len 1            fpTarget)) (MVec (VecRepr _ 1            fpSource))
    = MVG.basicUnsafeMove (MVS.MVector len fpTarget) (MVS.MVector len fpSource)
  basicUnsafeMove (MVec (VecRepr len strideTarget fpTarget)) (MVec (VecRepr _ strideSource fpSource)) = do
    --
    -- In the general case, vectors can intersect. Depending on the interposition of elements,
    -- they should be copied either from the beginning to the end (target further than source)
    -- or from the end to the beginning (source further than target).
    -- In addition, in case of different stride one have to change the direction of copying.
    -- Example:
    -- carrier vector: [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
    -- source  vector: [         3, 4, 5, 6, 7          ] (i.e., offset 3, stride 1)
    -- target  vector: [   1,    3,    5,    7,   9     ] (i.e., offset 1, stride 2)
    -- So if one copy from left to right, the "7" will be erased too early:
    --                 [   3,    4,    5,    6,   6 {- must be "7"! -} ]
    -- And if one copy from right to left, the "3" will be erased:
    --                 [   4,    4,    5,    6,   7     ]
    -- Therefore, in this case one should copy elements from left to right to "5" and then
    -- from right to left to "5":
    --                     <---------- 5 --------->
    --                 [   3,    4,    5,    6,   7     ]
    -- To do this, we find `intersectPosition` index and copy relative to it in different directions.
    --
    let strideDiff = strideSource - strideTarget
    let diff = distancePtr (getPtr fpSource) (getPtr fpTarget)
    let intersectPosition = diff `div` strideDiff
    if | diff /= 0 && strideDiff /= 0 && intersectPosition >= 0 && intersectPosition < len ->
        if fpTarget < fpSource then do
             copy 0         (intersectPosition + 1)
             copy (len - 1) intersectPosition
        else do
             copy intersectPosition       (-1)
             copy (intersectPosition + 1) len
       | fpTarget  < fpSource        -> copy 0         len
       | fpTarget  > fpSource        -> copy (len - 1) (-1)
       -- below fpTarget == fpSource, and note that we can skip copying elements with the same address
       | strideTarget < strideSource -> copy 1         len
       | otherwise                   -> copy (len - 1) 0
   where
    copy from to =
      let delta = if from < to then 1 else (-1)
      in unsafePrimToPrim
         $ unsafeWithForeignPtr fpSource $ \pSource ->
           unsafeWithForeignPtr fpTarget $ \pTarget ->
               let loop i | i == to   = pure ()
                          | otherwise = do pokeElemOff pTarget (i * strideTarget) =<< peekElemOff pSource (i * strideSource)
                                           loop $ i + delta
               in loop from


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
           C.copy (C.toB len) p (C.toB inc) p' (C.toB inc')
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
       C.axpy (C.toB lenX) a pX (C.toB incX)
                             pY (C.toB incY)

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
    C.scal (C.toB lenX) a pX (C.toB incX)



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
       C.dot (C.toB lenX) pX (C.toB incX) pY (C.toB incY)

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
       C.dotc (C.toB lenX) pX (C.toB incX) pY (C.toB incY)

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
         C.nrm2 (C.toB lenX) pX (C.toB incX)
