{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE ImportQualifiedPost   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
-- |
-- Definition of strided storable vectors
module Vecvec.LAPACK.Internal.Vector
  ( MVec(..)
  , fromMVector
  , Vec(..)
  ) where

import Control.DeepSeq         (NFData(..), NFData1(..))
import Control.Monad
import Control.Monad.Primitive
import Data.Primitive.Ptr      hiding (advancePtr)
import Data.Word
import Foreign.Storable
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Marshal.Array
import Text.Read

import Data.Vector.Storable         qualified as VS
import Data.Vector.Storable.Mutable qualified as MVS
import Data.Vector.Generic          qualified as VG
import Data.Vector.Generic.Mutable  qualified as MVG
import Data.Vector.Fusion.Bundle    qualified as Bundle
import Data.Vector.Fusion.Util      (liftBox)

import Vecvec.LAPACK.Internal.Compat

----------------------------------------------------------------
-- Mutable
----------------------------------------------------------------

-- | Mutable stided storable vector. This means that elements are not
--   necessarily consecutive in memory. This is necessary in order to
--   have zero-copy rows and columns in matrices.
data MVec s a = MVec
  !Int -- Size of vector
  !Int -- Stride of vector
  {-# UNPACK #-} !(ForeignPtr a)

-- | /O(1)/ Convert storable vector to strided storable vector
fromMVector :: MVS.MVector s a -> MVec s a
fromMVector (MVS.MVector len buf) = MVec len 1 buf


instance VS.Storable a => MVG.MVector MVec a where
  {-# INLINE basicLength #-}
  basicLength (MVec n _ _) = n
  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeSlice j m (MVec _ inc fp)
    = MVec m inc (updPtr (`advancePtr` (inc*j)) fp)
  {-# INLINE basicOverlaps #-}
  basicOverlaps (MVec lenA incA fpA) (MVec lenB incB fpB)
    = between pA pB (pB `advancePtr` (lenB*incB)) || between pB pA (pA `advancePtr` (lenA*incA))
    where
      between x y z = x >= y && x < z
      pA = getPtr fpA
      pB = getPtr fpB
  {-# INLINE basicUnsafeNew #-}
  basicUnsafeNew = fmap fromMVector . MVG.basicUnsafeNew
  {-# INLINE basicInitialize #-}
  basicInitialize (MVec len 1   fp) = MVG.basicInitialize (MVS.MVector len fp)
  -- FIXME: Can we improve? Specialize for different sizes???
  basicInitialize (MVec len inc fp)
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
  basicUnsafeRead (MVec _ inc fp) i
    = unsafePrimToPrim
    $ unsafeWithForeignPtr fp (\p -> peekElemOff p (i*inc))
  {-# INLINE basicUnsafeWrite #-}
  basicUnsafeWrite (MVec _ inc fp) i a
    = unsafePrimToPrim
    $ unsafeWithForeignPtr fp (\p -> pokeElemOff p (i*inc) a)
  {-# INLINE basicSet #-}
  basicSet (MVec len inc fp) a
    = unsafePrimToPrim
    $ unsafeWithForeignPtr fp
    $ \p -> forM_ [0 .. len-1] (\i -> pokeElemOff p (i*inc) a)
  {-# INLINE basicUnsafeCopy #-}
  basicUnsafeCopy (MVec len 1    fpA) (MVec _ 1    fpB)
    = MVG.basicUnsafeCopy (MVS.MVector len fpA) (MVS.MVector len fpB)
  -- FIXME: We just fall back to elementwise copy via Storable. Can we do better?
  basicUnsafeCopy (MVec len incA fpA) (MVec _ incB fpB)
    = unsafePrimToPrim
    $ unsafeWithForeignPtr fpA $ \pA ->
      unsafeWithForeignPtr fpB $ \pB ->
      let loop i | i >= len  = pure ()
                 | otherwise = do pokeElemOff pA (i * incA) =<< peekElemOff pB (i * incB)
                                  loop (i + 1)
      in loop 0
  {-# INLINE basicUnsafeMove #-}
  basicUnsafeMove (MVec len 1    fpA) (MVec _ 1    fpB)
    = MVG.basicUnsafeMove (MVS.MVector len fpA) (MVS.MVector len fpB)
  -- FIXME: We don't handle possible overlap
  basicUnsafeMove (MVec len incA fpA) (MVec _ incB fpB)
    = unsafePrimToPrim
    $ unsafeWithForeignPtr fpA $ \pA ->
      unsafeWithForeignPtr fpB $ \pB ->
      let loop i | i >= len  = pure ()
                 | otherwise = do pokeElemOff pA (i * incA) =<< peekElemOff pB (i * incB)
                                  loop (i + 1)
      in loop 0


----------------------------------------------------------------
-- Immutable
----------------------------------------------------------------

-- | Immutable stided storable vector. This means that elements are not
--   necessarily consecutive in memory. This is necessary in order to
--   have zero-copy rows and columns in matrices.
data Vec a = Vec
  !Int -- Size of vector
  !Int -- Stride of vector
  {-# UNPACK #-} !(ForeignPtr a)

type instance VG.Mutable Vec = MVec


instance NFData (Vec a) where
  rnf (Vec _ _ _) = ()

instance NFData1 Vec where
  liftRnf _ (Vec _ _ _) = ()

instance (Show a, Storable a) => Show (Vec a) where
  showsPrec = VG.showsPrec

instance (Read a, Storable a) => Read (Vec a) where
  readPrec = VG.readPrec
  readListPrec = readListPrecDefault

instance (Storable a, Eq a) => Eq (Vec a) where
  {-# INLINE (==) #-}
  xs == ys = Bundle.eq (VG.stream xs) (VG.stream ys)

instance (Storable a, Ord a) => Ord (Vec a) where
  {-# INLINE compare #-}
  compare xs ys = Bundle.cmp (VG.stream xs) (VG.stream ys)
  {-# INLINE (<) #-}
  xs < ys = Bundle.cmp (VG.stream xs) (VG.stream ys) == LT
  {-# INLINE (<=) #-}
  xs <= ys = Bundle.cmp (VG.stream xs) (VG.stream ys) /= GT
  {-# INLINE (>) #-}
  xs > ys = Bundle.cmp (VG.stream xs) (VG.stream ys) == GT
  {-# INLINE (>=) #-}
  xs >= ys = Bundle.cmp (VG.stream xs) (VG.stream ys) /= LT

instance VS.Storable a => VG.Vector Vec a where
  {-# INLINE basicUnsafeFreeze #-}
  basicUnsafeFreeze (MVec len inc fp) = pure $ Vec len inc fp
  {-# INLINE basicUnsafeThaw #-}
  basicUnsafeThaw (Vec len inc fp) = pure $ MVec len inc fp
  {-# INLINE basicLength #-}
  basicLength (Vec n _ _) = n
  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeSlice j m (Vec _ inc fp) = Vec m inc (updPtr (`advancePtr` (inc*j)) fp)
  {-# INLINE basicUnsafeIndexM #-}
  basicUnsafeIndexM (Vec _ inc fp) i
    = return
    . unsafeInlineIO
    $ unsafeWithForeignPtr fp
    $ \p -> peekElemOff p (i * inc)
  {-# INLINE basicUnsafeCopy #-}
  basicUnsafeCopy (MVec len 1 fp) (Vec _ 1 fq)
    = VG.basicUnsafeCopy (MVS.MVector len fp) (VS.unsafeFromForeignPtr0 fq len)
  basicUnsafeCopy !dst !src = loop 0
    where
      !n = VG.basicLength src
      loop i | i >= n    = return ()
             | otherwise = do x <- liftBox $ VG.basicUnsafeIndexM src i
                              MVG.basicUnsafeWrite dst i x
                              loop (i+1)
