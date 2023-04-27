{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE ImportQualifiedPost   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
-- |
-- Definition of strided storable vectors
module Vecvec.LAPACK.Internal.Vector
  ( Vec(..)
  ) where

import Control.DeepSeq         (NFData(..), NFData1(..))
import Control.Monad
import Control.Monad.Primitive
import Data.Coerce
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
import Vecvec.LAPACK.Internal.Vector.Mutable

----------------------------------------------------------------
-- Mutable
----------------------------------------------------------------





----------------------------------------------------------------
-- Immutable
----------------------------------------------------------------

-- | Immutable stided storable vector. This means that elements are not
--   necessarily consecutive in memory. This is necessary in order to
--   have zero-copy rows and columns in matrices.
newtype Vec a = Vec (VecRepr a)

type instance VG.Mutable Vec = MVec

instance NFData (Vec a) where
  rnf (Vec (VecRepr _ _ _)) = ()

instance NFData1 Vec where
  liftRnf _ (Vec (VecRepr _ _ _)) = ()

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

instance AsInput s Vec where
  {-# INLINE asInput #-}
  asInput = pure . coerce

instance VS.Storable a => VG.Vector Vec a where
  {-# INLINE basicUnsafeFreeze #-}
  basicUnsafeFreeze = pure . coerce
  {-# INLINE basicUnsafeThaw #-}
  basicUnsafeThaw = pure . coerce
  {-# INLINE basicLength #-}
  basicLength (Vec v) = vecSize v
  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeSlice j m (Vec (VecRepr _ inc fp))
    = Vec (VecRepr m inc (updPtr (`advancePtr` (inc*j)) fp))
  {-# INLINE basicUnsafeIndexM #-}
  basicUnsafeIndexM (Vec (VecRepr _ inc fp)) i
    = return
    . unsafeInlineIO
    $ unsafeWithForeignPtr fp
    $ \p -> peekElemOff p (i * inc)
  {-# INLINE basicUnsafeCopy #-}
  basicUnsafeCopy (MVec (VecRepr len 1 fp)) (Vec (VecRepr _ 1 fq))
    = VG.basicUnsafeCopy (MVS.MVector len fp) (VS.unsafeFromForeignPtr0 fq len)
  basicUnsafeCopy !dst !src = loop 0
    where
      !n = VG.basicLength src
      loop i | i >= n    = return ()
             | otherwise = do x <- liftBox $ VG.basicUnsafeIndexM src i
                              MVG.basicUnsafeWrite dst i x
                              loop (i+1)

----------------------------------------------------------------
-- Classes
----------------------------------------------------------------

-- -- | Type class for
-- class AsInput s v where
--   asInput :: v a -> Vec a

-- instance AsInput s Vec where
--   {-# INLINE asInput #-}
--   asInput = id
-- instance s ~ s => AsInput s (MVec s') where
--   {-# INLINE asInput #-}
--   asInput (MVec len inc fp) = Vec len inc fp
