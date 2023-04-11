{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost        #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
-- |
module Vecvec.LAPACK where

import Data.Coerce
import Data.Vector.Storable qualified as VS
import Data.Vector.Storable.Mutable qualified as MVS
import Data.Vector.Generic  qualified as VG
import Data.Vector.Generic.Mutable qualified as MVG

-- | Newtype wrapper for ordinary storable vectors. We need it in
--   order to be able to use optimized BLAS routines for defining
--   instances.
newtype Vec a = Vec { getVector :: VS.Vector a }
  deriving newtype (Show, Eq, Ord)

newtype MVec s a = MVec { getMVec :: MVS.MVector s a }


type instance VG.Mutable Vec = MVec

instance VS.Storable a => MVG.MVector MVec a where
  basicLength          = coerce (MVG.basicLength          @MVS.MVector)
  basicUnsafeSlice     = coerce (MVG.basicUnsafeSlice     @MVS.MVector)
  basicOverlaps        = coerce (MVG.basicOverlaps        @MVS.MVector)
  basicUnsafeNew       = coerce (MVG.basicUnsafeNew       @MVS.MVector)
  basicInitialize      = coerce (MVG.basicInitialize      @MVS.MVector)
  basicUnsafeReplicate = coerce (MVG.basicUnsafeReplicate @MVS.MVector)
  basicUnsafeRead      = coerce (MVG.basicUnsafeRead      @MVS.MVector)
  basicUnsafeWrite     = coerce (MVG.basicUnsafeWrite     @MVS.MVector)
  basicClear           = coerce (MVG.basicClear           @MVS.MVector)
  basicSet             = coerce (MVG.basicSet             @MVS.MVector)
  basicUnsafeCopy      = coerce (MVG.basicUnsafeCopy      @MVS.MVector)
  basicUnsafeMove      = coerce (MVG.basicUnsafeMove      @MVS.MVector)
  basicUnsafeGrow      = coerce (MVG.basicUnsafeGrow      @MVS.MVector)
  {-# INLINE basicLength          #-}
  {-# INLINE basicUnsafeSlice     #-}
  {-# INLINE basicOverlaps        #-}
  {-# INLINE basicUnsafeNew       #-}
  {-# INLINE basicInitialize      #-}
  {-# INLINE basicUnsafeReplicate #-}
  {-# INLINE basicUnsafeRead      #-}
  {-# INLINE basicUnsafeWrite     #-}
  {-# INLINE basicClear           #-}
  {-# INLINE basicSet             #-}
  {-# INLINE basicUnsafeCopy      #-}
  {-# INLINE basicUnsafeMove      #-}
  {-# INLINE basicUnsafeGrow      #-}

instance VS.Storable a => VG.Vector Vec a where
  basicUnsafeFreeze = coerce (VG.basicUnsafeFreeze @VS.Vector)
  basicUnsafeThaw   = coerce (VG.basicUnsafeThaw   @VS.Vector)
  basicLength       = coerce (VG.basicLength       @VS.Vector)
  basicUnsafeSlice  = coerce (VG.basicUnsafeSlice  @VS.Vector)
  basicUnsafeIndexM = coerce (VG.basicUnsafeIndexM @VS.Vector)
  basicUnsafeCopy   = coerce (VG.basicUnsafeCopy   @VS.Vector)
  elemseq (Vec v)   = VG.elemseq v
  {-# INLINE basicUnsafeFreeze #-}
  {-# INLINE basicUnsafeThaw   #-}
  {-# INLINE basicLength       #-}
  {-# INLINE basicUnsafeSlice  #-}
  {-# INLINE basicUnsafeIndexM #-}
  {-# INLINE basicUnsafeCopy   #-}
  {-# INLINE elemseq           #-}
