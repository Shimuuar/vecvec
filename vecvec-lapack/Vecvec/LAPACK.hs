{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE CPP                        #-}
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
module Vecvec.LAPACK
  ( -- * Vector data types
    Vec
  , MVec
  , fromMVector
  ) where

import Control.Monad
import Control.Monad.Primitive
import Data.Primitive.Ptr      hiding (advancePtr)
import Data.Word
import Foreign.Storable
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Marshal.Array
import GHC.ForeignPtr   ( ForeignPtr(..) )
#if MIN_VERSION_base(4,15,0)
import GHC.ForeignPtr       ( unsafeWithForeignPtr )
#endif

import Data.Vector.Storable         qualified as VS
import Data.Vector.Storable.Mutable qualified as MVS
import Data.Vector.Generic          qualified as VG
import Data.Vector.Generic.Mutable  qualified as MVG
import Data.Vector.Fusion.Util      (liftBox)


-- | Newtype wrapper for ordinary storable vectors. We need it in
--   order to be able to use optimized BLAS routines for defining
--   instances.
data Vec a = Vec
  !Int -- Size of vector
  !Int -- Stride of vector
  {-# UNPACK #-} !(ForeignPtr a)
  -- deriving newtype (Show, Eq, Ord)

data MVec s a = MVec
  !Int -- Size of vector
  !Int -- Stride of vector
  {-# UNPACK #-} !(ForeignPtr a)


fromMVector :: MVS.MVector s a -> MVec s a
fromMVector (MVS.MVector len buf) = MVec len 1 buf


type instance VG.Mutable Vec = MVec




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



getPtr :: ForeignPtr a -> Ptr a
{-# INLINE getPtr #-}
getPtr (ForeignPtr addr _) = Ptr addr

updPtr :: (Ptr a -> Ptr a) -> ForeignPtr a -> ForeignPtr a
{-# INLINE updPtr #-}
updPtr f (ForeignPtr p c) = case f (Ptr p) of { Ptr q -> ForeignPtr q c }


#if !MIN_VERSION_base(4,15,0)
-- | A compatibility wrapper for 'GHC.ForeignPtr.unsafeWithForeignPtr' provided
-- by GHC 9.0.1 and later.
--
-- Only to be used when the continuation is known not to
-- unconditionally diverge lest unsoundness can result.
unsafeWithForeignPtr :: ForeignPtr a -> (Ptr a -> IO b) -> IO b
unsafeWithForeignPtr = withForeignPtr
#endif
