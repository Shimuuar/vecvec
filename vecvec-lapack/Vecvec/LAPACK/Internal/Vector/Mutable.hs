{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE ImportQualifiedPost   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
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
  , fromMVector
  ) where

import Control.DeepSeq         (NFData(..), NFData1(..))
import Control.Monad
import Control.Monad.ST
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

-- | It's convenient to be able to use both mutable and immutable
--   vector as inputs when them in read-only way.
class AsInput s v where
  asInput :: v a -> ST s (VecRepr a)

instance s ~ s => AsInput s (MVec s') where
  {-# INLINE asInput #-}
  asInput (MVec v) = pure v





-- | Mutable stided storable vector. This means that elements are not
--   necessarily consecutive in memory. This is necessary in order to
--   have zero-copy rows and columns in matrices.
newtype MVec s a = MVec (VecRepr a)


-- | /O(1)/ Convert storable vector to strided storable vector. Vector
--   will use same buffer.
fromMVector :: MVS.MVector s a -> MVec s a
fromMVector (MVS.MVector len buf) = MVec (VecRepr len 1 buf)


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
