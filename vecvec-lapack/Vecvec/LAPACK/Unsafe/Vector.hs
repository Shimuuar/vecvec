{-# LANGUAGE TypeFamilies #-}
-- |
-- Definition of strided storable vectors
module Vecvec.LAPACK.Unsafe.Vector
  ( Vec(..)
  , LAPACKy
  ) where

import Control.DeepSeq         (NFData(..), NFData1(..))
import Control.Monad.ST
import Control.Monad.Primitive
import Data.Coerce
import Foreign.Storable
import Foreign.Marshal.Array
import Text.Read

import Data.Vector.Fixed.Cont       qualified as FC
import Data.Vector.Fixed.Cont       (ContVec(..),Fun(..))
import Data.Vector.Storable         qualified as VS
import Data.Vector.Storable.Mutable qualified as MVS
import Data.Vector.Generic          qualified as VG
import Data.Vector.Generic.Mutable  qualified as MVG
import Data.Vector.Fusion.Bundle    qualified as Bundle
import Data.Vector.Fusion.Util      (liftBox)

import Vecvec.Classes
import Vecvec.Classes.NDArray
import Vecvec.Classes.Deriving
import Vecvec.Classes.Containers
import Vecvec.LAPACK.Unsafe.Compat
import Vecvec.LAPACK.Unsafe.Vector.Mutable (LAPACKy, MVec(..), VecRepr(..), InVector(..)
                                           ,blasDotc, blasScal, blasAxpy, clone)


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

instance InVector s Vec where
  {-# INLINE vectorRepr #-}
  vectorRepr = pure . coerce

instance (i ~ Int, Storable a) => Slice (i, Length) (Vec a) where
  {-# INLINE sliceMaybe #-}
  sliceMaybe = implSliceVector
instance (i ~ Int, Storable a) => Slice (i, End) (Vec a) where
  {-# INLINE sliceMaybe #-}
  sliceMaybe = implSliceVector
instance (Storable a) => Slice (Int, Int) (Vec a) where
  {-# INLINE sliceMaybe #-}
  sliceMaybe = implSliceVector
instance (i ~ Int, Storable a) => Slice (Range i) (Vec a) where
  {-# INLINE sliceMaybe #-}
  sliceMaybe = implSliceVector

deriving newtype instance (Slice1D i, Storable a) => Slice (Strided i) (Vec a)

type instance Rank Vec = FC.N1

instance Storable a => HasShape Vec a where
  shapeAsCVec = FC.mk1 . VG.length
  {-# INLINE shapeAsCVec #-}

instance Storable a => NDArray Vec a where
  basicUnsafeIndex v (ContVec cont) = VG.unsafeIndex v (cont (Fun id))
  {-# INLINE basicUnsafeIndex #-}


instance VS.Storable a => VG.Vector Vec a where
  {-# INLINE basicUnsafeFreeze #-}
  basicUnsafeFreeze = pure . coerce
  {-# INLINE basicUnsafeThaw #-}
  basicUnsafeThaw = pure . coerce
  {-# INLINE basicLength #-}
  basicLength (Vec v) = v.vecSize
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

instance Constrained Vec where type ElemConstraint Vec = Storable
instance CFunctor Vec where
  cmap = VG.map
  {-# INLINE cmap #-}


----------------------------------------------------------------
-- BLAS wrappers
----------------------------------------------------------------

instance LAPACKy a => AdditiveSemigroup (Vec a) where
  v .+. u = runST $ do
    mr <- clone v
    blasAxpy 1 u mr
    VG.unsafeFreeze mr

instance LAPACKy a => AdditiveQuasigroup (Vec a) where
  v .-. u = runST $ do
    mr <- clone v
    blasAxpy -1 u mr
    VG.unsafeFreeze mr
  negateV v = runST $ do
    mr <- clone v
    blasScal -1 mr
    VG.unsafeFreeze mr

instance LAPACKy a => VectorSpace (Vec a) where
  type Scalar (Vec a) = a
  a *. v = runST $ do
    mr <- clone v
    blasScal a mr
    VG.unsafeFreeze mr
  (.*) = flip (*.)

instance (NormedScalar a, LAPACKy a) => InnerSpace (Vec a) where
  v <.> u = runST $ blasDotc v u
  -- nrm2 return _norm_ of vector not a norm squared. For now we
  -- revert to in-haskell implementation
  {-# INLINE magnitudeSq #-}
  magnitudeSq = coerce (magnitudeSq @(AsVector Vec a))
