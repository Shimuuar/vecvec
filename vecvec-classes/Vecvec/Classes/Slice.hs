{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE ImportQualifiedPost   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
-- | Type classes for working with N-dimensional possibly sparse
-- arrays. All indices are assumed to be 0-based. No assumptions about
-- data layout in memory are made.
module Vecvec.Classes.Slice
  ( -- * Indexing
    Shape(..)
  , shape
  , nCols
  , nRows
  , NDim
  , IsShape(..)
  , Index(..)
  , (!?)
  , (!)
    -- * Slicing
  , Slice(..)
  , slice
  , Slice1D(..)
    -- ** Slice parameters
  , Range(..)
  , End(..)
  , Length(..)
    -- * Default implementations
  , implSliceVector
  , implSliceMVector
  ) where

import Data.Kind
import Data.Vector                   qualified as V
import Data.Vector.Unboxed           qualified as VU
import Data.Vector.Storable          qualified as VS
import Data.Vector.Primitive         qualified as VP
import Data.Vector.Generic           qualified as VG
import Data.Vector.Mutable           qualified as MV
import Data.Vector.Fixed             qualified as F
import Data.Vector.Fixed.Cont        qualified as FC
import Data.Vector.Unboxed.Mutable   qualified as MVU
import Data.Vector.Storable.Mutable  qualified as MVS
import Data.Vector.Primitive.Mutable qualified as MVP
import Data.Vector.Generic.Mutable   qualified as MVG

import GHC.Generics (Generic)
import GHC.TypeLits


----------------------------------------------------------------
-- Indexing
----------------------------------------------------------------

-- | Number of dimensions in array. 1 for vectors, 2 for matrices, etc. By
--   convention matrices use @(n_rows,n_columns)@.
type family NDim (v :: Type -> Type)  :: Nat

type instance NDim V.Vector  = 1
type instance NDim VU.Vector = 1
type instance NDim VS.Vector = 1
type instance NDim VP.Vector = 1
type instance NDim (MV.MVector  s) = 1
type instance NDim (MVU.MVector s) = 1
type instance NDim (MVS.MVector s) = 1
type instance NDim (MVP.MVector s) = 1



-- | Data type which could represent shape of N-dimensional array
--   where N is compile-time constant. That is size of each of its
--   dimensions.
--
--   This type class exists in order to allow use of plain @Int@ and
--   vectors of ints (such as tuples, etc) at the same time.
class IsShape shape (n :: Nat) where
  shapeToCVec   :: shape -> F.ContVec n Int
  shapeFromCVec :: F.ContVec n Int -> shape

instance n ~ 1 => IsShape Int n where
  shapeToCVec   = FC.mk1
  shapeFromCVec = FC.head
  {-# INLINE shapeToCVec   #-}
  {-# INLINE shapeFromCVec #-}

instance (n ~ F.Dim v, F.Vector v Int, a ~ Int) => IsShape (v a) n where
  shapeToCVec   = F.cvec
  shapeFromCVec = F.vector
  {-# INLINE shapeToCVec   #-}
  {-# INLINE shapeFromCVec #-}


-- | Possibly sparse N-dimensional arrays. Its return type is very
-- polymorphic
class Shape v a where
  shapeCVec :: v a -> FC.ContVec (NDim v) Int

-- | Get shape of an array.
shape :: (IsShape shape (NDim v), Shape v a) => v a -> shape
shape = shapeFromCVec . shapeCVec
{-# INLINE shape #-}

nCols :: (NDim v ~ 2, Shape v a) => v a -> Int
nCols v = FC.runContVec (FC.Fun $ \_ n -> n) (shapeCVec v)
{-# INLINE nCols #-}

nRows :: (NDim v ~ 2, Shape v a) => v a -> Int
nRows v = FC.runContVec (FC.Fun $ \n _ -> n) (shapeCVec v)
{-# INLINE nRows #-}

instance Shape V.Vector a where
  shapeCVec = FC.mk1 . V.length
  {-# INLINE shapeCVec #-}
instance VS.Storable a => Shape VS.Vector a where
  shapeCVec = FC.mk1 . VS.length
  {-# INLINE shapeCVec #-}
instance VP.Prim a => Shape VP.Vector a where
  shapeCVec = FC.mk1 . VP.length
  {-# INLINE shapeCVec #-}
instance VU.Unbox a => Shape VU.Vector a where
  shapeCVec = FC.mk1 . VU.length
  {-# INLINE shapeCVec #-}

instance Shape (MV.MVector s) a where
  shapeCVec = FC.mk1 . MV.length
  {-# INLINE shapeCVec #-}
instance VS.Storable a => Shape (MVS.MVector s) a where
  shapeCVec = FC.mk1 . MVS.length
  {-# INLINE shapeCVec #-}
instance VP.Prim a => Shape (MVP.MVector s) a where
  shapeCVec = FC.mk1 . MVP.length
  {-# INLINE shapeCVec #-}
instance VU.Unbox a => Shape (MVU.MVector s) a where
  shapeCVec = FC.mk1 . MVU.length
  {-# INLINE shapeCVec #-}



class Shape v a => Index v a where
  indexCVec :: v a -> F.ContVec (NDim v) Int -> Maybe a

(!?) :: (Index v a, IsShape idx (NDim v)) => v a -> idx -> Maybe a
v !? idx = indexCVec v (shapeToCVec idx)
{-# INLINE (!?) #-}

(!) :: (Index v a, IsShape idx (NDim v)) => v a -> idx -> a
v ! idx = case v !? idx of
  Just a  -> a
  Nothing -> error "Index out of bounds"
{-# INLINE (!) #-}

instance Index V.Vector a where
  {-# INLINE indexCVec #-}
  indexCVec v idx = v VG.!? FC.runContVec (FC.Fun id) idx

instance VS.Storable a => Index VS.Vector a where
  {-# INLINE indexCVec #-}
  indexCVec v idx = v VG.!? FC.runContVec (FC.Fun id) idx

instance VP.Prim a => Index VP.Vector a where
  {-# INLINE indexCVec #-}
  indexCVec v idx = v VG.!? FC.runContVec (FC.Fun id) idx

instance VU.Unbox a => Index VU.Vector a where
  {-# INLINE indexCVec #-}
  indexCVec v idx = v VG.!? FC.runContVec (FC.Fun id) idx




----------------------------------------------------------------
-- Slicing
----------------------------------------------------------------

-- | Very generic type class for slicing vectors, matrices etc. It's
--   expected that slice is done in /O(1)/. There are many possible
--   parameterizations of slice and different data structures require
--   different parameterizations. This type class is used to abstract
--   over this.
class Slice i v where
  -- | Expected /O(1)/. Return slice of vector @v@ or @Nothing@ if
  --   required slice is not valid.
  sliceMaybe :: i -> v -> Maybe v

slice :: Slice i v => i -> v -> v
{-# INLINE slice #-}
slice idx v = case sliceMaybe idx v of
  Just v' -> v'
  Nothing -> error "Invalid slice"


-- | Type class for data types describing slice of dense one dimension
--   vector with 0-based indices
class Slice1D idx where
  -- | Compute offset and size of slice. Function should return
  --   @Nothing@ if slice is not valid
  computeSlice1D :: Int             -- ^ Size of vector
                 -> idx             -- ^ Slice description
                 -> Maybe (Int,Int)

instance (i ~ Int) => Slice1D (i, Length) where
  {-# INLINE computeSlice1D #-}
  computeSlice1D len (i, Length sz)
    -- FIXME: overflows and stuff
    | i  < 0       = Nothing
    | sz < 0       = Nothing
    | len < i + sz = Nothing
    | otherwise    = Just (i,sz)

instance (i ~ Int) => Slice1D (i, End) where
  {-# INLINE computeSlice1D #-}
  computeSlice1D len (i, _)
    | i < 0     = Nothing
    | i > len   = Nothing
    | otherwise = Just (i, len - i)

instance (i ~ Int) => Slice1D (Range i) where
  {-# INLINE computeSlice1D #-}
  computeSlice1D len (i0 :.. j0)
    | i   < 0      = Nothing
    | sz  < 0      = Just (i, 0)
    | len < i + sz = Nothing
    | otherwise    = Just (i, sz)
    where
      mirror k | k >= 0    = k
               | otherwise = len + k
      i  = mirror i0
      j  = mirror j0
      sz = j - i





-- | Semiopen range @a :.. b@
data Range a = a :.. a
  deriving stock (Show,Eq,Ord,Generic)

-- | Used for creating slice to end of vector.
data End = End
  deriving stock (Show,Eq,Ord,Generic)

-- | Use length of vector for a slice.
newtype Length = Length { unLength :: Int }
  deriving stock (Show,Eq,Ord,Generic)




instance (Slice1D idx) => Slice idx (V.Vector a) where
  {-# INLINE sliceMaybe #-}
  sliceMaybe = implSliceVector

instance (Slice1D idx, VP.Prim a) => Slice idx (VP.Vector a) where
  {-# INLINE sliceMaybe #-}
  sliceMaybe = implSliceVector

instance (Slice1D idx, VS.Storable a) => Slice idx (VS.Vector a) where
  {-# INLINE sliceMaybe #-}
  sliceMaybe = implSliceVector

instance (Slice1D idx, VU.Unbox a) => Slice idx (VU.Vector a) where
  {-# INLINE sliceMaybe #-}
  sliceMaybe = implSliceVector


instance (Slice1D idx) => Slice idx (MV.MVector s a) where
  {-# INLINE sliceMaybe #-}
  sliceMaybe = implSliceMVector

instance (Slice1D idx, VP.Prim a) => Slice idx (MVP.MVector s a) where
  {-# INLINE sliceMaybe #-}
  sliceMaybe = implSliceMVector

instance (Slice1D idx, VS.Storable a) => Slice idx (MVS.MVector s a) where
  {-# INLINE sliceMaybe #-}
  sliceMaybe = implSliceMVector

instance (Slice1D idx, VU.Unbox a) => Slice idx (MVU.MVector s a) where
  {-# INLINE sliceMaybe #-}
  sliceMaybe = implSliceMVector



----------------------------------------------------------------
-- Default implementations
----------------------------------------------------------------

-- | Default implementation of 'sliceMaybe' in terms of 'Slice1D'.
implSliceVector :: (Slice1D idx, VG.Vector v a) => idx -> v a -> Maybe (v a)
{-# INLINE implSliceVector #-}
implSliceVector idx vec = do
  (i,n) <- computeSlice1D (VG.length vec) idx
  Just $ VG.slice i n vec

-- | Default implementation of 'sliceMaybe' in terms of 'Slice1D'.
implSliceMVector :: (Slice1D idx, MVG.MVector v a) => idx -> v s a -> Maybe (v s a)
{-# INLINE implSliceMVector #-}
implSliceMVector idx vec = do
  (i,n) <- computeSlice1D (MVG.length vec) idx
  Just $ MVG.slice i n vec
