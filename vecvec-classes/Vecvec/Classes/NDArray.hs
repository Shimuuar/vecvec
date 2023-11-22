{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE ImportQualifiedPost   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ViewPatterns          #-}
-- | Type classes for working with N-dimensional possibly sparse
-- arrays. All indices are assumed to be 0-based. No assumptions about
-- data layout in memory are made.
module Vecvec.Classes.NDArray
  ( -- * Indexing
    NDim
  , HasShape(..)
  , shape
  , nCols
  , nRows
  , NDArray(..)
  , index
  , indexMaybe
  , (!)
  , (!?)
  , unsafeIndex
  , IsShape(..)
  , pattern N1
  , pattern N2
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

import Control.Monad
import Data.Kind
import Data.Vector                   qualified as V
import Data.Vector.Mutable           qualified as MV
import Data.Vector.Generic           qualified as VG
import Data.Vector.Generic.Mutable   qualified as MVG
import Data.Vector.Unboxed           qualified as VU
import Data.Vector.Unboxed.Mutable   qualified as MVU
import Data.Vector.Storable          qualified as VS
import Data.Vector.Storable.Mutable  qualified as MVS
import Data.Vector.Primitive         qualified as VP
import Data.Vector.Primitive.Mutable qualified as MVP
import Data.Vector.Fixed             qualified as F
import Data.Vector.Fixed.Cont        qualified as FC
import Data.Vector.Fixed.Cont        (ContVec(..),runContVec,Fun(..))
import GHC.Generics (Generic)
import GHC.TypeLits

import Vecvec.Classes


----------------------------------------------------------------
-- Generic type class for working with shapes
----------------------------------------------------------------

-- | Data type which could represent shape of N-dimensional array.
--   It's product of N @Int@s. There are two instances: one for Int
--   which allows bare Int as shape and index for arrays and N-element
--   arrays of @Int@s which include tuples.
class IsShape shape (n :: Nat) where
  shapeToCVec   :: shape -> ContVec n Int
  shapeFromCVec :: ContVec n Int -> shape

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


----------------------------------------------------------------
-- ND-arrays
----------------------------------------------------------------

-- | Dimension of N-dimensional array: 1 for vectors, 2 for matrices, etc
type family NDim (arr :: Type -> Type) :: Nat

-- | Type class for N-dimensional arrays. It's superclass for
--   'NDArray' since both immutable and mutable arrays could be
--   instance.
class F.Arity (NDim arr) => HasShape arr a where
  -- | Return shape of N-dimensional array as a tuple of @Int@s.
  shapeAsCVec :: arr a -> ContVec (NDim arr) Int

-- | Get shape of an array.
shape :: (IsShape shape (NDim arr), HasShape arr a) => arr a -> shape
shape = shapeFromCVec . shapeAsCVec
{-# INLINE shape #-}

-- | Number of columns of two dimensional array (@k@, if size is @(n,k)@)
nCols :: (NDim arr ~ 2, HasShape arr a) => arr a -> Int
nCols v = runContVec (Fun $ \_ n -> n) (shapeAsCVec v)
{-# INLINE nCols #-}

-- | Number of rows of two dimensional array (@n@, if size is @(n,k)@)
nRows :: (NDim arr ~ 2, HasShape arr a) => arr a -> Int
nRows v = runContVec (Fun $ \n _ -> n) (shapeAsCVec v)
{-# INLINE nRows #-}

pattern N1 :: IsShape shape 1 => Int -> shape
pattern N1 i <- (runContVec (Fun id) . shapeToCVec @_ @1 -> i)
  where
    N1 i = shapeFromCVec (FC.mk1 i)
{-# INLINE   N1 #-}
{-# COMPLETE N1 #-}

pattern N2 :: IsShape shape 2 => Int -> Int -> shape
pattern N2 i j <- (runContVec (Fun (\i j -> (i,j))) . shapeToCVec @_ @2 -> (i,j))
  where
    N2 i j = shapeFromCVec (FC.mk2 i j)
{-# INLINE   N2 #-}
{-# COMPLETE N2 #-}


----------------------------------------------------------------
-- Shape of an array
----------------------------------------------------------------

-- | Type class for N-dimensional arrays where each dimension is
--   zero-indexed. There's no restriction on actual representation
--   which could be dense or sparse.
--
--   Method of this type class accept and return shape and index
--   'ContVec' it's convenient and generic representation whic GHC can
--   optimize well.
class HasShape arr a => NDArray arr a where
  -- | /O(1)/ Use 'index' or '!' instead.
  indexCVec :: arr a -> ContVec (NDim arr) Int -> a
  {-# INLINE indexCVec #-}
  indexCVec arr idx
    -- FIXME: Does GHC optimize this well? We call idx twice.
    | ok        = unsafeIndexCVec arr idx
    | otherwise = error "Index out of range"
    where ok = FC.and
             $ FC.zipWith (\sz i -> i >= 0 && i < sz) (shapeAsCVec arr) idx
  -- | /O(1)/ Use 'indexMaybe' instead.
  indexCVecMaybe :: arr a -> ContVec (NDim arr) Int -> Maybe a
  {-# INLINE indexCVecMaybe #-}
  indexCVecMaybe arr idx = do
    -- FIXME: Does GHC optimize this well? We call idx twice.
    FC.zipWithM_ (\sz i -> guard $ i >= 0 && i < sz) (shapeAsCVec arr) idx
    Just $ unsafeIndexCVec arr idx

  -- | /O(1)/ Use 'unsafeIndex' instead.
  unsafeIndexCVec :: arr a -> ContVec (NDim arr) Int -> a


-- | /O(1)/ Return element of an array at given index. Will throw
index :: (NDArray arr a, IsShape idx (NDim arr)) => arr a -> idx -> a
index arr idx = indexCVec arr (shapeToCVec idx)
{-# INLINE index #-}

-- | /O(1)/ Return element of an array at given index. Will throw
indexMaybe :: (NDArray arr a, IsShape idx (NDim arr)) => arr a -> idx -> Maybe a
indexMaybe arr idx = indexCVecMaybe arr (shapeToCVec idx)
{-# INLINE indexMaybe #-}

-- | /O(1)/ Return element of an array at given index. Will throw
(!) :: (NDArray arr a, IsShape idx (NDim arr)) => arr a -> idx -> a
(!) = index
{-# INLINE (!) #-}

-- | /O(1)/ Return element of an array at given index. Will throw
(!?) :: (NDArray arr a, IsShape idx (NDim arr)) => arr a -> idx -> Maybe a
(!?) = indexMaybe
{-# INLINE (!?) #-}

-- | /O(1)/ Return element of an array at given index. Will throw
unsafeIndex :: (NDArray arr a, IsShape idx (NDim arr)) => arr a -> idx -> a
unsafeIndex arr idx = unsafeIndexCVec arr (shapeToCVec idx)
{-# INLINE unsafeIndex #-}



type instance NDim V.Vector  = 1
type instance NDim VS.Vector = 1
type instance NDim VU.Vector = 1
type instance NDim VP.Vector = 1

type instance NDim (MV.MVector  s) = 1
type instance NDim (MVS.MVector s) = 1
type instance NDim (MVU.MVector s) = 1
type instance NDim (MVP.MVector s) = 1

instance HasShape V.Vector a where
  shapeAsCVec = FC.mk1 . VG.length
  {-# INLINE shapeAsCVec #-}
instance VS.Storable a => HasShape VS.Vector a where
  shapeAsCVec = FC.mk1 . VG.length
  {-# INLINE shapeAsCVec #-}
instance VU.Unbox a => HasShape VU.Vector a where
  shapeAsCVec = FC.mk1 . VG.length
  {-# INLINE shapeAsCVec #-}
instance VP.Prim a => HasShape VP.Vector a where
  shapeAsCVec = FC.mk1 . VG.length
  {-# INLINE shapeAsCVec #-}

instance HasShape (MV.MVector s) a where
  shapeAsCVec = FC.mk1 . MVG.length
  {-# INLINE shapeAsCVec #-}
instance VS.Storable a => HasShape (MVS.MVector s) a where
  shapeAsCVec = FC.mk1 . MVG.length
  {-# INLINE shapeAsCVec #-}
instance VU.Unbox a => HasShape (MVU.MVector s) a where
  shapeAsCVec = FC.mk1 . MVG.length
  {-# INLINE shapeAsCVec #-}
instance VP.Prim a => HasShape (MVP.MVector s) a where
  shapeAsCVec = FC.mk1 . MVG.length
  {-# INLINE shapeAsCVec #-}

instance NDArray V.Vector a where
  indexCVec       v (ContVec cont) = v VG.!             (cont (Fun id))
  indexCVecMaybe  v (ContVec cont) = v VG.!?            (cont (Fun id))
  unsafeIndexCVec v (ContVec cont) = v `VG.unsafeIndex` (cont (Fun id))
  {-# INLINE indexCVec       #-}
  {-# INLINE indexCVecMaybe  #-}
  {-# INLINE unsafeIndexCVec #-}
instance VS.Storable a => NDArray VS.Vector a where
  indexCVec       v (ContVec cont) = v VG.!             (cont (Fun id))
  indexCVecMaybe  v (ContVec cont) = v VG.!?            (cont (Fun id))
  unsafeIndexCVec v (ContVec cont) = v `VG.unsafeIndex` (cont (Fun id))
  {-# INLINE indexCVec       #-}
  {-# INLINE indexCVecMaybe  #-}
  {-# INLINE unsafeIndexCVec #-}
instance VU.Unbox a => NDArray VU.Vector a where
  indexCVec       v (ContVec cont) = v VG.!             (cont (Fun id))
  indexCVecMaybe  v (ContVec cont) = v VG.!?            (cont (Fun id))
  unsafeIndexCVec v (ContVec cont) = v `VG.unsafeIndex` (cont (Fun id))
  {-# INLINE indexCVec       #-}
  {-# INLINE indexCVecMaybe  #-}
  {-# INLINE unsafeIndexCVec #-}
instance VP.Prim a => NDArray VP.Vector a where
  indexCVec       v (ContVec cont) = v VG.!             (cont (Fun id))
  indexCVecMaybe  v (ContVec cont) = v VG.!?            (cont (Fun id))
  unsafeIndexCVec v (ContVec cont) = v `VG.unsafeIndex` (cont (Fun id))
  {-# INLINE indexCVec       #-}
  {-# INLINE indexCVecMaybe  #-}
  {-# INLINE unsafeIndexCVec #-}

type instance NDim (Tr   v) = NDim v
type instance NDim (Conj v) = NDim v


instance (HasShape arr a, NDim arr ~ 2) => HasShape (Tr arr) a where
  shapeAsCVec (Tr arr) = swapFC2 $ shapeAsCVec arr
  {-# INLINE shapeAsCVec #-}
instance (HasShape arr a, NDim arr ~ 2) => HasShape (Conj arr) a where
  shapeAsCVec (Conj arr) = swapFC2 $ shapeAsCVec arr
  {-# INLINE shapeAsCVec #-}

instance (NDArray arr a, NDim arr ~ 2) => NDArray (Tr arr) a where
  indexCVec       (Tr arr) = indexCVec       arr . swapFC2
  indexCVecMaybe  (Tr arr) = indexCVecMaybe  arr . swapFC2
  unsafeIndexCVec (Tr arr) = unsafeIndexCVec arr . swapFC2
  {-# INLINE indexCVec       #-}
  {-# INLINE indexCVecMaybe  #-}
  {-# INLINE unsafeIndexCVec #-}
instance (NDArray arr a, NDim arr ~ 2) => NDArray (Conj arr) a where
  indexCVec       (Conj arr) = indexCVec       arr . swapFC2
  indexCVecMaybe  (Conj arr) = indexCVecMaybe  arr . swapFC2
  unsafeIndexCVec (Conj arr) = unsafeIndexCVec arr . swapFC2
  {-# INLINE indexCVec       #-}
  {-# INLINE indexCVecMaybe  #-}
  {-# INLINE unsafeIndexCVec #-}

swapFC2 :: ContVec 2 a -> ContVec 2 a
swapFC2 (ContVec cont) = ContVec $ \(Fun f) -> cont (Fun $ flip f)
{-# INLINE swapFC2 #-}



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
