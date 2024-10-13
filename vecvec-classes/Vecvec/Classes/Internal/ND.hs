{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE DerivingStrategies     #-}
{-# LANGUAGE DerivingVia            #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ImportQualifiedPost    #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PatternSynonyms        #-}
{-# LANGUAGE QuantifiedConstraints  #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE ViewPatterns           #-}
module Vecvec.Classes.Internal.ND 
  ( -- * ND-arrays
    -- ** Array shape
    Rank
  , HasShape(..)
  , shape
  , nCols
  , nRows
  , IsShape(..)
  , inBounds
  , inBoundsCVec
  , pattern N1
  , pattern N2
    -- ** Slicing
  , Slice(..)
  , slice
  , Slice1D(..)
  , Range(..)
  , End(..)
  , Length(..)
    -- ** Mutable arrays
  , NDMutable(..)
  , unsafeReadArr
  , readArr
  , unsafeWriteArr
  , writeArr
    -- ** Immutable arrays
  , NDArray(..)
  , unsafeIndex
  , index
  , indexMaybe
  , (!)
  , (!?)   
    -- * Default implementations
  , inRange
  , implSliceVector
  , implSliceMVector
  ) where

import Control.Monad.ST
import Control.Monad.Primitive
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
import Data.Vector.Fixed.Cont        (Arity,ContVec(..),runContVec,Fun(..))
import GHC.TypeLits
import GHC.Generics

import Vecvec.Classes

----------------------------------------------------------------
-- Generic type class for working with shapes
----------------------------------------------------------------

-- | Data type which could represent shape of N-dimensional array.
--   It's product of N @Int@s. There are two instances: one for Int
--   which allows bare Int as shape and index for arrays and N-element
--   arrays of @Int@s which include tuples.
class IsShape shape (n :: Nat) | shape -> n where
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

-- | Patterns for matching on shape of arrays of rank-1.
pattern N1 :: IsShape shape 1 => Int -> shape
pattern N1 i <- (runContVec (Fun id) . shapeToCVec @_ @1 -> i)
  where
    N1 i = shapeFromCVec (FC.mk1 i)
{-# INLINE   N1 #-}
{-# COMPLETE N1 #-}

-- | Patterns for matching on shape of arrays of rank-2.
pattern N2 :: IsShape shape 2 => Int -> Int -> shape
pattern N2 i j <- (runContVec (Fun (\i j -> (i,j))) . shapeToCVec @_ @2 -> (i,j))
  where
    N2 i j = shapeFromCVec (FC.mk2 i j)
{-# INLINE   N2 #-}
{-# COMPLETE N2 #-}


-- | Check that index is in bounds for a N-dimensional array.
inBounds
  :: (IsShape idx n, Arity n)
  => idx -- ^ 0-based index
  -> idx -- ^ Size of array
  -> Bool
{-# INLINE inBounds #-}
inBounds idx size
  = FC.and
  $ FC.zipWith inRange (shapeToCVec idx) (shapeToCVec size)

-- | Check that index is in bounds for a N-dimensional array.
inBoundsCVec
  :: (Arity n)
  => ContVec n Int -- ^ 0-based index
  -> ContVec n Int -- ^ Size of array
  -> Bool
{-# INLINE inBoundsCVec #-}
inBoundsCVec idx size
  = FC.and
  $ FC.zipWith inRange idx size


----------------------------------------------------------------
-- Slicing
----------------------------------------------------------------

-- | Very generic type class for slicing N-dimensional arrays. It's
--   expected that slice is created in /O(1)/ and shares underlying
--   buffer with parent array. @i@ is a specification of slice and @v@
--   is array being sliced.
class Slice i v where
  -- | Expected /O(1)/. Return slice of vector @v@ or @Nothing@ if
  --   required slice is not valid.
  sliceMaybe :: i -> v -> Maybe v

-- | Compute slice of an array @v@. This function raises an exception
--   if slice specification is not valid.
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
  computeSlice1D len (mirror len -> i, Length sz)
    -- FIXME: overflows and stuff
    | i  < 0       = Nothing
    | sz < 0       = Nothing
    | len < i + sz = Nothing
    | otherwise    = Just (i,sz)

instance (i ~ Int) => Slice1D (i, End) where
  {-# INLINE computeSlice1D #-}
  computeSlice1D len (mirror len -> i, _)
    | i < 0     = Nothing
    | i > len   = Nothing
    | otherwise = Just (i, len - i)

instance (i ~ Int) => Slice1D (Range i) where
  {-# INLINE computeSlice1D #-}
  computeSlice1D len ((mirror len -> i) :.. (mirror len -> j))
    | i   < 0      = Nothing
    | sz  < 0      = Just (i, 0)
    | len < i + sz = Nothing
    | otherwise    = Just (i, sz)
    where
      sz = j - i

mirror :: Int -> Int -> Int
{-# INLINE mirror #-}
mirror len i | i >= 0    = i
             | otherwise = len + i

instance Slice1D (Int,Int) where
  {-# INLINE computeSlice1D #-}
  computeSlice1D len (i,j) = computeSlice1D len (i:..j)

-- | Semiopen range @a :.. b@
data Range a = a :.. a
  deriving stock (Show,Eq,Ord,Generic)

-- | Used for creating slice to end of vector.
data End = End
  deriving stock (Show,Eq,Ord,Generic)

-- | Use length of vector for a slice.
newtype Length = Length { unLength :: Int }
  deriving stock (Show,Eq,Ord,Generic)


----------------------------------------------------------------
-- Generic ND-arrays
----------------------------------------------------------------

-- | Rank of N-dimensional array: 1 for vectors, 2 for matrices, etc.
type family Rank (arr :: Type -> Type) :: Nat

-- | Base type class for N-dimensional arrays. It only contains
--   functions for getting extent of array and checking whether index
--   is in range. Instances could be defined for both mutable and
--   immutable arrays.
class F.Arity (Rank arr) => HasShape arr a where
  -- | Return shape of N-dimensional array as a tuple of
  --   @Int@s. 'ContVec' is used as tuple parametric in rank which
  --   is both parametric in length and optimized well by GHC.
  shapeAsCVec :: arr a -> ContVec (Rank arr) Int

-- | Get shape of an array. Note that this function is polymorphic in
--   its return type.
shape :: (IsShape shape (Rank arr), HasShape arr a) => arr a -> shape
shape = shapeFromCVec . shapeAsCVec
{-# INLINE shape #-}

-- | Number of columns of two dimensional array (@k@, if size is @(n,k)@)
nCols :: (Rank arr ~ 2, HasShape arr a) => arr a -> Int
nCols v = runContVec (Fun $ \_ n -> n) (shapeAsCVec v)
{-# INLINE nCols #-}

-- | Number of rows of two dimensional array (@n@, if size is @(n,k)@)
nRows :: (Rank arr ~ 2, HasShape arr a) => arr a -> Int
nRows v = runContVec (Fun $ \n _ -> n) (shapeAsCVec v)
{-# INLINE nRows #-}



----------------------------------------------------------------
-- Mutable arrays
----------------------------------------------------------------

-- | Type class for N-dimensional arrays where each dimension is
--   zero-indexed. There's no restriction on actual representation
--   which could be dense or sparse.
class (forall s. HasShape (arr s) a) => NDMutable arr a where
  -- | /O(1)/ Read element of an array at given index. Provided index
  --   must be in bounds and function must return value in that
  --   case. Otherwise behavior is undefined.
  --
  -- **NOTE** Use 'unsafeReadArr' instead.
  basicUnsafeReadArr :: arr s a -> ContVec (Rank (arr s)) Int -> ST s a
  -- | /O(1)/ Write element to an array at a given index. Provided
  --   index must be in bounds and 'basicIsWritable' must return true
  --   for it.
  --
  -- **NOTE** Use 'unsafeWriteArr' instead.
  basicUnsafeWriteArr :: arr s a -> ContVec (Rank (arr s)) Int -> a -> ST s ()
  -- | Check that index is backed by storage and writable. Index must
  --   be in bounds.
  basicIsWritable :: arr s a -> ContVec (Rank (arr s)) Int -> Bool
  basicIsWritable _ _ = True
  {-# INLINE basicIsWritable #-}


-- | /O(1)/ Read element of an array without performing any range
--   checks. Only generic way to check whether index is in range is
--   to call 'basicRangeCheck'.
unsafeReadArr
  :: (NDMutable arr a, IsShape idx (Rank (arr s)), PrimMonad m, s ~ PrimState m)
  => arr s a -- ^ Mutable array
  -> idx     -- ^ Array index
  -> m a
{-# INLINE unsafeReadArr #-}
unsafeReadArr arr i = stToPrim $ basicUnsafeReadArr arr (shapeToCVec i)

-- | /O(1)/ Read element of an array. Should throw if element is out
--   of range. Note that successful read does not imply that it's
--   safe to write at same index for sparse matrices.
readArr
  :: (NDMutable arr a, IsShape idx (Rank (arr s)), PrimMonad m, s ~ PrimState m)
  => arr s a -- ^ Mutable array
  -> idx     -- ^ Array index
  -> m a
{-# INLINE readArr #-}
readArr arr (shapeToCVec -> i)
  | i `inBoundsCVec` sz = stToPrim $ basicUnsafeReadArr arr i
  | otherwise           = error "Read out of bounds"
  where
    sz = shapeAsCVec arr

-- | /O(1)/ Write element to an array without performing any range
--   checks. Only generic way to check whether index is in range is
--   to call 'basicRangeCheck'.
unsafeWriteArr
  :: (NDMutable arr a, IsShape idx (Rank (arr s)), PrimMonad m, s ~ PrimState m)
  => arr s a -- ^ Mutable array
  -> idx     -- ^ Index 
  -> a       -- ^ Value to write
  -> m ()
{-# INLINE unsafeWriteArr #-}
unsafeWriteArr arr i a = stToPrim $ basicUnsafeWriteArr arr (shapeToCVec i) a

-- | /O(1)/ Write element to an array. Will throw exception is index
--   is invalid. It could be because it's out of range or attempt to
--   write to sparse area of ND-array is made.
writeArr
  :: (NDMutable arr a, IsShape idx (Rank (arr s)), PrimMonad m, s ~ PrimState m)
  => arr s a -- ^ Mutable array
  -> idx     -- ^ Index 
  -> a       -- ^ Value to write
  -> m ()
{-# INLINE writeArr #-}
writeArr arr (shapeToCVec -> i) a
  | not $ i `inBoundsCVec` sz   = error "OutOfRange"
  | not $ basicIsWritable arr i = error "Invalid write"
  | otherwise                   = stToPrim $ basicUnsafeWriteArr arr i a
  where
    sz = shapeAsCVec arr


----------------------------------------------------------------
-- Immutable arrays
----------------------------------------------------------------

-- | Type class for N-dimensional arrays where each dimension is
--   zero-indexed. There's no restriction on actual representation
--   which could be dense or sparse.
class HasShape arr a => NDArray arr a where
  -- | /O(1)/ Index array. Provided index must be in bounds and
  --   function must return value in that case. Otherwise behavior is
  --   undefined.
  --
  --  **NOTE** use 'index' or '!' instead.
  basicUnsafeIndex :: arr a -> ContVec (Rank arr) Int -> a

-- | /O(1)/ Index dense array without performing any range
--   checks. Caller must ensure that index is in range elementwise.
unsafeIndex :: (NDArray arr a, IsShape idx (Rank arr)) => arr a -> idx -> a
unsafeIndex arr = basicUnsafeIndex arr . shapeToCVec
{-# INLINE unsafeIndex #-}

-- | /O(1)/ Return element of an array at given index. Will throw if
--   index is out of range.
index :: (NDArray arr a, IsShape idx (Rank arr)) => arr a -> idx -> a
index arr (shapeToCVec -> i)
  | i `inBoundsCVec` sz = basicUnsafeIndex arr i
  | otherwise           = error "OutOfRange"
  where
    sz = shapeAsCVec arr
{-# INLINE index #-}

-- | /O(1)/ Return element of an array at given index. Will throw if
--   index is out of range.
(!) :: (NDArray arr a, IsShape idx (Rank arr)) => arr a -> idx -> a
(!) = index
{-# INLINE (!) #-}

-- | /O(1)/ Return element of an array at given index. Will throw
indexMaybe :: (NDArray arr a, IsShape idx (Rank arr)) => arr a -> idx -> Maybe a
indexMaybe arr (shapeToCVec -> i)
  | i `inBoundsCVec` sz = Just $ basicUnsafeIndex arr i
  | otherwise           = Nothing
  where
    sz = shapeAsCVec arr
{-# INLINE indexMaybe #-}

-- | /O(1)/ Return element of an array at given index. Will throw
(!?) :: (NDArray arr a, IsShape idx (Rank arr)) => arr a -> idx -> Maybe a
(!?) = indexMaybe
{-# INLINE (!?) #-}



----------------------------------------------------------------
-- Instances
----------------------------------------------------------------

type instance Rank V.Vector  = 1
type instance Rank VS.Vector = 1
type instance Rank VU.Vector = 1
type instance Rank VP.Vector = 1

type instance Rank (MV.MVector  s) = 1
type instance Rank (MVS.MVector s) = 1
type instance Rank (MVU.MVector s) = 1
type instance Rank (MVP.MVector s) = 1

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

type instance Rank (Tr   v) = Rank v
type instance Rank (Conj v) = Rank v

instance (HasShape arr a, Rank arr ~ 2) => HasShape (Tr arr) a where
  shapeAsCVec (Tr arr) = swapFC2 $ shapeAsCVec arr
  {-# INLINE shapeAsCVec #-}
instance (HasShape arr a, Rank arr ~ 2) => HasShape (Conj arr) a where
  shapeAsCVec (Conj arr) = swapFC2 $ shapeAsCVec arr
  {-# INLINE shapeAsCVec #-}


instance NDArray V.Vector a where
  basicUnsafeIndex v (ContVec cont) = VG.unsafeIndex v (cont (Fun id))
  {-# INLINE basicUnsafeIndex #-}
instance VS.Storable a => NDArray VS.Vector a where
  basicUnsafeIndex v (ContVec cont) = VG.unsafeIndex v (cont (Fun id))
  {-# INLINE basicUnsafeIndex #-}
instance VU.Unbox a => NDArray VU.Vector a where
  basicUnsafeIndex v (ContVec cont) = VG.unsafeIndex v (cont (Fun id))
  {-# INLINE basicUnsafeIndex #-}
instance VP.Prim a => NDArray VP.Vector a where
  basicUnsafeIndex v (ContVec cont) = VG.unsafeIndex v (cont (Fun id))
  {-# INLINE basicUnsafeIndex #-}


instance NDMutable MV.MVector a where
  basicUnsafeReadArr  v (ContVec idx)   = idx $ Fun $ MVG.unsafeRead v
  basicUnsafeWriteArr v (ContVec idx) a = idx $ Fun $ \i -> MVG.unsafeWrite v i a
  {-# INLINE basicUnsafeReadArr  #-}
  {-# INLINE basicUnsafeWriteArr #-}
instance (VS.Storable a) => NDMutable MVS.MVector a where
  basicUnsafeReadArr  v (ContVec idx)   = idx $ Fun $ MVG.unsafeRead v
  basicUnsafeWriteArr v (ContVec idx) a = idx $ Fun $ \i -> MVG.unsafeWrite v i a
  {-# INLINE basicUnsafeReadArr  #-}
  {-# INLINE basicUnsafeWriteArr #-}
instance (VU.Unbox a) => NDMutable MVU.MVector a where
  basicUnsafeReadArr  v (ContVec idx)   = idx $ Fun $ MVG.unsafeRead v
  basicUnsafeWriteArr v (ContVec idx) a = idx $ Fun $ \i -> MVG.unsafeWrite v i a
  {-# INLINE basicUnsafeReadArr  #-}
  {-# INLINE basicUnsafeWriteArr #-}
instance (VP.Prim a) => NDMutable MVP.MVector a where
  basicUnsafeReadArr  v (ContVec idx)   = idx $ Fun $ MVG.unsafeRead v
  basicUnsafeWriteArr v (ContVec idx) a = idx $ Fun $ \i -> MVG.unsafeWrite v i a
  {-# INLINE basicUnsafeReadArr  #-}
  {-# INLINE basicUnsafeWriteArr #-}


instance (NDArray arr a, Rank arr ~ 2) => NDArray (Tr arr) a where
  basicUnsafeIndex (Tr arr) = basicUnsafeIndex arr . swapFC2
  {-# INLINE basicUnsafeIndex #-}
instance (NDArray arr a, Rank arr ~ 2) => NDArray (Conj arr) a where
  basicUnsafeIndex (Conj arr) = basicUnsafeIndex arr . swapFC2
  {-# INLINE basicUnsafeIndex #-}


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



swapFC2 :: ContVec 2 a -> ContVec 2 a
swapFC2 (ContVec cont) = ContVec $ \(Fun f) -> cont (Fun $ flip f)
{-# INLINE swapFC2 #-}



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

-- | Function which uses trick with unsigned comparison to save one
--   comparison when checking whether index in @[0,n)@ range.
inRange :: Int -- ^ Index
        -> Int -- ^ Size of buffer
        -> Bool
inRange i n = (fromIntegral i :: Word) < (fromIntegral n :: Word)
{-# INLINE inRange #-}
