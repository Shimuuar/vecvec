{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE ImportQualifiedPost   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
-- |
module Vecvec.Classes.Slice where

import GHC.Generics (Generic)
import Data.Vector           qualified as V
import Data.Vector.Unboxed   qualified as VU
import Data.Vector.Storable  qualified as VS
import Data.Vector.Primitive qualified as VP
import Data.Vector.Generic   qualified as VG


class Slice i v where
  sliceMaybe :: i -> v -> Maybe v


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
    | i  < 0        = Nothing
    | sz < 0        = Nothing
    | len <= i + sz = Nothing
    | otherwise     = Just (i,sz)

instance (i ~ Int) => Slice1D (i, End) where
  {-# INLINE computeSlice1D #-}
  computeSlice1D len (i, _)
    | i < 0     = Nothing
    | i >= len  = Nothing
    | otherwise = Just (i, len - i)

instance (i ~ Int) => Slice1D (Range i) where
  {-# INLINE computeSlice1D #-}
  computeSlice1D len (i0 :.. j0)
    | i <  0    = Nothing
    | j <  0    = Nothing
    | i >= len  = Nothing
    | j >= len  = Nothing
    | j < i     = Just (i, 0)
    | otherwise = Just (i, j-i)
    where
      mirror k | k > 0     = k
               | otherwise = len - 1 - k
      i = mirror i0
      j = mirror j0

      




-- | Semiopen range @a :.. b@
data Range a = a :.. a
  deriving stock (Show,Eq,Ord,Generic)

data End = End
  deriving stock (Show,Eq,Ord,Generic)

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




implSliceVector :: (Slice1D idx, VG.Vector v a) => idx -> v a -> Maybe (v a)
{-# INLINE implSliceVector #-}
implSliceVector idx vec = do
  (i,n) <- computeSlice1D (VG.length vec) idx
  Just $ VG.slice i n vec
