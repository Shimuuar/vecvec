{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ImpredicativeTypes #-}
-- |
-- Unboxed vectors with fixed length
module Data.Vector.Fixed.Unboxed (
    Vec
  ) where

import Control.Monad
import Control.Monad.ST
import Data.Primitive.ByteArray
import Data.Primitive
import Prelude hiding (length,replicate,zipWith,map,foldl)

import Data.Classes.AdditiveGroup
import Data.Classes.VectorSpace

import Data.Vector.Fixed



----------------------------------------------------------------
-- Data type
----------------------------------------------------------------

-- | Unboxed vector with fixed length
data Vec n a = Vec {-# UNPACK #-} !Int       -- Offset from start
                   {-# UNPACK #-} !ByteArray -- Data array

type instance Dim (Vec n) = n

instance (Arity n, Prim a) => Vector (Vec n) a where
  construct = constructVec
  inspect   = inspectVec
  {-# INLINE construct #-}
  {-# INLINE inspect   #-}


newtype T_idx n = T_idx Int

inspectVec :: forall n a b. (Arity n, Prim a) => Vec n a -> Fun n a b -> b
{-# INLINE inspectVec #-}
inspectVec v (Fun f)
  = apply (\(T_idx i) -> (index i v, T_idx (i+1)))
          (T_idx 0 :: T_idx n)
          f

constructVec :: forall n a. (Arity n, Prim a) => Fun n a (Vec n a)
{-# INLINE constructVec #-}
constructVec = Fun $
  accum  step
        (fini :: T_new a Z -> Vec n a)
        (new  :: T_new a n)

data T_new a n = T_new Int (forall s. ST s (MutableByteArray s))

fini :: (Arity n, Prim a) => T_new a Z -> Vec n a
fini (T_new _ st) = runST $ do
  vec <- unsafeFreezeByteArray =<< st
  return $! Vec 0 vec

step :: (Prim a) => T_new a (S n) -> a -> T_new a n
step (T_new i st) x = T_new (i+1) $ do
  arr <- st
  writeByteArray arr i x
  return arr

new :: forall n a. (Arity n, Prim a) => T_new a n
new = T_new 0 $ newByteArray $! arity (undefined :: n) * sizeOf (undefined :: a)



----------------------------------------------------------------
-- Instances
----------------------------------------------------------------

instance (Arity n, Prim a, Show a) => Show (Vec n a) where
  show = show . toList

type instance Scalar (Vec n a) = a

instance (Arity n, Prim a, Num a) => AdditiveMonoid (Vec n a) where
  zeroV = replicate 0
  (^+^) = zipWith (+)
  {-# INLINE zeroV #-}
  {-# INLINE (^+^) #-}

instance (Arity n, Prim a, Num a) => AdditiveGroup (Vec n a) where
  negateV = map negate
  (^-^)   = zipWith (-)
  {-# INLINE negateV #-}
  {-# INLINE (^-^)   #-}

instance (Arity n, Prim a, Num a) => LeftModule  (Vec n a) where
  a *^ v = map (a *) v
  {-# INLINE (*^) #-}

instance (Arity n, Prim a, Num a) => RightModule (Vec n a) where
  v ^* a = map (a *) v
  {-# INLINE (^*) #-}

instance (Arity n, Prim a, Num a) => InnerSpace (Vec n a) where
  v <.> u = foldl (+) 0 $ zipWith (*) u v
  {-# INLINE (<.>) #-}


----------------------------------------------------------------
-- Helpers
----------------------------------------------------------------

-- Low level indexing operation
index :: Prim a => Int -> Vec n a -> a
index n (Vec off arr) = indexByteArray arr (off + n)
