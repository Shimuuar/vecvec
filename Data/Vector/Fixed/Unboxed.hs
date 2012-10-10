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

import Control.Monad.ST
import Data.Primitive.ByteArray
import Data.Primitive

import Data.Vector.Fixed



----------------------------------------------------------------
-- Data type
----------------------------------------------------------------

data Vec n a = Vec {-# UNPACK #-} !Int       -- Offset from start
                   {-# UNPACK #-} !ByteArray -- Data array

type instance Dim (Vec n) = n

instance (Arity n, Prim a) => Vector (Vec n) a where
  construct = undefined
  inspect   = inspectVec

-- Deconstruct vector
inspectVec :: forall n a b. (Arity n, Prim a) => Vec n a -> Fun n a b -> b
inspectVec v (Fun f)
  = apply (\(T_idx i) -> (index i v, T_idx (i+1)))
          (T_idx 0 `asVec` v)
          f

-- Create vector
constructVec :: forall s n a. (Arity n, Prim a) => Fun n a (ST s (Vec n a))
constructVec
  = Fun $ accumM
          step
          (fini
             :: T_alloc s a Z -> ST s (Vec n a)
          )
          (fresh :: ST s (T_alloc s a n))

step :: Prim a => T_alloc s a (S k) -> a -> ST s (T_alloc s a k)
step (T_alloc i arr) x = do
  writeByteArray arr i x
  return $! T_alloc (i+1) arr

fini :: Prim a => T_alloc s a Z -> ST s (Vec n a)
fini (T_alloc _ arr) = do
  xs <- unsafeFreezeByteArray arr
  return $! Vec 0 xs

fresh :: ST s (T_alloc s a n)
fresh = do
  arr <- newByteArray 0
  return $ T_alloc 0 arr


funST :: forall n a b. (Arity n) => Fun n a (forall s. ST s b) -> Fun n a b
funST f = fmap runST f



newtype BoxST a = BoxST (forall s. ST s a)

chain :: BoxST a -> BoxST b -> BoxST b
chain (BoxST a) (BoxST b) = BoxST $ a >> b

data T_alloc s a n = T_alloc Int (MutableByteArray s)

----------------------------------------------------------------
-- Helpers
----------------------------------------------------------------

-- Index accumulator
newtype T_idx n = T_idx Int

asVec :: T_idx n -> Vec n a -> T_idx n
asVec x _ = x


-- Low level indexing operation
index :: Prim a => Int -> Vec n a -> a
index n (Vec off arr) = indexByteArray arr (off + n)
