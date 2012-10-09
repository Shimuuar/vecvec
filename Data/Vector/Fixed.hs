{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Generic vectors with fixed length. In style of Roman of Leshchinskiy
-- <http://unlines.wordpress.com/2010/11/15/generics-for-small-fixed-size-vectors/>
module Data.Vector.Fixed (
    -- * Type-level naturals
    Z
  , S
    -- * N-ary functions
  , Fn
  , Fun
  , Arity(..)
    -- *
  , replicate
  , map
  , foldl
  , zipWith
  ) where

import Data.Complex
import Prelude hiding (replicate,map,zipWith,foldl)

type instance Dim Complex = S (S Z)
instance RealFloat a => Vector Complex a where
  construct = Fun (:+)
  inspect (x :+ y) (Fun f) = f x y

----------------------------------------------------------------
-- N-ary functions
----------------------------------------------------------------

-- | Zero
data Z
-- | Successor of n
data S n

-- | Type family for n-ary functions
type family   Fn n a b
type instance Fn Z     a b = b
type instance Fn (S n) a b = a -> Fn n a b

-- | Newtype which is used to make Fn injective
newtype Fun n a b = Fun (Fn n a b)


-- | Type class for handling N-ary functions
class Arity n where
  -- | Accumulation. Basically a fold
  accum :: (forall m. tag (S m) -> a -> tag m) -- ^ Reduction function
        -> (tag Z -> b)                        -- ^ Final case
        -> tag n                               -- ^ Initial tag
        -> Fn n a b                            -- ^ Reduction function
  -- | Unfold
  apply :: (forall m. tag (S m) -> (a, tag m)) -- ^ Unfolding function
        -> tag n                               -- ^ Initial tag
        -> Fn n a b                            -- ^ Function which produces unfolded result
        -> b

instance Arity Z where
  accum _ g t = g t
  apply _ _ h = h
  {-# INLINE accum #-}
  {-# INLINE apply #-}

instance Arity n => Arity (S n) where
  accum f g t = \a -> accum f g (f t a)
  apply f t h = case f t of (a,u) -> apply f u (h a)
  {-# INLINE accum #-}
  {-# INLINE apply #-}



----------------------------------------------------------------
-- Type class for vectors
----------------------------------------------------------------

-- | Type family vector size
type family Dim (v :: * -> *)

-- | Type class for short vectors with fixed length
class Arity (Dim v) => Vector v a where
  -- | Function for creation of vectors
  construct :: Fun (Dim v) a (v a)
  -- | Deconstruction of vectors
  inspect   :: v a -> Fun (Dim v) a b -> b


-- | Newtype wrapper for vector length
-- newtype VecLen n = VecLen Int


----------------------------------------------------------------
-- Generic functions
----------------------------------------------------------------

-- | Replicate value /n/ times.
replicate :: Vector v a => a -> v a
{-# INLINE replicate #-}
replicate x = replicateF x
            $ construct

data T_replicate n = T_replicate

replicateF :: forall n a b. Arity n => a -> Fun n a b -> b
replicateF x (Fun h)
  = apply (\T_replicate -> (x, T_replicate))
          (T_replicate :: T_replicate n)
          h


----------------------------------------------------------------

-- | Left fold over vector
foldl :: Vector v a => (b -> a -> b) -> b -> v a -> b
{-# INLINE foldl #-}
foldl f z v = inspect v
            $ foldlF f z

newtype T_foldl b n = T_foldl b

foldlF :: forall n a b. Arity n => (b -> a -> b) -> b -> Fun n a b
foldlF f b = Fun $ accum (\(T_foldl b) a -> T_foldl (f b a))
                         (\(T_foldl b) -> b)
                         (T_foldl b :: T_foldl b n)


----------------------------------------------------------------

-- | Map over vector
map :: (Vector v a, Vector v b) => (a -> b) -> v a -> v b
{-# INLINE map #-}
map f v = inspect v
        $ mapF f
        $ construct

newtype T_map b c n = T_map (Fn n b c)

mapF :: forall n a b c. Arity n => (a -> b) -> Fun n b c -> Fun n a c
mapF f (Fun h) = Fun $ accum (\(T_map h) a -> T_map (h (f a)))
                             (\(T_map h)   -> h)
                             (T_map h :: T_map b c n)



----------------------------------------------------------------

-- | Zip two vector together.
zipWith :: (Vector v a, Vector v b, Vector v c)
        => (a -> b -> c) -> v a -> v b -> v c
{-# INLINE zipWith #-}
zipWith f v u = inspect u
              $ inspect v
              $ zipWithF f
              $ construct

-- E. Kmett's version of zipWith. It must be checked for performance0

data T_zip a c r n = T_zip (Vec n a) (Fn n c r)

zipWithF :: forall n a b c d. Arity n
         => (a -> b -> c) -> Fun n c d -> Fun n a (Fun n b d)
zipWithF f (Fun g0) = 
  fmap (\v -> Fun $ accum
              (\(T_zip (Vec (a:as)) g) b -> T_zip (Vec as) (g (f a b)))
              (\(T_zip _ x) -> x)
              (T_zip v g0 :: T_zip a c d n)
       ) construct

newtype Fmap a b n = Fmap (Fn n a b)
instance Arity n => Functor (Fun n a) where
  fmap (f :: b -> c) (Fun g0 :: Fun n a b) = Fun $ accum
        (\(Fmap g) a -> Fmap (g a))
        (\(Fmap x) -> f x)
         (Fmap g0 :: Fmap a b n)


newtype Vec n a = Vec [a]

type instance Dim (Vec n) = n

instance Arity n => Vector (Vec n) a where
  construct = Fun $ accum
              (\(Reverse xs) x -> Reverse (x:xs))
              (\(Reverse xs) -> Vec (reverse xs) :: Vec n a)
              (Reverse [] :: Reverse a n)
  inspect v (Fun f) = apply (\(Flip (Vec (x:xs))) -> (x, Flip (Vec xs))) (Flip v) f


newtype Reverse a n = Reverse [a]

reverseF :: forall n a b c. Arity n => Fun n a [a]
reverseF = Fun $ accum
           (\(Reverse xs) x -> Reverse (x:xs))
           (\(Reverse xs) -> xs)
           (Reverse [] :: Reverse a n)

toListF :: forall n a b c. Arity n => Fun n a [a]
toListF = Fun $ accum
          (\(Reverse xs) x -> Reverse (x:xs))
          (\(Reverse xs) -> reverse xs)
          (Reverse [] :: Reverse a n)

-- toList :: (Vector v a) => v a -> [a]
-- toList v = inspect v $ toListF

newtype Flip f a n = Flip (f n a)
