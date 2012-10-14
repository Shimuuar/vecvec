{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Generic vectors with fixed length. In style of Roman of Leshchinskiy
-- <http://unlines.wordpress.com/2010/11/15/generics-for-small-fixed-size-vectors/>
--
-- Note that vector operations do not fuse. Probably it could be fixed.
module Data.Vector.Fixed (
    -- * Type-level naturals
    Z
  , S
    -- * N-ary functions
  , Fn
  , Fun(..)
  , Arity(..)
    -- * Vector type class
  , Dim
  , Vector(..)
  , length
    -- * Generic functions
    -- ** Construction
  , replicate
  , basis
    -- *** Literals
  , New
  , vec
  , con
  , (|>)
    -- ** Transformation
  , map
  , foldl
  , zipWith
    -- ** Conversion
  , toList
    -- * Special types
  , VecList(..)
  ) where

import Data.Complex
import Prelude hiding (replicate,map,zipWith,foldl,length)


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

newtype T_fmap a b n = T_fmap (Fn n a b)

instance Arity n => Functor (Fun n a) where
  fmap (f :: b -> c) (Fun g0 :: Fun n a b)
     = Fun $ accum
             (\(T_fmap g) a -> T_fmap (g a))
             (\(T_fmap x) -> f x)
             (T_fmap g0 :: T_fmap a b n)
  {-# INLINE fmap #-}


-- | Type class for handling N-ary functions
class Arity n where
  -- | Accumulation. Basically a fold
  accum :: (forall k. tag (S k) -> a -> tag k) -- ^ Reduction function
        -> (tag Z -> b)                        -- ^ Final case
        -> tag n                               -- ^ Initial tag
        -> Fn n a b                            -- ^ Reduction function
  -- | Monadic accumulation
  accumM :: Monad m
         => (forall k. tag (S k) -> a -> m (tag k))
         -> (tag Z -> m b)
         -> m (tag n)
         -> Fn n a (m b)
  -- | Unfold
  apply :: (forall k. tag (S k) -> (a, tag k)) -- ^ Unfolding function
        -> tag n                               -- ^ Initial tag
        -> Fn n a b                            -- ^ Function which produces unfolded result
        -> b
  -- | Arity
  arity :: n -> Int

instance Arity Z where
  accum  _ g t = g t
  accumM _ g t = g =<< t
  apply  _ _ h = h
  arity  _ = 0
  {-# INLINE accum  #-}
  {-# INLINE accumM #-}
  {-# INLINE apply  #-}
  {-# INLINE arity  #-}

instance Arity n => Arity (S n) where
  accum  f g t = \a -> accum f g (f t a)
  accumM f g t = \a -> accumM f g $ flip f a =<< t
  apply  f t h = case f t of (a,u) -> apply f u (h a)
  arity  n = 1 + arity (prevN n)
    where
      prevN :: S n -> n
      prevN _ = undefined
  {-# INLINE accum  #-}
  {-# INLINE accumM #-}
  {-# INLINE apply  #-}



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

length :: Arity (Dim v) => v a -> Int
{-# INLINE length #-}
length = arity . dim
  where
    dim :: v a -> Dim v
    dim _ = undefined



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

-- | Unit vector along Nth axis,
basis :: forall v a. (Vector v a, Num a) => Int -> v a
{-# INLINE basis #-}
basis n = basisF n $ construct

newtype T_basis n = T_basis Int

basisF :: forall n a b. (Num a, Arity n) => Int -> Fun n a b -> b
basisF n0 (Fun f)
  = apply (\(T_basis n) -> ((if n == 0 then 1 else 0) :: a, T_basis (n - 1)))
          (T_basis n0 :: T_basis n)
          f

----------------------------------------------------------------

-- | Newtype wrapper
newtype New n v a = New (Fn n a (v a))

-- | Convert to vector
vec :: New Z v a -> v a
{-# INLINE vec #-}
vec (New v) = v

-- | Seed constructor
con :: Vector v a => New (Dim v) v a
{-# INLINE con #-}
con = f2n construct

(|>) :: New (S n) v a -> a -> New n v a
{-# INLINE  (|>) #-}
New f |> a = New (f a)
infixl 1 |>

f2n :: Fun n a (v a) -> New n v a
{-# INLINE f2n #-}
f2n (Fun f) = New f


----------------------------------------------------------------

-- | Left fold over vector
foldl :: Vector v a => (b -> a -> b) -> b -> v a -> b
{-# INLINE foldl #-}
foldl f z v = inspect v
            $ foldlF f z

newtype T_foldl b n = T_foldl b

foldlF :: forall n a b. Arity n => (b -> a -> b) -> b -> Fun n a b
foldlF f b = Fun $ accum (\(T_foldl r) a -> T_foldl (f r a))
                         (\(T_foldl r) -> r)
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
mapF f (Fun h) = Fun $ accum (\(T_map g) a -> T_map (g (f a)))
                             (\(T_map g)   -> g)
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

data T_zip a c r n = T_zip (VecList n a) (Fn n c r)

zipWithF :: forall n a b c d. Arity n
         => (a -> b -> c) -> Fun n c d -> Fun n a (Fun n b d)
zipWithF f (Fun g0) =
  fmap (\v -> Fun $ accum
              (\(T_zip (VecList (a:as)) g) b -> T_zip (VecList as) (g (f a b)))
              (\(T_zip _ x) -> x)
              (T_zip v g0 :: T_zip a c d n)
       ) construct


----------------------------------------------------------------

-- | Convert vector to the list
toList :: (Vector v a) => v a -> [a]
toList v
  = case inspect v construct of VecList xs -> xs



----------------------------------------------------------------
-- Data types
----------------------------------------------------------------

-- | Vector based on the lists. Not very useful by itself but is
--   necessary for implementation.
newtype VecList n a = VecList [a]
                      deriving (Show,Eq)

type instance Dim (VecList n) = n

newtype Flip f a n = Flip (f n a)

newtype T_list a n = T_list ([a] -> [a])

-- It's vital to avoid 'reverse' and build list using [a]->[a]
-- functions. Reverse is recursive and interferes with inlining.
instance Arity n => Vector (VecList n) a where
  construct = Fun $ accum
    (\(T_list xs) x -> T_list (xs . (x:)))
    (\(T_list xs) -> VecList (xs []) :: VecList n a)
    (T_list id :: T_list a n)
  inspect v (Fun f) = apply
    (\(Flip (VecList (x:xs))) -> (x, Flip (VecList xs)))
    (Flip v)
    f
  {-# INLINE construct #-}
  {-# INLINE inspect   #-}



----------------------------------------------------------------
-- Instances
----------------------------------------------------------------

type instance Dim Complex = S (S Z)

instance RealFloat a => Vector Complex a where
  construct = Fun (:+)
  inspect (x :+ y) (Fun f) = f x y
