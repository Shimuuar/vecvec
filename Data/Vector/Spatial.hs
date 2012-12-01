{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TypeFamilies #-}
-- | Orphan instances for spatial vectors
module Data.Vector.Spatial (
  ) where

import Control.Monad
import Prelude hiding (sum,replicate,zipWith,map,foldl)

import Data.Primitive (Prim)
import Data.Classes.AdditiveGroup
import Data.Classes.VectorSpace
import Foreign.Storable (Storable)

import           Data.Vector.Fixed           as F
import qualified Data.Vector.Fixed.Unboxed   as U
import qualified Data.Vector.Fixed.Storable  as S
import qualified Data.Vector.Fixed.Primitive as P
import qualified Data.Vector.Fixed.Boxed     as B

----------------------------------------------------------------
-- Instances
----------------------------------------------------------------

type instance Scalar (U.Vec n a) = a
type instance Scalar (S.Vec n a) = a
type instance Scalar (P.Vec n a) = a
type instance Scalar (B.Vec n a) = a


instance (U.Unbox n a, Num a) => AdditiveMonoid (U.Vec n a) where
  zeroV = replicate 0
  (^+^) = zipWith (+)
  {-# INLINE zeroV #-}
  {-# INLINE (^+^) #-}
instance (Arity n, Storable a, Num a) => AdditiveMonoid (S.Vec n a) where
  zeroV = replicate 0
  (^+^) = zipWith (+)
  {-# INLINE zeroV #-}
  {-# INLINE (^+^) #-}
instance (Arity n, Prim a, Num a) => AdditiveMonoid (P.Vec n a) where
  zeroV = replicate 0
  (^+^) = zipWith (+)
  {-# INLINE zeroV #-}
  {-# INLINE (^+^) #-}
instance (Arity n, Num a) => AdditiveMonoid (B.Vec n a) where
  zeroV = replicate 0
  (^+^) = zipWith (+)
  {-# INLINE zeroV #-}
  {-# INLINE (^+^) #-}


instance (U.Unbox n a, Num a) => AdditiveGroup (U.Vec n a) where
  negateV = map negate
  (^-^)   = zipWith (-)
  {-# INLINE negateV #-}
  {-# INLINE (^-^)   #-}
instance (Arity n, Storable a, Num a) => AdditiveGroup (S.Vec n a) where
  negateV = map negate
  (^-^)   = zipWith (-)
  {-# INLINE negateV #-}
  {-# INLINE (^-^)   #-}
instance (Arity n, Prim a, Num a) => AdditiveGroup (P.Vec n a) where
  negateV = map negate
  (^-^)   = zipWith (-)
  {-# INLINE negateV #-}
  {-# INLINE (^-^)   #-}
instance (Arity n, Num a) => AdditiveGroup (B.Vec n a) where
  negateV = map negate
  (^-^)   = zipWith (-)
  {-# INLINE negateV #-}
  {-# INLINE (^-^)   #-}

instance (U.Unbox n a, Num a) => LeftModule (U.Vec n a) where
  a *^ v = map (a *) v
  {-# INLINE (*^) #-}
instance (Arity n, Storable a, Num a) => LeftModule (S.Vec n a) where
  a *^ v = map (a *) v
  {-# INLINE (*^) #-}
instance (Arity n, Prim a, Num a) => LeftModule (P.Vec n a) where
  a *^ v = map (a *) v
  {-# INLINE (*^) #-}
instance (Arity n, Num a) => LeftModule (B.Vec n a) where
  a *^ v = map (a *) v
  {-# INLINE (*^) #-}

instance (U.Unbox n a, Num a) => RightModule (U.Vec n a) where
  v ^* a = map (* a) v
  {-# INLINE (^*) #-}
instance (Arity n, Storable a, Num a) => RightModule (S.Vec n a) where
  v ^* a = map (* a) v
  {-# INLINE (^*) #-}
instance (Arity n, Prim a, Num a) => RightModule (P.Vec n a) where
  v ^* a = map (* a) v
  {-# INLINE (^*) #-}
instance (Arity n, Num a) => RightModule (B.Vec n a) where
  v ^* a = map (* a) v
  {-# INLINE (^*) #-}

instance (U.Unbox n a, Num a) => InnerSpace (U.Vec n a) where
  v <.> u = sum $! zipWith (*) v u
  {-# INLINE (<.>) #-}
instance (Arity n, Storable a, Num a) => InnerSpace (S.Vec n a) where
  v <.> u = sum $! zipWith (*) v u
  {-# INLINE (<.>) #-}
instance (Arity n, Prim a, Num a) => InnerSpace (P.Vec n a) where
  v <.> u = sum $! zipWith (*) v u
  {-# INLINE (<.>) #-}
instance (Arity n, Num a) => InnerSpace (B.Vec n a) where
  v <.> u = sum $! zipWith (*) v u
  {-# INLINE (<.>) #-}
