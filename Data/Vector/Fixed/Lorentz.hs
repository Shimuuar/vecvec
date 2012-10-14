{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- |
-- Unboxed Lorentz vectors
module Data.Vector.Fixed.Lorentz (
    -- * Data type
    LorentzN
  , Lorentz
    -- * Boosts
  , Gamma(..)
  , Rapidity(..)
  , Speed(..)
  ) where

import Control.Monad
import Data.Primitive (Prim)
import Prelude hiding (length,replicate,zipWith,map,foldl)

import Data.Classes.AdditiveGroup
import Data.Classes.VectorSpace

import Data.Vector.Fixed
import Data.Vector.Fixed.Unboxed


----------------------------------------------------------------
-- Data type
----------------------------------------------------------------

-- | Generic n-dimensional Lorentz vector
newtype LorentzN n a = Lorentz (Vec (S n) a)

-- | Normal 4-dimensional Lorentz vector
type Lorentz = LorentzN (S (S (S Z)))

type instance Dim (LorentzN n) = S n

instance (Arity n, Prim a) => Vector (LorentzN n) a where
  construct             = fmap Lorentz construct
  inspect (Lorentz v) f = inspect v f

instance (Arity n, Prim a, Show a) => Show (LorentzN n a) where
  show = show . toList


----------------------------------------------------------------
-- Boosts
----------------------------------------------------------------

-- | Gamma factor
newtype Gamma = Gamma { getGamma :: Double }
                deriving (Show,Eq,Ord,Num)

-- | Rapidity
newtype Rapidity = Rapidity { getRapidity :: Double }
                 deriving (Show,Eq,Ord,Num)

-- | Speed in fractions of c
newtype Speed = Speed { getSpeed :: Double }
                 deriving (Show,Eq,Ord,Num)



----------------------------------------------------------------
-- Instances
----------------------------------------------------------------

type instance Scalar (LorentzN n a) = a

instance (Arity n, Prim a, Num a) => AdditiveMonoid (LorentzN n a) where
  zeroV = Lorentz zeroV
  Lorentz v ^+^ Lorentz u = Lorentz $ v ^+^ u
  {-# INLINE zeroV #-}
  {-# INLINE (^+^) #-}

instance (Arity n, Prim a, Num a) => AdditiveGroup (LorentzN n a) where
  negateV (Lorentz v)     = Lorentz $ negateV v
  Lorentz v ^-^ Lorentz u = Lorentz $ v ^-^ u
  {-# INLINE negateV #-}
  {-# INLINE (^-^)   #-}

instance (Arity n, Prim a, Num a) => LeftModule  (LorentzN n a) where
  a *^ Lorentz v = Lorentz $ a *^ v
  {-# INLINE (*^) #-}

instance (Arity n, Prim a, Num a) => RightModule (LorentzN n a) where
  Lorentz v ^* a = Lorentz $ v ^* a
  {-# INLINE (^*) #-}

-- 
instance (Arity n, Prim a, Num a) => InnerSpace (LorentzN n a) where
  v <.> u = foldl (+) 0 $ izipWith minkovsky v u
    where
      minkovsky 0 x y =   x*y
      minkovsky _ x y = -(x*y)
  {-# INLINE (<.>) #-}
