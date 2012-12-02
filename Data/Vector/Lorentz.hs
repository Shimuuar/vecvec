{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- |
-- Unboxed Lorentz vectors
module Data.Vector.Lorentz (
    -- * Data type
    LorentzN
  , Lorentz
    -- * Boosts
  , Gamma(..)
  , Rapidity(..)
  , Speed(..)
  ) where

import Control.Monad
import Prelude hiding (length,replicate,zipWith,map,foldl,sum)

import Data.Classes.AdditiveGroup
import Data.Classes.VectorSpace

import Data.Vector.Fixed         as F
import Data.Vector.Fixed.Unboxed


----------------------------------------------------------------
-- Data type
----------------------------------------------------------------

-- | Generic n-dimensional Lorentz vector
newtype LorentzN n a = Lorentz (Vec (S n) a)

-- | Normal 4-dimensional Lorentz vector
type Lorentz = LorentzN N3

type instance Dim  (LorentzN n) = S n

instance (Unbox (S n) a) => Vector (LorentzN n) a where
  construct             = fmap Lorentz construct
  inspect (Lorentz v) f = inspect v f



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

instance (Unbox (S n) a, Num a) => AdditiveMonoid (LorentzN n a) where
  zeroV = replicate 0
  (^+^) = zipWith (+)
  {-# INLINE zeroV #-}
  {-# INLINE (^+^) #-}

instance (Unbox (S n) a, Num a) => AdditiveGroup (LorentzN n a) where
  negateV = map negate
  (^-^)   = zipWith (-)
  {-# INLINE negateV #-}
  {-# INLINE (^-^)   #-}

instance (Unbox (S n) a, Num a) => LeftModule  (LorentzN n a) where
  a *^ v = map (a *) v
  {-# INLINE (*^) #-}

instance (Unbox (S n) a, Num a) => RightModule (LorentzN n a) where
  v ^* a = map (* a) v
  {-# INLINE (^*) #-}

instance (Unbox (S n) a, Num a) => InnerSpace (LorentzN n a) where
  v <.> u = sum $ izipWith minkovsky v u
    where
      minkovsky 0 x y =   x*y
      minkovsky _ x y = -(x*y)
  {-# INLINE (<.>) #-}
