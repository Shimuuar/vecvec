{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ViewPatterns #-}
-- |
-- Unboxed Lorentz vectors
module Data.Vector.Lorentz (
    -- * Data type
    LorentzG
  , LorentzU
  , Lorentz2
  , Lorentz3
  , Lorentz4
  , Lorentz
  , spatialPart
    -- * Boosts
    -- ** Variables
  , Speed(..)
  , Gamma(..)
  , Rapidity(..)
  , Convert(..)
    -- ** Boosts
  , BoostParam(..)
  , boostAxis
  , boostX
  , boostY
  , boostZ
  ) where

import Control.Monad
import Prelude hiding (length,replicate,zipWith,map,foldl,sum)

import Data.Monoid    (Monoid(..))
import Data.Classes.AdditiveGroup
import Data.Classes.VectorSpace

import           Data.Vector.Fixed (Vector,VectorN,Dim,S,N2,N3,N4,(!))
import qualified Data.Vector.Fixed as F
import Data.Vector.Fixed.Unboxed   (Vec)


----------------------------------------------------------------
-- Data type
----------------------------------------------------------------

-- | Generic Lorentz vector which could be based on any array-based
--   vector. Parameter /n/ is size of vector.
newtype LorentzG v n a = Lorentz (v n a)
                         deriving Show

type instance Dim (LorentzG v n) = n

instance (VectorN v n a) => Vector (LorentzG v n) a where
  construct             = fmap Lorentz F.construct
  inspect (Lorentz v) f = F.inspect v f

instance (VectorN v n a) => VectorN (LorentzG v) n a

-- | Lorentz vector which uses unboxed vector as strorage
type LorentzU = LorentzG Vec

-- | 2-dimensional Lorentz vector.
type Lorentz2 v = LorentzG v N2

-- | 3-dimensional Lorentz vector.
type Lorentz3 v = LorentzG v N3

-- | 4-dimensional Lorentz vector.
type Lorentz4 v = LorentzG v N4

-- | Unboxed 4-dimensional Lorentz vector
type Lorentz = LorentzU N4


-- | Spatial part of the Lorentz vector.
spatialPart :: (VectorN v n a, VectorN v (S n) a)
            => LorentzG v (S n) a -> v n a
spatialPart (Lorentz v) = F.tail v
{-# INLINE spatialPart #-}



----------------------------------------------------------------
-- Boost variables
----------------------------------------------------------------

-- | Speed in fractions of c
newtype Speed a = Speed { getSpeed :: a }
                 deriving (Show,Eq,Ord)

-- | Gamma factor. Negative values of gamma factor are accepted and
--   means then boost will be performed in opposite direction.
newtype Gamma a = Gamma { getGamma :: a }
                deriving (Show,Eq,Ord)

-- | Rapidity
newtype Rapidity a = Rapidity { getRapidity :: a }
                 deriving (Show,Eq,Ord)

instance Num a => Monoid (Rapidity a) where
  mempty = Rapidity 0
  mappend (Rapidity a) (Rapidity b) = Rapidity $ a + b


-- | Class for total conversion functions
class Convert a b where
  convert :: a -> b

instance (Floating a, a ~ a') => Convert (Speed a) (Gamma a') where
  convert (Speed v)    = Gamma $ signum v / sqrt (1 - v*v)
instance (Floating a, a ~ a') => Convert (Speed a) (Rapidity a) where
  convert (Speed v)    = Rapidity $ atanh v
instance (Floating a, a ~ a') => Convert (Gamma a) (Speed a') where
  convert (Gamma γ)    = Speed $ signum γ * sqrt ((g2 -1) / g2) where g2 = γ*γ
instance (Floating a, a ~ a') => Convert (Gamma a) (Rapidity a') where
  convert (Gamma γ)    = Rapidity $ signum γ * acosh (abs γ)
instance (Floating a, a ~ a')  => Convert (Rapidity a) (Speed a') where
  convert (Rapidity φ) = Speed $ tanh φ
instance (Floating a, a ~ a') => Convert (Rapidity a) (Gamma a') where
  convert (Rapidity φ) = Gamma $ signum φ * cosh φ



----------------------------------------------------------------
-- Boost
----------------------------------------------------------------

-- | Boost for 1+1 space.
class BoostParam b where
  -- | Lorentz transformation for @(t,x)@ pair.
  boost1D :: (Floating a)
          => b a                  -- ^ Boost parameter
          -> (a,a) -> (a,a)


instance BoostParam Speed where
  boost1D (Speed v) (t,x)
    = ( γ*(   t - v*x)
      , γ*(-v*t +   x))
    where
      γ = abs $ getGamma $ convert (Speed v)
  {-# INLINE boost1D #-}

instance BoostParam Gamma where
  boost1D (Gamma γ) (t,x)
    = ( γ*(   t - v*x)
      , γ*(-v*t +   x))
    where
      Speed v = convert (Gamma γ)
  {-# INLINE boost1D #-}

instance BoostParam Rapidity where
  boost1D (Rapidity φ) (t,x)
    = ( c*t - s*x
      ,-s*t + c*x)
    where
      c = cosh φ
      s = sinh φ
  {-# INLINE boost1D #-}

-- | Boost along axis
boostAxis :: (BoostParam b, VectorN v n a, Floating a)
          => b a                -- ^ Boost parameter
          -> Int                -- ^ Axis number. /X/-0, /Y/-1 ...
          -> LorentzG v n a
          -> LorentzG v n a
{-# INLINE boostAxis #-}
boostAxis b n v
  = F.imap trans v
  where
    trans i a | i == 0    = t'
              | i == n+1  = x'
              | otherwise = a
    -- 1D boost
    (t',x') = boost1D b (t,x)
    t       = v ! 0
    x       = v ! (n+1)

-- | Boost along X axis.
boostX :: (BoostParam b, VectorN v (S (S n)) a, Floating a)
       => b a                -- ^ Boost parameter
       -> LorentzG v (S (S n)) a
       -> LorentzG v (S (S n)) a
{-# INLINE boostX #-}
boostX b = boostAxis b 0

-- | Boost along X axis.
boostY :: (BoostParam b, VectorN v (S (S (S n))) a, Floating a)
       => b a                -- ^ Boost parameter
       -> LorentzG v (S (S (S n))) a
       -> LorentzG v (S (S (S n))) a
{-# INLINE boostY #-}
boostY b = boostAxis b 1

-- | Boost along X axis.
boostZ :: (BoostParam b, VectorN v (S (S (S (S n)))) a, Floating a)
       => b a                -- ^ Boost parameter
       -> LorentzG v (S (S (S (S n)))) a
       -> LorentzG v (S (S (S (S n)))) a
{-# INLINE boostZ #-}
boostZ b = boostAxis b 2


----------------------------------------------------------------
-- Instances
----------------------------------------------------------------

type instance Scalar (LorentzG v n a) = a

instance (VectorN v n a, Num a) => AdditiveMonoid (LorentzG v n a) where
  zeroV = F.replicate 0
  (.+.) = F.zipWith (+)
  {-# INLINE zeroV #-}
  {-# INLINE (.+.) #-}

instance (VectorN v n a, Num a) => AdditiveGroup (LorentzG v n a) where
  negateV = F.map negate
  (.-.)   = F.zipWith (-)
  {-# INLINE negateV #-}
  {-# INLINE (.-.)   #-}

instance (VectorN v n a, Num a) => LeftModule  (LorentzG v n a) where
  a *. v = F.map (a *) v
  {-# INLINE (*.) #-}

instance (VectorN v n a, Num a) => RightModule (LorentzG v n a) where
  v .* a = F.map (* a) v
  {-# INLINE (.*) #-}

instance (VectorN v n a, Num a) => InnerSpace (LorentzG v n a) where
  v <.> u = F.sum $ F.izipWith minkovsky v u
    where
      minkovsky 0 x y =   x*y
      minkovsky _ x y = -(x*y)
  {-# INLINE (<.>) #-}
