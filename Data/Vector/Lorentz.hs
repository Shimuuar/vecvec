{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
    LorentzN
  , Lorentz
  , spatialPart
    -- * Boosts
    -- ** Variables
  , Speed(..)
  , Gamma(..)
  , Rapidity(..)
  , Convert(..)
    -- ** Transformations
  , Boost1D(..)
  ) where

import Control.Monad
import Prelude hiding (length,replicate,zipWith,map,foldl,sum)

import Data.Monoid    (Monoid(..))
import Data.Classes.AdditiveGroup
import Data.Classes.VectorSpace

import           Data.Vector.Fixed (Vector,VectorN,Dim,S,N2,N3,N4)
import qualified Data.Vector.Fixed as F
import Data.Vector.Fixed.Unboxed   (Vec,Unbox)


----------------------------------------------------------------
-- Data type
----------------------------------------------------------------

-- | Generic Lorentz vector which could be based on any array-based
--   vector. Parameter /n/ is size of vector.
newtype LorentzG v n a = Lorentz (v n a)

type instance Dim (LorentzG v n) = n

instance (VectorN v n a) => Vector (LorentzG v n) a where
  construct             = fmap Lorentz F.construct
  inspect (Lorentz v) f = F.inspect v f

instance (VectorN v n a) => VectorN (LorentzG v) n a

-- | Lorentz vector which uses unboxed vector as strorage
type LorentzU = LorentzG Vec

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
-- Boosts
----------------------------------------------------------------

-- | Speed in fractions of c
newtype Speed = Speed { getSpeed :: Double }
                 deriving (Show,Eq,Ord)

-- | Gamma factor
newtype Gamma = Gamma { getGamma :: Double }
                deriving (Show,Eq,Ord)

-- | Rapidity
newtype Rapidity = Rapidity { getRapidity :: Double }
                 deriving (Show,Eq,Ord)

instance Monoid Rapidity where
  mempty = Rapidity 0
  mappend (Rapidity a) (Rapidity b) = Rapidity $ a + b


-- | Class for total conversion functions
class Convert a b where
  convert :: a -> b

instance Convert Speed Gamma where
  convert (Speed v)    = Gamma $ 1 / sqrt (1 - v*v)
instance Convert Speed Rapidity where
  convert (Speed v)    = Rapidity $ atanh v
instance Convert Gamma Speed where
  convert (Gamma γ)    = Speed $ sqrt $ (g2 -1) / g2 where g2 = γ*γ
instance Convert Gamma Rapidity where
  convert (Gamma γ)    = Rapidity $ acosh γ
instance Convert Rapidity Speed where
  convert (Rapidity φ) = Speed $ tanh φ
instance Convert Rapidity Gamma where
  convert (Rapidity φ) = Gamma $ cosh φ

-- | Boost for 1+1 space.
class Boost1D a where
  boost1D :: (Vector v Double, Dim v ~ N2)
          =>  a                  -- ^ Boost parameter
          -> v Double -> v Double

instance Boost1D Speed where
  boost1D (Speed v) (F.convert -> (t,x))
    = F.mk2 (γ*(t + v*x))
            (γ*(v*t + x))
    where
      Gamma γ = convert (Speed v)

instance Boost1D Gamma where
  boost1D (Gamma γ) (F.convert -> (t,x))
    = F.mk2 (γ*(t + v*x))
            (γ*(v*t + x))
    where
      Speed v = convert (Gamma γ)

instance Boost1D Rapidity where
  boost1D (Rapidity φ) (F.convert -> (t,x))
    = F.mk2 (c*t + s*x)
            (s*t + c*x)
    where
      c = cosh φ
      s = sinh φ



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
