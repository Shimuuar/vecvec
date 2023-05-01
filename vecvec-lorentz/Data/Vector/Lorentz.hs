{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ViewPatterns               #-}
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
  , splitLorentz
    -- * Constructors
  , fromMomentum
  , fromEnergy
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
  , boostAlong
  , factorLorentz
    -- * Helper functions
  , momentumToE
  ) where

import Control.DeepSeq
import Control.Monad
import Data.Coerce

import           Data.Vector.Fixed (Vector,VectorN,Dim,(!))
import qualified Data.Vector.Fixed      as F
import qualified Data.Vector.Fixed.Cont as FC
import Data.Vector.Fixed.Unboxed   (Vec)
import GHC.TypeLits

import Vecvec.Classes
import Vecvec.Classes.Convert


----------------------------------------------------------------
-- Data type
----------------------------------------------------------------

-- | Generic Lorentz vector which could be based on any array-based
--   vector. Parameter /n/ is size of vector.
newtype LorentzG v (n :: Nat) a = Lorentz (v n a)
  deriving stock   (Show, Eq)
  deriving newtype NFData

type instance Dim (LorentzG v n) = n

instance (VectorN v n a) => Vector (LorentzG v n) a where
  construct             = fmap Lorentz F.construct
  inspect (Lorentz v) f = F.inspect v f

instance (VectorN v n a) => VectorN (LorentzG v) n a

-- | Lorentz vector which uses unboxed vector as strorage
type LorentzU = LorentzG Vec

-- | 2-dimensional Lorentz vector.
type Lorentz2 v = LorentzG v 2

-- | 3-dimensional Lorentz vector.
type Lorentz3 v = LorentzG v 3

-- | 4-dimensional Lorentz vector.
type Lorentz4 v = LorentzG v 4

-- | Unboxed 4-dimensional Lorentz vector
type Lorentz = LorentzU 4


-- | Spatial part of the Lorentz vector.
spatialPart :: (VectorN v n a, VectorN v (n+1) a)
            => LorentzG v (n+1) a -> v n a
spatialPart = F.tail
{-# INLINE spatialPart #-}

-- | Split Lorentz vector.
splitLorentz :: ( VectorN v n a
                , VectorN v (n+1) a
                , 1 <= (n+1)
                )
             => LorentzG v (n+1) a -> (a, v n a)
splitLorentz p = (F.head p, F.tail p)
{-# INLINE splitLorentz #-}

-- | Constrcut energy-momentum vector from mass and momentum of
--   particle
fromMomentum
  :: ( VectorN v n     a
     , VectorN v (n+1) a
     , Floating a
     , NormedScalar a
     , R a ~ a
     )
  => a                          -- ^ Mass of particle
  -> v n a                      -- ^ Momentum
  -> LorentzG v (n+1) a
{-# INLINE fromMomentum #-}
fromMomentum m p
  = F.cons e p
  where
    e = sqrt $ m*m + magnitudeSq (AsFixedVec p)

-- | Construct energy-momentum vector from energy and mass of
--   particle. Obviously wa can't recover direction so we have to use
--   1+1 lorentz vectors. Still it's useful for simple calculations
fromEnergy
  :: (VectorN v 2 a, Floating a)
  => a                          -- ^ Mass of particle
  -> a                          -- ^ Energy of particle
  -> LorentzG v 2 a
{-# INLINE fromEnergy #-}
fromEnergy m e = F.mk2 e (sqrt $ e*e - m*m)


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

instance Num a => Semigroup (Rapidity a) where
  (<>) = coerce ((+) @a)
instance Num a => Monoid (Rapidity a) where
  mempty = Rapidity 0

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
  -- | Invert boost parameter
  invertBoostP :: Num a => b a -> b a


instance BoostParam Speed where
  boost1D (Speed v) (t,x)
    = ( γ*(   t - v*x)
      , γ*(-v*t +   x))
    where
      γ = abs $ getGamma $ convert (Speed v)
  {-# INLINE boost1D #-}
  invertBoostP = Speed . negate . getSpeed

instance BoostParam Gamma where
  boost1D (Gamma γ) (t,x)
    = ( abs γ*(   t - v*x)
      , abs γ*(-v*t +   x))
    where
      Speed v = convert (Gamma γ)
  {-# INLINE boost1D #-}
  invertBoostP = Gamma . negate . getGamma

instance BoostParam Rapidity where
  boost1D (Rapidity φ) (t,x)
    = ( c*t - s*x
      ,-s*t + c*x)
    where
      c = cosh φ
      s = sinh φ
  {-# INLINE boost1D #-}
  invertBoostP = Rapidity . negate . getRapidity

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
boostX :: (BoostParam b, VectorN v n a, Floating a, 2 <= n)
       => b a                -- ^ Boost parameter
       -> LorentzG v n a
       -> LorentzG v n a
{-# INLINE boostX #-}
boostX b = boostAxis b 0

-- | Boost along X axis.
boostY :: (BoostParam b, VectorN v n a, Floating a, 3 <= n)
       => b a                -- ^ Boost parameter
       -> LorentzG v n a
       -> LorentzG v n a
{-# INLINE boostY #-}
boostY b = boostAxis b 1

-- | Boost along X axis.
boostZ :: (BoostParam b, VectorN v n a, Floating a, 4 <= n)
       => b a                -- ^ Boost parameter
       -> LorentzG v n a
       -> LorentzG v n a
{-# INLINE boostZ #-}
boostZ b = boostAxis b 2


-- | Boost along arbitrary direction
boostAlong
  :: ( VectorN v (n+1) a, VectorN v n a
     , BoostParam b
     , InnerSpace (v n a), Scalar (v n a) ~ a, Floating a
     , 1 <= n+1
     , R a ~ a
     )
  => b a                        -- ^ Boost parameter
  -> v n a                      -- ^ Vector along which boost should
                                --   be performed. Need not to be normalized.
  -> LorentzG v (n+1) a
  -> LorentzG v (n+1) a
{-# INLINE boostAlong #-}
boostAlong b k p
  = F.cons t' (proj'*.n .+. xPerp)
  where
    -- Split vector into spatial
    (t,x) = splitLorentz p
    proj  = x <.> n
    xPerp = x .-. proj*.n
    -- boost boost
    (t',proj') = boost1D b (t,proj)
    -- Normalized direction
    n = k ./ magnitude k

-- | Factor Lorentz vector into vector with zero spatial part and
--   Lorentz transformation
--
-- > let (m,f) = factorLorentz v
-- > f (m,0,0,0) == v
factorLorentz :: Lorentz Double -> (Double, Lorentz Double -> Lorentz Double)
factorLorentz v
  = (m, boostAlong (Gamma (e/m)) p)
  where
    m     = magnitude    v
    (e,p) = splitLorentz v


----------------------------------------------------------------
-- Instances
----------------------------------------------------------------

deriving via (AsFixedVec (LorentzG v n) a)
    instance (VectorN v n a, Num a) => AdditiveSemigroup (LorentzG v n a)
deriving via (AsFixedVec (LorentzG v n) a)
    instance (VectorN v n a, Num a) => AdditiveMonoid (LorentzG v n a)
deriving via (AsFixedVec (LorentzG v n) a)
    instance (VectorN v n a, Num a) => AdditiveQuasigroup (LorentzG v n a)
deriving via (AsFixedVec (LorentzG v n) a)
    instance (VectorN v n a, Num a) => VectorSpace (LorentzG v n a)

instance (VectorN v n a, NormedScalar a) => InnerSpace (LorentzG v n a) where
  v <.> u = F.sum $ F.izipWith minkovsky v u
    where
      minkovsky 0 x y =   conjugate x * y
      minkovsky _ x y = -(conjugate x * y)
  magnitudeSq = FC.sum . FC.imap minkovsky . FC.cvec
    where
      minkovsky 0 x = scalarNormSq x
      minkovsky _ x = -(scalarNormSq x)
  {-# INLINE (<.>)       #-}
  {-# INLINE magnitudeSq #-}


----------------------------------------------------------------
-- Helpers
----------------------------------------------------------------

-- | Convert module of momentum to energy if particle mass is known.
momentumToE :: Double -> Double -> Double
momentumToE m p = sqrt $ m*m + p*p
