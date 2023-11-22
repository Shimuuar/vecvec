{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
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
-- This library contains implementation of operations on Lorentz
-- vectors including boosts.
module Data.Vector.Lorentz (
    -- * Data type
    LorentzG(..)
  , LorentzCV
  , Lorentz2
  , Lorentz3
  , Lorentz4
    -- ** Constructors
  , fromMomentum
    -- ** Change of representation
  , changeLorentzRep
  , toLorentzCV
  , fromLorentzCV
    -- ** Inspection
  , spatialPart
  , splitLorentz
    -- * Boosts
    -- ** Variables
  , Speed(..)
  , Gamma(..)
  , Rapidity(..)
    -- ** Boosts
  , BoostParam(..)
  , boostAxis
  , boostX
  , boostY
  , boostZ
  , boostAlong
  -- , factorLorentz
    -- * Reexports
  , Convert(..)
  ) where

import Control.DeepSeq
import Data.Coerce
import Data.Proxy

import           Data.Vector.Fixed (Vector,Dim,(!))
import qualified Data.Vector.Fixed      as F
import qualified Data.Vector.Fixed.Cont as FC
import Data.Vector.Fixed.Unboxed   (Vec)
import GHC.TypeLits
import GHC.Records

import Vecvec.Classes
import Vecvec.Classes.Convert


----------------------------------------------------------------
-- Data type
----------------------------------------------------------------

-- | @1+n@-dimensional Lorentz vectors. First element of vector is
--   time-like and rest are time-like. Metric is @(+1,-1,-1,-1)@.
--
--   Internally it's represented by type @v@ which should have
--   'Vector' instance.
--
--   As an experiment many values corresponding to vector are
--   accessible using @OverloadedRecordDot@.
newtype LorentzG v a = Lorentz (v a)
  deriving stock   (Show, Eq)
  deriving newtype NFData

type instance Dim (LorentzG v) = Dim v

instance (Vector v a) => Vector (LorentzG v) a where
  construct             = fmap Lorentz F.construct
  inspect (Lorentz v) f = F.inspect v f


-- | Energy of energy-momentum vector.
instance (Vector v a, 1 <= Dim v) => HasField "energy" (LorentzG v a) a where
  getField = flip F.index (Proxy @0)
  {-# INLINE getField #-}
-- | @x@ component of momentum (same as @x@),
instance (Vector v a, 2 <= Dim v) => HasField "px" (LorentzG v a) a where
  getField = flip F.index (Proxy @1)
  {-# INLINE getField #-}
-- | @y@ component of momentum (same as @y@).
instance (Vector v a, 3 <= Dim v) => HasField "py" (LorentzG v a) a where
  getField = flip F.index (Proxy @2)
  {-# INLINE getField #-}
-- | @z@ component of momentum (same as @z@)
instance (Vector v a, 4 <= Dim v) => HasField "pz" (LorentzG v a) a where
  getField = flip F.index (Proxy @3)
  {-# INLINE getField #-}

-- | Time, if vector is treated as coordinate
instance (Vector v a, 1 <= Dim v) => HasField "t" (LorentzG v a) a where
  getField = flip F.index (Proxy @0)
  {-# INLINE getField #-}
-- | @x@ coordinate (same as @px@),
instance (Vector v a, 2 <= Dim v) => HasField "x" (LorentzG v a) a where
  getField = flip F.index (Proxy @1)
  {-# INLINE getField #-}
-- | @y@ coordinate (same as @py@).
instance (Vector v a, 3 <= Dim v) => HasField "y" (LorentzG v a) a where
  getField = flip F.index (Proxy @2)
  {-# INLINE getField #-}
-- | @z@ coordinate (same as @pz@)
instance (Vector v a, 4 <= Dim v) => HasField "z" (LorentzG v a) a where
  getField = flip F.index (Proxy @3)
  {-# INLINE getField #-}



-- | Lorentz vector which uses 'F.ContVec' as representation. It's
--   advisable to perform calculations using this type since underlying
--   @ByteArray@ is impenetrable array for GHC optimizer.
type LorentzCV n = LorentzG (F.ContVec n)

-- | 2-dimensional (1+1) Lorentz vector. Unboxed 'Vec' is used as a
--   representation.
type Lorentz2 = LorentzG (Vec 2)

-- | 3-dimensional (1+2) Lorentz vector. Unboxed 'Vec' is used as a
--   representation.
type Lorentz3 = LorentzG (Vec 3)

-- | 4-dimensional (1+3) Lorentz vector. Unboxed 'Vec' is used as a
--   representation.
type Lorentz4 = LorentzG (Vec 4)



-- | Spatial part of the Lorentz vector.
spatialPart :: (Vector v a, Vector p a, Dim v ~ Dim p + 1)
            => LorentzG v a -> p a
spatialPart = F.tail
{-# INLINE spatialPart #-}

-- | Split Lorentz vector into temporal and spatial part.
splitLorentz :: ( Vector v a, Vector p a, Dim v ~ Dim p + 1, 1 <= Dim v
                )
             => LorentzG v a -> (a, p a)
splitLorentz p = (F.head p, F.tail p)
{-# INLINE splitLorentz #-}


-- | Change representation of Lorentz vector
changeLorentzRep :: (Vector v a, Vector w a, Dim v ~ Dim w) => LorentzG v a -> LorentzG w a
{-# INLINE changeLorentzRep #-}
changeLorentzRep (Lorentz v) = Lorentz (F.convert v)

-- | Switch to 'F.ContVec' as representation.
toLorentzCV :: (Vector v a) => LorentzG v a -> LorentzCV (Dim v) a
{-# INLINE toLorentzCV #-}
toLorentzCV (Lorentz v) = Lorentz (F.cvec v)

-- | Switch from 'F.ContVec' as representation
fromLorentzCV :: (Vector v a) => LorentzCV (Dim v) a -> LorentzG v a
{-# INLINE fromLorentzCV #-}
fromLorentzCV (Lorentz v) = Lorentz (F.vector v)


-- | Construct energy-momentum vector from mass and momentum of
--   particle.
fromMomentum
  :: ( Vector v a
     , Vector p a
     , Dim v ~ Dim p + 1
     , Floating a
     , NormedScalar a
     , R a ~ a
     )
  => a                          -- ^ Mass of particle \(m\)
  -> p a                      -- ^ Spatial momentum \(p\)
  -> LorentzG v a
{-# INLINE fromMomentum #-}
fromMomentum m p
  = F.cons e p
  where
    e = sqrt $ m*m + magnitudeSq (AsFixedVec p)

-- -- | Construct energy-momentum vector from energy and mass of
-- --   particle. Obviously wa can't recover direction so we have to use
-- --   1+1 lorentz vectors. Still it's useful for simple calculations
-- fromEnergy
--   :: (VectorN v 2 a, Floating a)
--   => a                          -- ^ Mass of particle
--   -> a                          -- ^ Energy of particle
--   -> LorentzG v 2 a
-- {-# INLINE fromEnergy #-}
-- fromEnergy m e = F.mk2 e (sqrt $ e*e - m*m)


----------------------------------------------------------------
-- Boost variables
----------------------------------------------------------------

-- | Speed of particle as fraction of \(c\).
newtype Speed a = Speed { getSpeed :: a }
                 deriving (Show,Eq,Ord,Functor)

-- | Gamma factor. Negative values of gamma factor are accepted and
--   means then boost will be performed in opposite direction.
newtype Gamma a = Gamma { getGamma :: a }
                deriving (Show,Eq,Ord,Functor)

-- | Rapidity
newtype Rapidity a = Rapidity { getRapidity :: a }
                 deriving (Show,Eq,Ord,Functor)

instance Num a => Semigroup (Rapidity a) where
  (<>) = coerce ((+) @a)
instance Num a => Monoid (Rapidity a) where
  mempty = Rapidity 0

instance (Floating a, Ord a, a ~ a') => Convert (Speed a) (Gamma a') where
  convert (Speed v) | v >= 0    = Gamma gam
                    | otherwise = Gamma (-gam)
    where gam = 1 / sqrt (1 - v*v)

instance (Floating a, a ~ a') => Convert (Speed a) (Rapidity a) where
  convert (Speed v)    = Rapidity $ atanh v

instance (Floating a, Ord a, a ~ a') => Convert (Gamma a) (Speed a') where
  convert (Gamma γ)
    | γ > 0     = Speed v
    | otherwise = Speed (-v)
    where
      v  = sqrt ((γ2 - 1) / γ2)
      γ2 = γ*γ

instance (Floating a, Ord a, a ~ a') => Convert (Gamma a) (Rapidity a') where
  convert (Gamma γ)
    | γ > 0     = Rapidity φ
    | otherwise = Rapidity (-φ)
    where
      φ = acosh (abs γ)

instance (Floating a, a ~ a')  => Convert (Rapidity a) (Speed a') where
  convert (Rapidity φ) = Speed $ tanh φ

instance (Floating a, Ord a, a ~ a') => Convert (Rapidity a) (Gamma a') where
  convert (Rapidity φ)
    | φ >= 0    = Gamma $ cosh φ
    | otherwise = Gamma $ negate $ cosh φ



----------------------------------------------------------------
-- Boost
----------------------------------------------------------------

-- | Boost for 1+1 Minkowski space. @b@ is variable which is used for
-- boost parameterization.
--
-- \[\left\{\begin{aligned}
-- t' &= \gamma(t - vx) \\
-- x' &= \gamma(-vt + x) \\
-- \end{aligned}\right.\]
--
-- ===__Examples__
--
-- >>> boost1D (Gamma 2) (1,0)
-- (2.0,-1.7320508075688772)
--
-- >>> boost1D (Speed 0.25) (1,0)
-- (1.0327955589886444,-0.2581988897471611)
--
-- >>> boost1D (Rapidity 2) (1,0)
-- (3.7621956910836314,-3.626860407847019)
class BoostParam b where
  -- | Boost for @(t,x)@ pair.
  boost1D :: (Floating a, Ord a)
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
  invertBoostP = Speed . negate . getSpeed
  {-# INLINE boost1D      #-}
  {-# INLINE invertBoostP #-}

instance BoostParam Gamma where
  boost1D (Gamma γ) (t,x)
    | γ >= 0    = (  γ*(   t - v*x)
                  ,  γ*(-v*t +   x))
    | otherwise = ( -γ*(   t + v*x)
                  , -γ*( v*t +   x))
    where
      γ2 = γ * γ
      v  = sqrt ((γ2 - 1) / γ2)
  invertBoostP = Gamma . negate . getGamma
  {-# INLINE boost1D      #-}
  {-# INLINE invertBoostP #-}

instance BoostParam Rapidity where
  boost1D (Rapidity φ) (t,x)
    = (  c*t - s*x
      , -s*t + c*x)
    where
      c = cosh φ
      s = sinh φ
  invertBoostP = Rapidity . negate . getRapidity
  {-# INLINE boost1D      #-}
  {-# INLINE invertBoostP #-}

-- | Boost along axis. Will throw @error@ if axis index is out of range.
boostAxis :: (BoostParam b, Vector v a, Floating a, Ord a)
          => b a                -- ^ Boost parameter
          -> Int                -- ^ Axis number. /X/-0, /Y/-1 ...
          -> LorentzG v a
          -> LorentzG v a
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
boostX :: (BoostParam b, Vector v a, Floating a, Ord a, 2 <= Dim v)
       => b a                -- ^ Boost parameter
       -> LorentzG v a
       -> LorentzG v a
{-# INLINE boostX #-}
boostX b = boostAxis b 0

-- | Boost along X axis.
boostY :: (BoostParam b, Vector v a, Floating a, Ord a, 3 <= Dim v)
       => b a                -- ^ Boost parameter
       -> LorentzG v a
       -> LorentzG v a
{-# INLINE boostY #-}
boostY b = boostAxis b 1

-- | Boost along X axis.
boostZ :: (BoostParam b, Vector v a, Floating a, Ord a, 4 <= Dim v)
       => b a                -- ^ Boost parameter
       -> LorentzG v a
       -> LorentzG v a
{-# INLINE boostZ #-}
boostZ b = boostAxis b 2


-- | Boost along arbitrary direction
boostAlong
  :: ( Vector v a, Vector p a, Dim v ~ Dim p + 1, 1 <= Dim v
     , BoostParam b
     , NormedScalar a, Floating a, Ord a
     , R a ~ a
     )
  => b a               -- ^ Boost parameter
  -> p a               -- ^ Vector along which boost should be
                       --   performed. Need not to be normalized.
  -> LorentzG v a
  -> LorentzG v a
{-# INLINE boostAlong #-}
boostAlong b (F.cvec -> k) (toLorentzCV -> p)
  = Lorentz
  $ F.vector
  $ F.cons t' (proj'*.n .+. x_perp)
  where
    -- Split vector into spatial and temporal parts and further
    -- decompose spatial part into parallel and perpendicular
    -- components
    (t,x)  = splitLorentz p
    proj   = x <.> n
    x_perp = x .-. proj*.n
    -- Perform boost
    (t',proj') = boost1D b (t,proj)
    n = k ./ magnitude k


-- -- | Factor Lorentz vector into vector with zero spatial part and
-- --   Lorentz transformation
-- --
-- -- > let (m,f) = factorLorentz v
-- -- > f (m,0,0,0) == v
-- factorLorentz :: Lorentz Double -> (Double, Lorentz Double -> Lorentz Double)
-- factorLorentz v
--   = (m, boostAlong (Gamma (e/m)) p)
--   where
--     m     = magnitude    v
--     (e,p) = splitLorentz v


----------------------------------------------------------------
-- Instances
----------------------------------------------------------------

deriving via (AsFixedVec (LorentzG v) a)
    instance (Vector v a, Num a) => AdditiveSemigroup (LorentzG v a)
deriving via (AsFixedVec (LorentzG v) a)
    instance (Vector v a, Num a) => AdditiveMonoid (LorentzG v a)
deriving via (AsFixedVec (LorentzG v) a)
    instance (Vector v a, Num a) => AdditiveQuasigroup (LorentzG v a)
deriving via (AsFixedVec (LorentzG v) a)
    instance (Vector v a, Num a) => VectorSpace (LorentzG v a)

instance (Vector v a, NormedScalar a) => InnerSpace (LorentzG v a) where
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
