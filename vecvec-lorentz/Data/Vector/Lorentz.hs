{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
-- |
-- Implementation of Lorentz vectors and boosts.
module Data.Vector.Lorentz (
    -- * Data type
    LorentzG(..)
  , LorentzCV
  , LorentzU
  , Lorentz2
  , Lorentz3
  , Lorentz4
  , Lorentz
    -- ** Constructors
  , fromMomentum
  , fromEnergy
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
  , factorLorentz
    -- * Helper functions
  , momentumToE
    -- * Reexports
  , Convert(..)
  ) where

import Control.DeepSeq
import Data.Coerce
import Data.Proxy
import Data.Vector.Fixed         (Vector,VectorN,Dim,(!))
import Data.Vector.Fixed         qualified as F
import Data.Vector.Fixed.Cont    qualified as FC
import Data.Vector.Fixed.Unboxed (Vec)
import GHC.TypeLits

import Vecvec.Classes
import Vecvec.Classes.Containers (Convert(..))
import Vecvec.Classes.Geometry


----------------------------------------------------------------
-- Data type
----------------------------------------------------------------

-- | @n@-dimensional Lorentz vectors. By convention @0@ element of
--   vector is time-like and rest @n-1@ are space-like. Metric is
--   @(+1,-1,-1,-1)@.
--
--   @v@ is type of vector used for representation.
newtype LorentzG v (n :: Nat) a = Lorentz (v n a)
  deriving stock   (Show, Eq)
  deriving newtype NFData

type instance Dim (LorentzG v n) = n

instance (VectorN v n a) => Vector (LorentzG v n) a where
  construct             = fmap Lorentz F.construct
  inspect (Lorentz v) f = F.inspect v f

instance (VectorN v n a) => VectorN (LorentzG v) n a

-- | Lorentz vector which uses unboxed vector for storage
type LorentzU = LorentzG Vec

-- | Lorentz vector which uses 'F.ContVec' as representation. It's
--   advisable to perform calculations using this type since underlying
--   @ByteArray@ is impenetrable array for GHC optimizer.
type LorentzCV = LorentzG F.ContVec

-- | 2-dimensional (1+1) Lorentz vector.
type Lorentz2 v = LorentzG v 2

-- | 3-dimensional (1+2) Lorentz vector.
type Lorentz3 v = LorentzG v 3

-- | 4-dimensional (1+3) Lorentz vector.
type Lorentz4 v = LorentzG v 4

-- | Unboxed 4-dimensional Lorentz vector.
type Lorentz = LorentzU 4


-- | Spatial part of the Lorentz vector.
spatialPart :: (VectorN v n a, VectorN v (n+1) a)
            => LorentzG v (n+1) a -> v n a
spatialPart = F.tail
{-# INLINE spatialPart #-}

-- | Split Lorentz vector into temporal and spatial part.
splitLorentz :: ( VectorN v n a
                , VectorN v (n+1) a
                , 1 <= (n+1)
                )
             => LorentzG v (n+1) a -> (a, v n a)
splitLorentz p = (F.head p, F.tail p)
{-# INLINE splitLorentz #-}


-- | Change representation of Lorentz vector
changeLorentzRep :: (VectorN v n a, VectorN w n a) => LorentzG v n a -> LorentzG w n a
{-# INLINE changeLorentzRep #-}
changeLorentzRep (Lorentz v) = Lorentz (F.convert v)

-- | Switch to 'F.ContVec' as representation.
toLorentzCV :: (VectorN v n a) => LorentzG v n a -> LorentzCV n a
{-# INLINE toLorentzCV #-}
toLorentzCV (Lorentz v) = Lorentz (F.cvec v)

-- | Switch from 'F.ContVec' as representation
fromLorentzCV :: (VectorN v n a) => LorentzCV n a -> LorentzG v n a
{-# INLINE fromLorentzCV #-}
fromLorentzCV (Lorentz v) = Lorentz (F.vector v)


-- | Construct energy-momentum vector from mass and momentum of
--   particle.
fromMomentum
  :: ( VectorN v n     a
     , VectorN v (n+1) a
     , Floating a
     , NormedScalar a
     , R a ~ a
     )
  => a                          -- ^ Mass of particle \(m\)
  -> v n a                      -- ^ Spatial momentum \(p\)
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
  -- | Lorentz transformation for @(t,x)@ pair.
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
  {-# INLINE boost1D #-}
  invertBoostP = Speed . negate . getSpeed

instance BoostParam Gamma where
  boost1D (Gamma γ) (t,x)
    | γ >= 0    = (  γ*(   t - v*x)
                  ,  γ*(-v*t +   x))
    | otherwise = ( -γ*(   t + v*x)
                  , -γ*( v*t +   x))
    where
      γ2 = γ * γ
      v  = sqrt ((γ2 - 1) / γ2)
  {-# INLINE boost1D #-}
  invertBoostP = Gamma . negate . getGamma

instance BoostParam Rapidity where
  boost1D (Rapidity φ) (t,x)
    = (  c*t - s*x
      , -s*t + c*x)
    where
      c = cosh φ
      s = sinh φ
  {-# INLINE boost1D #-}
  invertBoostP = Rapidity . negate . getRapidity

-- | Boost along axis
boostAxis :: (BoostParam b, VectorN v n a, Floating a, Ord a)
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
boostX :: (BoostParam b, VectorN v n a, Floating a, Ord a, 2 <= n)
       => b a                -- ^ Boost parameter
       -> LorentzG v n a
       -> LorentzG v n a
{-# INLINE boostX #-}
boostX b = boostAxis b 0

-- | Boost along X axis.
boostY :: (BoostParam b, VectorN v n a, Floating a, Ord a, 3 <= n)
       => b a                -- ^ Boost parameter
       -> LorentzG v n a
       -> LorentzG v n a
{-# INLINE boostY #-}
boostY b = boostAxis b 1

-- | Boost along X axis.
boostZ :: (BoostParam b, VectorN v n a, Floating a, Ord a, 4 <= n)
       => b a                -- ^ Boost parameter
       -> LorentzG v n a
       -> LorentzG v n a
{-# INLINE boostZ #-}
boostZ b = boostAxis b 2


-- | Boost along arbitrary direction
boostAlong
  :: ( VectorN v (n+1) a, VectorN v n a, 1 <= n+1
     , BoostParam b
     , NormedScalar a, Floating a, Ord a
     , R a ~ a
     )
  => b a                        -- ^ Boost parameter
  -> v n a                      -- ^ Vector along which boost should
                                --   be performed. Need not to be normalized.
  -> LorentzG v (n+1) a
  -> LorentzG v (n+1) a
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

instance (2 <= n, n <= 4, F.VectorN v n a) => FieldX a (LorentzG v n a) where
  _X   = F.elementTy  (Proxy @1)
  getX = flip F.index (Proxy @1)
  {-# INLINE _X   #-}
  {-# INLINE getX #-}
instance (3 <= n, n <= 4, F.VectorN v n a) => FieldY a (LorentzG v n a) where
  _Y   = F.elementTy  (Proxy @2)
  getY = flip F.index (Proxy @2)
  {-# INLINE _Y   #-}
  {-# INLINE getY #-}  
instance (4 <= n, n <= 4, F.VectorN v n a) => FieldZ a (LorentzG v n a) where
  _Z   = F.elementTy  (Proxy @3)
  getZ = flip F.index (Proxy @3)
  {-# INLINE _Z   #-}
  {-# INLINE getZ #-}  

----------------------------------------------------------------
-- Helpers
----------------------------------------------------------------

-- | Convert module of momentum to energy if particle mass is known.
momentumToE :: Double -> Double -> Double
momentumToE m p = sqrt $ m*m + p*p
