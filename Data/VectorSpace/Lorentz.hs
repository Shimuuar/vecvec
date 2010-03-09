{-# LANGUAGE TypeFamilies #-}
module Data.VectorSpace.Lorentz ( -- * Lorentz vector
                                  Lorentz(..)
                                , spatialPart
                                , spatialMag
                                , spatialMagSq
                                -- ** Convert between rapidity, gamma factor & speed
                                , vToRapidity
                                , vToGamma
                                , rapidityToGamma
                                , rapidityToV
                                , gammaToV
                                , gammaToRapidity
                                -- * Boosts 
                                -- ** One dimesional boosts
                                , Boost1D
                                -- *** Constructors & deconstructors
                                , rapidityBoost1D
                                , boost1Drapidity
                                , gammaBoost1D
                                , boost1Dgamma
                                , vBoost1D
                                , boost1Dv
                                -- *** Perform boosts on lorentz vectors
                                , boostX
                                , boostY
                                , boostZ
                                , boost1D
                                , boost1Dn
                                ) where

import Data.Monoid
import Data.VectorSpace
import Data.VectorSpace.Spatial

----------------------------------------------------------------
-- Lorentz vectors
----------------------------------------------------------------

-- | Lorentz vector
data Lorentz a = Lorentz { lorentzT :: a
                         , lorentzX :: a
                         , lorentzY :: a
                         , lorentzZ :: a
                         }
                 deriving (Eq, Show, Read)

-- | Spatial part of Lorentz vector
spatialPart :: Lorentz a -> Vec3D a
spatialPart (Lorentz _ x y z) = Vec3D x y z

-- | Spatial part of Lorentz vector
spatialMagSq :: Num a => Lorentz a -> a
spatialMagSq (Lorentz _ x y z) = x*x + y*y + z*z

-- | Spatial part of Lorentz vector
spatialMag :: Floating a => Lorentz a -> a
spatialMag = sqrt . spatialMagSq

instance Num a => AdditiveGroup (Lorentz a) where
    zeroV = Lorentz 0 0 0 0
    (Lorentz t1 x1 y1 z1) ^+^ (Lorentz t2 x2 y2 z2) = Lorentz (t1+t2) (x1+x2) (y1+y2) (z1+z2)
    negateV (Lorentz t x y z) = Lorentz (-t) (-x) (-y) (-z)

instance Num a => VectorSpace (Lorentz a) where 
    type Scalar (Lorentz a) = a 
    a *^ (Lorentz t x y z) = Lorentz (a*t) (a*x) (a*y) (a*z)

instance Num a => InnerSpace (Lorentz a) where 
    (Lorentz t1 x1 y1 z1) <.> (Lorentz t2 x2 y2 z2) = t1*t2 - x1*x2 - y1*y2 - z1*z2


-- | Convert speed to rapidity
vToRapidity :: Floating a => a -> a
vToRapidity = atanh

-- | Convert speed in speed of light units to gamma factor
vToGamma :: Floating a => a -> a
vToGamma v = 1 / sqrt( 1 - v**2 )

-- | Convert rapidity to gamma factor
rapidityToGamma :: Floating a => a -> a
rapidityToGamma = cosh

-- | Convert rapidity to speed
rapidityToV :: Floating a => a -> a
rapidityToV = tanh

-- | Convert gamma factor to speed 
gammaToV :: Floating a => a -> a
gammaToV g = sqrt( (g**2 - 1)/ g**2 )

-- | Convert gamma factor to rapidity
gammaToRapidity :: Floating a => a -> a
gammaToRapidity = acosh


----------------------------------------------------------------
-- 1D Boosts
----------------------------------------------------------------

-- | One dimensinal boost. It deserve specian treatment since it forms
--   a group. Internally it's parametrized by rapidity.
newtype Boost1D a = Boost1D a
                    deriving (Eq,Show,Read)

instance Floating a => Monoid (Boost1D a) where
    mempty = Boost1D 0
    mappend (Boost1D x) (Boost1D y) = Boost1D (x + y)


-- | Create 1D boost from rapidity
rapidityBoost1D :: Floating a => a -> Boost1D a
rapidityBoost1D = Boost1D

-- | Get rapidity from 1D boost
boost1Drapidity :: Floating a => Boost1D a -> a
boost1Drapidity (Boost1D x) = x

-- | construct boost from gamma factor
gammaBoost1D :: Floating a => a -> Boost1D a
gammaBoost1D = Boost1D . gammaToRapidity

-- | Get gamma factor which correspond to boost
boost1Dgamma :: Floating a => Boost1D a -> a
boost1Dgamma (Boost1D x) = rapidityToGamma x

-- | Construct boost from speed 
vBoost1D :: Floating a => a -> Boost1D a 
vBoost1D = Boost1D . vToRapidity

-- | Get speed
boost1Dv :: Floating a => Boost1D a -> a
boost1Dv (Boost1D x) = rapidityToV x

-- | Boost along X axis
boostX :: Floating a => Boost1D a -> Lorentz a -> Lorentz a
boostX (Boost1D φ) (Lorentz t x y z) = Lorentz (cosh φ * t + sinh φ * x) (sinh φ * t + cosh φ * x) y z

-- | Boost along Y axis
boostY :: Floating a => Boost1D a -> Lorentz a -> Lorentz a
boostY (Boost1D φ) (Lorentz t x y z) = Lorentz (cosh φ * t + sinh φ * y) x (sinh φ * t + cosh φ * y) z 

-- | Boost along Z axis 
boostZ :: Floating a => Boost1D a -> Lorentz a -> Lorentz a
boostZ (Boost1D φ) (Lorentz t x y z) = Lorentz (cosh φ * t + sinh φ * z) x y (sinh φ * t + cosh φ * z)

-- | Boost along arbitrary axis
boost1D :: Floating a => Boost1D a -> Vec3D a -> Lorentz a -> Lorentz a
boost1D b v = boost1Dn b (v ^/ magnitude v)

-- | Boost along arbitrary axis. Same as boost1D but direction vector
--   is assumed to be normalized. It isn't checked
boost1Dn :: Floating a => Boost1D a -> Vec3D a -> Lorentz a -> Lorentz a
boost1Dn (Boost1D φ) n v@(Lorentz t _ _ _) = 
    let p     = spatialPart v
        pMod  = n <.> p
        (Vec3D x' y' z') = p ^+^ ((ch*pMod + sh*t - pMod) *^ n)
        ch = cosh φ
        sh = sinh φ
    in Lorentz (ch*t + sh*pMod) x' y' z'


----------------------------------------------------------------
-- Generi boosts
----------------------------------------------------------------

{-
boost :: Floating a => Vec3D a -> Lorentz a -> Lorentz a 
boost l (Lorentz t x y z) =
    let gamma  = magnitude l
        gamma1 = gamma - 1 

        v     = gammaToV gamma
        (Vec3D nx ny nz) = l ^/ gamma 
        t' = gamma*(t + v*(nx*x + ny*y + nz*z))
        x' = gamma*v*nx*t + (gamma1*nx*nx + 1)*x + gamma1*nx*ny*y       + gamma1*nx*nz*z
        y' = gamma*v*ny*t + gamma1*nx*ny*x       + (gamma1*ny*ny + 1)*y + gamma1*ny*nz*z
        z' = gamma*v*nz*t + gamma1*nx*nz*x       + gamma1*ny*nz*y       + (gamma1*nz*nz + 1)*z
    in Lorentz t' x' y' z'

boostRap :: Floating a => Vec3D a -> Lorentz a -> Lorentz a  
boostRap rap = let r = magnitude rap
                   g = rapidityToGamma r
               in boost ((g/r) *^ rap)
-}
