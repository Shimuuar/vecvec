{-# LANGUAGE TypeFamilies #-}
module Data.VectorSpace.Lorentz ( -- * Lorentz vector
                                  Lorentz(..)
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
                                ) where

import Data.Monoid
import Data.VectorSpace
import Data.VectorSpace.LowDim

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
--   a group.
--
--   It's parametrized by rapidity.
newtype Boost1D a = Boost1D a
                    deriving (Eq,Show,Read)

instance Floating a => Monoid (Boost1D a) where
    mempty = Boost1D 0
    mappend (Boost1D x) (Boost1D y) = Boost1D (x + y)


rapidityBoost1D :: Floating a => a -> Boost1D a
rapidityBoost1D = Boost1D

boost1Drapidity :: Floating a => Boost1D a -> a
boost1Drapidity (Boost1D x) = x

gammaBoost1D :: Floating a => a -> Boost1D a
gammaBoost1D = Boost1D . gammaToRapidity

boost1Dgamma :: Floating a => Boost1D a -> a
boost1Dgamma (Boost1D x) = rapidityToGamma x

vBoost1D :: Floating a => a -> Boost1D a 
vBoost1D = Boost1D . vToRapidity

boost1Dv :: Floating a => Boost1D a -> a
boost1Dv (Boost1D x) = rapidityToV x

-- | Boost along X axis
boostX :: Floating a => Boost1D a -> Lorentz a -> Lorentz a
boostX (Boost1D φ) (Lorentz t x y z) = Lorentz (cosh φ * t + sinh φ * x) (sinh φ * t + cosh φ * x) y z

-- | Boost againist Y axis
boostY :: Floating a => Boost1D a -> Lorentz a -> Lorentz a
boostY (Boost1D φ) (Lorentz t x y z) = Lorentz (cosh φ * t + sinh φ * y) x (sinh φ * t + cosh φ * y) z 

-- | Boost againist Z axis 
boostZ :: Floating a => Boost1D a -> Lorentz a -> Lorentz a
boostZ (Boost1D φ) (Lorentz t x y z) = Lorentz (cosh φ * t + sinh φ * z) x y (sinh φ * t + cosh φ * z)

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
