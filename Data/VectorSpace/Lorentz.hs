{-# LANGUAGE TypeFamilies #-}
module Data.VectorSpace.Lorentz ( Lorentz(..)
                                , vToGamma
                                , gammaToV
                                -- * Boosts 
                                , boostX
                                , boostY
                                , boostZ
                                , boost
                                ) where

import Data.VectorSpace
import Data.VectorSpace.LowDim

-- | Lorentz vector
data Lorentz a = Lorentz a a a a
                 deriving (Eq, Show)

instance Num a => AdditiveGroup (Lorentz a) where
    zeroV = Lorentz 0 0 0 0
    (Lorentz t1 x1 y1 z1) ^+^ (Lorentz t2 x2 y2 z2) = Lorentz (t1+t2) (x1+x2) (y1+y2) (z1+z2)
    negateV (Lorentz t x y z) = Lorentz (-t) (-x) (-y) (-z)

instance Num a => VectorSpace (Lorentz a) where 
    type Scalar (Lorentz a) = a 
    a *^ (Lorentz t x y z) = Lorentz (a*t) (a*x) (a*y) (a*z)

instance Num a => InnerSpace (Lorentz a) where 
    (Lorentz t1 x1 y1 z1) <.> (Lorentz t2 x2 y2 z2) = t1*t2 - x1*x2 - y1*y2 - z1*z2


-- | Convert speed in speed of light units to gamma factor
vToGamma :: Floating a => a -> a
vToGamma v = 1 / sqrt( 1 - v**2 )

-- | Convert gamma factor to speed 
gammaToV :: Floating a => a -> a
gammaToV g = sqrt( (g**2 - 1)/ g**2 )

-- | boost along X axis
boostX :: Floating a => a -> Lorentz a -> Lorentz a
boostX gamma (Lorentz t x y z) = Lorentz (gamma*(t + v*x)) (gamma*(x + v*t)) y z
    where v = gammaToV gamma

-- | boost againist Y axis
boostY :: Floating a => a -> Lorentz a -> Lorentz a
boostY gamma (Lorentz t x y z) = Lorentz (gamma*(t + v*y)) x (gamma*(y + v*t)) z
    where v = gammaToV gamma

-- | Boost againist Z axis 
boostZ :: Floating a => a -> Lorentz a -> Lorentz a
boostZ gamma (Lorentz t x y z) = Lorentz (gamma*(t + v*z)) x y (gamma*(z + v*t)) 
    where v = gammaToV gamma

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

