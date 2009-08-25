{-# LANGUAGE TypeFamilies #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Data.VectorSpace.LowDim
-- Copyright   : Alexey Khudyakov
-- License     : BSD3 (see LICENSE)
-- 
-- Maintainer  : Alexey Khudyakov <alexey.skladnoy@gmail.com>
-- Stability   : experimental
-- Portability : Unportable
--
-- Low dimensional vectors (dimenstions 2 and 3) based on vector-space package
--
-----------------------------------------------------------------------------


module Data.VectorSpace.LowDim ( -- * Vector data types
                                 Vec2D(..)
                               , Vec3D(..)
                               -- * Unit vectors
                               , unitX2D
                               , unitY2D
                               , unitX3D
                               , unitY3D
                               , unitZ3D 
                               ) where

import Data.VectorSpace 


-- | 2-dimensional vector.
data Vec2D a = Vec2D a a 
               deriving (Show,Eq)

instance Num a => AdditiveGroup (Vec2D a) where
    zeroV = Vec2D 0 0 
    (Vec2D x1 y1) ^+^ (Vec2D x2 y2) = Vec2D (x1+x2) (y1+y2)
    negateV (Vec2D x y) = Vec2D (-x) (-y)
instance Num a => VectorSpace (Vec2D a) where 
    type Scalar (Vec2D a) = a 
    a *^ (Vec2D x y) = Vec2D (a*x) (a*y)
instance Num a => InnerSpace (Vec2D a) where
    (Vec2D x1 y1) <.> (Vec2D x2 y2) = x1*x2 + y1*y2


-- | 3-dimensional vector.
data Vec3D a = Vec3D a a a
               deriving (Show,Eq)

instance Num a => AdditiveGroup (Vec3D a) where
    zeroV = Vec3D 0 0 0
    (Vec3D x1 y1 z1) ^+^ (Vec3D x2 y2 z2) = Vec3D (x1+x2) (y1+y2) (z1+z2)
    negateV (Vec3D x y z) = Vec3D (-x) (-y) (-z)
instance Num a => VectorSpace (Vec3D a) where 
    type Scalar (Vec3D a) = a 
    a *^ (Vec3D x y z) = Vec3D (a*x) (a*y) (a*z)
instance Num a => InnerSpace (Vec3D a) where
    (Vec3D x1 y1 z1) <.> (Vec3D x2 y2 z2) = x1*x2 + y1*y2 + z1*z2


-- | Unit 2D vector pointing along X axis
unitX2D :: Num a => Vec2D a 
unitX2D = Vec2D 1 0 

-- | Unit 2D vector pointing along Y axis
unitY2D :: Num a => Vec2D a 
unitY2D = Vec2D 0 1

-- | Unit 3D vector pointing along X axis
unitX3D :: Num a => Vec3D a 
unitX3D = Vec3D 1 0 0

-- | Unit 3D vector pointing along Y axis
unitY3D :: Num a => Vec3D a 
unitY3D = Vec3D 0 1 0

-- | Unit 3D vector pointing along Z axis
unitZ3D :: Num a => Vec3D a 
unitZ3D = Vec3D 0 0 1
