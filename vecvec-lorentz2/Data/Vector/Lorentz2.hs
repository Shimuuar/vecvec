{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
-- |
module Data.Vector.Lorentz2 where


import Data.Kind (Type)
import Data.Vector.Fixed.Mono qualified as FM
import Data.Vector.Fixed.Mono (Dim,N2,N3,N4,ViaFixed(..),PeanoNum(..))
import Vecvec.Classes
-- import Vecvec.Classes.Deriving
import Vecvec.Classes.Containers (Convert(..))


data X
data Y
data Z
data Rho
data Phi


----------------------------------------------------------------

newtype V (v :: Type) (xs :: [Type]) = V v

type instance Dim  (V v xs) = Dim v
type instance Elem (V v xs) = Elem v

instance (FM.Prod a v, Dim v ~ Len xs) => FM.Prod a (V v xs) where
  inspect (V v) = FM.inspect v
  construct     = fmap V FM.construct
  {-# INLINE inspect   #-}
  {-# INLINE construct #-}
-- FIXME: Is distinction Prod/Vector correct?
instance (FM.Prod a v, Dim v ~ N2) => FM.Vector a (V v [X,Y  ])
instance (FM.Prod a v, Dim v ~ N3) => FM.Vector a (V v [X,Y,Z])


type GoodVec a v xs = ( a ~ Elem v
                      , Dim v ~ Len xs
                      , FM.Prod a v)

type Repr2 a v = ( FM.Prod a v, a ~ Elem v, Dim v ~ N2 )
type Repr3 a v = ( FM.Prod a v, a ~ Elem v, Dim v ~ N3 )
type Repr4 a v = ( FM.Prod a v, a ~ Elem v, Dim v ~ N4 )

deriving via ViaFixed (V v [X,Y]) instance (GoodVec a v [X,Y], Num a )         => AdditiveSemigroup  (V v [X,Y])
deriving via ViaFixed (V v [X,Y]) instance (GoodVec a v [X,Y], Num a )         => AdditiveMonoid     (V v [X,Y])
deriving via ViaFixed (V v [X,Y]) instance (GoodVec a v [X,Y], Num a )         => AdditiveQuasigroup (V v [X,Y])
deriving via ViaFixed (V v [X,Y]) instance (GoodVec a v [X,Y], NormedScalar a) => VectorSpace        (V v [X,Y])
deriving via ViaFixed (V v [X,Y]) instance (GoodVec a v [X,Y], NormedScalar a) => InnerSpace         (V v [X,Y])

deriving via ViaFixed (V v [X,Y,Z]) instance (GoodVec a v [X,Y,Z], Num a )         => AdditiveSemigroup  (V v [X,Y,Z])
deriving via ViaFixed (V v [X,Y,Z]) instance (GoodVec a v [X,Y,Z], Num a )         => AdditiveMonoid     (V v [X,Y,Z])
deriving via ViaFixed (V v [X,Y,Z]) instance (GoodVec a v [X,Y,Z], Num a )         => AdditiveQuasigroup (V v [X,Y,Z])
deriving via ViaFixed (V v [X,Y,Z]) instance (GoodVec a v [X,Y,Z], NormedScalar a) => VectorSpace        (V v [X,Y,Z])
deriving via ViaFixed (V v [X,Y,Z]) instance (GoodVec a v [X,Y,Z], NormedScalar a) => InnerSpace         (V v [X,Y,Z])


instance (RealFloat a, Repr2 a v, Repr2 a w) => Convert (V v [X,Y]) (V w [Rho,Phi]) where
  convert (FM.V2 x y) = FM.V2 r phi where
    r   = sqrt $ x*x + y*y
    phi = atan2 y x

instance (RealFloat a, Repr2 a v, Repr2 a w) => Convert (V v [Rho,Phi]) (V w [X,Y]) where
  convert (FM.V2 r phi) = FM.V2 x y where
    x = r * cos phi
    y = r * sin phi




-- | Length of type list expressed as type level naturals from
--   @fixed-vector@.
type family Len (xs :: [α]) :: PeanoNum where
  Len '[]      = 'Z
  Len (x : xs) = 'S (Len xs)
