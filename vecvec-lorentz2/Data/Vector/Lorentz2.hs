{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
-- |
module Data.Vector.Lorentz2 where


import Data.Vector.Fixed.Mono qualified as FM
import Data.Vector.Fixed.Mono (Dim,N2,N3,ViaFixed(..))
import Vecvec.Classes
-- import Vecvec.Classes.Deriving
import Vecvec.Classes.Containers (Convert(..))


data X
data Y
data Z
data Rho
data Phi


newtype V2 v x y   = V2 v
newtype V3 v x y z = V3 v

----------------------------------------------------------------
--
----------------------------------------------------------------

instance (FM.Prod a v, Dim v ~ N2) => FM.Prod a (V2 v x y) where
  inspect (V2 v) = FM.inspect v
  construct        = fmap V2 FM.construct
  {-# INLINE inspect   #-}
  {-# INLINE construct #-}
-- FIXME: Is distinction Prod/Vector correct?
instance (FM.Prod a v, Dim v ~ N2) => FM.Vector a (V2 v x y)

instance (FM.Prod a v, Dim v ~ N3) => FM.Prod a (V3 v x y z) where
  inspect (V3 v) f = FM.inspect v f
  construct        = fmap V3 FM.construct
  {-# INLINE inspect   #-}
  {-# INLINE construct #-}
-- FIXME: Is distinction Prod/Vector correct?
instance (FM.Prod a v, Dim v ~ N3) => FM.Vector a (V3 v x y z)

type instance FM.Dim (V2 v x y  ) = FM.N2
type instance FM.Dim (V3 v x y z) = FM.N3



----------------------------------------------------------------
-- Vector space instances
----------------------------------------------------------------

type instance Elem (V2 v x y) = Elem v

deriving via ViaFixed (V2 v X Y)
  instance (Num a, Dim v ~ N2, FM.Prod a v) => AdditiveSemigroup (V2 v X Y)
deriving via ViaFixed (V2 v X Y)
  instance (Num a, Dim v ~ N2, FM.Prod a v) => AdditiveMonoid (V2 v X Y)
deriving via ViaFixed (V2 v X Y)
  instance (Num a, Dim v ~ N2, FM.Prod a v) => AdditiveQuasigroup (V2 v X Y)

-- FIXME: Doesn't work when a is not known.
--
deriving via ViaFixed (V2 v X Y)
  instance (a ~ Elem v, Num a, Dim v ~ N2, FM.Prod a v) => VectorSpace (V2 v X Y)
deriving via ViaFixed (V2 v X Y)
  instance (a ~ Elem v, NormedScalar a, Dim v ~ N2, FM.Prod a v) => InnerSpace (V2 v X Y)





deriving via ViaFixed (V3 v X Y Z)
  instance (Num a, Dim v ~ N3, FM.Prod a v) => AdditiveSemigroup (V3 v X Y Z)
deriving via ViaFixed (V3 v X Y Z)
  instance (Num a, Dim v ~ N3, FM.Prod a v) => AdditiveMonoid (V3 v X Y Z)
deriving via ViaFixed (V3 v X Y Z)
  instance (Num a, Dim v ~ N3, FM.Prod a v) => AdditiveQuasigroup (V3 v X Y Z)


----------------------------------------------------------------
-- Conversions
----------------------------------------------------------------

instance (FM.Prod a v, FM.Prod a w, Dim v ~ N2, Dim w ~ N2
         ) => Convert (V2 v X Y) (V2 w X Y) where
  convert = FM.vector . FM.cvec 


-- FIXME: atan2 requires RealFloat and this could harm generality when
--        using for types which aren't floats.
instance (RealFloat a, FM.Prod a v, FM.Prod a w, Dim v ~ N2, Dim w ~ N2
         ) => Convert (V2 v X Y) (V2 w Rho Phi) where
  convert (FM.V2 x y) = FM.V2 r phi
    where
      r   = sqrt $ x*x + y*y
      phi = atan2 y x
      
instance (Floating a, FM.Prod a v, FM.Prod a w, Dim v ~ N2, Dim w ~ N2
         ) => Convert (V2 v Rho Phi) (V2 w X Y) where
  convert (FM.V2 r phi) = FM.V2 x y where
    x = r * cos phi
    y = r * sin phi
