{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}
-- |
module TST.Tools.Model
  ( -- * Model-related classes
    TestData(..)
  , Conclusion(..)
  , P(..)
  , (===>)
  , eq
    -- * QC extras
  ) where

import Test.QuickCheck
import Data.Bifunctor
import qualified Data.Vector as DV
import qualified Data.Vector.Generic as DVG
import qualified Data.Vector.Primitive as DVP
import qualified Data.Vector.Storable as DVS
import qualified Data.Vector.Unboxed as DVU
import Vecvec.LAPACK.Internal.Vector as VV

import Control.Monad.Trans.Writer
import Data.Complex
import Data.Functor.Identity

import qualified Vecvec.Classes.NDArray                  as Slice


----------------------------------------------------------------
-- Model API
----------------------------------------------------------------

-- | Type class for values which has corresponding model for which
--   functions which we desire to test could be easily implemented.
class (Testable (EqTest a), Conclusion (EqTest a)) => TestData a where
  type Model a
  model :: a -> Model a
  unmodel :: Model a -> a
  -- | Property which is returned as equivalence test
  type EqTest a
  type instance EqTest a = Property
  -- | Equivalence test. To be used with quickcheck
  equal :: a -> a -> EqTest a
  default equal :: (Eq a, EqTest a ~ Property) => a -> a -> EqTest a
  equal x y = property (x == y)

-- | Attach predicate to parameters of testable function.
class Conclusion p where
  type Predicate p
  predicate :: Predicate p -> p -> p

instance Conclusion Property where
  type Predicate Property = Bool
  predicate = (==>)

instance Conclusion p => Conclusion (a -> p) where
  type Predicate (a -> p) = a -> Predicate p
  predicate f p = \x -> predicate (f x) (p x)


newtype P a = P { unP :: EqTest a }

infixr 0 ===>
(===>) :: TestData a => Predicate (EqTest a) -> P a -> P a
p ===> P a = P (predicate p a)

infix 4 `eq`
eq :: TestData a => a -> Model a -> P a
eq x y = P (equal x (unmodel y))


instance TestData a => Testable (P a) where
  property (P a) = property a



----------------------------------------------------------------
-- Instances for models
----------------------------------------------------------------

instance (Eq a, TestData a) => TestData (DV.Vector a) where
  type Model (DV.Vector a) = [Model a]
  model   = map model   . DV.toList
  unmodel = DV.fromList . map unmodel

instance (Eq a, DVP.Prim a, TestData a) => TestData (DVP.Vector a) where
  type Model (DVP.Vector a) = [Model a]
  model   = map model    . DVP.toList
  unmodel = DVP.fromList . map unmodel

instance (Eq a, DVS.Storable a, TestData a) => TestData (DVS.Vector a) where
  type Model (DVS.Vector a) = [Model a]
  model   = map model    . DVS.toList
  unmodel = DVS.fromList . map unmodel

instance (Eq a, DVU.Unbox a, TestData a) => TestData (DVU.Vector a) where
  type Model (DVU.Vector a) = [Model a]
  model   = map model    . DVU.toList
  unmodel = DVU.fromList . map unmodel

-- TODO use TST.Model

instance (Eq a, DVS.Storable a, TestData a) => TestData (VV.Vec a) where
  type Model (VV.Vec a) = [Model a]
  model = map model . DVG.toList
  -- We want to exercise both stride=1 and >1 but list doesn't have
  -- extra bit for this case. So we cheat and use list length for that
  unmodel lst
    | odd (length lst) = DVG.fromList $ unmodel <$> lst
    | otherwise        = Slice.slice ((0,Slice.End) `Strided` stride)
                       $ DVG.fromList
                       $ replicate stride =<< map unmodel lst
    where stride = 2
  type EqTest (VV.Vec a) = Property
  equal x y = property (x == y)




instance TestData () where
  type Model () = ()
  model   = id
  unmodel = id

instance TestData Bool where
  type Model Bool = Bool
  model   = id
  unmodel = id

instance TestData Int where
  type Model Int = Int
  model   = id
  unmodel = id

instance TestData Ordering where
  type Model Ordering = Ordering
  model   = id
  unmodel = id

instance TestData Float where
  type Model Float = Float
  model   = id
  unmodel = id
  equal x y = property (x == y || (isNaN x && isNaN y))

instance TestData Double where
  type Model Double = Double
  model   = id
  unmodel = id
  equal x y = property (x == y || (isNaN x && isNaN y))

instance TestData a => TestData (Complex a) where
  type Model (Complex a) = Complex (Model a)
  model   = fmap model
  unmodel = fmap unmodel
  equal (r1 :+ i1) (r2 :+ i2) = equal r1 r2 .&&. equal i1 i2

-- Functorish models
-- All of these need UndecidableInstances although they are actually well founded. Oh well.
instance (Eq a, TestData a) => TestData (Maybe a) where
  type Model (Maybe a) = Maybe (Model a)
  model   = fmap model
  unmodel = fmap unmodel

instance (Eq a, TestData a, Eq b, TestData b) => TestData (Either a b) where
  type Model (Either a b) = Either (Model a) (Model b)
  model   = bimap model model
  unmodel = bimap unmodel unmodel

instance (Eq a, TestData a) => TestData [a] where
  type Model [a] = [Model a]
  model   = fmap model
  unmodel = fmap unmodel

instance (Eq a, TestData a) => TestData (Identity a) where
  type Model (Identity a) = Identity (Model a)
  model   = fmap model
  unmodel = fmap unmodel

instance (Eq a, TestData a, Eq b, TestData b, Monoid a) => TestData (Writer a b) where
  type Model (Writer a b) = Writer (Model a) (Model b)
  model   = mapWriter model
  unmodel = mapWriter unmodel

instance (Eq a, Eq b, TestData a, TestData b) => TestData (a,b) where
  type Model (a,b) = (Model a, Model b)
  model   (a,b) = (model   a, model   b)
  unmodel (a,b) = (unmodel a, unmodel b)

instance (Eq a, Eq b, Eq c, TestData a, TestData b, TestData c) => TestData (a,b,c) where
  type Model (a,b,c) = (Model a, Model b, Model c)
  model   (a,b,c) = (model   a, model   b, model   c)
  unmodel (a,b,c) = (unmodel a, unmodel b, unmodel c)

instance (Arbitrary a, Show a, TestData a, TestData b) => TestData (a -> b) where
  type Model (a -> b) = Model a -> Model b
  model   f = model   . f . unmodel
  unmodel f = unmodel . f . model

  type EqTest (a -> b) = a -> EqTest b
  equal f g = \x -> equal (f x) (g x)
