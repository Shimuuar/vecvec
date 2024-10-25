{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE FieldSelectors       #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
-- |
module TST.Tools.Model
  ( -- * Model-related classes
    TestData(..)
  , TestEquiv(..)
  , EqTest
  , LiftTestEq(..)
  , Conclusion(..)
  , P(..)
  , (===>)
  , equivalent
    -- * Deriving and special instances
  , ModelSelf(..)
  , ModelFunctor(..)
  )  where

import Control.Monad.Trans.Writer
import Data.Bifunctor
import Data.Coerce
import Data.Complex
import Data.Function
import Data.Functor.Identity
import Data.Functor.Classes
import Data.Vector           qualified as DV
import Data.Vector.Generic   qualified as DVG
import Data.Vector.Primitive qualified as DVP
import Data.Vector.Storable  qualified as DVS
import Data.Vector.Unboxed   qualified as DVU

import Test.QuickCheck

import Vecvec.LAPACK.Vector as VV
import Vecvec.Classes.NDArray qualified as Slice

----------------------------------------------------------------
-- Model API
----------------------------------------------------------------

-- | Type class for values which has corresponding model for which
--   functions which we desire to test could be easily implemented.
class TestData a where
  type Model a
  model   :: a -> Model a
  unmodel :: Model a -> a

-- | Equivalence relation between data types. It's same as 'Eq' except
--   for treatment of NaNs.
class TestEquiv a where
  equiv :: a -> a -> Bool
  default equiv :: (Eq a) => a -> a -> Bool
  equiv = (==)



type family EqTest a where
  EqTest (a -> b) = a -> EqTest b
  EqTest  a       = Property

-- | Type class which is u
class (Testable (EqTest a), Conclusion (EqTest a), TestData a) => LiftTestEq a where
  -- | Equivalence test. To be used with quickcheck
  equal :: a -> Model a -> EqTest a

instance {-# OVERLAPPABLE #-} (TestData a, TestEquiv a, EqTest a ~ Property) => LiftTestEq a where
  equal a m = property (equiv a (unmodel m))

instance (Show a, Arbitrary a, TestData a, LiftTestEq b) => LiftTestEq (a -> b) where
  equal f g = \x -> equal (f x) (g (model x))




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


-- | Newtype wrapper for property
newtype P a = P { unP :: EqTest a }

-- | Only run tests when predicate succeeds
(===>) :: Conclusion (EqTest a) => Predicate (EqTest a) -> P a -> P a
p ===> P a = P (predicate p a)
infixr 0 ===>

-- | Test implementation and its model for equivalence
equivalent :: LiftTestEq a => a -> Model a -> P a
equivalent x y = P (equal x y)


instance (Testable (EqTest a)) => Testable (P a) where
  property (P a) = property a

----------------------------------------------------------------
-- Deriving via
----------------------------------------------------------------

-- | Newtype for deriving 'TestData' instance where model is same as
--   type itself. 'Eq' is used for equality tests.
newtype ModelSelf a = ModelSelf a

instance TestData (ModelSelf a) where
  type Model (ModelSelf a) = a
  model   = coerce
  unmodel = coerce

instance Eq a => TestEquiv (ModelSelf a) where
  equiv = coerce ((==) @a)

-- | Newtype for deriving 'TestData' for functorial data type
newtype ModelFunctor f a = ModelFunctor (f a)

instance (Functor f, TestData a) => TestData (ModelFunctor f a) where
  type Model (ModelFunctor f a) = f (Model a)
  model (ModelFunctor f) = fmap model f
  unmodel = ModelFunctor . fmap unmodel

instance (Eq1 f, TestEquiv a) => TestEquiv (ModelFunctor f a) where
  equiv (ModelFunctor f) (ModelFunctor g) = liftEq equiv f g


----------------------------------------------------------------
-- Instances for models
----------------------------------------------------------------

instance (TestData a, TestData b) => TestData (a -> b) where
  type Model (a -> b) = Model a -> Model b
  model   f = model   . f . unmodel
  unmodel f = unmodel . f . model

deriving via ModelSelf () instance TestData ()
deriving via ModelSelf () instance TestEquiv ()

deriving via ModelSelf Bool instance TestData Bool
deriving via ModelSelf Bool instance TestEquiv Bool

deriving via ModelSelf Int instance TestData Int
deriving via ModelSelf Int instance TestEquiv Int

deriving via ModelSelf Ordering instance TestData Ordering
deriving via ModelSelf Ordering instance TestEquiv Ordering

deriving via ModelSelf Float instance TestData Float
instance TestEquiv Float where
  equiv x y = x == y || (isNaN x && isNaN y)

deriving via ModelSelf Double instance TestData Double
instance TestEquiv Double where
  equiv x y = x == y || (isNaN x && isNaN y)

deriving via ModelFunctor Complex a instance TestData a => TestData (Complex a)
deriving via ModelFunctor Complex a instance TestEquiv  a => TestEquiv (Complex a)

deriving via ModelFunctor Identity a instance TestData a => TestData (Identity a)
deriving via ModelFunctor Identity a instance TestEquiv  a => TestEquiv (Identity a)

deriving via ModelFunctor Maybe a instance TestData a => TestData (Maybe a)
deriving via ModelFunctor Maybe a instance TestEquiv  a => TestEquiv (Maybe a)

deriving via ModelFunctor [] a instance TestData a => TestData [a]
deriving via ModelFunctor [] a instance TestEquiv  a => TestEquiv [a]


instance (TestData a, TestData b) => TestData (Either a b) where
  type Model (Either a b) = Either (Model a) (Model b)
  model   = bimap model   model
  unmodel = bimap unmodel unmodel

instance (TestEquiv a, TestEquiv b) => TestEquiv (Either a b) where
  equiv = liftEq2 equiv equiv


instance (TestData a, TestData b) => TestData (a,b) where
  type Model (a,b) = (Model a, Model b)
  model   (a,b) = (model   a, model   b)
  unmodel (a,b) = (unmodel a, unmodel b)

instance (TestEquiv a, TestEquiv b) => TestEquiv (a,b) where
  equiv (a1,b1) (a2,b2) = equiv a1 a2 && equiv b1 b2


instance (TestData a, TestData b, TestData c) => TestData (a,b,c) where
  type Model (a,b,c) = (Model a, Model b, Model c)
  model   (a,b,c) = (model   a, model   b, model   c)
  unmodel (a,b,c) = (unmodel a, unmodel b, unmodel c)

instance (TestEquiv a, TestEquiv b, TestEquiv c) => TestEquiv (a,b,c) where
  equiv (a1,b1,c1) (a2,b2,c2) = equiv a1 a2 && equiv b1 b2 && equiv c1 c2


instance (TestData a, TestData b) => TestData (Writer a b) where
  type Model (Writer a b) = Writer (Model a) (Model b)
  model   = mapWriter model
  unmodel = mapWriter unmodel

instance (TestEquiv a, TestEquiv b) => TestEquiv (Writer a b) where
  equiv = equiv `on` runWriter



----------------------------------------
-- Vectors

instance (TestData a) => TestData (DV.Vector a) where
  type Model (DV.Vector a) = [Model a]
  model   = map model   . DV.toList
  unmodel = DV.fromList . map unmodel

instance (DVP.Prim a, TestData a) => TestData (DVP.Vector a) where
  type Model (DVP.Vector a) = [Model a]
  model   = map model    . DVP.toList
  unmodel = DVP.fromList . map unmodel

instance (DVS.Storable a, TestData a) => TestData (DVS.Vector a) where
  type Model (DVS.Vector a) = [Model a]
  model   = map model    . DVS.toList
  unmodel = DVS.fromList . map unmodel

instance (DVU.Unbox a, TestData a) => TestData (DVU.Vector a) where
  type Model (DVU.Vector a) = [Model a]
  model   = map model    . DVU.toList
  unmodel = DVU.fromList . map unmodel

instance (DVS.Storable a, TestData a) => TestData (VV.Vec a) where
  type Model (VV.Vec a) = [Model a]
  model = map model . DVG.toList
  -- We want to exercise both stride=1 and >1 but list doesn't have
  -- extra bit for this case. So we cheat and use list length for that
  unmodel lst
    | odd (length lst) = DVG.fromList $ unmodel <$> lst
    | otherwise        = Slice.slice ((0,Slice.End) `Slice.Strided` stride)
                       $ DVG.fromList
                       $ replicate stride =<< map unmodel lst
    where stride = 2

instance (                TestEquiv a) => TestEquiv (DV.Vector  a) where equiv = DV.eqBy equiv
instance (DVP.Prim a,     TestEquiv a) => TestEquiv (DVP.Vector a) where equiv = DVP.eqBy equiv
instance (DVS.Storable a, TestEquiv a) => TestEquiv (DVS.Vector a) where equiv = DVS.eqBy equiv
instance (DVU.Unbox a,    TestEquiv a) => TestEquiv (DVU.Vector a) where equiv = DVU.eqBy equiv
instance (DVS.Storable a, TestEquiv a) => TestEquiv (VV.Vec a)     where equiv = DVG.eqBy equiv
