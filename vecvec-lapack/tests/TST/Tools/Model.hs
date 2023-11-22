{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE ImportQualifiedPost   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}
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
    -- * Vector
  , TagVector(..)
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

import Vecvec.LAPACK.Internal.Vector as VV
import Vecvec.Classes.NDArray qualified as Slice
import Vecvec.Classes

----------------------------------------------------------------
-- Model API
----------------------------------------------------------------

-- | Type class for values which has corresponding model for which
--   functions which we desire to test could be easily implemented.
class TestData t a where
  type Model t a
  model   :: t -> a -> Model t a
  unmodel :: t -> Model t a -> a

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
class (Testable (EqTest a), Conclusion (EqTest a), TestData t a) => LiftTestEq t a where
  -- | Equivalence test. To be used with quickcheck
  equal :: t -> a -> Model t a -> EqTest a

instance {-# OVERLAPPABLE #-} (TestData t a, TestEquiv a, EqTest a ~ Property) => LiftTestEq t a where
  equal t a m = property (equiv a (unmodel t m))

instance (Show a, Arbitrary a, TestData t a, LiftTestEq t b) => LiftTestEq t (a -> b) where
  equal t f g = \x -> equal t (f x) (g (model t x))




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
equivalent :: LiftTestEq t a => t -> a -> Model t a -> P a
equivalent t x y = P (equal t x y)


instance (Testable (EqTest a)) => Testable (P a) where
  property (P a) = property a

----------------------------------------------------------------
-- Deriving via
----------------------------------------------------------------

-- | Newtype for deriving 'TestData' instance where model is same as
--   type itself. 'Eq' is used for equality tests.
newtype ModelSelf a = ModelSelf a

instance TestData t (ModelSelf a) where
  type Model t (ModelSelf a) = a
  model   _ = coerce
  unmodel _ = coerce

instance Eq a => TestEquiv (ModelSelf a) where
  equiv = coerce ((==) @a)

-- | Newtype for deriving 'TestData' for functorial data type
newtype ModelFunctor f a = ModelFunctor (f a)

instance (Functor f, TestData t a) => TestData t (ModelFunctor f a) where
  type Model t (ModelFunctor f a) = f (Model t a)
  model t (ModelFunctor f) = fmap (model t) f
  unmodel t = ModelFunctor . fmap (unmodel t)

instance (Eq1 f, TestEquiv a) => TestEquiv (ModelFunctor f a) where
  equiv (ModelFunctor f) (ModelFunctor g) = liftEq equiv f g


----------------------------------------------------------------
-- Instances for models
----------------------------------------------------------------

instance (TestData t a, TestData t b) => TestData t (a -> b) where
  type Model t (a -> b) = Model t a -> Model t b
  model   t f = model   t . f . unmodel t
  unmodel t f = unmodel t . f . model   t

deriving via ModelSelf () instance TestData t ()
deriving via ModelSelf () instance TestEquiv ()

deriving via ModelSelf Bool instance TestData t Bool
deriving via ModelSelf Bool instance TestEquiv Bool

deriving via ModelSelf Int instance TestData t Int
deriving via ModelSelf Int instance TestEquiv Int

deriving via ModelSelf Ordering instance TestData t Ordering
deriving via ModelSelf Ordering instance TestEquiv Ordering

deriving via ModelSelf Float instance TestData t Float
instance TestEquiv Float where
  equiv x y = x == y || (isNaN x && isNaN y)

deriving via ModelSelf Double instance TestData t Double
instance TestEquiv Double where
  equiv x y = x == y || (isNaN x && isNaN y)

deriving via ModelFunctor Complex a instance TestData t a => TestData t (Complex a)
deriving via ModelFunctor Complex a instance TestEquiv  a => TestEquiv (Complex a)

deriving via ModelFunctor Identity a instance TestData t a => TestData t (Identity a)
deriving via ModelFunctor Identity a instance TestEquiv  a => TestEquiv (Identity a)

deriving via ModelFunctor Maybe a instance TestData t a => TestData t (Maybe a)
deriving via ModelFunctor Maybe a instance TestEquiv  a => TestEquiv (Maybe a)

deriving via ModelFunctor [] a instance TestData t a => TestData t [a]
deriving via ModelFunctor [] a instance TestEquiv  a => TestEquiv [a]


instance (TestData t a, TestData t b) => TestData t (Either a b) where
  type Model t (Either a b) = Either (Model t a) (Model t b)
  model   t = bimap (model   t) (model   t)
  unmodel t = bimap (unmodel t) (unmodel t)

instance (TestEquiv a, TestEquiv b) => TestEquiv (Either a b) where
  equiv = liftEq2 equiv equiv


instance (TestData t a, TestData t b) => TestData t (a,b) where
  type Model t (a,b) = (Model t a, Model t b)
  model   t (a,b) = (model   t a, model   t b)
  unmodel t (a,b) = (unmodel t a, unmodel t b)

instance (TestEquiv a, TestEquiv b) => TestEquiv (a,b) where
  equiv (a1,b1) (a2,b2) = equiv a1 a2 && equiv b1 b2


instance (TestData t a, TestData t b, TestData t c) => TestData t (a,b,c) where
  type Model t (a,b,c) = (Model t a, Model t b, Model t c)
  model   t (a,b,c) = (model   t a, model   t b, model   t c)
  unmodel t (a,b,c) = (unmodel t a, unmodel t b, unmodel t c)

instance (TestEquiv a, TestEquiv b, TestEquiv c) => TestEquiv (a,b,c) where
  equiv (a1,b1,c1) (a2,b2,c2) = equiv a1 a2 && equiv b1 b2 && equiv c1 c2


instance (TestData t a, TestData t b) => TestData t (Writer a b) where
  type Model t (Writer a b) = Writer (Model t a) (Model t b)
  model   t = mapWriter (model   t)
  unmodel t = mapWriter (unmodel t)

instance (TestEquiv a, TestEquiv b) => TestEquiv (Writer a b) where
  equiv = equiv `on` runWriter



----------------------------------------
-- Vectors

-- | Type tag for testing functions defined in vector package
data TagVector = TagVector

instance (TestData TagVector a) => TestData TagVector (DV.Vector a) where
  type Model TagVector (DV.Vector a) = [Model TagVector a]
  model   t = map (model t) . DV.toList
  unmodel t = DV.fromList   . map (unmodel t)

instance (DVP.Prim a, TestData TagVector a) => TestData TagVector (DVP.Vector a) where
  type Model TagVector (DVP.Vector a) = [Model TagVector a]
  model   t = map (model t) . DVP.toList
  unmodel t = DVP.fromList  . map (unmodel t)

instance (DVS.Storable a, TestData TagVector a) => TestData TagVector (DVS.Vector a) where
  type Model TagVector (DVS.Vector a) = [Model TagVector a]
  model   t = map (model t) . DVS.toList
  unmodel t = DVS.fromList  . map (unmodel t)

instance (DVU.Unbox a, TestData TagVector a) => TestData TagVector (DVU.Vector a) where
  type Model TagVector (DVU.Vector a) = [Model TagVector a]
  model   t = map (model t) . DVU.toList
  unmodel t = DVU.fromList  . map (unmodel t)

instance (DVS.Storable a, TestData TagVector a) => TestData TagVector (VV.Vec a) where
  type Model TagVector (VV.Vec a) = [Model TagVector a]
  model t = map (model t) . DVG.toList
  -- We want to exercise both stride=1 and >1 but list doesn't have
  -- extra bit for this case. So we cheat and use list length for that
  unmodel t lst
    | odd (length lst) = DVG.fromList $ unmodel t <$> lst
    | otherwise        = Slice.slice ((0,Slice.End) `Strided` stride)
                       $ DVG.fromList
                       $ replicate stride =<< map (unmodel t) lst
    where stride = 2

instance (                TestEquiv a) => TestEquiv (DV.Vector  a) where equiv = DV.eqBy equiv
instance (DVP.Prim a,     TestEquiv a) => TestEquiv (DVP.Vector a) where equiv = DVP.eqBy equiv
instance (DVS.Storable a, TestEquiv a) => TestEquiv (DVS.Vector a) where equiv = DVS.eqBy equiv
instance (DVU.Unbox a,    TestEquiv a) => TestEquiv (DVU.Vector a) where equiv = DVU.eqBy equiv
instance (DVS.Storable a, TestEquiv a) => TestEquiv (VV.Vec a)     where equiv = DVG.eqBy equiv


----------------------------------------
-- Vecvec

-- instance (TestData t (v a), v' a' ~ Model t (v a)) => TestData t (Tr v a) where
--   type Model t (Tr v a) = Tr v' a'

-- deriving via ModelFunctor Tr a instance TestData t a => TestData t (Tr a)
-- deriving via ModelFunctor Tr a instance TestEquiv  a => TestEquiv  (Tr a)

-- deriving via ModelFunctor Conj a instance TestData t a => TestData t (Conj a)
-- deriving via ModelFunctor Conj a instance TestEquiv  a => TestEquiv  (Conj a)
