{-# LANGUAGE ConstraintKinds         #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE ImportQualifiedPost     #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE UndecidableSuperClasses #-}
-- |
module Vecvec.Classes.Containers
  ( Convert(..)
    -- * Constrained classes
  , Constrained(..)
  , CFunctor(..)
    -- ** Operations on constraints
  , NoConstaint
  , AndConstraint
  ) where

import Data.Kind
import Data.Vector                 qualified as V
import Data.Vector.Unboxed         qualified as VU
import Data.Vector.Storable        qualified as VS
import Data.Vector.Primitive       qualified as VP

----------------------------------------------------------------
-- Helpers
----------------------------------------------------------------

-- | Type class for conversions from one data type to
--   another. Function is assumed to be total
class Convert a b where
  convert :: a -> b

----------------------------------------------------------------
-- Constrained type classes
----------------------------------------------------------------

-- | Type class which places no constrains on element
class    NoConstaint a
instance NoConstaint a

class (f a, g a) => AndConstraint (f :: Type -> Constraint) (g :: Type -> Constraint) a
instance (f a, g a) => AndConstraint f g a



-- | Constraint on element of container which is required for
--   operations on container's elements. Think of data types like
--   storable or unboxed vectors.
class Constrained (f :: Type -> Type) where
  -- | Constraint on element.
  type ElemConstraint f :: Type -> Constraint


-- | Functor which allows constraints on elements of container.
class Constrained f => CFunctor f where
  cmap :: (ElemConstraint f a, ElemConstraint f b)
       => (a -> b) -> f a -> f b


instance Constrained []          where type ElemConstraint []         = NoConstaint
instance Constrained Maybe       where type ElemConstraint Maybe      = NoConstaint
instance Constrained (Either a)  where type ElemConstraint (Either a) = NoConstaint
instance Constrained V.Vector    where type ElemConstraint V.Vector   = NoConstaint
instance Constrained VS.Vector   where type ElemConstraint VS.Vector  = VS.Storable
instance Constrained VP.Vector   where type ElemConstraint VP.Vector  = VP.Prim
instance Constrained VU.Vector   where type ElemConstraint VU.Vector  = VU.Unbox

instance CFunctor [] where
  cmap = fmap
instance CFunctor Maybe where
  cmap = fmap
instance CFunctor (Either a) where
  cmap = fmap

instance CFunctor V.Vector where
  cmap = V.map
  {-# INLINE cmap #-}
instance CFunctor VS.Vector where
  cmap = VS.map
  {-# INLINE cmap #-}
instance CFunctor VP.Vector where
  cmap = VP.map
  {-# INLINE cmap #-}
instance CFunctor VU.Vector where
  cmap = VU.map
  {-# INLINE cmap #-}
