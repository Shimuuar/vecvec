{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}
-- |
-- Helper type classes for working with geometry. @Lens'@ in this
-- module is standard van Laarhoven lenses: @Lens' s a = ∀f. Functor f
-- ⇒ (a → f a) → (s → f s)@
module Vecvec.Classes.Geometry
  ( -- * Access to coordinates
    FieldX(..)
  , FieldY(..)
  , FieldZ(..)
  ) where

import GHC.TypeNats
import Data.Proxy
import Data.Vector.Fixed           qualified as F
import Data.Vector.Fixed.Cont      qualified as FC
import Data.Vector.Fixed.Boxed     qualified as FB
import Data.Vector.Fixed.Unboxed   qualified as FU
import Data.Vector.Fixed.Storable  qualified as FS
import Data.Vector.Fixed.Primitive qualified as FP


type Lens' s a = forall f. Functor f => (a -> f a) -> (s -> f s)

-- | Provides access to @X@ component of a vector.
class FieldX a s | s -> a where
  _X   :: Lens' s a
  getX :: s -> a

-- | Provides access to @Y@ component of a vector.
class FieldY a s | s -> a where
  _Y   :: Lens' s a
  getY :: s -> a

-- | Lens for vectors that has coordinate Z
class FieldZ a s | s -> a where
  _Z   :: Lens' s a
  getZ :: s -> a


instance (n <= 3, F.Arity n, F.Index F.Z (FC.Peano n)
         ) => FieldX a (FB.Vec n a) where
  _X   = F.elementTy  (Proxy @0)
  getX = flip F.index (Proxy @0)
  {-# INLINE _X   #-}
  {-# INLINE getX #-}
instance (n <= 3, F.Arity n, F.Index F.N1 (FC.Peano n)
         ) => FieldY a (FB.Vec n a) where
  _Y   = F.elementTy  (Proxy @1)
  getY = flip F.index (Proxy @1)
  {-# INLINE _Y   #-}
  {-# INLINE getY #-}
instance (n <= 3, F.Arity n, F.Index F.N2 (FC.Peano n)
         ) => FieldZ a (FB.Vec n a) where
  _Z   = F.elementTy  (Proxy @2)
  getZ = flip F.index (Proxy @2)
  {-# INLINE _Z   #-}
  {-# INLINE getZ #-}

instance (n <= 3, F.Arity n, F.Index F.Z (FC.Peano n), FU.Unbox n a
         ) => FieldX a (FU.Vec n a) where
  _X   = F.elementTy  (Proxy @0)
  getX = flip F.index (Proxy @0)
  {-# INLINE _X   #-}
  {-# INLINE getX #-}
instance (n <= 3, F.Arity n, F.Index F.N1 (FC.Peano n), FU.Unbox n a
         ) => FieldY a (FU.Vec n a) where
  _Y   = F.elementTy  (Proxy @1)
  getY = flip F.index (Proxy @1)
  {-# INLINE _Y   #-}
  {-# INLINE getY #-}
instance (n <= 3, F.Arity n, F.Index F.N2 (FC.Peano n), FU.Unbox n a
         ) => FieldZ a (FU.Vec n a) where
  _Z   = F.elementTy  (Proxy @2)
  getZ = flip F.index (Proxy @2)
  {-# INLINE _Z   #-}
  {-# INLINE getZ #-}

instance (n <= 3, F.Arity n, F.Index F.Z (FC.Peano n), FS.Storable a
         ) => FieldX a (FS.Vec n a) where
  _X   = F.elementTy  (Proxy @0)
  getX = flip F.index (Proxy @0)
  {-# INLINE _X   #-}
  {-# INLINE getX #-}
instance (n <= 3, F.Arity n, F.Index F.N1 (FC.Peano n), FS.Storable a
         ) => FieldY a (FS.Vec n a) where
  _Y   = F.elementTy  (Proxy @1)
  getY = flip F.index (Proxy @1)
  {-# INLINE _Y   #-}
  {-# INLINE getY #-}
instance (n <= 3, F.Arity n, F.Index F.N2 (FC.Peano n), FS.Storable a
         ) => FieldZ a (FS.Vec n a) where
  _Z   = F.elementTy  (Proxy @2)
  getZ = flip F.index (Proxy @2)
  {-# INLINE _Z   #-}
  {-# INLINE getZ #-}

instance (n <= 3, F.Arity n, F.Index F.Z (FC.Peano n), FP.Prim a
         ) => FieldX a (FP.Vec n a) where
  _X   = F.elementTy  (Proxy @0)
  getX = flip F.index (Proxy @0)
  {-# INLINE _X   #-}
  {-# INLINE getX #-}
instance (n <= 3, F.Arity n, F.Index F.N1 (FC.Peano n), FP.Prim a
         ) => FieldY a (FP.Vec n a) where
  _Y   = F.elementTy  (Proxy @1)
  getY = flip F.index (Proxy @1)
  {-# INLINE _Y   #-}
  {-# INLINE getY #-}
instance (n <= 3, F.Arity n, F.Index F.N2 (FC.Peano n), FP.Prim a
         ) => FieldZ a (FP.Vec n a) where
  _Z   = F.elementTy  (Proxy @2)
  getZ = flip F.index (Proxy @2)
  {-# INLINE _Z   #-}
  {-# INLINE getZ #-}
