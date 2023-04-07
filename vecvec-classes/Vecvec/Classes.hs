{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost        #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
-- |
-- This module provides type classes for working with linear algebra.
-- Design of numeric type classes in base is rather unfortunate. We
-- have to define type classes for addition but other wise treat 'Num'
-- as poor man's ring and 'Fractional' as poor man's field in the name
-- of compatibility with wider ecosystem.
module Vecvec.Classes
  ( -- * Additive groups
    AdditiveSemigroup(..)
  , AdditiveMonoid(..)
  , AdditiveQuasigroup(..)
  , AdditiveGroup
    -- * Vector spaces
  , VectorSpace(..)
  , NormedScalar(..)
  , InnerSpace(..)
  , magnitude
    -- * Deriving via
  , AsNum(..)
  , AsVector(..)
  , AsFixedVec(..)
  ) where

import Data.Coerce
import Data.Int
import Data.Word
import Data.Complex          (Complex(..))
import Data.Complex          qualified as Complex

import Data.Vector               qualified as V
import Data.Vector.Unboxed       qualified as VU
import Data.Vector.Storable      qualified as VS
import Data.Vector.Primitive     qualified as VP
import Data.Vector.Generic       qualified as VG
import Data.Vector.Fusion.Bundle qualified as Bundle


import Data.Vector.Fixed           qualified as F
import Data.Vector.Fixed.Cont      qualified as FC
import Data.Vector.Fixed.Unboxed   qualified as FU
import Data.Vector.Fixed.Boxed     qualified as FB
import Data.Vector.Fixed.Storable  qualified as FS
import Data.Vector.Fixed.Primitive qualified as FP

----------------------------------------------------------------
-- Additive group
----------------------------------------------------------------

-- | Values that support addition. Operation must be associative up to
--   usual floating point schenangians. It's not really a semigroup
--   since we admit non-total addition.
--
-- == Laws
--
-- > (a .+. b) .+. c == a .+. (b .+. c) -- Associativity
--
-- Also if one side of equation is defined other must be defined too.
class AdditiveSemigroup a where
  (.+.) :: a -> a -> a

-- | Additive monoid.
--
-- == Laws
--
-- Addition and subtraction (if it's also an instance of
-- 'AdditiveQuasigroup') must be total
class AdditiveSemigroup a => AdditiveMonoid a where
  zeroV :: a

-- | Additive semigroup with subtraction. Note that we do not require
--   existence of zero. This is because we allow instances for vector
--   with variable length. Those could be though as union of vector
--   spaces of different dimension so there's no common zero.
--
-- == Laws
--
-- Laws hold if operation is defined.
--
--   > x .+. (y .-. y) == x
--   > x .-. y == x .+. negateV y
class AdditiveSemigroup a => AdditiveQuasigroup a where
  (.-.)   :: a -> a -> a
  negateV :: a -> a

-- | Proper additive group
type AdditiveGroup a = (AdditiveQuasigroup a, AdditiveMonoid a)

infixl 6 .+., .-.


----------------------------------------------------------------
-- Vector spaces
----------------------------------------------------------------

-- | Vector space. Not it's not quite a vector space
class AdditiveQuasigroup v => VectorSpace v where
  type Scalar v
  (*.) :: Scalar v -> v -> v
  (.*) :: v -> Scalar v -> v

infixl 7 .*
infixr 7 *.

-- FIXME: What to do with ./ /.?

-- | Vector space equipped with inner product. We follow convention of
--   physics and conjugate first element of scalar product
--
-- == Laws
--
-- > (a *. v) <.> w = conjugate a *. (v <.> w)
-- > v <.> (a *. w) = a           *. (v <.> w)
class (NormedScalar (Scalar v), VectorSpace v) => InnerSpace v where
  -- | Inner product.
  (<.>)       :: v -> v -> Scalar v
  -- | Compute squared magnitude of a vector. Note that depending on
  --   choice of metric it need not to be positive (Minkowski space as)
  magnitudeSq :: v -> R (Scalar v)

infix 7 <.>

-- | Compute magnitude of vector
magnitude :: (Floating (R (Scalar v)), InnerSpace v) => v -> R (Scalar v)
magnitude = sqrt . magnitudeSq

-- | Scalar for which we could compute norm. In fact we need this type
--   class in order to be able to compute norm of vector over complex
--   field.
class (Num v, Num (R v)) => NormedScalar v where
  -- | Type representing norm of scalar
  type R v
  conjugate    :: v -> v
  scalarNormSq :: v -> R v



----------------------------------------------------------------
-- Deriving
----------------------------------------------------------------

-- | Derive instances using methods from 'Num'
newtype AsNum a = AsNum a

instance Num a => AdditiveSemigroup (AsNum a) where
  (.+.) = coerce ((+) @a)
instance Num a => AdditiveQuasigroup (AsNum a) where
  (.-.)   = coerce ((-) @a)
  negateV = coerce (negate @a)
instance Num a => AdditiveMonoid (AsNum a) where
  zeroV = AsNum 0
instance Num a => VectorSpace (AsNum a) where
  type Scalar (AsNum a) = a
  (.*) = coerce ((*) @a)
  (*.) = coerce ((*) @a)
instance (NormedScalar a) => InnerSpace (AsNum a) where
  AsNum a <.> AsNum b = conjugate a * b
  magnitudeSq = coerce (scalarNormSq @a)

-- | Derive instances for a vector
newtype AsVector v a = AsVector (v a)

instance (Num a, VG.Vector v a) => AdditiveSemigroup (AsVector v a) where
  (.+.) = coerce (zipWith' @v @a (+))
  {-# INLINE (.+.) #-}
instance (Num a, VG.Vector v a) => AdditiveQuasigroup (AsVector v a) where
  (.-.)   = coerce (zipWith' @v @a (-))
  negateV = coerce (VG.map   @v @a negate)
  {-# INLINE (.-.)   #-}
  {-# INLINE negateV #-}
instance (Num a, VG.Vector v a) => VectorSpace (AsVector v a) where
  type Scalar (AsVector v a) = a
  x *. v = coerce (VG.map @v @a (x*)) v
  v .* x = coerce (VG.map @v @a (*x)) v
  {-# INLINE (.*) #-}
  {-# INLINE (*.) #-}
instance (NormedScalar a, VG.Vector v a) => InnerSpace (AsVector v a) where
  v <.> u = coerce (zipWithSum @v @a (\a b -> conjugate a * b)) v u
  magnitudeSq = coerce (mapWithSum @v @a scalarNormSq)
  {-# INLINE (<.>)       #-}
  {-# INLINE magnitudeSq #-}




-- | Derive vector-space instances for instance of 'F.Vector'. Scalar
--   is type parameter of a type. Operations from 'Num' data type are
--   used for addition and multiplication
newtype AsFixedVec v a = AsFixedVec (v a)

instance (Num a, F.Vector v a) => AdditiveSemigroup (AsFixedVec v a) where
  (.+.) = coerce (F.zipWith @v @a (+))
  {-# INLINE (.+.) #-}
instance (Num a, F.Vector v a) => AdditiveMonoid    (AsFixedVec v a) where
  zeroV = coerce (F.replicate @v @a 0)
  {-# INLINE zeroV #-}
instance (Num a, F.Vector v a) => AdditiveQuasigroup (AsFixedVec v a) where
  (.-.) = coerce (F.zipWith @v @a (-))
  negateV = coerce (F.map @v @a negate)
  {-# INLINE negateV #-}
  {-# INLINE (.-.)   #-}
instance (Num a, F.Vector v a) => VectorSpace (AsFixedVec v a) where
  type Scalar (AsFixedVec v a) = a
  a *. v = coerce (F.map @v @a (a*)) v
  v .* a = coerce (F.map @v @a (*a)) v
  {-# INLINE (*.) #-}
  {-# INLINE (.*) #-}
instance (NormedScalar a, F.Vector v a) => InnerSpace (AsFixedVec v a) where
  magnitudeSq (AsFixedVec v) = FC.sum $ FC.map scalarNormSq $ FC.cvec v
  AsFixedVec v <.> AsFixedVec u
    = FC.sum $ FC.zipWith (\a b -> conjugate a * b) (FC.cvec v) (FC.cvec u)
  {-# INLINE magnitudeSq #-}
  {-# INLINE (<.>)       #-}



deriving via AsNum Integer instance AdditiveSemigroup  Integer
deriving via AsNum Integer instance AdditiveMonoid     Integer
deriving via AsNum Integer instance AdditiveQuasigroup Integer

deriving via AsNum Int instance AdditiveSemigroup  Int
deriving via AsNum Int instance AdditiveMonoid     Int
deriving via AsNum Int instance AdditiveQuasigroup Int

deriving via AsNum Int8 instance AdditiveSemigroup  Int8
deriving via AsNum Int8 instance AdditiveMonoid     Int8
deriving via AsNum Int8 instance AdditiveQuasigroup Int8

deriving via AsNum Int16 instance AdditiveSemigroup  Int16
deriving via AsNum Int16 instance AdditiveMonoid     Int16
deriving via AsNum Int16 instance AdditiveQuasigroup Int16

deriving via AsNum Int32 instance AdditiveSemigroup  Int32
deriving via AsNum Int32 instance AdditiveMonoid     Int32
deriving via AsNum Int32 instance AdditiveQuasigroup Int32

deriving via AsNum Int64 instance AdditiveSemigroup  Int64
deriving via AsNum Int64 instance AdditiveMonoid     Int64
deriving via AsNum Int64 instance AdditiveQuasigroup Int64


deriving via AsNum Word instance AdditiveSemigroup  Word
deriving via AsNum Word instance AdditiveMonoid     Word
deriving via AsNum Word instance AdditiveQuasigroup Word

deriving via AsNum Word8 instance AdditiveSemigroup  Word8
deriving via AsNum Word8 instance AdditiveMonoid     Word8
deriving via AsNum Word8 instance AdditiveQuasigroup Word8

deriving via AsNum Word16 instance AdditiveSemigroup  Word16
deriving via AsNum Word16 instance AdditiveMonoid     Word16
deriving via AsNum Word16 instance AdditiveQuasigroup Word16

deriving via AsNum Word32 instance AdditiveSemigroup  Word32
deriving via AsNum Word32 instance AdditiveMonoid     Word32
deriving via AsNum Word32 instance AdditiveQuasigroup Word32

deriving via AsNum Word64 instance AdditiveSemigroup  Word64
deriving via AsNum Word64 instance AdditiveMonoid     Word64
deriving via AsNum Word64 instance AdditiveQuasigroup Word64

deriving via AsNum Float instance AdditiveSemigroup  Float
deriving via AsNum Float instance AdditiveMonoid     Float
deriving via AsNum Float instance AdditiveQuasigroup Float
deriving via AsNum Float instance VectorSpace        Float
deriving via AsNum Float instance InnerSpace         Float

deriving via AsNum Double instance AdditiveSemigroup  Double
deriving via AsNum Double instance AdditiveMonoid     Double
deriving via AsNum Double instance AdditiveQuasigroup Double
deriving via AsNum Double instance VectorSpace        Double
deriving via AsNum Double instance InnerSpace         Double

deriving via AsNum (Complex a) instance RealFloat a => AdditiveSemigroup  (Complex a)
deriving via AsNum (Complex a) instance RealFloat a => AdditiveMonoid     (Complex a)
deriving via AsNum (Complex a) instance RealFloat a => AdditiveQuasigroup (Complex a)
deriving via AsNum (Complex a) instance RealFloat a => VectorSpace        (Complex a)
deriving via AsNum (Complex a) instance RealFloat a => InnerSpace         (Complex a)

instance (AdditiveSemigroup a, AdditiveSemigroup b) => AdditiveSemigroup (a,b) where
  (a1,b1) .+. (a2,b2) = (a1.+.a2, b1.+.b2)
instance (AdditiveMonoid a, AdditiveMonoid b) => AdditiveMonoid (a,b) where
  zeroV = (zeroV, zeroV)
instance (AdditiveQuasigroup a, AdditiveQuasigroup b) => AdditiveQuasigroup (a,b) where
  (a1,b1) .-. (a2,b2) = (a1.-.a2, b1.-.b2)
  negateV (a,b) = (negateV a, negateV b)
instance ( VectorSpace a, VectorSpace b, Scalar a ~ Scalar b
         ) => VectorSpace (a,b) where
  type Scalar (a,b) = Scalar a
  x *. (a,b) = (x *. a, x *. b)
  (a,b) .* x = (a .* x, b .* x)
instance ( InnerSpace a, InnerSpace b, Scalar a ~ Scalar b
         ) => InnerSpace (a,b) where
  (a1,b1) <.> (a2,b2) = a1<.>a2 + b1<.>b2
  magnitudeSq (a,b) = magnitudeSq a + magnitudeSq b



deriving via AsVector V.Vector a instance Num a          => AdditiveSemigroup  (V.Vector a)
deriving via AsVector V.Vector a instance Num a          => AdditiveQuasigroup (V.Vector a)
deriving via AsVector V.Vector a instance Num a          => VectorSpace        (V.Vector a)
deriving via AsVector V.Vector a instance NormedScalar a => InnerSpace         (V.Vector a)

deriving via AsVector VU.Vector a instance (Num a, VU.Unbox a)          => AdditiveSemigroup  (VU.Vector a)
deriving via AsVector VU.Vector a instance (Num a, VU.Unbox a)          => AdditiveQuasigroup (VU.Vector a)
deriving via AsVector VU.Vector a instance (Num a, VU.Unbox a)          => VectorSpace        (VU.Vector a)
deriving via AsVector VU.Vector a instance (NormedScalar a, VU.Unbox a) => InnerSpace         (VU.Vector a)

deriving via AsVector VS.Vector a instance (Num a, VS.Storable a)          => AdditiveSemigroup  (VS.Vector a)
deriving via AsVector VS.Vector a instance (Num a, VS.Storable a)          => AdditiveQuasigroup (VS.Vector a)
deriving via AsVector VS.Vector a instance (Num a, VS.Storable a)          => VectorSpace        (VS.Vector a)
deriving via AsVector VS.Vector a instance (NormedScalar a, VS.Storable a) => InnerSpace         (VS.Vector a)

deriving via AsVector VP.Vector a instance (Num a, VP.Prim a)          => AdditiveSemigroup  (VP.Vector a)
deriving via AsVector VP.Vector a instance (Num a, VP.Prim a)          => AdditiveQuasigroup (VP.Vector a)
deriving via AsVector VP.Vector a instance (Num a, VP.Prim a)          => VectorSpace        (VP.Vector a)
deriving via AsVector VP.Vector a instance (NormedScalar a, VP.Prim a) => InnerSpace         (VP.Vector a)



deriving via (AsFixedVec (FB.Vec n) a) instance (F.Arity n, Num a)          => AdditiveSemigroup  (FB.Vec n a)
deriving via (AsFixedVec (FB.Vec n) a) instance (F.Arity n, Num a)          => AdditiveMonoid     (FB.Vec n a)
deriving via (AsFixedVec (FB.Vec n) a) instance (F.Arity n, Num a)          => AdditiveQuasigroup (FB.Vec n a)
deriving via (AsFixedVec (FB.Vec n) a) instance (F.Arity n, Num a)          => VectorSpace        (FB.Vec n a)
deriving via (AsFixedVec (FB.Vec n) a) instance (F.Arity n, NormedScalar a) => InnerSpace         (FB.Vec n a)

deriving via (AsFixedVec (FU.Vec n) a) instance (FU.Unbox n a, Num a)          => AdditiveSemigroup  (FU.Vec n a)
deriving via (AsFixedVec (FU.Vec n) a) instance (FU.Unbox n a, Num a)          => AdditiveMonoid     (FU.Vec n a)
deriving via (AsFixedVec (FU.Vec n) a) instance (FU.Unbox n a, Num a)          => AdditiveQuasigroup (FU.Vec n a)
deriving via (AsFixedVec (FU.Vec n) a) instance (FU.Unbox n a, Num a)          => VectorSpace        (FU.Vec n a)
deriving via (AsFixedVec (FU.Vec n) a) instance (FU.Unbox n a, NormedScalar a) => InnerSpace         (FU.Vec n a)

deriving via (AsFixedVec (FS.Vec n) a) instance (F.Arity n, FS.Storable a, Num a)          => AdditiveSemigroup  (FS.Vec n a)
deriving via (AsFixedVec (FS.Vec n) a) instance (F.Arity n, FS.Storable a, Num a)          => AdditiveMonoid     (FS.Vec n a)
deriving via (AsFixedVec (FS.Vec n) a) instance (F.Arity n, FS.Storable a, Num a)          => AdditiveQuasigroup (FS.Vec n a)
deriving via (AsFixedVec (FS.Vec n) a) instance (F.Arity n, FS.Storable a, Num a)          => VectorSpace        (FS.Vec n a)
deriving via (AsFixedVec (FS.Vec n) a) instance (F.Arity n, FS.Storable a, NormedScalar a) => InnerSpace         (FS.Vec n a)

deriving via (AsFixedVec (FP.Vec n) a) instance (F.Arity n, FP.Prim a, Num a)          => AdditiveSemigroup  (FP.Vec n a)
deriving via (AsFixedVec (FP.Vec n) a) instance (F.Arity n, FP.Prim a, Num a)          => AdditiveMonoid     (FP.Vec n a)
deriving via (AsFixedVec (FP.Vec n) a) instance (F.Arity n, FP.Prim a, Num a)          => AdditiveQuasigroup (FP.Vec n a)
deriving via (AsFixedVec (FP.Vec n) a) instance (F.Arity n, FP.Prim a, Num a)          => VectorSpace        (FP.Vec n a)
deriving via (AsFixedVec (FP.Vec n) a) instance (F.Arity n, FP.Prim a, NormedScalar a) => InnerSpace         (FP.Vec n a)


instance NormedScalar Float where
  type R Float = Float
  conjugate = id
  scalarNormSq x = x * x

instance NormedScalar Double where
  type R Double = Double
  conjugate = id
  scalarNormSq x = x * x

instance RealFloat a => NormedScalar (Complex a) where
  type R (Complex a) = a
  conjugate = Complex.conjugate
  scalarNormSq (r1 :+ i1) = r1*r1 + i1*i1


----------------------------------------------------------------
-- zipWith which errors when vector have different length
----------------------------------------------------------------

zipWith' :: (VG.Vector v a, VG.Vector v b, VG.Vector v c)
         => (a -> b -> c) -> v a -> v b -> v c
{-# INLINE zipWith' #-}
zipWith' f va vb
  | na /= nb  = error "Length mismatch"
  | otherwise = VG.zipWith f va vb
  where
    na = VG.length va
    nb = VG.length vb

zipWithSum :: (VG.Vector v a, VG.Vector v b, Num c)
           => (a -> b -> c) -> v a -> v b -> c
{-# INLINE zipWithSum #-}
zipWithSum f va vb
  | na /= nb  = error "Length mismatch"
  | otherwise = Bundle.foldl' (+) 0 $ Bundle.zipWith f (VG.stream va) (VG.stream vb)
  where
    na = VG.length va
    nb = VG.length vb

mapWithSum :: (VG.Vector v a, Num b)
           => (a -> b) -> v a -> b
{-# INLINE mapWithSum #-}
mapWithSum f = Bundle.foldl' (+) 0 . Bundle.map f . VG.stream
