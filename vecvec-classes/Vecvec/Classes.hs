{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}
-- |
-- This module provides type classes for working with linear algebra.
-- Numeric hierarchy in @base@ is not suitable for working with linear
-- spaces (vectors, matrices, etc.) so we define additional type
-- classes but keep using standard numeric class for working with scalars.
--
-- We also use standard convention from other languages where
-- operations on inputs of mismatched size result in an
-- exception. This has consequences for type class hierarchy. This
-- means that \"semigroups\", etc are not actually semigroups since
-- addition is not total and they don't form groups since there's no
-- unique zero.
--
-- * For addition see: 'AdditiveSemigroup', 'AdditiveMonoid',
--   'AdditiveQuasigroup', 'AdditiveGroup'
--
-- * For vector space operations see: 'VectorSpace', 'InnerSpace'.
--
-- * For matrix×matrix and matrix×vector multiplication see 'MatMul'.
--
-- All law specified in this module are only expected to hold only up
-- to usual floating point shenanigans. For standard ways of deriving
-- instances see module "Vecvec.Classes.Deriving".
module Vecvec.Classes
  ( -- * Additive groups
    AdditiveSemigroup(..)
  , AdditiveMonoid(..)
  , AdditiveQuasigroup(..)
  , AdditiveGroup
    -- * Vector spaces
  , VectorSpace(..)
  , (./)
  , NormedScalar(..)
  , InnerSpace(..)
  , magnitude
  , normalize
  , normalizeMag
    -- * Matrix operations
  , MatMul(..)
  , Tr(..)
  , Conj(..)
    -- * Deriving via wrappers
  , MonoidFromAdditive(..)
  ) where

import Data.Coerce
import Data.Int
import Data.Word
import Data.Functor.Classes
import Data.Complex          (Complex(..))
import Data.Complex          qualified as Complex

import Data.Vector                 qualified as V
import Data.Vector.Unboxed         qualified as VU
import Data.Vector.Storable        qualified as VS
import Data.Vector.Primitive       qualified as VP
import Data.Vector.Generic         qualified as VG
import Data.Vector.Fusion.Bundle   qualified as Bundle
import Data.Vector.Fixed           qualified as F
import Data.Vector.Fixed.Cont      qualified as FC
import Data.Vector.Fixed.Unboxed   qualified as FU
import Data.Vector.Fixed.Boxed     qualified as FB
import Data.Vector.Fixed.Storable  qualified as FS
import Data.Vector.Fixed.Primitive qualified as FP
import GHC.Generics                (Generic)

import Vecvec.Classes.Internal.Types



----------------------------------------------------------------
-- Additive group
----------------------------------------------------------------

-- | Values that support associative addition. It's not proper
--   semigroup since we allow partial functions, e.g. addition of
--   vector of different length could throw exception.
--
-- == Laws
--
-- > 1: (a .+. b) .+. c == a .+. (b .+. c) -- Associativity
-- > 2: (a .+. b) .+. c ≠ ⊥   ⇒    a .+. (b .+. c) ≠ ⊥
class AdditiveSemigroup a where
  (.+.) :: a -> a -> a

-- | Additive monoid. Which means that exists unique zero. For example
--   vectors of variable length fail this condition: there's no value
--   that would act as zero for every vector. It's also excepted that
--   if such zero exists addition should be defined for all pairs.
--
-- == Laws
--
-- > 1: a .+. zeroV = a
-- > 2: zeroV .+. a = a
-- > 3: a .+. b     ≠ ⊥
class AdditiveSemigroup a => AdditiveMonoid a where
  zeroV :: a

-- | Additive semigroup with subtraction which is inverse of
--   addition. We define laws in a way that don't require existence of
--   zero. For example there's no unique zero for vector of variable
--   size.
--
-- == Laws
--
-- Laws hold if operation is defined.
--
-- > 1: x .+. (y .-. y) == (x .+. y) .-. y == x
-- > 2: x .-. y == x .+. negateV y
class AdditiveSemigroup a => AdditiveQuasigroup a where
  (.-.)   :: a -> a -> a
  negateV :: a -> a

-- | Proper additive group which has both subtraction and unique zero.
type AdditiveGroup a = (AdditiveQuasigroup a, AdditiveMonoid a)

infixl 6 .+., .-.


----------------------------------------------------------------
-- Vector spaces
----------------------------------------------------------------

-- | Vector space. Note it's not quite a vector space since we don't
--   require 'AdditiveGroup' as superclass, only
--   'AdditiveQuasigroup'. This is done in order to allow instances
--   for variable size vectors, matrices, etc. Both left and right
--   multiplication are defined in for case when multiplication is not
--   commutative.
class AdditiveQuasigroup v => VectorSpace v where
  type Scalar v
  -- | Elementwise multiplication by scalar on the left.
  (*.) :: Scalar v -> v -> v
  -- | Elementwise multiplication by scalar on the right.
  (.*) :: v -> Scalar v -> v

infixl 7 .*
infixr 7 *.

-- | Divide by scalar.
(./) :: (Fractional (Scalar v), VectorSpace v) => v -> Scalar v -> v
v ./ x = v .* recip x
{-# INLINE (./) #-}


-- | Vector space equipped with inner product. We follow convention of
--   physics and conjugate first element of scalar product
--
-- == Laws
--
-- > 1: (a *. v) <.> w = conjugate a *. (v <.> w)
-- > 2: v <.> (a *. w) = a           *. (v <.> w)
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
{-# INLINE magnitude #-}

-- | Normalize vector to unit length.
normalize :: (InnerSpace v, a ~ Scalar v, Fractional a, Floating (R a)) => v -> v
normalize v = v ./ fromR n where n = magnitude v
{-# INLINE normalize #-}

-- | Normalize vector and return its magnitude at the same time.
normalizeMag :: (InnerSpace v, a ~ Scalar v, Fractional a, Floating (R a)) => v -> (v, R a)
normalizeMag v = (v ./ fromR n, n) where n = magnitude v
{-# INLINE normalizeMag #-}


-- | Scalar for which we could compute norm. In fact we need this type
--   class in order to be able to compute norm of vector over complex
--   field.
--
-- > 1: ∀x. isReal (x * conjugate x) == True
-- > 2: ∀x. isReal (fromR x)         == True
-- > 3: ∀x. isReal x  ⇒  x == conjugate x
class (Num v, Num (R v)) => NormedScalar v where
  -- | Type representing norm of scalar. @R@ stands for real
  type R v
  -- | Conjugate value.
  conjugate    :: v -> v
  -- | Compute square of norm of a value.
  scalarNormSq :: v -> R v
  -- | Compute norm of a scalar.
  scalarNorm :: (Floating (R v), NormedScalar v) => v -> R v
  scalarNorm = sqrt . scalarNormSq
  {-# INLINE scalarNorm #-}
  -- | Convert norm to a scalar
  fromR        :: R v -> v
  -- | Check whether value is pure real.
  isReal       :: v -> Bool


----------------------------------------------------------------
-- Matrix operations
----------------------------------------------------------------

-- | This type class provides overloading for matrix-matrix and matrix
--   vector multiplication. There are a lot of representations of
--   matrices: general, symmetric, banded, etc. For convenience we
--   need to provide overloads for all implemented operations.
class MatMul a f g h | f g -> h where
  -- | Apply linear map @f : g → h@ to element of some vector space @g@.
  (@@) :: f a -> g a -> h a

infixl 7 @@

-- | Newtype for representing vector or matrix as transposed. In
--   particular it's used in 'MatMul' instances.
newtype Tr v a = Tr { getTr :: v a }
  deriving stock   (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)
  deriving newtype (AdditiveSemigroup,AdditiveMonoid,AdditiveQuasigroup,VectorSpace,InnerSpace)

-- | Newtype for representing vector or matrix as hermitian conjugate. In
--   particular it's used in 'MatMul' instances.
newtype Conj v a = Conj { getConj :: v a }
  deriving stock   (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)
  deriving newtype (AdditiveSemigroup,AdditiveMonoid,AdditiveQuasigroup,VectorSpace,InnerSpace)

instance Eq1 v => Eq1 (Tr v) where
  liftEq f (Tr a) (Tr b) = liftEq f a b
instance Eq1 v => Eq1 (Conj v) where
  liftEq f (Conj a) (Conj b) = liftEq f a b


----------------------------------------------------------------
-- Deriving
----------------------------------------------------------------


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


-- | Derive 'Semigroup' and 'Monoid' in terms of 'AdditiveSemigroup'
--   and 'AdditiveMonoid'
newtype MonoidFromAdditive a = MonoidFromAdditive a
  deriving stock (Show,Eq,Ord)

instance AdditiveSemigroup a => Semigroup (MonoidFromAdditive a) where
  (<>) = coerce ((.+.) @a)

instance AdditiveMonoid a => Monoid (MonoidFromAdditive a) where
  mempty = coerce (zeroV @a)


----------------------------------------------------------------
-- Instances boilerplate
----------------------------------------------------------------

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

instance (AdditiveSemigroup a, AdditiveSemigroup b, AdditiveSemigroup c
         ) => AdditiveSemigroup (a,b,c) where
  (a1,b1,c1) .+. (a2,b2,c2) = (a1.+.a2, b1.+.b2, c1.+.c2)
instance (AdditiveMonoid a, AdditiveMonoid b, AdditiveMonoid c
         ) => AdditiveMonoid (a,b,c) where
  zeroV = (zeroV, zeroV, zeroV)
instance (AdditiveQuasigroup a, AdditiveQuasigroup b, AdditiveQuasigroup c
         ) => AdditiveQuasigroup (a,b,c) where
  (a1,b1,c1) .-. (a2,b2,c2) = (a1.-.a2, b1.-.b2, c1.-.c2)
  negateV (a,b,c) = (negateV a, negateV b, negateV c)
instance ( VectorSpace a, VectorSpace b, VectorSpace c, Scalar a ~ Scalar b, Scalar a ~ Scalar c
         ) => VectorSpace (a,b,c) where
  type Scalar (a,b,c) = Scalar a
  x *. (a,b,c) = (x *. a, x *. b, x *. c)
  (a,b,c) .* x = (a .* x, b .* x, c .* x)
instance ( InnerSpace a, InnerSpace b, InnerSpace c, Scalar a ~ Scalar b, Scalar a ~ Scalar c
         ) => InnerSpace (a,b,c) where
  (a1,b1,c1) <.> (a2,b2,c2) = a1<.>a2 + b1<.>b2 + c1<.>c2
  magnitudeSq (a,b,c) = magnitudeSq a + magnitudeSq b + magnitudeSq c

instance (AdditiveSemigroup a, AdditiveSemigroup b, AdditiveSemigroup c, AdditiveSemigroup d
         ) => AdditiveSemigroup (a,b,c,d) where
  (a1,b1,c1,d1) .+. (a2,b2,c2,d2) = (a1.+.a2, b1.+.b2, c1.+.c2, d1.+.d2)
instance (AdditiveMonoid a, AdditiveMonoid b, AdditiveMonoid c, AdditiveMonoid d
         ) => AdditiveMonoid (a,b,c,d) where
  zeroV = (zeroV, zeroV, zeroV,zeroV)
instance (AdditiveQuasigroup a, AdditiveQuasigroup b, AdditiveQuasigroup c, AdditiveQuasigroup d
         ) => AdditiveQuasigroup (a,b,c,d) where
  (a1,b1,c1,d1) .-. (a2,b2,c2,d2) = (a1.-.a2, b1.-.b2, c1.-.c2, d1.-.d2)
  negateV (a,b,c,d) = (negateV a, negateV b, negateV c, negateV d)
instance ( VectorSpace a, VectorSpace b, VectorSpace c, VectorSpace d
         , Scalar a ~ Scalar b, Scalar a ~ Scalar c, Scalar a ~ Scalar d
         ) => VectorSpace (a,b,c,d) where
  type Scalar (a,b,c,d) = Scalar a
  x *. (a,b,c,d) = (x *. a, x *. b, x *. c, x *. d)
  (a,b,c,d) .* x = (a .* x, b .* x, c .* x, d .* x)
instance ( InnerSpace a, InnerSpace b, InnerSpace c, InnerSpace d
         , Scalar a ~ Scalar b, Scalar a ~ Scalar c, Scalar a ~ Scalar d
         ) => InnerSpace (a,b,c,d) where
  (a1,b1,c1,d1) <.> (a2,b2,c2,d2) = a1<.>a2 + b1<.>b2 + c1<.>c2 + d1<.>d2
  magnitudeSq (a,b,c,d) = magnitudeSq a + magnitudeSq b + magnitudeSq c + magnitudeSq d



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

deriving via (AsFixedVec (F.ContVec n) a) instance (F.ArityPeano n, Num a)          => AdditiveSemigroup  (F.ContVec n a)
deriving via (AsFixedVec (F.ContVec n) a) instance (F.ArityPeano n, Num a)          => AdditiveMonoid     (F.ContVec n a)
deriving via (AsFixedVec (F.ContVec n) a) instance (F.ArityPeano n, Num a)          => AdditiveQuasigroup (F.ContVec n a)
deriving via (AsFixedVec (F.ContVec n) a) instance (F.ArityPeano n, Num a)          => VectorSpace        (F.ContVec n a)
deriving via (AsFixedVec (F.ContVec n) a) instance (F.ArityPeano n, NormedScalar a) => InnerSpace         (F.ContVec n a)


instance NormedScalar Float where
  type R Float = Float
  conjugate      = id
  scalarNormSq x = x * x
  scalarNorm     = id
  fromR          = id
  isReal _       = True

instance NormedScalar Double where
  type R Double = Double
  conjugate      = id
  scalarNormSq x = x * x
  scalarNorm     = id
  fromR          = id
  isReal _       = True

instance RealFloat a => NormedScalar (Complex a) where
  type R (Complex a) = a
  conjugate             = Complex.conjugate
  scalarNormSq (r :+ i) = r*r + i*i
  fromR x               = x :+ 0
  isReal (_ :+ im)      = im == 0


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
