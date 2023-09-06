{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost        #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE PartialTypeSignatures      #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ViewPatterns               #-}
-- |
-- Tests for solving linear systems. Here we want to tests that we
-- indeed produce solution for linear system and call LAPACK
-- correctly. But we don't care about numerical properties much
module TST.LinSolve where

import Control.Monad
import Data.Typeable
import Data.Vector               qualified as V
import Data.Vector.Unboxed       qualified as VU
import Data.Vector.Storable      qualified as VS
import Data.Vector.Primitive     qualified as VP
import Data.Vector.Generic       qualified as VG
import Test.Tasty
import Test.Tasty.QuickCheck

import Vecvec.Classes
import Vecvec.Classes.Util
import Vecvec.LAPACK                       qualified as VV
import Vecvec.LAPACK.FFI                   (S,D,C,Z)
import Vecvec.LAPACK.Vector                (Vec)
import Vecvec.LAPACK.Matrix.Dense          (Matrix)
import Vecvec.LAPACK.Matrix.Dense          qualified as Mat
import Vecvec.LAPACK.Matrix.Decomposition

import TST.Orphanage ()
import TST.Model
import TST.Decomposition (Epsilon(..))


-- | Run tests for solvers for linear systems
tests :: TestTree
tests = testGroup "LinSolve"
  [ testSimpleSolve @Vec
  , testSimpleSolve @V.Vector
  , testSimpleSolve @VS.Vector
  , testSimpleSolve @VU.Vector
  , testSimpleSolve @Matrix
  , testSimpleSolve @[]
  ]


-- | Run tests for simple solver for each possible scalar type
testSimpleSolve
  :: forall rhs.
     ( ArbitraryRHS rhs S, ArbitraryRHS rhs D, ArbitraryRHS rhs C, ArbitraryRHS rhs Z
     , LinearEqRHS  rhs S, LinearEqRHS  rhs D, LinearEqRHS  rhs C, LinearEqRHS  rhs Z
     , Show (rhs S), Show (rhs D), Show (rhs C), Show (rhs Z)
     , Typeable rhs
     )
  => TestTree  
testSimpleSolve = testGroup ("RHS = " ++ qualTypeName @rhs)
  [ testProperty "S" $ prop_SimpleSolve @rhs @S
  , testProperty "D" $ prop_SimpleSolve @rhs @D
  , testProperty "C" $ prop_SimpleSolve @rhs @C
  , testProperty "Z" $ prop_SimpleSolve @rhs @Z
  ]


prop_SimpleSolve
  :: forall rhs a.
     (ArbitraryRHS rhs a, LinearEqRHS rhs a, VV.LAPACKy a)
  => LinSimple rhs a
  -> Property
prop_SimpleSolve (LinSimple a rhs)
  = checkLinEq a (solveLinEq a rhs) rhs

-- | Simple linear equation @Ax = b@ for a given right hand side
data LinSimple rhs a = LinSimple (Matrix a) (rhs a)

instance (Show a, Show (rhs a), VS.Storable a) => Show (LinSimple rhs a) where
  show (LinSimple a rhs) = "A = \n"++show a++"\nb =\n"++show rhs
  
instance ( Show a,Eq a,StorableZero a, VV.LAPACKy a,ScalarModel a,Typeable a,ArbitraryRHS rhs a
         ) => Arbitrary (LinSimple rhs a) where
  arbitrary = do
    sz  <- genSize @Int
    a   <- genNonsingularMatrix sz
    rhs <- arbitraryRHS sz
    pure $ LinSimple a rhs


-- | Generate arbitrary right hand side for equation and check it for validity
class ArbitraryRHS rhs a where
  arbitraryRHS :: ScalarModel a => Int -> Gen (rhs a)
  checkLinEq   :: Matrix a -> rhs a -> rhs a -> Property

instance (VV.LAPACKy a, Epsilon (R a), Floating (R a), Ord (R a), Eq a, Show a, Typeable a
         ) => ArbitraryRHS Matrix a where
  arbitraryRHS sz = do
    n <- choose (1,4)
    fromModel <$> arbitraryShape (sz,n)
  checkLinEq a (Mat.toColList -> xs) (Mat.toColList -> bs)
    | length xs /= length bs = error "Lengths do not match"
    | otherwise              = property $ all nearZero deltas
    where
      deltas = [ a @@ x .-. b | (x,b) <- xs `zip` bs]


instance (VV.LAPACKy a, Epsilon (R a), Floating (R a), Ord (R a)) => ArbitraryRHS Vec a where
  arbitraryRHS n = VG.replicateM n genScalar
  checkLinEq a x b = property $ nearZero delta where
    delta = a @@ x .-. b

instance (VV.LAPACKy a, Epsilon (R a), Floating (R a), Ord (R a)) => ArbitraryRHS V.Vector a where
  arbitraryRHS n = VG.replicateM n genScalar
  checkLinEq a x b = property $ nearZero delta where
    delta = a @@ (VG.convert x :: Vec a) .-. VG.convert b

instance (VU.Unbox a, VV.LAPACKy a, Epsilon (R a), Floating (R a), Ord (R a)) => ArbitraryRHS VU.Vector a where
  arbitraryRHS n = VG.replicateM n genScalar
  checkLinEq a x b = property $ nearZero delta where
    delta = a @@ (VG.convert x :: Vec a) .-. VG.convert b

instance (VV.LAPACKy a, Epsilon (R a), Floating (R a), Ord (R a)) => ArbitraryRHS VS.Vector a where
  arbitraryRHS n = VG.replicateM n genScalar
  checkLinEq a x b = property $ nearZero delta where
    delta = a @@ (VG.convert x :: Vec a) .-. VG.convert b

instance (VP.Prim a, VV.LAPACKy a, Epsilon (R a), Floating (R a), Ord (R a)) => ArbitraryRHS VP.Vector a where
  arbitraryRHS n = VG.replicateM n genScalar
  checkLinEq a x b = property $ nearZero delta where
    delta = a @@ (VG.convert x :: Vec a) .-. VG.convert b

instance (VV.LAPACKy a, Epsilon (R a), Floating (R a), Ord (R a)) => ArbitraryRHS [] a where
  arbitraryRHS n = replicateM n genScalar
  checkLinEq a x b = property $ nearZero delta where
    delta = a @@ (VG.fromList x :: Vec a) .-. VG.fromList b

nearZero :: (VG.Vector v a, Epsilon (R a), NormedScalar a, Floating (R a), Ord (R a)) => v a -> Bool
nearZero = VG.all (\d -> scalarNorm d < epsilon)
