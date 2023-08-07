{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NegativeLiterals           #-}
{-# LANGUAGE QuantifiedConstraints      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- |
module QC (tests) where

import Data.Vector.Lorentz

import Numeric.AD
import Numeric.AD.Mode.Forward (Forward)
import Linear.V2

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.QuickCheck.Gen

import Vecvec.Classes



tests :: TestTree
tests = testGroup "Lorentz"
  [ testGroup "Conversions"
    [ testProperty "Gamma->V->Gamma" $ propInverse1D
        (getSpeed . convert . Gamma)
        (getGamma . convert . Speed)
        (1e-15, 1e-15)
        . getGamma
    , testProperty "V->Gamma->V" $ propInverse1D
        (getGamma . convert . Speed)
        (getSpeed . convert . Gamma)
        (1e-15, 1e-15)
        . getSpeed
    , testProperty "Gamma->Rap->Gamma" $ propInverse1D
        (getRapidity . convert . Gamma)
        (getGamma    . convert . Rapidity)
        (1e-15, 1e-15)
        . getGamma
    , testProperty "Rap->Gamma->Rap" $ propInverse1D
        (getGamma    . convert . Rapidity)
        (getRapidity . convert . Gamma)
        (1e-15, 1e-15)
        . getRapidity
    , testProperty "Gamma->Rap->Gamma" $ propInverse1D
        (getRapidity . convert . Gamma)
        (getGamma    . convert . Rapidity)
        (1e-15, 1e-15)
        . getGamma
    , testProperty "Rap->Gamma->Rap" $ propInverse1D
        (getGamma    . convert . Rapidity)
        (getRapidity . convert . Gamma)
        (1e-15, 1e-15)
        . getRapidity
    ]
  ]


----------------------------------------------------------------
-- Inversion tests
----------------------------------------------------------------

-- | Test for inverse of function. We need to account for rounding
--   errors properly. For that we need derivatives and obtain them
--   using ad.
propInverse1D
  :: (forall s. AD s (Forward Double) -> AD s (Forward Double))
     -- ^ Forward function
  -> (forall s. AD s (Forward Double) -> AD s (Forward Double))
     -- ^ Inverse function
  -> (Double,Double)
     -- ^ Precision for forward and inverse function
  -> Double
  -> Property
propInverse1D fun inv (errFun, errInv) x
  = counterexample ("X            = " ++ show x)
  $ counterexample ("Y            = " ++ show y)
  $ counterexample ("dy/dx        = " ++ show dy)
  $ counterexample ("X'           = " ++ show x')
  $ counterexample ("expected err = " ++ show expected)
  $ counterexample ("obsrved  err = " ++ show observed)
  $ (abs dy > 0)
  ==> observed <= expected
  where
    observed = abs ((x' - x) / x)
    expected = (errInv + errFun * (y/(x*dy)))
    -- Evaluate
    (x',_) = diff' (inv . fun) x
    (y,dy) = diff' fun         x



----------------------------------------------------------------
{-
p :: Testable prop => prop -> IO ()
p = quickCheck

runTests :: [(String, IO ())] -> IO ()
runTests = mapM_ $ \(name, test) -> putStrLn (" * " ++ name) >> test


δ,ε :: Double
δ = 7.450580596923828e-9
ε = 2.220446049250313e-16

-- I'm not sure that I got it right
(~=) :: Double -> Double -> Bool
x ~= y = (x == y) || (abs δx < ε) || (δx/x < δ)
    where
      δx = y - x

eqL :: Lorentz Double -> Lorentz Double -> Bool
eqL (Lorentz t x y z) (Lorentz t' x' y' z') = (t ~= t') && (x ~= x') && (y ~= y') && (z ~= z')

apprEqualTest :: (Double -> Double) -> Double -> Bool
apprEqualTest f x = x ~= f x

massTest :: Lorentz Double -> Lorentz Double -> Bool
massTest p1 p2 = magnitudeSq p1 ~= magnitudeSq p2
----------------------------------------------------------------

-- Gamma factor
newtype Gamma = Gamma { unGamma :: Double } deriving Show
-- Rapidity
newtype Rapidity = Rapidity { unRapidity :: Double } deriving Show
instance Arbitrary Rapidity where
    arbitrary = Rapidity <$> suchThat arbitrary (\x -> x>=0 && x<500)
-- Small rapidity (< 5)
newtype SmallRapidity = SmallRapidity { unSmallRapidity :: Double } deriving Show
instance Arbitrary SmallRapidity where
    arbitrary = SmallRapidity <$> choose (0,5)
-- Speed
newtype Speed = Speed { unSpeed :: Double } deriving Show
instance Arbitrary Speed where
    arbitrary = Speed <$> suchThat (choose (0,1)) (<1)
-- Get positive number
unP :: Positive a -> a
unP (Positive x) = x

instance Arbitrary a => Arbitrary (Vec2D a) where
    arbitrary = Vec2D <$> arbitrary <*> arbitrary

instance Arbitrary a => Arbitrary (Vec3D a) where
    arbitrary = Vec3D <$> arbitrary <*> arbitrary <*> arbitrary

newtype NonZeroVec3D a = NonZeroVec3D { nonZero3D :: Vec3D a } deriving Show
instance (Num a, Arbitrary a) => Arbitrary (NonZeroVec3D a) where
    arbitrary = NonZeroVec3D <$> suchThat arbitrary ((/=0) . magnitudeSq)

instance Arbitrary a => Arbitrary (Lorentz a) where
    arbitrary = Lorentz <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

-- Rapidity is restricted to [-5,5] range to avoid loss of precision
instance (Arbitrary a, Floating a, Random a) => Arbitrary (Boost1D a) where
    arbitrary = rapidityBoost1D <$> choose (-5,5)

----------------------------------------------------------------


testsBoost :: [(String, IO ())]
testsBoost =
    [ ("==== Tests for boosts ====", return ())
    -- Mass is conserved
    , ("Mass, X",   p (\(b,p)   -> massTest p (boostX b p)))
    , ("Mass, Y",   p (\(b,p)   -> massTest p (boostY b p)))
    , ("Mass, Z",   p (\(b,p)   -> massTest p (boostZ b p)))
    , ("Mass, n",   p (\(b,NonZeroVec3D n,p) -> massTest p (boost1D b n p)))
    -- Arbitrary directed boosts
    , ("1D X",      p (\(b,p) -> eqL (boostX b p) (boost1D  b unitX3D p)))
    , ("1D Y",      p (\(b,p) -> eqL (boostY b p) (boost1D  b unitY3D p)))
    , ("1D Y",      p (\(b,p) -> eqL (boostZ b p) (boost1D  b unitZ3D p)))
    , ("1Dn X",     p (\(b,p) -> eqL (boostX b p) (boost1Dn b unitX3D p)))
    , ("1Dn Y",     p (\(b,p) -> eqL (boostY b p) (boost1Dn b unitY3D p)))
    , ("1Dn Y",     p (\(b,p) -> eqL (boostZ b p) (boost1Dn b unitZ3D p)))
    -- Boosts are invertible
    , ("Inverse X", p (\(b,p) -> eqL p (boostX (inverseBoost b) . boostX b $ p)))
    , ("Inverse Y", p (\(b,p) -> eqL p (boostY (inverseBoost b) . boostY b $ p)))
    , ("Inverse Z", p (\(b,p) -> eqL p (boostZ (inverseBoost b) . boostZ b $ p)))
    , ("Inverse n", p (\(b,NonZeroVec3D n, p) -> eqL p (boost1D (inverseBoost b) n $ boost1D b n $ p)))
    , ("Inverse n'",p (\(b,NonZeroVec3D n, p) -> eqL p (boost1D b (negateV n) $ boost1D b n $ p)))
    ]
    where
      inverseBoost = rapidityBoost1D . negate . boost1Drapidity
-}

instance (Arbitrary a, Num a, Ord a) => Arbitrary (Gamma a) where
  arbitrary = Gamma <$> (arbitrary `suchThat` (\g -> g >= 1 || g < -1))

instance Arbitrary (Speed Double) where
  arbitrary = do v <-  genDouble `suchThat` \x -> x >= 0 && x < 1
                 sign <- arbitrary
                 pure $ Speed $ if sign then v else -v


deriving newtype instance Arbitrary (Rapidity Double)
