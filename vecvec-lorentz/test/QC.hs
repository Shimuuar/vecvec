{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE LexicalNegation            #-}
{-# LANGUAGE NegativeLiterals           #-}
{-# LANGUAGE QuantifiedConstraints      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE ViewPatterns               #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- |
module QC (tests) where

import Data.Typeable
import Data.Vector.Lorentz

import Numeric.AD
import Numeric.AD.Mode.Forward (Forward)
import Data.Number.CReal

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.QuickCheck.Gen


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
  , testGroup "Boost1D" $
    let prop :: forall b. (Arbitrary (b Double), Typeable b, BoostParam b, Functor b, Show (b Double)) => TestTree
        prop = testGroup (show (typeRep (Proxy @b)))
          [ testProperty "Norm conserved" (propBoost1DNorm @b)
          , testProperty "Inverse"        (propBoost1DInv  @b)
          ]
    in [ prop @Gamma
       , prop @Speed
       , prop @Rapidity
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


-- | Test that 1D boosts conserves norm.
--
--   Numeric analysis for boosts is _hard_ and they don't behave
--   particularly well (dreaded subtraction of close values!). o we
--   use computable reals for the test.
propBoost1DNorm
  :: (BoostParam b, Functor b)
  => b Double
  -> (Double,Double)
  -> Property
propBoost1DNorm b (realToFrac -> t, realToFrac -> x)
  = counterexample ("norm  = " ++ str_n)
  $ counterexample ("norm' = " ++ str_n')
  $ approxEqCReal n_digit str_n str_n'
  where
    (t',x') = boost1D (realToFrac <$> b) (t,x)
    n  = t*t   - x*x
    n' = t'*t' - x'*x'
    --
    str_n   = showCReal (n_digit+1) n
    str_n'  = showCReal (n_digit+1) n'
    n_digit = 12

-- | Test that 1D boosts computes inverse properly.
propBoost1DInv
  :: (BoostParam b, Functor b)
  => b Double
  -> (Double,Double)
  -> Property
propBoost1DInv (fmap realToFrac -> b) (realToFrac -> t, realToFrac -> x)
  = counterexample ("t  = " ++ str_t )
  $ counterexample ("x  = " ++ str_x )
  $ counterexample ("t' = " ++ str_t')
  $ counterexample ("x' = " ++ str_x')
  $ approxEqCReal n_digit str_t str_t'
 && approxEqCReal n_digit str_x str_x'
  where
    (t',x') = boost1D (invertBoostP b)
            $ boost1D b (t,x)
    --
    str_t   = showCReal (n_digit+1) t
    str_x   = showCReal (n_digit+1) x
    str_t'  = showCReal (n_digit+1) t'
    str_x'  = showCReal (n_digit+1) x'
    --
    n_digit = 12


approxEqCReal :: Int -> String -> String -> Bool
approxEqCReal n s_x s_y
  | abs x < 1 = abs(x - y)     < ε
  | otherwise = abs(x - y) / x < ε
  where
    ε = 10 ^^ -n
    x = read s_x :: Double
    y = read s_y :: Double



----------------------------------------------------------------
-- Orphanage
----------------------------------------------------------------

instance (Arbitrary a, Num a, Ord a) => Arbitrary (Gamma a) where
  arbitrary = Gamma <$> (arbitrary `suchThat` (\g -> g >= 1 || g < -1))

instance Arbitrary (Speed Double) where
  arbitrary = do v <-  genDouble `suchThat` \x -> x >= 0 && x < 1
                 sign <- arbitrary
                 pure $ Speed $ if sign then v else -v


deriving newtype instance Arbitrary (Rapidity Double)
