{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeSynonymInstances #-}
import Control.Applicative

import Data.Classes.VectorSpace
import qualified Data.Vector.Fixed as F
import           Data.Vector.Fixed.Unboxed (Unbox)
import Data.Vector.Lorentz

import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck
import System.Random (Random)
import Helpers

main :: IO ()
main = do
  defaultMain $ testGroup "Lorentz"
    [ testGroup "Conversions"
        [ testProperty "γ→v→γ" $ testConversion (T :: T (Gamma Double)) (T :: T (Speed    Double)) getGamma
        , testProperty "γ→φ→γ" $ testConversion (T :: T (Gamma Double)) (T :: T (Rapidity Double)) getGamma
        , testProperty "v→γ→v" $ testConversion (T :: T (Speed Double)) (T :: T (Gamma    Double)) getSpeed
        , testProperty "v→φ→v" $ testConversion (T :: T (Speed Double)) (T :: T (Rapidity Double)) getSpeed
        , testProperty "φ→v→φ" $ testConversion (T :: T (Rapidity Double)) (T :: T (Speed Double)) getRapidity
        , testProperty "φ→γ→φ" $ testConversion (T :: T (Rapidity Double)) (T :: T (Gamma Double)) getRapidity
        ]
    , testGroup "Norm preservation"
        [ testProperty "γ X" $ testTransformNorm (T :: T (Gamma Double)) boostX
        , testProperty "γ X" $ testTransformNorm (T :: T (Gamma Double)) boostY
        , testProperty "γ X" $ testTransformNorm (T :: T (Gamma Double)) boostZ
          --
        , testProperty "v X" $ testTransformNorm (T :: T (Speed Double)) boostX
        , testProperty "v X" $ testTransformNorm (T :: T (Speed Double)) boostY
        , testProperty "v X" $ testTransformNorm (T :: T (Speed Double)) boostZ
          --
        , testProperty "φ X" $ testTransformNorm (T :: T (Rapidity Double)) boostX
        , testProperty "φ X" $ testTransformNorm (T :: T (Rapidity Double)) boostY
        , testProperty "φ X" $ testTransformNorm (T :: T (Rapidity Double)) boostZ          
        ]
    ]
----------------------------------------------------------------
-- QC Instances
----------------------------------------------------------------

instance (Num a, Random a) => Arbitrary (Gamma a) where
  arbitrary = do
    γ    <- choose (1,500)
    sign <- arbitrary
    return $ if sign then Gamma γ else Gamma (-γ)

instance (Num a, Random a) => Arbitrary (Rapidity a) where
  arbitrary = Rapidity <$> choose (-10,10)

instance (Num a, Ord a, Random a) => Arbitrary (Speed a) where
  arbitrary = Speed <$> suchThat (choose (-1,1)) (\v -> v >= -1 && v < 1)

instance (Arbitrary a, Unbox 4 a) => Arbitrary (Lorentz a) where
  arbitrary =  F.mk4
           <$> arbitrary
           <*> arbitrary
           <*> arbitrary
           <*> arbitrary

----------------------------------------------------------------
-- Boost tests
----------------------------------------------------------------


-- Conversions should roundtrip correctly
testConversion :: forall a b. (Convert a b, Convert b a, Show a, Show b)
               => T a -> T b -> (a -> Double) -> a -> Property
testConversion _ _ get a
  = counterexample ("a  = " ++ show a )
  $ counterexample ("b  = " ++ show b )
  $ counterexample ("a' = " ++ show a')
  $ counterexample ("ε  = " ++ show (abs (x - x') / max x x'))
  $ eq 1e-9 x x'
  where
    x  = get a
    x' = get a'
    b  = convert a :: b
    a' = convert b

-- Lorentz transformation should preserve norm
testTransformNorm :: (BoostParam b)
                  => T (b Double)
                  -> (b Double -> Lorentz Double -> Lorentz Double)
                  ->  b Double -> Lorentz Double -> Property
testTransformNorm _ boost p v
  = counterexample ("m  = " ++ show m')
  $ counterexample ("m' = " ++ show m')
  $ counterexample ("ε  = " ++ show (abs (m - m') / max m m'))
  $ eq 1e-6 m m'
  where
    v' = boost p v
    m  = magnitudeSq v
    m' = magnitudeSq v'
