import Control.Applicative
import Control.Monad

import Data.VectorSpace
import Data.VectorSpace.Spatial
import Data.VectorSpace.Lorentz

import Test.QuickCheck
import System.Random
import Debug.Trace

----------------------------------------------------------------

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
instance Arbitrary Gamma where
    arbitrary = Gamma <$> suchThat arbitrary (> 1)
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

-- Tests for conversions between speed,rapidity and γ-factor
testsConvert :: [(String,IO())]
testsConvert = 
    [ ("==== Conversions ====", return ())
    , ("φ->γ->φ", p (apprEqualTest (gammaToRapidity . rapidityToGamma) . unRapidity))
    , ("φ->β->φ", p (apprEqualTest (vToRapidity     . rapidityToV    ) . unSmallRapidity))
    , ("γ->φ->γ", p (apprEqualTest (rapidityToGamma . gammaToRapidity) . unGamma))
    , ("γ->β->γ", p (apprEqualTest (vToGamma        . gammaToV       ) . unGamma))
    , ("β->φ->β", p (apprEqualTest (rapidityToV     . vToRapidity    ) . unSpeed))
    , ("β->γ->β", p (apprEqualTest (gammaToV        . vToGamma       ) . unSpeed))
    ]

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
