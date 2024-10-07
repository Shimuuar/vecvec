{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
-- |
-- Tests for solving linear systems. Here we want to tests that we
-- indeed produce solution for linear system and call LAPACK
-- correctly. But we don't care about numerical properties much
module TST.LinSolve (tests) where

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
import Vecvec.Classes.NDArray
import Vecvec.LAPACK.FFI             (S,D,C,Z)
import Vecvec.LAPACK.Vector          (Vec,LAPACKy)
import Vecvec.LAPACK.Matrix          (Matrix)
import Vecvec.LAPACK.Matrix          qualified as Mat
import Vecvec.LAPACK.Symmetric       (Symmetric)
import Vecvec.LAPACK.Hermitian       (Hermitian)
import Vecvec.LAPACK.LinAlg

import TST.Tools.MatModel
import TST.Tools.Util


-- | Run tests for solvers for linear systems
tests :: TestTree
tests = testGroup "LinAlg"
  [ testGroup "solveLinEq"
    [ testGroup "General"
      [ testSimpleSolve @Matrix @(Vec S) @S
      , testSimpleSolve @Matrix @(Vec D) @D
      , testSimpleSolve @Matrix @(Vec C) @C
      , testSimpleSolve @Matrix @(Vec Z) @Z
        --
      , testSimpleSolve @Matrix @(V.Vector S) @S
      , testSimpleSolve @Matrix @(V.Vector D) @D
      , testSimpleSolve @Matrix @(V.Vector C) @C
      , testSimpleSolve @Matrix @(V.Vector Z) @Z
        --
      , testSimpleSolve @Matrix @(VS.Vector S) @S
      , testSimpleSolve @Matrix @(VS.Vector D) @D
      , testSimpleSolve @Matrix @(VS.Vector C) @C
      , testSimpleSolve @Matrix @(VS.Vector Z) @Z
        --
      , testSimpleSolve @Matrix @(VU.Vector S) @S
      , testSimpleSolve @Matrix @(VU.Vector D) @D
      , testSimpleSolve @Matrix @(VU.Vector C) @C
      , testSimpleSolve @Matrix @(VU.Vector Z) @Z
        --
      , testSimpleSolve @Matrix @(VP.Vector S) @S
      , testSimpleSolve @Matrix @(VP.Vector D) @D
      , testSimpleSolve @Matrix @(VP.Vector C) @C
      , testSimpleSolve @Matrix @(VP.Vector Z) @Z
        --
      , testSimpleSolve @Matrix @(Matrix S) @S
      , testSimpleSolve @Matrix @(Matrix D) @D
      , testSimpleSolve @Matrix @(Matrix C) @C
      , testSimpleSolve @Matrix @(Matrix Z) @Z
        -- Tuples
      , testSimpleSolve @Matrix @(Matrix S, V.Vector S) @S
      , testSimpleSolve @Matrix @(Matrix D, V.Vector D) @D
        -- Lists
      , testSimpleSolve @Matrix @[Matrix S] @S
      , testSimpleSolve @Matrix @[Matrix D] @D
      ]
    , testGroup "Symmetric"
      [ testSimpleSolve @Symmetric @(Matrix S) @S
      , testSimpleSolve @Symmetric @(Matrix D) @D
      , testSimpleSolve @Symmetric @(Matrix C) @C
      , testSimpleSolve @Symmetric @(Matrix Z) @Z
      ]
    , testGroup "Hermitian"
      [ testSimpleSolve @Hermitian @(Matrix S) @S
      , testSimpleSolve @Hermitian @(Matrix D) @D
      , testSimpleSolve @Hermitian @(Matrix C) @C
      , testSimpleSolve @Hermitian @(Matrix Z) @Z
      ]
    ]
  , testGroup "invertMatrix"
    [ testProperty "S" $ prop_invertMatrix @S
    , testProperty "D" $ prop_invertMatrix @D
    , testProperty "C" $ prop_invertMatrix @C
    , testProperty "Z" $ prop_invertMatrix @Z
    ]
  ]


-- | Run tests for simple solver
testSimpleSolve
  :: forall mat rhs a.
     ( ArbitraryRHS rhs a
     , EquationRHS  rhs a
     , LinearEq     mat a
     , MatMul a mat Vec Vec
     , Scalar (mat a) ~ a
     , Arbitrary (LinSimple mat rhs a)
     , VS.Storable a
     , Show (mat a), Show rhs, Show a
     , Typeable rhs, Typeable mat, Typeable a
     )
  => TestTree
testSimpleSolve
  = testProperty ("Mat = "++qualTypeName @mat
               ++ " ; RHS = " ++ qualTypeName @rhs
               ++ " ; a = " ++ qualTypeName @a  )
  $ prop_SimpleSolve @mat @rhs @a


-- | Inverse is indeed inverse
prop_invertMatrix
  :: forall a.
     ( LAPACKy a, Epsilon (R a), Floating (R a), Ord (R a))
  => Nonsingular Matrix a
  -> Property
prop_invertMatrix (Nonsingular m)
  = property
  $ matrixNearZero z
  where
    m' = invertMatrix m
    z  = m' @@ m .-. Mat.eye (nCols m)

-- | Test that solution of linear system is indeed solution
prop_SimpleSolve
  :: forall mat rhs a. ( ArbitraryRHS rhs a
                       , EquationRHS  rhs a
                       , LinearEq     mat a
                       , MatMul a mat Vec Vec
                       , a ~ Scalar (mat a)
                       )
  => LinSimple mat rhs a
  -> Property
prop_SimpleSolve (LinSimple (Nonsingular a) rhs)
  = checkLinEq a (a \\\ rhs) rhs


-- | Simple linear equation @Ax = b@ for a given right hand side
data LinSimple mat rhs a = LinSimple (Nonsingular mat a) rhs

instance (Show a, Show (mat a), Show rhs, VS.Storable a) => Show (LinSimple mat rhs a) where
  show (LinSimple a rhs) = "A = \n"++show a++"\nb =\n"++show rhs

instance ( Show a,Eq a,LAPACKy a,SmallScalar a,Typeable a,ArbitraryRHS rhs a
         ) => Arbitrary (LinSimple Matrix rhs a) where
  arbitrary = do
    sz  <- genSize @Int
    a   <- genNonsingularMatrix sz
    rhs <- arbitraryRHS (Proxy @a) sz
    pure $ LinSimple (Nonsingular a) rhs

instance ( Show a,Eq a,LAPACKy a,SmallScalar a,Typeable a,ArbitraryRHS rhs a
         ) => Arbitrary (LinSimple Symmetric rhs a) where
  arbitrary = do
    sz  <- genSize @Int
    a   <- genNonsingularSymmetric sz
    rhs <- arbitraryRHS (Proxy @a) sz
    pure $ LinSimple (Nonsingular a) rhs

instance ( Show a,Eq a,LAPACKy a,SmallScalar a,Typeable a,ArbitraryRHS rhs a
         ) => Arbitrary (LinSimple Hermitian rhs a) where
  arbitrary = do
    sz  <- genSize @Int
    a   <- genNonsingularHermitian sz
    rhs <- arbitraryRHS (Proxy @a) sz
    pure $ LinSimple (Nonsingular a) rhs



-- | Generate arbitrary right hand side for equation and check it for validity
class ArbitraryRHS rhs a where
  arbitraryRHS :: SmallScalar a => Proxy a -> Int -> Gen rhs
  checkLinEq   :: ( Scalar (mat a) ~ a
                  , MatMul a mat Vec Vec
                  ) => mat a -> rhs -> rhs -> Property


instance (a ~ a', LAPACKy a, ComparableN a) => ArbitraryRHS (Matrix a) a' where
  arbitraryRHS _ sz = do
    n <- choose (1,4)
    arbitraryShape (sz,n)
  checkLinEq a (Mat.toColList -> xs) (Mat.toColList -> bs)
    | length xs /= length bs = error "Lengths do not match"
    | otherwise              = property $ all nearZero deltas
    where
      deltas = [ a @@ x .-. b | (x,b) <- xs `zip` bs]


instance (a ~ a', LAPACKy a, ComparableN a) => ArbitraryRHS (Vec a) a' where
  arbitraryRHS _ n = VG.replicateM n genScalar
  checkLinEq a x b = property $ nearZero delta where
    delta = a @@ x .-. b

instance (a ~ a', LAPACKy a, ComparableN a) => ArbitraryRHS (V.Vector a) a' where
  arbitraryRHS _ n = VG.replicateM n genScalar
  checkLinEq a x b = property $ nearZero delta where
    delta = a @@ (VG.convert x :: Vec a) .-. (VG.convert b :: Vec a)

instance (a ~ a', VU.Unbox a, LAPACKy a, ComparableN a) => ArbitraryRHS (VU.Vector a) a' where
  arbitraryRHS _ n = VG.replicateM n genScalar
  checkLinEq a x b = property $ nearZero delta where
    delta = a @@ (VG.convert x :: Vec a) .-. (VG.convert b :: Vec a)

instance (a ~ a', LAPACKy a, ComparableN a) => ArbitraryRHS (VS.Vector a) a' where
  arbitraryRHS _ n = VG.replicateM n genScalar
  checkLinEq a x b = property $ nearZero delta where
    delta = a @@ (VG.convert x :: Vec a) .-. (VG.convert b :: Vec a)

instance (a ~ a', VP.Prim a, LAPACKy a, ComparableN a) => ArbitraryRHS (VP.Vector a) a' where
  arbitraryRHS _ n = VG.replicateM n genScalar
  checkLinEq a x b = property $ nearZero delta where
    delta = a @@ (VG.convert x :: Vec a) .-. (VG.convert b :: Vec a)

instance ( ArbitraryRHS r1 a, ArbitraryRHS r2 a
         ) => ArbitraryRHS (r1,r2) a where
  arbitraryRHS p n = (,) <$> arbitraryRHS p n <*> arbitraryRHS p n
  checkLinEq a (x1,x2) (r1,r2)
    =    checkLinEq a x1 r1
    .&&. checkLinEq a x2 r2

instance (ArbitraryRHS r a) => ArbitraryRHS [r] a where
  arbitraryRHS p n = do
    l <- choose (0,3)
    replicateM l (arbitraryRHS p n)
  checkLinEq _ []     []     = property True
  checkLinEq _ []     _      = property False
  checkLinEq _ _      []     = property False
  checkLinEq a (x:xs) (b:bs) = checkLinEq a x  b
                          .&&. checkLinEq a xs bs



type ComparableN a = (Epsilon (R a), Floating (R a), Ord (R a))

nearZero :: (VG.Vector v a, ComparableN a, NormedScalar a) => v a -> Bool
nearZero = VG.all (\d -> scalarNorm d < epsilon)

matrixNearZero :: (VS.Storable a, ComparableN a, NormedScalar a) => Matrix a -> Bool
matrixNearZero = Mat.all (\d -> scalarNorm d < epsilon)
