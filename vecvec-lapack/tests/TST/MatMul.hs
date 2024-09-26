{-# LANGUAGE AllowAmbiguousTypes #-}
-- | Tests for matrix-vector and matrix-matrix multiplication.
module TST.MatMul (tests) where

import Data.Typeable
import Test.Tasty
import Test.Tasty.QuickCheck

import Vecvec.Classes
import Vecvec.LAPACK.Vector    (Vec)
import Vecvec.LAPACK.Matrix    (Matrix)
import Vecvec.LAPACK.Hermitian (Hermitian)
import Vecvec.LAPACK.Symmetric (Symmetric)
import Vecvec.LAPACK.FFI       (S,D,C,Z)

import TST.Tools.MatModel
import TST.Tools.Util

tests :: TestTree
tests = testGroup "MatMul"
  [ -- Matrix-vector
    prop_matmul @Matrix        @Vec @S
  , prop_matmul @Matrix        @Vec @D
  , prop_matmul @Matrix        @Vec @C
  , prop_matmul @Matrix        @Vec @Z
  , prop_matmul @(Tr Matrix)   @Vec @S
  , prop_matmul @(Tr Matrix)   @Vec @D
  , prop_matmul @(Tr Matrix)   @Vec @C
  , prop_matmul @(Tr Matrix)   @Vec @Z
  , prop_matmul @(Conj Matrix) @Vec @S
  , prop_matmul @(Conj Matrix) @Vec @D
  , prop_matmul @(Conj Matrix) @Vec @C
  , prop_matmul @(Conj Matrix) @Vec @Z
  -- Symmetric-vector
  , prop_matmul @Symmetric        @Vec @S
  , prop_matmul @Symmetric        @Vec @D
  , prop_matmul @Symmetric        @Vec @C
  , prop_matmul @Symmetric        @Vec @Z
  , prop_matmul @(Tr Symmetric)   @Vec @S
  , prop_matmul @(Tr Symmetric)   @Vec @D
  , prop_matmul @(Tr Symmetric)   @Vec @C
  , prop_matmul @(Tr Symmetric)   @Vec @Z
    -- Matrix-matrix
    -- 1.
  , prop_matmul @Matrix        @Matrix @S
  , prop_matmul @Matrix        @Matrix @D
  , prop_matmul @Matrix        @Matrix @C
  , prop_matmul @Matrix        @Matrix @Z
  , prop_matmul @(Tr Matrix)   @Matrix @S
  , prop_matmul @(Tr Matrix)   @Matrix @D
  , prop_matmul @(Tr Matrix)   @Matrix @C
  , prop_matmul @(Tr Matrix)   @Matrix @Z
  , prop_matmul @(Conj Matrix) @Matrix @S
  , prop_matmul @(Conj Matrix) @Matrix @D
  , prop_matmul @(Conj Matrix) @Matrix @C
  , prop_matmul @(Conj Matrix) @Matrix @Z
    -- 2.
  , prop_matmul @Matrix        @(Tr Matrix) @S
  , prop_matmul @Matrix        @(Tr Matrix) @D
  , prop_matmul @Matrix        @(Tr Matrix) @C
  , prop_matmul @Matrix        @(Tr Matrix) @Z
  , prop_matmul @(Tr Matrix)   @(Tr Matrix) @S
  , prop_matmul @(Tr Matrix)   @(Tr Matrix) @D
  , prop_matmul @(Tr Matrix)   @(Tr Matrix) @C
  , prop_matmul @(Tr Matrix)   @(Tr Matrix) @Z
  , prop_matmul @(Conj Matrix) @(Tr Matrix) @S
  , prop_matmul @(Conj Matrix) @(Tr Matrix) @D
  , prop_matmul @(Conj Matrix) @(Tr Matrix) @C
  , prop_matmul @(Conj Matrix) @(Tr Matrix) @Z
    -- 3.
  , prop_matmul @Matrix        @(Conj Matrix) @S
  , prop_matmul @Matrix        @(Conj Matrix) @D
  , prop_matmul @Matrix        @(Conj Matrix) @C
  , prop_matmul @Matrix        @(Conj Matrix) @Z
  , prop_matmul @(Tr Matrix)   @(Conj Matrix) @S
  , prop_matmul @(Tr Matrix)   @(Conj Matrix) @D
  , prop_matmul @(Tr Matrix)   @(Conj Matrix) @C
  , prop_matmul @(Tr Matrix)   @(Conj Matrix) @Z
  , prop_matmul @(Conj Matrix) @(Conj Matrix) @S
  , prop_matmul @(Conj Matrix) @(Conj Matrix) @D
  , prop_matmul @(Conj Matrix) @(Conj Matrix) @C
  , prop_matmul @(Conj Matrix) @(Conj Matrix) @Z
    -- Hermitian-dense
  , prop_matmul @Matrix    @Hermitian @S
  , prop_matmul @Matrix    @Hermitian @D
  , prop_matmul @Matrix    @Hermitian @C
  , prop_matmul @Matrix    @Hermitian @Z
  , prop_matmul @Hermitian @Matrix    @S
  , prop_matmul @Hermitian @Matrix    @D
  , prop_matmul @Hermitian @Matrix    @C
  , prop_matmul @Hermitian @Matrix    @Z
  , prop_matmul @Hermitian @Hermitian @S
  , prop_matmul @Hermitian @Hermitian @D
  , prop_matmul @Hermitian @Hermitian @C
  , prop_matmul @Hermitian @Hermitian @Z
    -- Symmetric-dense
  , prop_matmul @Matrix    @Symmetric @S
  , prop_matmul @Matrix    @Symmetric @D
  , prop_matmul @Matrix    @Symmetric @C
  , prop_matmul @Matrix    @Symmetric @Z
  , prop_matmul @Symmetric @Matrix    @S
  , prop_matmul @Symmetric @Matrix    @D
  , prop_matmul @Symmetric @Matrix    @C
  , prop_matmul @Symmetric @Matrix    @Z
  , prop_matmul @Symmetric @Symmetric @S
  , prop_matmul @Symmetric @Symmetric @D
  , prop_matmul @Symmetric @Symmetric @C
  , prop_matmul @Symmetric @Symmetric @Z
  ]

-- Test for generalized matrix-vector multiplication.
prop_matmul
  :: forall v1 v2 a vR.
     ( TestMat v1 a, TestMat v2 a, TestMat vR a
     , MatMul a (Model1M v1) (Model1M v2) (Model1M vR)
     , MatMul a  v1           v2           vR
     , Typeable v1, Typeable v2, Typeable a
     , Eq   (vR a)
     , Show (vR a)
     , Show (Model1M v1 a)
     , Show (Model1M v2 a)
     , Show (Model1M vR a)
     , ArbitraryShape (Model1M v1) a
     , ArbitraryShape (Model1M v2) a
     )
  => TestTree
prop_matmul
  = testProperty (qualTypeName @v1 ++ " x " ++ qualTypeName @v2 ++ " / " ++ qualTypeName @a)
  $ \(MM (m1 :: Model1M v1 a) (m2 :: Model1M v2 a)) ->
      let v1 = unmodelMat m1 :: v1 a
          v2 = unmodelMat m2 :: v2 a
          m  = m1 @@ m2
          v  = v1 @@ v2
      in id $ counterexample ("MODEL = " ++ show m)
            $ counterexample ("IMPL  = " ++ show v)
            $ v == unmodelMat m



-- | Generate matrices with correct size for multiplication
data MM a b = MM a b

instance (Show a, Show b) => Show (MM a b) where
  show (MM a b) = show a ++ "\n" ++ show b

instance ( ArbitraryShape m1 a
         , ArbitraryShape m2 a
         ) => Arbitrary (MM (m1 a) (m2 a)) where
  arbitrary = do
    k <- genSize
    MM <$> arbitraryNCols k <*> arbitraryNRows k
