{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost        #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ViewPatterns               #-}
-- | Tests for matrix-vector and matrix-matrix multiplication.
module TST.MatMul (tests) where

import Test.Tasty
import Test.Tasty.QuickCheck

import Vecvec.Classes
import Vecvec.Classes.NDArray
import Vecvec.LAPACK                       qualified as VV
import Vecvec.LAPACK.Internal.Matrix.Dense (Matrix)
import Vecvec.LAPACK.FFI                   (S,D,C,Z)

import TST.Tools.Model
import TST.Tools.Util

tests :: TestTree
tests = testGroup "MatMul"
  [ -- Matrix-vector
    prop_matmul @(Matrix S)        @(VV.Vec S) unMV
  , prop_matmul @(Matrix D)        @(VV.Vec D) unMV
  , prop_matmul @(Matrix C)        @(VV.Vec C) unMV
  , prop_matmul @(Matrix Z)        @(VV.Vec Z) unMV
  , prop_matmul @(Tr (Matrix S))   @(VV.Vec S) unMV
  , prop_matmul @(Tr (Matrix D))   @(VV.Vec D) unMV
  , prop_matmul @(Tr (Matrix C))   @(VV.Vec C) unMV
  , prop_matmul @(Tr (Matrix Z))   @(VV.Vec Z) unMV
  , prop_matmul @(Conj (Matrix S)) @(VV.Vec S) unMV
  , prop_matmul @(Conj (Matrix D)) @(VV.Vec D) unMV
  , prop_matmul @(Conj (Matrix C)) @(VV.Vec C) unMV
  , prop_matmul @(Conj (Matrix Z)) @(VV.Vec Z) unMV
    -- Matrix-matrix
    -- 1.
  , prop_matmul @(Matrix S)        @(Matrix S) unMM
  , prop_matmul @(Matrix D)        @(Matrix D) unMM
  , prop_matmul @(Matrix C)        @(Matrix C) unMM
  , prop_matmul @(Matrix Z)        @(Matrix Z) unMM
  , prop_matmul @(Tr (Matrix S))   @(Matrix S) unMM
  , prop_matmul @(Tr (Matrix D))   @(Matrix D) unMM
  , prop_matmul @(Tr (Matrix C))   @(Matrix C) unMM
  , prop_matmul @(Tr (Matrix Z))   @(Matrix Z) unMM
  , prop_matmul @(Conj (Matrix S)) @(Matrix S) unMM
  , prop_matmul @(Conj (Matrix D)) @(Matrix D) unMM
  , prop_matmul @(Conj (Matrix C)) @(Matrix C) unMM
  , prop_matmul @(Conj (Matrix Z)) @(Matrix Z) unMM
    -- 2.
  , prop_matmul @(Matrix S)        @(Tr (Matrix S)) unMM
  , prop_matmul @(Matrix D)        @(Tr (Matrix D)) unMM
  , prop_matmul @(Matrix C)        @(Tr (Matrix C)) unMM
  , prop_matmul @(Matrix Z)        @(Tr (Matrix Z)) unMM
  , prop_matmul @(Tr (Matrix S))   @(Tr (Matrix S)) unMM
  , prop_matmul @(Tr (Matrix D))   @(Tr (Matrix D)) unMM
  , prop_matmul @(Tr (Matrix C))   @(Tr (Matrix C)) unMM
  , prop_matmul @(Tr (Matrix Z))   @(Tr (Matrix Z)) unMM
  , prop_matmul @(Conj (Matrix S)) @(Tr (Matrix S)) unMM
  , prop_matmul @(Conj (Matrix D)) @(Tr (Matrix D)) unMM
  , prop_matmul @(Conj (Matrix C)) @(Tr (Matrix C)) unMM
  , prop_matmul @(Conj (Matrix Z)) @(Tr (Matrix Z)) unMM
    -- 3.
  , prop_matmul @(Matrix S)        @(Conj (Matrix S)) unMM
  , prop_matmul @(Matrix D)        @(Conj (Matrix D)) unMM
  , prop_matmul @(Matrix C)        @(Conj (Matrix C)) unMM
  , prop_matmul @(Matrix Z)        @(Conj (Matrix Z)) unMM
  , prop_matmul @(Tr (Matrix S))   @(Conj (Matrix S)) unMM
  , prop_matmul @(Tr (Matrix D))   @(Conj (Matrix D)) unMM
  , prop_matmul @(Tr (Matrix C))   @(Conj (Matrix C)) unMM
  , prop_matmul @(Tr (Matrix Z))   @(Conj (Matrix Z)) unMM
  , prop_matmul @(Conj (Matrix S)) @(Conj (Matrix S)) unMM
  , prop_matmul @(Conj (Matrix D)) @(Conj (Matrix D)) unMM
  , prop_matmul @(Conj (Matrix C)) @(Conj (Matrix C)) unMM
  , prop_matmul @(Conj (Matrix Z)) @(Conj (Matrix Z)) unMM
  ]

-- Test for generalized matrix-vector multiplication.
prop_matmul
  :: forall v1 v2 vR a.
     ( IsModel v1
     , IsModel v2
     , IsModel vR
     , MatMul (ModelRepr v1) (ModelRepr v2) (ModelRepr vR)
     , MatMul v1 v2 vR
     , Arbitrary a, Show a
     )
  => (a -> (ModelRepr v1, ModelRepr v2))
  -> TestTree
prop_matmul to_pair
  = testProperty (qualTypeName @v1 ++ " x " ++ qualTypeName @v2)
  $ \(to_pair -> (mr1, mr2)) ->
      let m1 = Model mr1    :: Model v1
          m2 = Model mr2    :: Model v2
          v1 = fromModel m1 :: v1
          v2 = fromModel m2 :: v2
          m  = mr1 @@ mr2
          v  = v1  @@ v2
      in id $ counterexample ("MODEL = " ++ show m)
            $ counterexample ("IMPL  = " ++ show v)
            $ v == fromModel (Model m)



-- | Generate matrices with correct size for multiplication
newtype MM a b = MM { unMM :: (a,b) }

instance (Show a, Show b) => Show (MM a b) where
  show (MM (a,b)) = show a ++ "\n" ++ show b

instance (NDim a ~ 2, NDim b ~ 2, ArbitraryShape a, ArbitraryShape b) => Arbitrary (MM a b) where
  arbitrary = do
    (n,k,m) <- genSize
    MM <$> ((,) <$> arbitraryShape (n,k) <*> arbitraryShape (k,m))

-- | Generate matrix and vector with correct size for multiplication
newtype MV a b = MV { unMV :: (a,b) }

instance (Show a, Show b) => Show (MV a b) where
  show (MV (a,b)) = show a ++ "\n" ++ show b

instance (NDim a ~ 2, NDim b ~ 1, ArbitraryShape a, ArbitraryShape b) => Arbitrary (MV a b) where
  arbitrary = do
    (n,k) <- genSize
    MV <$> ((,) <$> arbitraryShape (n,k) <*> arbitraryShape k)
