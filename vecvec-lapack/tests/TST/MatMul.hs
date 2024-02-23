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

import Data.Typeable
import Test.Tasty
import Test.Tasty.QuickCheck

import Vecvec.Classes
import Vecvec.Classes.NDArray
import Vecvec.LAPACK                           qualified as VV
import Vecvec.LAPACK.Internal.Matrix.Dense     (Matrix)
import Vecvec.LAPACK.Internal.Matrix.Symmetric (Symmetric)
import Vecvec.LAPACK.FFI                       (S,D,C,Z)

import TST.Tools.MatModel
import TST.Tools.Model                     (TestData1(..))
import TST.Tools.Util

tests :: TestTree
tests = testGroup "MatMul" 
  [ -- Matrix-vector
    prop_matmul @Matrix        @VV.Vec @S unMV
  , prop_matmul @Matrix        @VV.Vec @D unMV
  , prop_matmul @Matrix        @VV.Vec @C unMV
  , prop_matmul @Matrix        @VV.Vec @Z unMV
  , prop_matmul @(Tr Matrix)   @VV.Vec @S unMV
  , prop_matmul @(Tr Matrix)   @VV.Vec @D unMV
  , prop_matmul @(Tr Matrix)   @VV.Vec @C unMV
  , prop_matmul @(Tr Matrix)   @VV.Vec @Z unMV
  , prop_matmul @(Conj Matrix) @VV.Vec @S unMV
  , prop_matmul @(Conj Matrix) @VV.Vec @D unMV
  , prop_matmul @(Conj Matrix) @VV.Vec @C unMV
  , prop_matmul @(Conj Matrix) @VV.Vec @Z unMV
  -- Symmetric-vector
  , prop_matmul @Symmetric        @VV.Vec @S unMV1
  , prop_matmul @Symmetric        @VV.Vec @D unMV1
  , prop_matmul @Symmetric        @VV.Vec @C unMV1
  , prop_matmul @Symmetric        @VV.Vec @Z unMV1
  -- , prop_matmul @(Tr Symmetric)   @VV.Vec @S unMV1
  -- , prop_matmul @(Tr Symmetric)   @VV.Vec @D unMV1
  -- , prop_matmul @(Tr Symmetric)   @VV.Vec @C unMV1
  -- , prop_matmul @(Tr Symmetric)   @VV.Vec @Z unMV1
    -- Matrix-matrix
    -- 1.
  , prop_matmul @Matrix        @Matrix @S unMM
  , prop_matmul @Matrix        @Matrix @D unMM
  , prop_matmul @Matrix        @Matrix @C unMM
  , prop_matmul @Matrix        @Matrix @Z unMM
  , prop_matmul @(Tr Matrix)   @Matrix @S unMM
  , prop_matmul @(Tr Matrix)   @Matrix @D unMM
  , prop_matmul @(Tr Matrix)   @Matrix @C unMM
  , prop_matmul @(Tr Matrix)   @Matrix @Z unMM
  , prop_matmul @(Conj Matrix) @Matrix @S unMM
  , prop_matmul @(Conj Matrix) @Matrix @D unMM
  , prop_matmul @(Conj Matrix) @Matrix @C unMM
  , prop_matmul @(Conj Matrix) @Matrix @Z unMM
    -- 2.
  , prop_matmul @Matrix        @(Tr Matrix) @S unMM
  , prop_matmul @Matrix        @(Tr Matrix) @D unMM
  , prop_matmul @Matrix        @(Tr Matrix) @C unMM
  , prop_matmul @Matrix        @(Tr Matrix) @Z unMM
  , prop_matmul @(Tr Matrix)   @(Tr Matrix) @S unMM
  , prop_matmul @(Tr Matrix)   @(Tr Matrix) @D unMM
  , prop_matmul @(Tr Matrix)   @(Tr Matrix) @C unMM
  , prop_matmul @(Tr Matrix)   @(Tr Matrix) @Z unMM
  , prop_matmul @(Conj Matrix) @(Tr Matrix) @S unMM
  , prop_matmul @(Conj Matrix) @(Tr Matrix) @D unMM
  , prop_matmul @(Conj Matrix) @(Tr Matrix) @C unMM
  , prop_matmul @(Conj Matrix) @(Tr Matrix) @Z unMM
    -- 3.
  , prop_matmul @Matrix        @(Conj Matrix) @S unMM
  , prop_matmul @Matrix        @(Conj Matrix) @D unMM
  , prop_matmul @Matrix        @(Conj Matrix) @C unMM
  , prop_matmul @Matrix        @(Conj Matrix) @Z unMM
  , prop_matmul @(Tr Matrix)   @(Conj Matrix) @S unMM
  , prop_matmul @(Tr Matrix)   @(Conj Matrix) @D unMM
  , prop_matmul @(Tr Matrix)   @(Conj Matrix) @C unMM
  , prop_matmul @(Tr Matrix)   @(Conj Matrix) @Z unMM
  , prop_matmul @(Conj Matrix) @(Conj Matrix) @S unMM
  , prop_matmul @(Conj Matrix) @(Conj Matrix) @D unMM
  , prop_matmul @(Conj Matrix) @(Conj Matrix) @C unMM
  , prop_matmul @(Conj Matrix) @(Conj Matrix) @Z unMM
  ]

-- Test for generalized matrix-vector multiplication.
prop_matmul
  :: forall v1 v2 a vR p.
     ( TestMatrix1 v1 a, TestMatrix1 v2 a, TestMatrix1 vR a
     , MatMul (Model1M v1 a) (Model1M v2 a) (Model1M vR a)
     , MatMul (v1 a)         (v2 a)         (vR a)
     , Typeable v1, Typeable v2, Typeable a
     , Eq (vR a), Show (vR a), Show (Model1M vR a), Show p, Arbitrary p
     )
  => (p -> (Model1M v1 a, Model1M v2 a))
  -> TestTree
prop_matmul to_pair
  = testProperty (qualTypeName @v1 ++ " x " ++ qualTypeName @v2 ++ " / " ++ qualTypeName @a)
  $ \(to_pair -> (m1, m2)) ->
      let v1 = liftUnmodel TagMat id m1 :: v1 a
          v2 = liftUnmodel TagMat id m2 :: v2 a
          m  = m1 @@ m2
          v  = v1 @@ v2
      in id $ counterexample ("MODEL = " ++ show m)
            $ counterexample ("IMPL  = " ++ show v)
            $ v == liftUnmodel TagMat id m



-- | Generate matrices with correct size for multiplication
newtype MM a b = MM { unMM :: (a,b) }

instance (Show a, Show b) => Show (MM a b) where
  show (MM (a,b)) = show a ++ "\n" ++ show b

instance ( Rank m1 ~ 2
         , Rank m2 ~ 2
         , CreationRank m1 ~ 2
         , CreationRank m2 ~ 2
         , ArbitraryShape m1 a
         , ArbitraryShape m2 a
         ) => Arbitrary (MM (m1 a) (m2 a)) where
  arbitrary = do
    (n,k,m) <- genSize
    MM <$> ((,) <$> arbitraryShape (n,k) <*> arbitraryShape (k,m))

-- | Generate matrix and vector with correct size for multiplication
newtype MV a b = MV { unMV :: (a,b) }

instance (Show a, Show b) => Show (MV a b) where
  show (MV (a,b)) = show a ++ "\n" ++ show b

instance ( Rank m ~ 2
         , Rank v ~ 1
         , CreationRank m ~ 2
         , CreationRank v ~ 1
         , ArbitraryShape m a
         , ArbitraryShape v a
         ) => Arbitrary (MV (m a) (v a)) where
  arbitrary = do
    (n,k) <- genSize
    MV <$> ((,) <$> arbitraryShape (n,k) <*> arbitraryShape k)

-- | Generate matrix and vector with correct size for multiplication
newtype MV1 a b = MV1 { unMV1 :: (a,b) }

instance (Show a, Show b) => Show (MV1 a b) where
  show (MV1 (a,b)) = show a ++ "\n" ++ show b

instance ( Rank m ~ 2
         , Rank v ~ 1
         , CreationRank m ~ 1
         , CreationRank v ~ 1
         , ArbitraryShape m a
         , ArbitraryShape v a
         ) => Arbitrary (MV1 (m a) (v a)) where
  arbitrary = do
    n <- genSize @Int
    MV1 <$> ((,) <$> arbitraryShape n <*> arbitraryShape n)
