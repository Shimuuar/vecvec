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
-- Tests for matrix decomposition
module TST.Decomposition (tests, Epsilon(..)) where

import Foreign.Storable (Storable)
import Data.Typeable
import Data.Vector.Generic qualified as VG
import Test.Tasty
import Test.Tasty.QuickCheck

import Vecvec.Classes
import Vecvec.Classes.NDArray
import Vecvec.LAPACK                       qualified as VV
import Vecvec.LAPACK.FFI                   (S,D,C,Z)
import Vecvec.LAPACK.Matrix.Dense          (Matrix,gdiag)
import Vecvec.LAPACK.Matrix.Dense          qualified as Mat
import Vecvec.LAPACK.LinAlg

import TST.Tools.Model

tests :: TestTree
tests = testGroup "Decomposition"
  [ testGroup "SVD"
    [ testSVD @S
    , testSVD @D
    , testSVD @C
    , testSVD @Z
    ]
  ]


testSVD :: forall a. (VV.LAPACKy a, Typeable a, ScalarModel a, Eq a, Show a
                     , Storable (R a), Epsilon (R a), Ord (R a), Floating (R a)
                     )
        => TestTree
testSVD = testGroup (show (typeOf (undefined :: a)))
  [ testProperty "SVD valid"   $ prop_SVD_valid     @a
  , testProperty "U,V unitary" $ prop_SVD_unitarity @a
  ]

-- | Check that SVD decomposition of matrix is really decomposition
prop_SVD_valid
  :: ( VV.LAPACKy a, Typeable a, Show a, Eq a, ScalarModel a
     , Storable (R a), Epsilon (R a), Ord (R a), Floating (R a)
     )
  => Model (Matrix a)
  -> Property
prop_SVD_valid (fromModel -> mat)
  = counterexample ("mat   = \n" ++ show mat)
  $ counterexample ("mat'  = \n" ++ show mat')
  $ counterexample ("delta = \n" ++ show delta)
  $ Mat.all (\x -> scalarNorm x < epsilon) delta
  where
    sz        = shape mat
    (u,sig,v) = decomposeSVD mat
    mat'      = u @@ gdiag sz (VG.map fromR sig) @@ v
    delta     = mat' .-. mat

-- | Check that SVD decomposition of matrix is really decomposition
prop_SVD_unitarity
  :: ( VV.LAPACKy a, Typeable a, Show a, Eq a, ScalarModel a
     , Storable (R a), Epsilon (R a), Ord (R a), Floating (R a)
     )
  => Model (Matrix a)
  -> Property
prop_SVD_unitarity (fromModel -> mat)
  = counterexample ("mat   = \n" ++ show mat)
  $ counterexample ("delta U = \n" ++ show deltaU)
  $ counterexample ("delta V = \n" ++ show deltaV)
  $  Mat.all (\x -> scalarNorm x < epsilon) deltaU
  && Mat.all (\x -> scalarNorm x < epsilon) deltaV
  where
    (n,k)   = shape mat
    (u,_,v) = decomposeSVD mat
    deltaU  = (Conj u @@ u) .-. Mat.eye n
    deltaV  = (Conj v @@ v) .-. Mat.eye k



class Epsilon a where
  epsilon :: a

instance Epsilon Float  where epsilon = 1e-4
instance Epsilon Double where epsilon = 1e-12
  
