{-# LANGUAGE AllowAmbiguousTypes #-}
-- |
-- Tests for matrix decomposition
module TST.Decomposition (tests) where

import Foreign.Storable (Storable)
import Data.Complex     (Complex)
import Data.Typeable
import Data.Vector.Generic qualified as VG
import Test.Tasty
import Test.Tasty.QuickCheck

import Vecvec.Classes
import Vecvec.Classes.NDArray
import Vecvec.Classes.Containers
import Vecvec.LAPACK.FFI             (S,D,C,Z)
import Vecvec.LAPACK.Matrix          (Matrix,LAPACKy,gdiag)
import Vecvec.LAPACK.Matrix          qualified as Mat
import Vecvec.LAPACK.LinAlg

import TST.Tools.MatModel
import TST.Tools.Util


tests :: TestTree
tests = testGroup "Decomposition"
  [ testGroup "SVD"
    [ testSVD @S
    , testSVD @D
    , testSVD @C
    , testSVD @Z
    ]
  , testGroup "eig"
    [ testEig @S
    , testEig @D
    , testEig @C
    , testEig @Z
    ]  
  ]


testSVD :: forall a. ( LAPACKy a, Typeable a, SmallScalar a, Show a
                     , Storable (R a), Epsilon (R a), Ord (R a), Floating (R a)
                     )
        => TestTree
testSVD = testGroup (show (typeOf (undefined :: a)))
  [ testProperty "SVD valid"   $ prop_SVD_valid     @a
  , testProperty "U,V unitary" $ prop_SVD_unitarity @a
  ]

-- | Check that SVD decomposition of matrix is really decomposition
prop_SVD_valid
  :: ( LAPACKy a, Show a
     , Storable (R a)
     , Epsilon (R a)
     , Ord (R a)
     , Floating (R a)
     )
  => Matrix a
  -> Property
prop_SVD_valid mat
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
  :: ( LAPACKy a, Show a
     , Storable (R a), Epsilon (R a), Ord (R a), Floating (R a)
     )
  => Matrix a
  -> Property
prop_SVD_unitarity mat
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


testEig
  :: forall a. ( LAPACKy a, Typeable a, SmallScalar a, Show a, ToComplex a
               , Storable (R a), Epsilon (R a), Ord (R a), RealFloat (R a)
               , Show (R a), LAPACKy (R a), LAPACKy (Complex (R a))
               )
  => TestTree
testEig = testGroup (show (typeOf (undefined :: a)))
  [ testProperty "eig valid"   $ prop_eig_valid     @a
  ]

prop_eig_valid
  :: ( LAPACKy a, Show a, ToComplex a, LAPACKy (R a), LAPACKy (Complex (R a))
     , Storable (R a), Epsilon (R a), Ord (R a), RealFloat (R a), Show (R a))
  => Square a
  -> Property
prop_eig_valid (Square mat)
  = conjoin [ case magnitude (v1 .-. v2) < epsilon of
                True  -> property True
                False -> counterexample ("λ  = " ++ show λ)
                       $ counterexample ("v  = " ++ show v)
                       $ counterexample ("Av = " ++ show v1)
                       $ counterexample ("λV = " ++ show v2)
                         False
            | (λ,v) <- eigvecs
            , let v1 = VG.map toComplex $ cmap toComplex mat @@ v
                  v2 = VG.map ((*λ) . toComplex) v
              
            ] 
  where
    eigvecs = case eig mat of
      (val,vec) -> VG.toList val `zip` Mat.toColList vec
