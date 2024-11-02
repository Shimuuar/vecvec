{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies        #-}
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
import Vecvec.LAPACK.Hermitian       (Hermitian)
import Vecvec.LAPACK.Symmetric       (Symmetric)
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
  , testGroup "eigenvector"
    [ testGroup "eig"
      [ testEig @S
      , testEig @D
      , testEig @C
      , testEig @Z
      ]
    , testGroup "eigH"
      [ testEigH @S
      , testEigH @D
      , testEigH @C
      , testEigH @Z
      ]
    , testGroup "eigS"
      [ testEigS @S
      , testEigS @D
      ]
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
               , Storable (R a), Epsilon (R a), RealFloat (R a)
               , Show (R a), LAPACKy (Complex (R a))
               )
  => TestTree
testEig = testGroup (show (typeOf (undefined :: a)))
  [ testProperty "eig valid"     $ prop_eig_valid     @a
  , testProperty "eigvals valid" $ prop_eigvals_valid @a
  ]

prop_eig_valid
  :: ( LAPACKy a, ToComplex a, LAPACKy (Complex (R a))
     , Storable (R a), Epsilon (R a), RealFloat (R a), Show (R a))
  => Square a
  -> Property
prop_eig_valid (Square mat)
  = conjoin $ concat
    [ [counterexample "Eigenvector is eigenvector"
      $ counterexample ("λ  = " ++ show λ)
      $ counterexample ("v  = " ++ show v)
      $ counterexample ("Av = " ++ show v1)
      $ counterexample ("λV = " ++ show v2)
      $ magnitude (v1 .-. v2) < epsilon
      , counterexample "Eigenvector magnitude"
      $ counterexample ("λ  = " ++ show λ)
      $ counterexample ("v  = " ++ show v)
      $ abs (magnitude v - 1) < epsilon
      ]
    | (λ,v) <- eigvecs
    , let v1 = VG.map toComplex $ cmap toComplex mat @@ v
          v2 = VG.map ((*λ) . toComplex) v
    ]
  where
    eigvecs = case eig mat of
      (val,vec) -> VG.toList val `zip` Mat.toColList vec

prop_eigvals_valid
  :: ( LAPACKy a, Storable (R a), Eq (R a), Show (R a))
  => Square a
  -> Property
prop_eigvals_valid (Square mat)
  = counterexample ("e1 = " ++ show e1)
  $ counterexample ("e2 = " ++ show e2)
  $ e1 == e2
  where
    (e1,_) = eig mat
    e2     = eigvals mat

testEigH
  :: forall a. ( LAPACKy a, Typeable a, SmallScalar a, Show a
               , Storable (R a), Epsilon (R a), RealFloat (R a)
               , Show (R a)
               )
  => TestTree
testEigH = testGroup (show (typeOf (undefined :: a)))
  [ testProperty "eigH valid"     $ prop_eigH_valid     @a
  , testProperty "eigvalsH valid" $ prop_eigvalsH_valid @a
  ]

prop_eigH_valid
  :: ( LAPACKy a, Show a
     , Storable (R a), Epsilon (R a), RealFloat (R a), Show (R a))
  => Hermitian a
  -> Property
prop_eigH_valid mat
  = conjoin $ concat
    [ [counterexample "Eigenvector is eigenvector"
      $ counterexample ("λ  = " ++ show λ)
      $ counterexample ("v  = " ++ show v)
      $ counterexample ("Av = " ++ show v1)
      $ counterexample ("λV = " ++ show v2)
      $ magnitude (v1 .-. v2) < epsilon
      , counterexample "Eigenvector magnitude"
      $ counterexample ("λ  = " ++ show λ)
      $ counterexample ("v  = " ++ show v)
      $ abs (magnitude v - 1) < epsilon
      ]
    | (λ,v) <- eigvecs
    , let v1 = mat @@ v
          v2 = VG.map (* fromR λ) v
    ]
  where
    eigvecs = case eigH mat of
      (val,vec) -> VG.toList val `zip` Mat.toColList vec

prop_eigvalsH_valid
  :: ( LAPACKy a 
     , Storable (R a), RealFloat (R a), Show (R a))
  => Hermitian a
  -> Property
prop_eigvalsH_valid mat
  = counterexample ("e1 = " ++ show e1)
  $ counterexample ("e2 = " ++ show e2)
  $ e1 == e2
  where
    (e1,_) = eigH     mat
    e2     = eigvalsH mat

testEigS
  :: forall a. ( a ~ R a, LAPACKy a, Typeable a, SmallScalar a, Show a
               , Storable (R a), Epsilon (R a), RealFloat (R a)
               )
  => TestTree
testEigS = testGroup (show (typeOf (undefined :: a)))
  [ testProperty "eigS     valid" $ prop_eigS_valid @a
  , testProperty "eigvalsS valid" $ prop_eigvalsS_valid @a
  ]

prop_eigS_valid
  :: ( a ~ R a, LAPACKy a, Show a
     , Storable (R a), Epsilon (R a), RealFloat (R a))
  => Symmetric a
  -> Property
prop_eigS_valid mat
  = conjoin $ concat
    [ [counterexample "Eigenvector is eigenvector"
      $ counterexample ("λ  = " ++ show λ)
      $ counterexample ("v  = " ++ show v)
      $ counterexample ("Av = " ++ show v1)
      $ counterexample ("λV = " ++ show v2)
      $ magnitude (v1 .-. v2) < epsilon
      , counterexample "Eigenvector magnitude"
      $ counterexample ("λ  = " ++ show λ)
      $ counterexample ("v  = " ++ show v)
      $ abs (magnitude v - 1) < epsilon
      ]
    | (λ,v) <- eigvecs
    , let v1 = mat @@ v
          v2 = VG.map (* fromR λ) v
    ]
  where
    eigvecs = case eigS mat of
      (val,vec) -> VG.toList val `zip` Mat.toColList vec

prop_eigvalsS_valid
  :: ( a ~ R a, LAPACKy a 
     , Storable (R a), RealFloat (R a), Show (R a))
  => Symmetric a
  -> Property
prop_eigvalsS_valid mat
  = counterexample ("e1 = " ++ show e1)
  $ counterexample ("e2 = " ++ show e2)
  $ e1 == e2
  where
    (e1,_) = eigS     mat
    e2     = eigvalsS mat
