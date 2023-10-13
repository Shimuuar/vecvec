-- |
-- Tests for dense matrices
module TST.MatDense (tests) where

import Test.Tasty
import Test.Tasty.QuickCheck
import Text.Show.Functions  ()

import Vecvec.LAPACK.Matrix.Dense (Matrix)
import Vecvec.LAPACK.Matrix.Dense qualified as Mat

import TST.Tools.Orphanage ()
import TST.Tools.Model
import TST.Tools.MatModel


tests :: TestTree
tests = testGroup "ops.Matrix.Dense"
  [ testProperty "replicate"
    ( (Mat.replicate . getSize2D =~= mdlReplicate . getSize2D)
    :: P (Size2D -> Double -> Matrix Double)
    )
  , testProperty "generate"
    (  (Mat.generate . getSize2D =~= mdlGenerate . getSize2D)
    :: P (Size2D -> (Int -> Int -> Double) -> Matrix Double)
    )
  ]


mdlReplicate :: (Int,Int) -> a -> ModelMat a
mdlReplicate (n,k) a
  = mkMat $ replicate n $ replicate k a

mdlGenerate :: (Int,Int) -> (Int -> Int -> a) -> ModelMat a
mdlGenerate (n,k) f = mkMat
  [ f i <$> [0 .. k-1]
  | i <- [0 .. n-1]
  ]
