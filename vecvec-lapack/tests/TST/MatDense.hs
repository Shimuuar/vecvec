-- |
-- Tests for dense matrices
module TST.MatDense (tests) where

import Test.Tasty
import Test.Tasty.QuickCheck
import Text.Show.Functions  ()

import Vecvec.LAPACK.Matrix qualified as Mat

import TST.Tools.Orphanage ()
import TST.Tools.MatModel


tests :: TestTree
tests = testGroup "ops.Matrix.Dense"
  [ testProperty "replicate" $
    \(Size2D sz) (a :: Double)
    -> Mat.replicate sz a == unmodelMat (mdlReplicate sz a)
  , testProperty "generate" $
    \(Size2D sz) (f :: Int -> Int -> Double)
    -> Mat.generate sz f == unmodelMat (mdlGenerate sz f)
  ]


mdlReplicate :: (Int,Int) -> a -> ModelMat a
mdlReplicate (n,k) a
  = ModelMat' $ replicate n $ replicate k a

mdlGenerate :: (Int,Int) -> (Int -> Int -> a) -> ModelMat a
mdlGenerate (n,k) f = ModelMat'
  [ f i <$> [0 .. k-1]
  | i <- [0 .. n-1]
  ]
