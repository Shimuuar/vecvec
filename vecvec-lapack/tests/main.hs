import Test.Tasty (defaultMain,testGroup)
import qualified TST.MatMul
import qualified TST.Slice
import qualified TST.Vector.Property
import qualified TST.VectorSpace
import qualified TST.Decomposition
import qualified TST.LinSolve
import qualified TST.MatDense
import qualified TST.Memory

main :: IO ()
main = defaultMain $ testGroup "vecvec"
  [ TST.Vector.Property.tests
  , TST.Slice.tests
  , TST.VectorSpace.tests
  , TST.MatDense.tests
  , TST.MatMul.tests
  , TST.Decomposition.tests
  , TST.LinSolve.tests
  , TST.Memory.tests
  ]
