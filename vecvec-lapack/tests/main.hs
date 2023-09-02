import Test.Tasty (defaultMain,testGroup)
import qualified TST.MatMul
import qualified TST.Slice
import qualified TST.Vector.Property
import qualified TST.VectorSpace
import qualified TST.Decomposition

main :: IO ()
main = defaultMain $ testGroup "vecvec"
  [ TST.MatMul.tests
  , TST.Slice.tests
  , TST.VectorSpace.tests
  , TST.Decomposition.tests
  , TST.Vector.Property.tests
  ]
