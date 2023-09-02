import Test.Tasty (defaultMain,testGroup)
import qualified TST.MatMul
import qualified TST.Slice
import qualified TST.Vector.Property
import qualified TST.VectorSpace

main :: IO ()
main = defaultMain $ testGroup "vecvec"
  [ TST.MatMul.tests
  , TST.Slice.tests
  , TST.VectorSpace.tests
  , TST.Vector.Property.tests
  ]
