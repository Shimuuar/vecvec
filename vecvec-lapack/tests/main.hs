import Test.Tasty (defaultMain,testGroup)
import qualified TST.Model
import qualified TST.Slice
import qualified TST.Vector.Property


main :: IO ()
main = defaultMain $ testGroup "vecvec"
  [ TST.Model.tests
  , TST.Slice.tests
  , TST.Vector.Property.tests
  ]
