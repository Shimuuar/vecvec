import Test.Tasty (defaultMain,testGroup)
import qualified TST.Model
import qualified TST.Slice
import qualified TST.Property


main :: IO ()
main = defaultMain $ testGroup "vecvec"
  [ TST.Model.tests
  , TST.Slice.tests
  , TST.Property.tests
  ]
