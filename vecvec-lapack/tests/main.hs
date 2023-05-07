import Test.Tasty (defaultMain,testGroup)
import qualified TST.Model
import qualified TST.Slice


main :: IO ()
main = defaultMain $ testGroup "vecvec"
  [ TST.Model.tests
  , TST.Slice.tests
  ]
