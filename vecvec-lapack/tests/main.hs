import Test.Tasty (defaultMain,testGroup)
import qualified TST.Model

main :: IO ()
main = defaultMain $ testGroup "vecvec"
  [ TST.Model.tests
  ]
