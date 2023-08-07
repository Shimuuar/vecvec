import Test.Tasty
import qualified QC

main :: IO ()
main = defaultMain $ testGroup "vecvec-lorentz"
  [ QC.tests
  ]
