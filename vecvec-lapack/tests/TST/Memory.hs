module TST.Memory (tests) where

import           Control.Monad.ST
import           Data.Foldable                         (for_)
import qualified Data.Vector.Generic                   as VG
import qualified Data.Vector.Generic.Mutable           as MVG

import Vecvec.Classes.NDArray                (Length (..), slice)
import Vecvec.LAPACK.Vector

import Test.Tasty
import Test.Tasty.HUnit


tests :: TestTree
tests = testGroup "memory"
  [ testGroup "basicUnsafeMove"
    [ testCase "miscellaneous intersections" $ do
        let carrierLength = 200
        let sourceOffset  = 30
        for_ [1..10] $ \len ->
          for_ [-5,-4..5] $ \sourceTargetOffset ->
            for_ [1..10] $ \sourceStride ->
              for_ [1..10] $ \targetStride -> do
                let caseNamePrefix = "(sourceTargetOffset = " ++ show sourceTargetOffset ++ ", "
                                  ++ "sourceStride = " ++ show sourceStride ++ ", "
                                  ++ "targetStride = " ++ show targetStride ++ ") "
                let targetOffset = sourceOffset + sourceTargetOffset
                    sliceSrc     = (sourceOffset, Length (len * sourceStride)) `Strided` sourceStride
                    sliceTgt     = (targetOffset, Length (len * targetStride)) `Strided` targetStride
                -- According to specification of `move`,
                -- "If the vectors do not overlap, then this is equivalent to copy.
                -- Otherwise, the copying is performed as if the source vector were
                -- copied to a temporary vector and then the temporary vector was
                -- copied to the target vector."
                -- So for the test, we compare the result after using `basicUnafeMove` and
                -- after using the intermediate buffer for copying -- the results should match.
                let (carrier1', carrier2', target1', target2' :: Vec Int) = runST $ do
                      carrier1    <- MVG.generateM carrierLength pure
                      let source1 = slice sliceSrc carrier1
                      let target1 = slice sliceTgt carrier1
                      MVG.move target1 source1
                      --
                      carrier2    <- MVG.generateM carrierLength pure
                      let source2 = slice sliceSrc carrier2
                      let target2 = slice sliceTgt carrier2
                      tmp <- MVG.clone source2
                      MVG.copy target2 tmp
                      --
                      (,,,) <$> VG.freeze target1 <*> VG.freeze target2 <*> VG.freeze carrier1 <*> VG.freeze carrier2
                assertEqual (caseNamePrefix ++  "Carrier vectors should be the same") carrier1' carrier2'
                assertEqual (caseNamePrefix ++  "Target vectors should be the same")  target1'  target2'
      ]
  ]

