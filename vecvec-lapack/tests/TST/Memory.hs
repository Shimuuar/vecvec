module TST.Memory (tests) where


import           Control.Monad.ST
import           Data.Foldable                         (for_)
import qualified Data.Vector.Generic                   as VG
import qualified Data.Vector.Generic.Mutable           as MVG

import           Vecvec.Classes.NDArray                (Length (..), slice)
import qualified Vecvec.LAPACK                         as VV
import           Vecvec.LAPACK.Internal.Vector.Mutable

import           Test.Tasty
import           Test.Tasty.HUnit


tests :: TestTree
tests = testGroup "memory"
  [ testGroup "basicUnsafeMove"
    [ testCase "miscellaneous intersections" $ do
        let carrierLength = 200
        let sourceOffset  = 30
        for_ [1..10] $ \sourceLength ->
          for_ [-5,-4..5] $ \sourceTargetOffset ->
            for_ [1..10] $ \sourceStride ->
              for_ [1..10] $ \targetStride -> do
                let targetLength = sourceLength
                let targetOffset = sourceOffset + sourceTargetOffset
                let caseNamePrefix =    "(sourceTargetOffset = " ++ show sourceTargetOffset ++ ", "
                                     ++ "sourceStride = " ++ show sourceStride ++ ", "
                                     ++ "targetStride = " ++ show targetStride ++ ") "
                -- According to specification of `move`,
                -- "If the vectors do not overlap, then this is equivalent to copy.
                -- Otherwise, the copying is performed as if the source vector were
                -- copied to a temporary vector and then the temporary vector was
                -- copied to the target vector."
                -- So for the test, we compare the result after using `basicUnafeMove` and
                -- after using the intermediate buffer for copying -- the results should match.
                let (carrier1', carrier2', target1', target2' :: VV.Vec Int) = runST $ do
                      carrier1    <- MVG.generateM carrierLength pure
                      let source1 =  slice (Strided (sourceOffset, Length (sourceLength * sourceStride)) sourceStride) carrier1
                      let target1 =  slice (Strided (targetOffset, Length (targetLength * targetStride)) targetStride) carrier1
                      MVG.basicUnsafeMove target1 source1
                      --
                      carrier2    <- MVG.generateM carrierLength pure
                      let source2 =  slice (Strided (sourceOffset, Length (sourceLength * sourceStride)) sourceStride) carrier2
                      let target2 =  slice (Strided (targetOffset, Length (targetLength * targetStride)) targetStride) carrier2
                      tmp <- MVG.clone source2
                      MVG.basicUnsafeCopy target2 tmp
                      --
                      (,,,) <$> VG.unsafeFreeze target1 <*> VG.unsafeFreeze target2 <*> VG.unsafeFreeze carrier1 <*> VG.unsafeFreeze carrier2
                assertEqual (caseNamePrefix ++  "Carrier vectors should be the same") carrier1' carrier2'
                assertEqual (caseNamePrefix ++  "Target vectors should be the same")  target1'  target2'
      ]
  ]

