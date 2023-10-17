module TST.Memory (tests) where


import           Control.Monad.ST
import           Data.Foldable                         (for_)
import qualified Data.Vector.Generic                   as VG
import qualified Data.Vector.Generic.Mutable           as MVG

import           Vecvec.Classes.Slice
import qualified Vecvec.LAPACK                         as VV
import           Vecvec.LAPACK.Internal.Vector.Mutable (Strided (..))

import           Test.Tasty
import           Test.Tasty.HUnit


tests :: TestTree
tests = testGroup "memory"
  [ testGroup "basicUnsafeMove"
    [ testCase "miscellaneous intersections" $ do
        let carrierLength   = 200
        let sourceOffset    = 30
        let carrierOriginal = [1..carrierLength]
        for_ [1..10] $ \sourceLength ->
          for_ [-5,-4..5] $ \sourceTargetOffset ->
            for_ [1..10] $ \sourceStride ->
              for_ [1..10] $ \targetStride -> do
                let targetLength    = sourceLength
                let (sourceOriginal :: [Int], carrierResult:: VV.Vec Int, targetResult :: VV.Vec Int) = runST $ do
                      carrier          <- MVG.generateM carrierLength (pure . (carrierOriginal !!))
                      let targetOffset =  sourceOffset + sourceTargetOffset
                      let source       =  slice (Strided (sourceOffset, Length (sourceLength * sourceStride)) sourceStride) carrier
                      let target       =  slice (Strided (targetOffset, Length (targetLength * targetStride)) targetStride) carrier
                      sourceOriginal'  <- MVG.foldr' (:) [] source
                      --
                      MVG.basicUnsafeMove target source
                      --
                      target'  <- VG.unsafeFreeze target
                      carrier' <- VG.unsafeFreeze carrier
                      pure (sourceOriginal', carrier', target')
                let caseNamePrefix =    "(sourceTargetOffset = " ++ show sourceTargetOffset ++ ", "
                                     ++ "sourceStride = " ++ show sourceStride ++ ", "
                                     ++ "targetStride = " ++ show targetStride ++ ") "
                -- 1. check that carrier is not modified on non-target places
                let initTargetIndex = sourceOffset + sourceTargetOffset
                let modifiedIndexes = take targetLength [initTargetIndex, initTargetIndex + targetStride..]
                assertBool
                  (caseNamePrefix ++ "All carrier vector values (except modified in target vector) should be the same")
                  (all (\(idx, orig, carr) -> orig == carr || idx `elem` modifiedIndexes)
                       (zip3 [0..] carrierOriginal (VG.toList carrierResult))
                  )
                -- 2. check that target is a copy of the source
                assertEqual
                  (caseNamePrefix ++ "Target is a copy of the source")
                  sourceOriginal
                  (VG.toList targetResult)
      ]
  ]

