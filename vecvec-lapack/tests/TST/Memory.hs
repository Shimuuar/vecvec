{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications    #-}
module TST.Memory (tests) where

import Data.Foldable                         (for_)
import Data.Vector.Generic                   qualified as VG
import Data.Vector.Generic.Mutable           qualified as MVG

import Vecvec.Classes.NDArray
import Vecvec.LAPACK                         (Strided(..))
import Vecvec.LAPACK.Vector.Mutable          (MVec)

import Test.Tasty
import Test.Tasty.HUnit


tests :: TestTree
tests = testGroup "memory"
  [ testGroup "basicUnsafeMove"
    [ testCase "miscellaneous intersections" $ do
        let carrierLength = 200
        let sourceOffset  = 30
        for_ [1..10] $ \len ->
          for_ [-5,-4..5] $ \commonOffset ->
            for_ [1..10] $ \sourceStride ->
              for_ [1..10] $ \targetStride -> do
                let targetOffset = sourceOffset + commonOffset
                    sliceSrc     = (sourceOffset, Length (len * sourceStride)) `Strided` sourceStride
                    sliceTgt     = (targetOffset, Length (len * targetStride)) `Strided` targetStride
                let caseNamePrefix = unlines
                      [ "sourceOffset = " ++ show sourceOffset
                      , "targetOffset = " ++ show targetOffset
                      , "sourceStride = " ++ show sourceStride
                      , "targetStride = " ++ show targetStride
                      , "length       = " ++ show len
                      ]
                -- move specification:
                --
                -- If the vectors do not overlap, then this is equivalent to copy.
                -- Otherwise, the copying is performed as if the source vector were
                -- copied to a temporary vector and then the temporary vector was
                -- copied to the target vector.
                carrier <- MVG.generate @_ @MVec carrierLength id
                let source = slice sliceSrc carrier
                    target = slice sliceTgt carrier
                MVG.move target source
                -- Reference implementation using temporary
                carrierRef <- MVG.generate @_ @MVec carrierLength id
                let sourceRef = slice sliceSrc carrierRef
                    targetRef = slice sliceTgt carrierRef
                MVG.copy targetRef =<< MVG.clone sourceRef
                -- Check for equivality
                do impl <- VG.freeze target
                   ref  <- VG.freeze targetRef
                   assertEqual (caseNamePrefix ++  "Target vectors should be the same") ref impl
                --
                do impl <- VG.freeze carrier
                   ref  <- VG.freeze carrierRef
                   assertEqual (caseNamePrefix ++  "Carrier vectors should be the same") ref impl
      ]
  ]
