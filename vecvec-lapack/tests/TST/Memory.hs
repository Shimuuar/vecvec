{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE RecordWildCards    #-}
-- |
module TST.Memory (tests) where

import Control.Monad
import Data.Typeable
import Data.Foldable
import Data.Traversable
import Test.Tasty
import Test.Tasty.QuickCheck
import           Test.Tasty
import           Test.Tasty.HUnit as T

import Data.Vector.Generic   qualified as VG
import Data.Vector           qualified as V
import Data.Vector.Unboxed   qualified as VU
import Data.Vector.Storable  qualified as VS
import Data.Vector.Primitive qualified as VP
import Data.Vector.Generic.Mutable  qualified as MVG
import Vecvec.LAPACK.Internal.Vector.Mutable qualified as MVGX
import Data.Vector.Generic  qualified as VG

import Vecvec.Classes.Slice
import Vecvec.LAPACK         qualified as VV
import Vecvec.LAPACK.Internal.Vector.Mutable (Strided(..))
import Foreign.Ptr
-- import Vecvec.LAPACK.Internal.Compat
import GHC.ForeignPtr        (ForeignPtr(..))
import Data.Primitive.Ptr    (Ptr(..))
import Foreign.Storable
import Control.Monad.ST


getPtr :: ForeignPtr a -> Ptr a
{-# INLINE getPtr #-}
getPtr (ForeignPtr addr _) = Ptr addr


distancePtr :: forall a . Storable a => Ptr a -> Ptr a -> Int
-- distancePtr ptrFrom ptrTo = (ptrTo `minusPtr` ptrFrom) `div` (sizeOf (undefined :: a))
distancePtr = distancePtr' undefined -- TODO WTF?? why ambigous??
 where
    distancePtr' :: Storable a => a -> Ptr a -> Ptr a -> Int
    distancePtr' x ptrFrom ptrTo = (ptrTo `minusPtr` ptrFrom) `div` sizeOf x

assertArrays :: (VS.Storable a, Eq a, Show a) => VV.Vec a -> [a] -> VV.Vec a -> [a] -> VV.Vec a -> [a] -> IO ()
assertArrays givenV1 expectedV1 givenV2 expectedV2 givenV3 expectedV3 = do
  assertEqual "V1" (VG.fromList expectedV1) givenV1
  assertEqual "V2" (VG.fromList expectedV2) givenV2
  assertEqual "V3" (VG.fromList expectedV3) givenV3


tests :: TestTree
tests = testGroup "memory"
  [ testCase "simple" $ do
        --
        -- TODO check also with **strided** vectors! -- via properties!!
        --
        -- let (vec :: VV.MVec MVG.RealWorld Int)  = runST $ MVG.basicUnsafeReplicate 10 123
        let (vecCarrier :: VV.Vec Int, vecSlice1 :: VV.Vec Int, vecSlice2 :: VV.Vec Int)  = runST $ do
                    -- vec <- MVG.basicUnsafeReplicate 10 123
                    vec <- MVG.generateM 20 (pure . (+10)) -- [10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29]
                    let v1 = MVG.slice 0 10 vec            -- [10, 11, 12, 13, 14, 15, 16, 17, 18, 19]
                    let v2 = MVG.slice 5 10 vec            --                     [15, 16, 17, 18, 19, 20, 21, 22, 23, 14]
                    -- vec2 <- MVG.basicUnsafeReplicate 10 321
                    -- MVG.basicUnsafeMove v1 {-target-} v2 {-source-} -- OK
                    MVG.basicUnsafeMove v2 {-target-} v1 {-source-}
                                                           -- [10, 11, 12, 13, 14, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 25, 26, 27, 28, 29]
                    --
                    vecf <- VG.freeze vec
                    v1f  <- VG.freeze v1
                    v2f  <- VG.freeze v2
                    pure (vecf, v1f, v2f)

        --print vecCarrier
        --print vecSlice1
        --print vecSlice2

        assertArrays vecCarrier [10,11,12,13,14,10,11,12,13,14,15,16,17,18,19,25,26,27,28,29]
                     vecSlice1  [10,11,12,13,14,10,11,12,13,14]
                     vecSlice2  [10,11,12,13,14,15,16,17,18,19]
  , testCase "intersected with stride" $ do
        let (vecCarrier :: VV.Vec Int, vecSlice1 :: VV.Vec Int, vecSlice2 :: VV.Vec Int, d, d2)  = runST $ do
                    vec <- MVG.generateM 10 (pure . (+10)) -- [10, 11, 12, 13, 14, 15, 16, 17, 18, 19]
                    let vec1 = slice (Strided (0, End) 2) vec
                    let vec2 = slice (2, Length 5) vec
                    let MVGX.MVec vec1repr = vec1
                        MVGX.MVec vec2repr = vec2
                        vb1 = MVGX.vecBuffer vec1repr
                        vb2 = MVGX.vecBuffer vec2repr
                        diff = minusPtr (getPtr vb2) (getPtr vb1)
                        diff2 = distancePtr (getPtr vb1) (getPtr vb2)
                    MVG.basicUnsafeMove vec2 {-target-} vec1 {-source-}
                    (,,,,) <$> VG.freeze vec <*> VG.freeze vec1 <*> VG.freeze vec2 <*> pure diff <*> pure diff2

        --print vecCarrier
        --print vecSlice1
        --print vecSlice2

        --putStrLn "***********"
        --print d
        --print d2
        --putStrLn "^^^^^^^^^^^"
        --                      [10,11,12,13,14,15,16,17,18,19
        assertArrays vecCarrier [10,11,10,12,14,16,18,17,18,19]
                                    -- ^^^^^----^^^^^ reinserted part
                     vecSlice1  [10,   10,   14,   18,   18]
                     vecSlice2        [10,12,14,16,18]
  , testCase "options" $ do
        let carrierLength = 200
            -- length = 10
        let targetOffset = 10
            mvecToList vec = MVG.foldr' (:) [] vec
        for_ [1..10] $ \length ->
            for_ [-5,-4..5] $ \sourceTargetOffset ->
                for_ [1..10] $ \sourceStride ->
                    for_ [1..10] $ \targetStride -> do
                        let (vecOriginal' :: [Int], vecSourceLst :: [Int], vecCarrier :: VV.Vec Int, vecSource :: VV.Vec Int, vecTarget :: VV.Vec Int) = runST $ do
                                let sourceOffset = targetOffset + sourceTargetOffset
                                let originalVector = [10 .. (10 + carrierLength - 1)]
                                -- vecOriginal <- MVG.generateM carrierLength (pure . (+10))
                                vecOriginal <- MVG.generateM carrierLength (\i -> pure $ originalVector !! i)
                                -- let vecOriginal = VG.fromList originalVector
                                let vecSource = slice (Strided (targetOffset, Length (length * sourceStride)) sourceStride) vecOriginal
                                let vecTarget = slice (Strided (sourceOffset, Length (length * targetStride)) targetStride) vecOriginal
                                -- MVG.set vecTarget 0
                                --
                                vecSourceLst' <- mvecToList vecSource

                                --
                                MVG.basicUnsafeMove vecTarget vecSource
                                --
                                vecSource' <- VG.unsafeFreeze vecSource
                                vecTarget' <- VG.unsafeFreeze vecTarget
                                vecOriginal' <- VG.unsafeFreeze vecOriginal
                                pure (originalVector, vecSourceLst', vecOriginal', vecSource', vecTarget')
                        {-
                        putStrLn "~~~~~~~~~~~~~~~~~~~~~~~~~~"
                        print vecOriginal'
                        print vecSourceLst
                        print vecCarrier
                        print vecSource
                        print vecTarget
                        -}
                        -- 2. check that carrier is good-modified
                        let prefix =    "(sourceTargetOffset = " ++ show sourceTargetOffset ++ ", "
                                     ++ "sourceStride = " ++ show sourceStride ++ ", "
                                     ++ "targetStride = " ++ show targetStride ++ ") "
                        let initTarget = targetOffset + sourceTargetOffset
                            modifiedIndexes = take length [initTarget, initTarget + targetStride..]
                        assertBool
                            (prefix ++ "All carrier vector values (except modified in target vector) should be the same")
                            (all (\(idx, orig, carr) -> orig == carr || idx `elem` modifiedIndexes)
                                 (zip3 [0..] vecOriginal' (VG.toList vecCarrier))
                            )
                        -- 1. check that target is good
                        assertEqual (prefix ++ "Target is OK") vecSourceLst (VG.toList vecTarget)
        -- pure ()

  ]









