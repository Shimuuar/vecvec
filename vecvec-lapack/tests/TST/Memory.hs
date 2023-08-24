{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
-- |
module TST.Memory (tests) where

import Data.Typeable
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
import Data.Vector.Generic  qualified as VG

import Vecvec.Classes.Slice
import Vecvec.LAPACK         qualified as VV
import Vecvec.LAPACK.Internal.Vector.Mutable (Strided(..))

import Control.Monad.ST

tests :: TestTree
tests = testGroup "memory"
  [ testCase "simple" $ do
        putStrLn "UPS"
        --
        -- TODO check also with **strided** vectors! -- via properties!!
        --
        -- let (vec :: VV.MVec MVG.RealWorld Int)  = runST $ MVG.basicUnsafeReplicate 10 123
        let (vecOrig :: VV.Vec Int, vecSlice1 :: VV.Vec Int, vecSlice2 :: VV.Vec Int)  = runST $ do
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
        print vecOrig
        print vecSlice1
        print vecSlice2
        pure ()
  ]

