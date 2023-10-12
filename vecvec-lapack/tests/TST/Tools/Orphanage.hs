{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- |
module TST.Tools.Orphanage where

import Control.Monad.Trans.Writer
import Data.Vector               qualified as V
import Data.Vector.Storable      qualified as VS
import Data.Vector.Unboxed       qualified as VU
import Data.Vector.Primitive     qualified as VP
import Data.Vector.Generic       qualified as VG
import Data.Vector.Fusion.Bundle qualified as S

import Test.Tasty.QuickCheck

import Vecvec.Classes
import Vecvec.Classes.NDArray        qualified as Slice
import Vecvec.LAPACK.Internal.Vector as VV

----------------------------------------------------------------
-- Other instances
----------------------------------------------------------------


instance Show a => Show (S.Bundle v a) where
    show s = "Data.Vector.Fusion.Bundle.fromList " ++ show (S.toList s)


----------------------------------------------------------------
-- QC instances
----------------------------------------------------------------

deriving newtype instance Arbitrary a => Arbitrary (Tr   a)
deriving newtype instance Arbitrary a => Arbitrary (Conj a)

deriving newtype instance CoArbitrary a => CoArbitrary (Tr   a)
deriving newtype instance CoArbitrary a => CoArbitrary (Conj a)


instance Arbitrary a => Arbitrary (V.Vector a) where
    arbitrary = fmap V.fromList arbitrary

instance CoArbitrary a => CoArbitrary (V.Vector a) where
    coarbitrary = coarbitrary . V.toList


instance (Arbitrary a, VP.Prim a) => Arbitrary (VP.Vector a) where
    arbitrary = fmap VP.fromList arbitrary

instance (CoArbitrary a, VP.Prim a) => CoArbitrary (VP.Vector a) where
    coarbitrary = coarbitrary . VP.toList


instance (Arbitrary a, VS.Storable a) => Arbitrary (VS.Vector a) where
    arbitrary = fmap VS.fromList arbitrary

instance (CoArbitrary a, VS.Storable a) => CoArbitrary (VS.Vector a) where
    coarbitrary = coarbitrary . VS.toList


instance (Arbitrary a, VU.Unbox a) => Arbitrary (VU.Vector a) where
    arbitrary = fmap VU.fromList arbitrary

instance (CoArbitrary a, VU.Unbox a) => CoArbitrary (VU.Vector a) where
    coarbitrary = coarbitrary . VU.toList


instance Arbitrary a => Arbitrary (S.Bundle v a) where
    arbitrary = fmap S.fromList arbitrary

instance CoArbitrary a => CoArbitrary (S.Bundle v a) where
    coarbitrary = coarbitrary . S.toList


instance (Arbitrary a, VS.Storable a) => Arbitrary (VV.Vec a) where
    arbitrary = do
        stride <- choose (1,3)
        lst <- arbitrary
        let vec = Slice.slice ((0,Slice.End) `Strided` stride) $ VG.fromList $ replicate stride =<< lst
        pure vec

instance (CoArbitrary a, VS.Storable a) => CoArbitrary (VV.Vec a) where
    coarbitrary = coarbitrary . VG.toList


instance (Arbitrary a, Arbitrary b) => Arbitrary (Writer a b) where
    arbitrary = do b <- arbitrary
                   a <- arbitrary
                   return $ writer (b,a)

instance CoArbitrary a => CoArbitrary (Writer a ()) where
    coarbitrary = coarbitrary . runWriter

