{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- |
module TST.Orphanage where

import Test.Tasty.QuickCheck
import Vecvec.Classes

deriving newtype instance Arbitrary a => Arbitrary (Tr   a)
deriving newtype instance Arbitrary a => Arbitrary (Conj a)
