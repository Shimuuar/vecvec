{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- |
module TST.Tools.Orphanage where

import           Data.Complex
import           System.Random
import           Test.Tasty.QuickCheck
import           Vecvec.Classes

deriving newtype instance Arbitrary a => Arbitrary (Tr   a)
deriving newtype instance Arbitrary a => Arbitrary (Conj a)


instance Random a => Random (Complex a) where
  randomR (al :+ bl, ah :+ bh) g =
    (\((a,b), g') -> (a :+ b, g')) $ randomR ((al,bl), (ah,bh)) g
  random g =
    (\((a,b),g') -> (a :+ b, g')) $ random g
  {-# INLINE random #-}
