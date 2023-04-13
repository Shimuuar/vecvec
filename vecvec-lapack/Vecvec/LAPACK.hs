{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost        #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
-- |
module Vecvec.LAPACK
  ( -- * Vector data types
    Vec
  , MVec
  , fromMVector
  ) where

-- import Control.Monad
-- import Control.Monad.Primitive
-- import Data.Primitive.Ptr      hiding (advancePtr)
-- import Data.Word
-- import Foreign.Storable
-- import Foreign.ForeignPtr
-- import Foreign.Ptr
-- import Foreign.Marshal.Array

-- import Data.Vector.Storable         qualified as VS
-- import Data.Vector.Storable.Mutable qualified as MVS
-- import Data.Vector.Generic          qualified as VG
-- import Data.Vector.Generic.Mutable  qualified as MVG
-- import Data.Vector.Fusion.Util      (liftBox)

import Vecvec.LAPACK.Internal.Vector

