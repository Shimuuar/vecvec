{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost        #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeOperators              #-}
-- |
module Vecvec.LAPACK.Internal.Matrix.Symmetric
  ( -- * Immutable matrix
    Symmetric(..)
    -- * Operations
    -- ** Conversion to\/from mutable
  , unsafeFreeze
  , freeze
  , thaw
    -- ** Creation
  , fromRowsFF
  , fromRowsFV
  , replicate
  , generate 
  ) where

import Control.Monad
import Control.Monad.Primitive
import Control.Monad.ST
import Data.Coerce
import Data.Vector.Generic     qualified as VG
import Foreign.Storable
import System.IO.Unsafe
import Prelude hiding (replicate)

import Vecvec.Classes
import Vecvec.Classes.NDArray
import Vecvec.LAPACK.Internal.Matrix.Dense.Mutable     qualified as MMat
import Vecvec.LAPACK.Internal.Matrix.Dense             qualified as Mat
import Vecvec.LAPACK.Internal.Matrix.Symmetric.Mutable qualified as MSym
import Vecvec.LAPACK.Internal.Compat
import Vecvec.LAPACK.Internal.Vector
import Vecvec.LAPACK.Internal.Vector.Mutable
import Vecvec.LAPACK.FFI                           qualified as C
import Vecvec.LAPACK.Utils

----------------------------------------------------------------
--
----------------------------------------------------------------

-- | Symmetric matrix
data Symmetric a = Symmetric () (MSym.MSymView a)

instance (Slice1D i, Storable a) => Slice i (Symmetric a) where
  {-# INLINE sliceMaybe #-}
  sliceMaybe i (Symmetric flag view) = do
    view' <- sliceMaybe i view
    pure $ Symmetric flag view'

instance (Show a, Storable a) => Show (Symmetric a) where
  show = show . toDense

instance MSym.AsSymInput s Symmetric where
  {-# INLINE asSymInput #-}
  asSymInput (Symmetric _ mat) = mat

unsafeFreeze :: (Storable a, PrimMonad m, s ~ PrimState m)
             => MSym.MSymmetric s a -> m (Symmetric a)
unsafeFreeze (MSym.MSymmetric view)
  = pure $ Symmetric flag view
  where
    flag = unsafePerformIO $ () <$ MSym.symmetrizeMSymView view


freeze :: (Storable a, PrimMonad m, s ~ PrimState m)
       => MSym.MSymmetric s a -> m (Symmetric a)
freeze = unsafeFreeze <=< MSym.clone

thaw :: (Storable a, PrimMonad m, s ~ PrimState m)
     => Symmetric a -> m (MSym.MSymmetric s a)
thaw = MSym.clone

toDense :: Symmetric a -> Mat.Matrix a
toDense (Symmetric () MSym.MSymView{..}) =
  Mat.Matrix MMat.MView
    { nrows      = size
    , ncols      = size
    , leadingDim = leadingDim
    , buffer     = buffer
    }

----------------------------------------------------------------
-- Creation
----------------------------------------------------------------

-- | Create matrix from list of rows.
fromRowsFF :: (Storable a, Foldable f, Foldable g)
           => f (g a) -> Symmetric a
fromRowsFF dat = runST $ unsafeFreeze =<< MSym.fromRowsFF dat

-- | Create matrix from list of rows.
fromRowsFV :: (Storable a, Foldable f, VG.Vector v a)
           => f (v a) -> Symmetric a
{-# INLINE fromRowsFV #-}
fromRowsFV dat = runST $ unsafeFreeze =<< MSym.fromRowsFV dat



-- | Fill matrix of given size with provided value.
--
-- ==== __Examples__
--
-- >>> replicate 2 (42::Double)
-- [ [42.0,42.0]
-- , [42.0,42.0]]
replicate :: (Storable a)
          => Int  -- ^ Matrix size
          -> a    -- ^ Element
          -> Symmetric a
replicate sz a = runST $ unsafeFreeze =<< MSym.replicate sz a

-- | Fill matrix of given size using function from indices to element.
--
-- ==== __Examples__
--
-- >>> generate 3 (\i j -> 100*i + j)
-- [ [0,1,2]
-- , [1,101,102]
-- , [2,102,202]]
generate :: (Storable a)
         => Int               -- ^ Matrix size
         -> (Int -> Int -> a) -- ^ Function that takes \(N_{row}\) and \(N_{column}\) as input
         -> Symmetric a
generate sz f = runST $ unsafeFreeze =<< MSym.generate sz f
