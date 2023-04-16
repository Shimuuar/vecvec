{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE TypeFamilies        #-}
-- |
module Vecvec.LAPACK.Internal.Matrix.Dense
  ( Matrix(..)
  , unsafeFreeze
  , freeze
  , thaw
  , unsafeRead
  ) where


import Control.Monad.Primitive
import Foreign.ForeignPtr
import Foreign.Storable

import Vecvec.LAPACK.Internal.Matrix.Dense.Mutable qualified as M
import Vecvec.LAPACK.Internal.Compat


-- | Immutable matrix
data Matrix a = Matrix
  { nrows      :: !Int
  , ncols      :: !Int
  , leadingDim :: !Int
  , buffer     :: !(ForeignPtr a)
  }

unsafeFreeze :: (Storable a, PrimMonad m, s ~ PrimState m)
             => M.MMatrix s a -> m (Matrix a)
unsafeFreeze M.MMatrix{..} = pure $ do
  Matrix { nrows      = nrowsM
         , ncols      = ncolsM
         , leadingDim = leadingDimM
         , buffer     = bufferM
         }

freeze :: (Storable a, PrimMonad m, s ~ PrimState m)
       => M.MMatrix s a -> m (Matrix a)
freeze M.MMatrix{..} = do
  -- FIXME: We need to allocate new buffer. Note that ncols /=
  --        leadingDim in general case. We ma need to copy row by row
  pure Matrix { nrows      = nrowsM
              , ncols      = ncolsM
              , leadingDim = ncolsM
              , buffer     = undefined
              }

thaw :: (Storable a, PrimMonad m, s ~ PrimState m)
     => Matrix a -> m (M.MMatrix s a)
thaw Matrix{..} = do
  -- FIXME: We need to allocate new buffer. Note that ncols /=
  --        leadingDim in general case. We ma need to copy row by row
  pure M.MMatrix { nrowsM      = nrows
                 , ncolsM      = ncols
                 , leadingDimM = ncols
                 , bufferM     = undefined
                 }

unsafeRead :: (Storable a) => Matrix a -> (Int, Int) -> a
{-# INLINE unsafeRead #-}
unsafeRead Matrix{..} (i,j)
  = unsafeInlineIO
  $ unsafeWithForeignPtr buffer $ \p -> do
    peekElemOff p (i * leadingDim + j) 
