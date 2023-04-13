{-# LANGUAGE CPP #-}
-- |
module Vecvec.LAPACK.Internal.Compat
  ( getPtr
  , updPtr
  , unsafeWithForeignPtr
  ) where

import Data.Primitive.Ptr    (Ptr(..))
import Foreign.ForeignPtr
import Foreign.Ptr
import GHC.ForeignPtr        (ForeignPtr(..))
#if MIN_VERSION_base(4,15,0)
import GHC.ForeignPtr        (unsafeWithForeignPtr)
#endif



getPtr :: ForeignPtr a -> Ptr a
{-# INLINE getPtr #-}
getPtr (ForeignPtr addr _) = Ptr addr

updPtr :: (Ptr a -> Ptr a) -> ForeignPtr a -> ForeignPtr a
{-# INLINE updPtr #-}
updPtr f (ForeignPtr p c) = case f (Ptr p) of { Ptr q -> ForeignPtr q c }

#if !MIN_VERSION_base(4,15,0)
-- | A compatibility wrapper for 'GHC.ForeignPtr.unsafeWithForeignPtr' provided
-- by GHC 9.0.1 and later.
--
-- Only to be used when the continuation is known not to
-- unconditionally diverge lest unsoundness can result.
unsafeWithForeignPtr :: ForeignPtr a -> (Ptr a -> IO b) -> IO b
unsafeWithForeignPtr = withForeignPtr
#endif
