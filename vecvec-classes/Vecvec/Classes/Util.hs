{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Various utility type class
module Vecvec.Classes.Util
  ( StorableZero(..)
  ) where

import Data.Complex
import Data.Primitive.Ptr
import Foreign.Ptr
import Foreign.Storable


-- | Data types which allow efficient setting buffer to zeros. If @a@
--   is instance of @Num@ or other numeric type class calling
--   @zeroOutBuffer@ should fill buffer with zeros.
class Storable a => StorableZero a where
  zeroOutBuffer :: Ptr a -- ^ Pointer to buffer
                -> Int   -- ^ Size of buffer in elements
                -> IO ()

instance StorableZero Float where
  zeroOutBuffer ptr n = setPtr ptr n 0

instance StorableZero Double where
  zeroOutBuffer ptr n = setPtr ptr n 0

instance StorableZero a => StorableZero (Complex a) where
  zeroOutBuffer ptr n = zeroOutBuffer (castPtr ptr :: Ptr a) (2*n)
