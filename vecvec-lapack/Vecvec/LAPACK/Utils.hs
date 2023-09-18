-- |
-- Utilities for wrinting code
module Vecvec.LAPACK.Utils
  ( -- * Loops
    loop0_
  ) where


-- | Loop over range @[0,n)@. 
loop0_ :: (Applicative m) => Int -> (Int -> m a) -> m ()
{-# INLINE loop0_ #-}
loop0_ n action = go 0 where
  go i | i >= n    = pure ()
       | otherwise = action i *> go (i+1)

