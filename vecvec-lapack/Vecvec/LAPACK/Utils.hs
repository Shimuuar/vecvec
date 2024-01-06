-- |
-- Utilities for wrinting code
module Vecvec.LAPACK.Utils
  ( -- * Loops
    loop_
  , loop0_
  , loopUp_
  ) where

-- | Loop over range @[k,n)@.
loop_
  :: (Applicative m)
  => Int          -- ^ @k@
  -> Int          -- ^ @n@
  -> (Int -> m a) -- ^ Callback
  -> m ()
{-# INLINE loop_ #-}
loop_ i0 n action = go i0 where
  go i | i >= n    = pure ()
       | otherwise = action i *> go (i+1)


-- | Loop over range @[0,n)@.
loop0_
  :: (Applicative m)
  => Int          -- ^ @n@
  -> (Int -> m a) -- ^ Callback
  -> m ()
{-# INLINE loop0_ #-}
loop0_ n action = go 0 where
  go i | i >= n    = pure ()
       | otherwise = action i *> go (i+1)

-- | Loop over square matrix of size @n@ which references only
--   elements on diagonal and above
loopUp_
  :: (Applicative m)
  => Int                 -- ^ Matrix size @n@
  -> (Int -> Int -> m a) -- ^ Callback \(N_{row}\) \(N_{column}\)
  -> m ()
{-# INLINE loopUp_ #-}
loopUp_ n action = loop0_ n $ \i -> loop_ i n $ \j -> action i j
