{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE TypeFamilies          #-}
-- |
-- Many optimization algorithms perform line search as one of the
-- steps. Which means solving following minimization problem (usually
-- approximately):
--
-- \[
-- \underset{\alpha}{\operatorname{argmin}}f(\mathbf{x}+\alpha\mathbf{p})
-- \]
--
-- Where \(\mathbf{p}\) is search direction. This module contains
-- various methods for solving this problem. They're generally only of
-- interest for implementors of optimization algorithms.
module Vecvec.Optim.LineSearch where

import Vecvec.Classes

----------------------------------------------------------------
-- Backtraking algorithms
----------------------------------------------------------------

-- | Cached function and its gradient
data CachedGrad v = CachedGrad
  { fun  :: !(Scalar v) -- ^ Function value at @x@
  , x    :: !v          -- ^ Value of @x@
  , grad :: !v          -- ^ Function gradient at point @x@
  }

-- | Cached function
data CachedFun v = CachedFun
  { fun  :: !(Scalar v) -- ^ Function value at @x@
  , x    :: !v          -- ^ Value of @x@
  }

data CachedBck v = CachedBck
  { fun   :: !(Scalar v) -- ^ Function value at @x@
  , alpha :: !(Scalar v) -- ^ α chosen for 
  , x     :: !v          -- ^ Value of @x@
  }



data Stream a = a :> Stream a

findS :: (a -> Bool) -> Stream a -> a
findS p = go where go (a :> as) | p a       = a
                                | otherwise = go as


simpleBacktracking
  :: (Num (Scalar v), VectorSpace v)
  => Scalar v        -- ^ Backtracking rate \(0<\rho<1\) (not checked)
  -> (v -> Scalar v) -- ^ Function
  -> v               -- ^ Initial point
  -> v               -- ^ Search direction
  -> Scalar v        -- ^ Initial value of \(\alpha\)
  -> Stream (CachedBck v)
simpleBacktracking ρ f x0 p = go where
  go !α = CachedBck { fun   = f x
                    , alpha = α
                    , x     = x
                    }
        :> go (ρ * α)
    where
      x = x0 .+. α*.p

-- | Find first iteration which satisfies first Wolfe condition.
findWolfe1
  :: (InnerSpace v, Ord (Scalar v))
  => Scalar v             -- ^ Constant \(c_1\)
  -> v                    -- ^ Search dretion
  -> CachedGrad v         -- ^ Initial point
  -> Stream (CachedBck v) -- ^ Stream of candidates
  -> CachedBck v
findWolfe1 c1 p pt0 = findS wolfe1 where
  wolfe1 pt = pt.fun < pt0.fun + pt.alpha * min_rate
  min_rate  = c1 * (pt0.grad <.> p)
