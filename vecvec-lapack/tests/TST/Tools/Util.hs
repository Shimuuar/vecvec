{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
-- |
module TST.Tools.Util
  ( -- * Utility
    qualTypeName
  , zipWithX
    -- * Precision
  , Epsilon(..)
  ) where

import Data.List (intercalate)
import Data.Typeable

-- | Pretty print name of type
qualTypeName :: forall v. (Typeable v) => String
qualTypeName = intercalate " "
             $ tyConModule con <> "." <> tyConName con
             : map showParam par
  where
    tyV = typeRep (Proxy @v)
    (con,par) = splitTyConApp tyV
    showParam p = case show p of
      s | ' ' `elem` s -> "("++s++")"
        | otherwise    -> s

-- | Variant of 'zipWith' which throws error when lists have different
--   lengths
zipWithX :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWithX _ []     []     = []
zipWithX f (a:as) (b:bs) = f a b : zipWithX f as bs
zipWithX _ _ _ = error "zipWithX: length mismatch"

-- | Absolute error for tests of numeric routines.
class Epsilon a where
  epsilon :: a

instance Epsilon Float  where epsilon = 1e-4
instance Epsilon Double where epsilon = 1e-12
