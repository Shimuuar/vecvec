{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
-- |
module TST.Tools.Util
  ( -- * Names
    qualTypeName
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



-- | Absolute error for tests of numeric routines.
class Epsilon a where
  epsilon :: a

instance Epsilon Float  where epsilon = 1e-4
instance Epsilon Double where epsilon = 1e-12
