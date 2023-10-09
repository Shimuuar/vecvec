{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
-- |
module TST.Tools.Util
  ( qualTypeName
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
