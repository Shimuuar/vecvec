{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
-- |
-- General helpers for defining tests
module TST.Tools.Util
  ( -- * Utility
    qualTypeName
  , zipWithX
    -- * Precision
  , Epsilon(..)
  ) where

import Data.List (intercalate)
import Data.Typeable
import Data.Monoid
import Text.Show

import Data.Vector           qualified as V
import Data.Vector.Unboxed   qualified as VU
import Data.Vector.Storable  qualified as VS
import Data.Vector.Primitive qualified as VP


import Vecvec.LAPACK                       qualified as VV
import Data.Complex
import Vecvec.Classes

-- | Pretty print name of type
qualTypeName :: forall v. (Typeable v) => String
qualTypeName = render False (getTy @v) ""
  where
    render _     (ty,[])
      = renderCon ty
    render paren (ty,param)
      = showParen paren
      $ renderCon ty
      . appEndo (foldMap (\p -> Endo $ showChar ' ' . render True (splitTyConApp p)) param)
    --
    renderCon con
      | con == conV  = showString "V.Vector"
      | con == conVS = showString "VS.Vector"
      | con == conVP = showString "VP.Vector"
      | con == conVU = showString "VU.Vector"
      | otherwise    = shows con
    --
    getTy :: forall a. Typeable a => (TyCon, [TypeRep])
    getTy = splitTyConApp (typeRep (Proxy @a))
    -- Special cases which where we want qualified names
    (conV,_)  = getTy @V.Vector
    (conVS,_) = getTy @VS.Vector
    (conVP,_) = getTy @VP.Vector
    (conVU,_) = getTy @VU.Vector


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
