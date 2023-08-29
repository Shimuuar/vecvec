-- |
-- Newtype for use with deriving via.
module Vecvec.Classes.Via
  ( AsNum(..)
  , AsVector(..)
  , AsMVector(..)
  , AsFixedVec(..)
  ) where

-- | Derive instances for data types which already have 'Num'
--   instance. It could be used to derive instances from
--   "Vecvec.Classes".
newtype AsNum a = AsNum a

-- | Derive instances for data type which is instance of
--   'Data.Vector.Generic.Vector'.
--
--   It could be used to derive instances for type classes from
--   "Vecvec.Classes" and "Vecvec.Classes.NDArray".
newtype AsVector v a = AsVector (v a)

-- | Derive instances for data type which is instance of
--   'Data.Vector.Generic.Mutable.MVector'.
--
--   It could be used to derive instances for type classes from
--   "Vecvec.Classes.NDArray".
newtype AsMVector v s a = AsMVector (v s a)

-- | Derive instances for data type which is instance of
--   'Data.Vector.Generic.Vector'.
--
--   It could be used to derive instances for type classes from
--   "Vecvec.Classes".
newtype AsFixedVec v a = AsFixedVec (v a)
