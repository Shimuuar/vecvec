{-# LANGUAGE MultiParamTypeClasses #-}
-- |
module Vecvec.Classes.Containers where


-- | Type class for conversions from one data type to
--   another. Function is assumed to be total
class Convert a b where
  convert :: a -> b
