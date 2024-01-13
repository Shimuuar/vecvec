{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE ImportQualifiedPost   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ViewPatterns          #-}
-- | Type classes for working with N-dimensional possibly sparse
-- arrays. All indices are assumed to be 0-based. No assumptions about
-- data layout in memory are made.
module Vecvec.Classes.NDArray
  ( -- * ND-arrays
    -- $ndarray
    -- ** Array shape
    Rank
  , HasShape(..)
  , shape
  , nCols
  , nRows
  , IsShape(..)
  , inBounds
  , pattern N1
  , pattern N2
    -- ** Immutable arrays
  , NDArray(..)
  , index
  , indexMaybe
  , (!)
  , (!?)
    -- * Slicing
  , Slice(..)
  , slice
  , Slice1D(..)
    -- ** Slice parameters
  , Range(..)
  , End(..)
  , Length(..)
    -- * Unsafe functions
  , unsafeIndex
    -- * Default implementations
  , implSliceVector
  , implSliceMVector
  ) where

import Vecvec.Classes.Internal.ND
















