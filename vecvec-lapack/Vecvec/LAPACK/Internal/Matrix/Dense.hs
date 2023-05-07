{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost        #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE NegativeLiterals           #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}
-- |
module Vecvec.LAPACK.Internal.Matrix.Dense
  ( -- * Immutable matrix
    Matrix(..)
  , pattern AsVec
  , nRows, nCols
  , unsafeRead
  , unsafeRow
  , unsafeCol
    -- * Creation of matrices
  , fromRowsFF
    -- * Conversion to\/from mutable
  , unsafeFreeze
  , freeze
  , thaw
  ) where

import Control.Monad
import Control.Monad.Primitive
import Control.Monad.ST
import Data.Coerce
import Data.List                    (intercalate)
import Data.Vector.Generic.Mutable  qualified as MVG
import Data.Vector.Generic          qualified as VG
import Foreign.Storable
import Foreign.Marshal.Array
import System.IO.Unsafe

import Vecvec.Classes
import Vecvec.Classes.Slice
import Vecvec.LAPACK.Internal.Matrix.Dense.Mutable qualified as M
import Vecvec.LAPACK.Internal.Compat
import Vecvec.LAPACK.Internal.Vector
import Vecvec.LAPACK.Internal.Vector.Mutable
import Vecvec.LAPACK.FFI                           qualified as C

-- | Immutable matrix
newtype Matrix a = Matrix (M.MView a)

pattern AsVec :: Vec a -> Matrix a
pattern AsVec v <- (tryVec -> Just v)

tryVec :: Matrix a -> Maybe (Vec a)
{-# INLINE tryVec #-}
tryVec (Matrix M.MView{..})
  | ncols /= leadingDim = Nothing
  | otherwise           = Just (Vec (VecRepr (ncols * nrows) 1 buffer))


nRows :: Matrix a -> Int
nRows (Matrix m) = M.nrows m

nCols :: Matrix a -> Int
nCols (Matrix m) = M.ncols m

instance M.AsMInput s Matrix where
  {-# INLINE asMInput #-}
  asMInput = coerce

instance (Show a, Storable a) => Show (Matrix a) where
  show m = "[ " ++ intercalate "\n, " [ show (unsafeRow m i) | i <- [0 .. nRows m - 1]] ++ "]"

instance (Eq a, Storable a) => Eq (Matrix a) where
  a == b
    | nRows a /= nRows b = False
    | nCols a /= nCols b = False
    | otherwise          = and
        [ unsafeRead a (i,j) == unsafeRead b (i,j)
        | i <- [0 .. nRows a - 1]
        , j <- [0 .. nCols a - 1]
        ]

deriving newtype instance (Slice1D i, Slice1D j, Storable a) => Slice (i,j) (Matrix a)

instance C.LAPACKy a => AdditiveSemigroup (Matrix a) where
  m1 .+. m2
    | nRows m1 /= nRows m2 || nCols m1 /= nCols m2 = error "Size mismatch"
    | otherwise = runST $ do
        -- Safe since matrix is newly allocated
        res@(M.AsMVec vres) <- M.clone m1
        case m2 of
          AsVec v2 -> unsafeBlasAxpy 1 v2 vres
          _        -> forM_ [0 .. nRows m1 - 1] $ \i -> do
            unsafeBlasAxpy 1 (unsafeRow m2 i) (M.unsafeRow res i)
        unsafeFreeze res

instance C.LAPACKy a => AdditiveQuasigroup (Matrix a) where
  m1 .-. m2
    | nRows m1 /= nRows m2 || nCols m1 /= nCols m2 = error "Size mismatch"
    | otherwise = runST $ do
        -- Safe since matrix is newly allocated
        res@(M.AsMVec vres) <- M.clone m1
        case m2 of
          AsVec v2 -> unsafeBlasAxpy -1 v2 vres
          _        -> forM_ [0 .. nRows m1 - 1] $ \i -> do
            unsafeBlasAxpy -1 (unsafeRow m2 i) (M.unsafeRow res i)
        unsafeFreeze res
  negateV m = -1 *. m

instance C.LAPACKy a => VectorSpace (Matrix a) where
  type Scalar (Matrix a) = a
  a *. m = runST $ do
    resV@(MVec (VecRepr _ _ buf)) <- MVG.new (nRows m * nCols m)
    let resM = M.MMatrix M.MView
          { ncols      = nCols m
          , nrows      = nRows m
          , leadingDim = nCols m
          , buffer     = buf
          }
    case m of
      AsVec v -> unsafeBlasAxpy a v resV
      _       -> forM_ [0 .. nRows m - 1] $ \i -> do
        unsafeBlasAxpy a (unsafeRow m i) (M.unsafeRow resM i)
    unsafeFreeze resM
  (.*) = flip (*.)

instance (C.LAPACKy a, a ~ a') => MatMul (Matrix a) (Vec a') (Vec a) where
  m @@ v
    | nCols m /= VG.length v = error "matrix size mismatch"
  mat @@ vecX = unsafePerformIO $ do
    vecY <- MVG.new (nRows mat)
    M.unsafeBlasGemv 1 mat vecX 0 vecY
    VG.unsafeFreeze vecY

unsafeFreeze :: (Storable a, PrimMonad m, s ~ PrimState m)
             => M.MMatrix s a -> m (Matrix a)
unsafeFreeze = pure . coerce


freeze :: (Storable a, PrimMonad m, s ~ PrimState m)
       => M.MMatrix s a -> m (Matrix a)
freeze = unsafeFreeze <=< M.clone

thaw :: (Storable a, PrimMonad m, s ~ PrimState m)
     => Matrix a -> m (M.MMatrix s a)
thaw = M.clone


unsafeRead :: (Storable a) => Matrix a -> (Int, Int) -> a
{-# INLINE unsafeRead #-}
unsafeRead (Matrix M.MView{..}) (i,j)
  = unsafeInlineIO
  $ unsafeWithForeignPtr buffer $ \p -> do
    peekElemOff p (i * leadingDim + j)



unsafeRow :: (Storable a) => Matrix a -> Int -> Vec a
unsafeRow (Matrix M.MView{..}) i =
  Vec (VecRepr ncols 1 (updPtr (`advancePtr` (leadingDim * i)) buffer))

unsafeCol :: (Storable a) => Matrix a -> Int -> Vec a
unsafeCol (Matrix M.MView{..}) i =
  Vec (VecRepr nrows leadingDim (updPtr (`advancePtr` i) buffer))

fromRowsFF :: (Storable a, Foldable f, Foldable g)
           => f (g a) -> Matrix a
fromRowsFF dat = runST $ unsafeFreeze =<< M.fromRowsFF dat
