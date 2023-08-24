{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost        #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE NegativeLiterals           #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}
-- |
module Vecvec.LAPACK.Internal.Matrix.Dense
  ( -- * Immutable matrix
    Matrix(..)
  , pattern AsVec
    -- * Operations
    -- ** Conversion to\/from mutable
  , unsafeFreeze
  , freeze
  , thaw
    -- ** Creation
  , fromRowsFF
  , replicate
  , generate
  , zeros
  , eye
  , diag
  , diagF
    -- ** Access
  , getCol
  , getRow
    -- * Unsafe variants
  , unsafeIndex
  , unsafeGetRow
  , unsafeGetCol
  ) where

import Control.Monad
import Control.Monad.Primitive
import Control.Monad.ST
import Data.Coerce
import Data.List                    (intercalate)
import Data.Vector.Generic.Mutable  qualified as MVG
import Data.Vector.Generic          qualified as VG
import Data.Vector.Fixed.Cont       qualified as FC
import Foreign.Storable
import Foreign.Marshal.Array
import System.IO.Unsafe
import Prelude hiding (replicate)

import Vecvec.Classes
import Vecvec.Classes.NDArray
import Vecvec.Classes.Util
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

instance M.AsMInput s Matrix where
  {-# INLINE asMInput #-}
  asMInput = coerce

type instance NDim Matrix = 2

instance Shape Matrix a where
  shapeCVec (Matrix M.MView{..}) = FC.mk2 nrows ncols
  {-# INLINE shapeCVec #-}



instance (Show a, Storable a) => Show (Matrix a) where
  show m = "[ " ++ intercalate "\n, " [ show (unsafeGetRow m i) | i <- [0 .. nRows m - 1]] ++ "]"

instance (Eq a, Storable a) => Eq (Matrix a) where
  a == b
    | nRows a /= nRows b = False
    | nCols a /= nCols b = False
    | otherwise          = and
        [ unsafeIndex a (i,j) == unsafeIndex b (i,j)
        | i <- [0 .. nRows a - 1]
        , j <- [0 .. nCols a - 1]
        ]

deriving newtype instance (Slice1D i, Slice1D j, Storable a) => Slice (i,j) (Matrix a)

instance C.LAPACKy a => AdditiveSemigroup (Matrix a) where
  m1 .+. m2
    | nRows m1 /= nRows m2 || nCols m1 /= nCols m2 = error "Size mismatch"
    | otherwise = runST $ M.clone m1 >>= \case
        res@(M.AsMVec vres) -> do
          case m2 of
            AsVec v2 -> unsafeBlasAxpy 1 v2 vres
            _        -> forM_ [0 .. nRows m1 - 1] $ \i -> do
              unsafeBlasAxpy 1 (unsafeGetRow m2 i) (M.unsafeGetRow res i)
          unsafeFreeze res
        -- Safe since matrix is newly allocated
        _ -> error "IMPOSSIBLE"

instance C.LAPACKy a => AdditiveQuasigroup (Matrix a) where
  m1 .-. m2
    | nRows m1 /= nRows m2 || nCols m1 /= nCols m2 = error "Size mismatch"
    | otherwise = runST $ M.clone m1 >>= \case
        res@(M.AsMVec vres) -> do
          case m2 of
            AsVec v2 -> unsafeBlasAxpy -1 v2 vres
            _        -> forM_ [0 .. nRows m1 - 1] $ \i -> do
              unsafeBlasAxpy -1 (unsafeGetRow m2 i) (M.unsafeGetRow res i)
          unsafeFreeze res
        -- Safe since matrix is newly allocated
        _ -> error "IMPOSSIBLE"
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
        unsafeBlasAxpy a (unsafeGetRow m i) (M.unsafeGetRow resM i)
    unsafeFreeze resM
  (.*) = flip (*.)



----------------------------------------------------------------
-- Matrix-Vector
----------------------------------------------------------------

instance (C.LAPACKy a, a ~ a') => MatMul (Matrix a) (Vec a') (Vec a) where
  m @@ v
    | nCols m /= VG.length v = error "matrix size mismatch"
  mat @@ vecX = unsafePerformIO $ do
    vecY <- MVG.new (nRows mat)
    M.unsafeBlasGemv C.NoTrans 1 mat vecX 0 vecY
    VG.unsafeFreeze vecY

instance (C.LAPACKy a, a ~ a') => MatMul (Tr (Matrix a)) (Vec a') (Vec a) where
  Tr m @@ v
    | nRows m /= VG.length v = error "matrix size mismatch"
  Tr mat @@ vecX = unsafePerformIO $ do
    vecY <- MVG.new (nCols mat)
    M.unsafeBlasGemv C.Trans 1 mat vecX 0 vecY
    VG.unsafeFreeze vecY

instance (C.LAPACKy a, a ~ a') => MatMul (Conj (Matrix a)) (Vec a') (Vec a) where
  Conj m @@ v
    | nRows m /= VG.length v = error "matrix size mismatch"
  Conj mat @@ vecX = unsafePerformIO $ do
    vecY <- MVG.new (nCols mat)
    M.unsafeBlasGemv C.ConjTrans 1 mat vecX 0 vecY
    VG.unsafeFreeze vecY



----------------------------------------------------------------
-- Matrix-Matrix (9 instances)
----------------------------------------------------------------

instance (C.LAPACKy a, a ~ a') => MatMul (Matrix a) (Matrix a') (Matrix a) where
  matA @@ matB
    | n /= n'   = error "Matrix size mismatch"
    | otherwise = unsafePerformIO $ do
        matC <- M.new (m,k)
        M.unsafeBlasGemm 1 C.NoTrans matA C.NoTrans matB 0 matC
        unsafeFreeze matC
    where
      m  = nRows matA
      n  = nCols matA
      n' = nRows matB
      k  = nCols matB

instance (C.LAPACKy a, a ~ a') => MatMul (Tr (Matrix a)) (Matrix a') (Matrix a) where
  Tr matA @@ matB
    | n /= n'   = error "Matrix size mismatch"
    | otherwise = unsafePerformIO $ do
        matC <- M.new (m,k)
        M.unsafeBlasGemm 1 C.Trans matA C.NoTrans matB 0 matC
        unsafeFreeze matC
    where
      n  = nRows matA
      m  = nCols matA
      n' = nRows matB
      k  = nCols matB

instance (C.LAPACKy a, a ~ a') => MatMul (Conj (Matrix a)) (Matrix a') (Matrix a) where
  Conj matA @@ matB
    | n /= n'   = error "Matrix size mismatch"
    | otherwise = unsafePerformIO $ do
        matC <- M.new (m,k)
        M.unsafeBlasGemm 1 C.ConjTrans matA C.NoTrans matB 0 matC
        unsafeFreeze matC
    where
      n  = nRows matA
      m  = nCols matA
      n' = nRows matB
      k  = nCols matB



instance (C.LAPACKy a, a ~ a') => MatMul (Matrix a) (Tr (Matrix a')) (Matrix a) where
  matA @@ Tr matB
    | n /= n'   = error "Matrix size mismatch"
    | otherwise = unsafePerformIO $ do
        matC <- M.new (m,k)
        M.unsafeBlasGemm 1 C.NoTrans matA C.Trans matB 0 matC
        unsafeFreeze matC
    where
      m  = nRows matA
      n  = nCols matA
      k  = nRows matB
      n' = nCols matB

instance (C.LAPACKy a, a ~ a') => MatMul (Tr (Matrix a)) (Tr (Matrix a')) (Matrix a) where
  Tr matA @@ Tr matB
    | n /= n'   = error "Matrix size mismatch"
    | otherwise = unsafePerformIO $ do
        matC <- M.new (m,k)
        M.unsafeBlasGemm 1 C.Trans matA C.Trans matB 0 matC
        unsafeFreeze matC
    where
      n  = nRows matA
      m  = nCols matA
      k  = nRows matB
      n' = nCols matB

instance (C.LAPACKy a, a ~ a') => MatMul (Conj (Matrix a)) (Tr (Matrix a')) (Matrix a) where
  Conj matA @@ Tr matB
    | n /= n'   = error "Matrix size mismatch"
    | otherwise = unsafePerformIO $ do
        matC <- M.new (m,k)
        M.unsafeBlasGemm 1 C.ConjTrans matA C.Trans matB 0 matC
        unsafeFreeze matC
    where
      n  = nRows matA
      m  = nCols matA
      k  = nRows matB
      n' = nCols matB


instance (C.LAPACKy a, a ~ a') => MatMul (Matrix a) (Conj (Matrix a')) (Matrix a) where
  matA @@ Conj matB
    | n /= n'   = error "Matrix size mismatch"
    | otherwise = unsafePerformIO $ do
        matC <- M.new (m,k)
        M.unsafeBlasGemm 1 C.NoTrans matA C.ConjTrans matB 0 matC
        unsafeFreeze matC
    where
      m  = nRows matA
      n  = nCols matA
      k  = nRows matB
      n' = nCols matB

instance (C.LAPACKy a, a ~ a') => MatMul (Tr (Matrix a)) (Conj (Matrix a')) (Matrix a) where
  Tr matA @@ Conj matB
    | n /= n'   = error "Matrix size mismatch"
    | otherwise = unsafePerformIO $ do
        matC <- M.new (m,k)
        M.unsafeBlasGemm 1 C.Trans matA C.ConjTrans matB 0 matC
        unsafeFreeze matC
    where
      n  = nRows matA
      m  = nCols matA
      k  = nRows matB
      n' = nCols matB

instance (C.LAPACKy a, a ~ a') => MatMul (Conj (Matrix a)) (Conj (Matrix a')) (Matrix a) where
  Conj matA @@ Conj matB
    | n /= n'   = error "Matrix size mismatch"
    | otherwise = unsafePerformIO $ do
        matC <- M.new (m,k)
        M.unsafeBlasGemm 1 C.ConjTrans matA C.ConjTrans matB 0 matC
        unsafeFreeze matC
    where
      n  = nRows matA
      m  = nCols matA
      k  = nRows matB
      n' = nCols matB





unsafeFreeze :: (Storable a, PrimMonad m, s ~ PrimState m)
             => M.MMatrix s a -> m (Matrix a)
unsafeFreeze = pure . coerce


freeze :: (Storable a, PrimMonad m, s ~ PrimState m)
       => M.MMatrix s a -> m (Matrix a)
freeze = unsafeFreeze <=< M.clone

thaw :: (Storable a, PrimMonad m, s ~ PrimState m)
     => Matrix a -> m (M.MMatrix s a)
thaw = M.clone


unsafeIndex :: (Storable a) => Matrix a -> (Int, Int) -> a
{-# INLINE unsafeIndex #-}
unsafeIndex (Matrix M.MView{..}) (i,j)
  = unsafeInlineIO
  $ unsafeWithForeignPtr buffer $ \p -> do
    peekElemOff p (i * leadingDim + j)



unsafeGetRow :: (Storable a) => Matrix a -> Int -> Vec a
unsafeGetRow (Matrix M.MView{..}) i =
  Vec (VecRepr ncols 1 (updPtr (`advancePtr` (leadingDim * i)) buffer))

unsafeGetCol :: (Storable a) => Matrix a -> Int -> Vec a
unsafeGetCol (Matrix M.MView{..}) i =
  Vec (VecRepr nrows leadingDim (updPtr (`advancePtr` i) buffer))

getRow :: (Storable a) => Matrix a -> Int -> Vec a
getRow m@(Matrix M.MView{..}) i
  | i < 0 || i >= nrows = error "Out of range"
  | otherwise           = unsafeGetRow m i 

getCol :: (Storable a) => Matrix a -> Int -> Vec a
getCol m@(Matrix M.MView{..}) i
  | i < 0 || i >= ncols = error "Out of range"
  | otherwise           = unsafeGetCol m i 

fromRowsFF :: (Storable a, Foldable f, Foldable g)
           => f (g a) -> Matrix a
fromRowsFF dat = runST $ unsafeFreeze =<< M.fromRowsFF dat


-- | Fill matrix of given size with provided value.
replicate :: (Storable a)
          => (Int,Int)
          -> a
          -> Matrix a
replicate sz a = runST $ unsafeFreeze =<< M.replicate sz a

-- | Fill matrix of given size using function from indices to element.
generate :: (Storable a)
         => (Int,Int)
         -> (Int -> Int -> a)
         -> Matrix a
generate sz f = runST $ unsafeFreeze =<< M.generate sz f

-- | Create matrix filled with zeros. It's more efficient than using
--   'replicate'.
zeros :: (StorableZero a) => (Int,Int) -> Matrix a
zeros sz = runST $ unsafeFreeze =<< M.zeros sz

-- | Create identity matrix
eye :: (StorableZero a, Num a) => Int -> Matrix a
eye n = runST $ unsafeFreeze =<< M.eye n

-- | Create diagonal matrix. Diagonal elements are stored in list-like
--   container.
diagF :: (StorableZero a, Foldable f) => f a -> Matrix a
diagF xs = runST $ unsafeFreeze =<< M.diagF xs

-- | Create diagonal matrix. Diagonal elements are stored in vector
diag :: (StorableZero a, VG.Vector v a) => v a -> Matrix a
{-# INLINE diag #-}
diag xs = runST $ unsafeFreeze =<< M.diag xs
