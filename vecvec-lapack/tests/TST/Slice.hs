{-# LANGUAGE AllowAmbiguousTypes #-}
-- |
module TST.Slice (tests) where

import Data.Typeable
import Test.Tasty
import Test.Tasty.QuickCheck

import Data.Vector.Generic   qualified as VG
import Data.Vector           qualified as V
import Data.Vector.Unboxed   qualified as VU
import Data.Vector.Storable  qualified as VS
import Data.Vector.Primitive qualified as VP

import Vecvec.Classes.NDArray
import Vecvec.LAPACK.Vector         (Vec)

tests :: TestTree
tests = testGroup "slice"
  [ sliceProperties @V.Vector
  , sliceProperties @VU.Vector
  , sliceProperties @VS.Vector
  , sliceProperties @VP.Vector
  , sliceProperties @Vec
  , testProperty "strided slice" prop_slice_stride
  ]

sliceProperties
  :: forall v. ( Slice (Int,End)    (v Int)
               , Slice (Int,Length) (v Int)
               , Slice (Range Int)  (v Int)
               , VG.Vector v Int, Typeable v)
  => TestTree
sliceProperties = testGroup title
  [ testProperty "(Int,End)"    $ prop_slice_end    @v
  , testProperty "(Int,Length)" $ prop_slice_len    @v
  , testProperty "Range+"       $ prop_slice_idxpos @v
  ]
  where
    (con,_) = splitTyConApp $ typeRep (Proxy @v)
    title   = tyConModule con <> "." <> tyConName con


-- Slice with (Int,End) is implemented correctly 
prop_slice_end :: forall v. (Slice (Int,End) (v Int), VG.Vector v Int) => Gen Property
prop_slice_end = do
  len <- choose (0, 100)
  i   <- choose (0, len)
  let vec   = VG.generate @v len id
      vec'  = VG.toList $ slice (i,End) vec
      model = drop i $ VG.toList vec
  pure $ property
       $ counterexample ("len = " ++ show len)
       $ counterexample ("i   = " ++ show i)
       $ vec' == model

-- Slice with (Int,End) is implemented correctly 
prop_slice_len :: forall v. (Slice (Int,Length) (v Int), VG.Vector v Int) => Gen Property
prop_slice_len = do
  len <- choose (0, 100)
  i   <- choose (0, len)
  sz  <- choose (0, len-i)
  let vec   = VG.generate @v len id
      vec'  = VG.toList $ slice (i, Length sz) vec
      model = take sz $ drop i $ VG.toList vec
  pure $ property
       $ counterexample ("len = " ++ show len)
       $ counterexample ("i   = " ++ show i)
       $ counterexample ("sz  = " ++ show sz)
       $ vec' == model

prop_slice_idxpos :: forall v. (Slice (Range Int) (v Int), VG.Vector v Int) => Gen Property
prop_slice_idxpos = do
  len <- choose (0, 100)
  i   <- case len of 0 -> pure 0
                     _ -> choose (0, len-1)
  j   <- choose (0, len)
  let vec   = VG.generate @v len id
      vec'  = VG.toList $ slice (i :.. j) vec
      model = [i .. j-1]
  pure $ property
       $ counterexample ("len = " ++ show len)
       $ counterexample ("i   = " ++ show i)
       $ counterexample ("j   = " ++ show j)
       $ vec' == model

prop_slice_stride :: Gen Property
prop_slice_stride = do
  len <- choose (0, 100)
  i   <- case len of 0 -> pure 0
                     _ -> choose (0, len-1)
  j   <- choose (0, len)
  s   <- choose (1,5)
  let vec   = VG.generate @Vec len id
      vec'  = VG.toList $ slice ((i :.. j) `Strided` s) vec
      model = [i, i+s .. j-1]
  pure $ property
       $ counterexample ("len = " ++ show len)
       $ counterexample ("i   = " ++ show i)
       $ counterexample ("j   = " ++ show j)
       $ vec' == model
