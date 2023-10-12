-- NOTE This is a copy from `vector` package
--
{-# LANGUAGE CPP               #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE TypeOperators     #-}
-- NOTE Since this file is almost entirely copied from `vector`,
-- we almost don't edit it, but just silence the warnings.
-- In the hope that the tests from `vector` will be separated into a separate package.
{-# OPTIONS_GHC -Wno-missing-signatures    #-}
module TST.Vector.Utilities where

import Test.QuickCheck
import Data.Foldable
import Data.Bifunctor
import qualified Data.Vector as DV
import qualified Data.Vector.Generic as DVG
import qualified Data.Vector.Primitive as DVP
import qualified Data.Vector.Storable as DVS
import qualified Data.Vector.Unboxed as DVU
import qualified Data.Vector.Fusion.Bundle as S
import Vecvec.LAPACK.Internal.Vector as VV

import Control.Monad (foldM, foldM_, zipWithM, zipWithM_)
import Control.Monad.Trans.Writer
import Data.Complex
import Data.Functor.Identity
import Data.List ( sortBy )
import Data.Maybe (catMaybes)

import qualified Vecvec.Classes.NDArray                  as Slice


class (Testable (EqTest a), Conclusion (EqTest a)) => TestData a where
  type Model a
  model :: a -> Model a
  unmodel :: Model a -> a

  type EqTest a
  type instance EqTest a = Property
  equal :: a -> a -> EqTest a
  default equal :: (Eq a, EqTest a ~ Property) => a -> a -> EqTest a
  equal x y = property (x == y)


instance (Eq a, TestData a) => TestData (S.Bundle v a) where
  type Model (S.Bundle v a) = [Model a]
  model   = map model  . S.toList
  unmodel = S.fromList . map unmodel

instance (Eq a, TestData a) => TestData (DV.Vector a) where
  type Model (DV.Vector a) = [Model a]
  model   = map model   . DV.toList
  unmodel = DV.fromList . map unmodel

instance (Eq a, DVP.Prim a, TestData a) => TestData (DVP.Vector a) where
  type Model (DVP.Vector a) = [Model a]
  model   = map model    . DVP.toList
  unmodel = DVP.fromList . map unmodel

instance (Eq a, DVS.Storable a, TestData a) => TestData (DVS.Vector a) where
  type Model (DVS.Vector a) = [Model a]
  model   = map model    . DVS.toList
  unmodel = DVS.fromList . map unmodel

-- TODO use TST.Model

instance (Eq a, DVS.Storable a, TestData a) => TestData (VV.Vec a) where
  type Model (VV.Vec a) = [Model a]
  model = map model . DVG.toList
  -- We want to exercise both stride=1 and >1 but list doesn't have
  -- extra bit for this case. So we cheat and use list length for that
  unmodel lst
    | odd (length lst) = DVG.fromList $ unmodel <$> lst
    | otherwise        = Slice.slice ((0,Slice.End) `Strided` stride)
                       $ DVG.fromList
                       $ replicate stride =<< map unmodel lst
    where stride = 2
  type EqTest (VV.Vec a) = Property
  equal x y = property (x == y)


instance (Eq a, DVU.Unbox a, TestData a) => TestData (DVU.Vector a) where
  type Model (DVU.Vector a) = [Model a]
  model   = map model    . DVU.toList
  unmodel = DVU.fromList . map unmodel

#define id_TestData(ty) \
instance TestData ty where { \
  type Model ty = ty;        \
  model = id;                \
  unmodel = id }             \

id_TestData(())
id_TestData(Bool)
id_TestData(Int)
id_TestData(Ordering)

instance TestData Float where
  type Model Float = Float
  model = id
  unmodel = id

  equal x y = property (x == y || (isNaN x && isNaN y))

instance TestData Double where
  type Model Double = Double
  model = id
  unmodel = id

  equal x y = property (x == y || (isNaN x && isNaN y))

instance TestData a => TestData (Complex a) where
  type Model (Complex a) = Complex (Model a)
  model   = fmap model
  unmodel = fmap unmodel
  equal (r1 :+ i1) (r2 :+ i2) = equal r1 r2 .&&. equal i1 i2

-- Functorish models
-- All of these need UndecidableInstances although they are actually well founded. Oh well.
instance (Eq a, TestData a) => TestData (Maybe a) where
  type Model (Maybe a) = Maybe (Model a)
  model = fmap model
  unmodel = fmap unmodel

instance (Eq a, TestData a, Eq b, TestData b) => TestData (Either a b) where
  type Model (Either a b) = Either (Model a) (Model b)
  model = bimap model model
  unmodel = bimap unmodel unmodel

instance (Eq a, TestData a) => TestData [a] where
  type Model [a] = [Model a]
  model = fmap model
  unmodel = fmap unmodel

instance (Eq a, TestData a) => TestData (Identity a) where
  type Model (Identity a) = Identity (Model a)
  model = fmap model
  unmodel = fmap unmodel

instance (Eq a, TestData a, Eq b, TestData b, Monoid a) => TestData (Writer a b) where
  type Model (Writer a b) = Writer (Model a) (Model b)
  model = mapWriter model
  unmodel = mapWriter unmodel

instance (Eq a, Eq b, TestData a, TestData b) => TestData (a,b) where
  type Model (a,b) = (Model a, Model b)
  model (a,b) = (model a, model b)
  unmodel (a,b) = (unmodel a, unmodel b)

instance (Eq a, Eq b, Eq c, TestData a, TestData b, TestData c) => TestData (a,b,c) where
  type Model (a,b,c) = (Model a, Model b, Model c)
  model (a,b,c) = (model a, model b, model c)
  unmodel (a,b,c) = (unmodel a, unmodel b, unmodel c)

instance (Arbitrary a, Show a, TestData a, TestData b) => TestData (a -> b) where
  type Model (a -> b) = Model a -> Model b
  model f = model . f . unmodel
  unmodel f = unmodel . f . model

  type EqTest (a -> b) = a -> EqTest b
  equal f g x = equal (f x) (g x)

newtype P a = P { unP :: EqTest a }

instance TestData a => Testable (P a) where
  property (P a) = property a

infix 4 `eq`
eq :: TestData a => a -> Model a -> P a
eq x y = P (equal x (unmodel y))

class Conclusion p where
  type Predicate p

  predicate :: Predicate p -> p -> p

instance Conclusion Property where
  type Predicate Property = Bool

  predicate = (==>)

instance Conclusion p => Conclusion (a -> p) where
  type Predicate (a -> p) = a -> Predicate p

  predicate f p = \x -> predicate (f x) (p x)

infixr 0 ===>
(===>) :: TestData a => Predicate (EqTest a) -> P a -> P a
p ===> P a = P (predicate p a)

notNull2 _ xs = not $ DVG.null xs
notNullS2 _ s = not $ S.null s

-- Generators
index_value_pairs :: Arbitrary a => Int -> Gen [(Int,a)]
index_value_pairs 0 = return []
index_value_pairs m = sized $ \n ->
  do
    len <- choose (0,n)
    is <- sequence [choose (0,m-1) | _i <- [1..len]]
    xs <- vector len
    return $ zip is xs

indices :: Int -> Gen [Int]
indices 0 = return []
indices m = sized $ \n ->
  do
    len <- choose (0,n)
    sequence [choose (0,m-1) | _i <- [1..len]]


-- Additional list functions
singleton x = [x]
snoc xs x = xs ++ [x]
generate n f = [f i | i <- [0 .. n-1]]
generateM n f = sequence [f i | i <- [0 .. n-1]]
slice i n xs = take n (drop i xs)
backpermute xs is = map (xs!!) is
prescanl f z = init . scanl f z
postscanl f z = tail . scanl f z
prescanr f z = tail . scanr f z
postscanr f z = init . scanr f z

accum :: (a -> b -> a) -> [a] -> [(Int,b)] -> [a]
accum f xs ps = go xs ps' 0
  where
    ps' = sortBy (\p q -> compare (fst p) (fst q)) ps

    go (x:xxs) ((i,y) : pps) j
      | i == j     = go (f x y : xxs) pps j
    go (x:xxs) pps j = x : go xxs pps (j+1)
    go [] _ _      = []

(//) :: [a] -> [(Int, a)] -> [a]
xs // ps = go xs ps' 0
  where
    ps' = sortBy (\p q -> compare (fst p) (fst q)) ps

    go (_x:xxs) ((i,y) : pps) j
      | i == j     = go (y:xxs) pps j
    go (x:xxs) pps j = x : go xxs pps (j+1)
    go [] _ _      = []


withIndexFirst m f = m (uncurry f) . zip [0..]

modifyList :: [a] -> (a -> a) -> Int -> [a]
modifyList xs f i = zipWith merge xs (replicate i Nothing ++ [Just f] ++ repeat Nothing)
  where
    merge x Nothing  = x
    merge x (Just g) = g x

writeList :: [a] -> Int -> a -> [a]
writeList xs i a = modifyList xs (const a) i

imap :: (Int -> a -> a) -> [a] -> [a]
imap = withIndexFirst map

imapM :: Monad m => (Int -> a -> m a) -> [a] -> m [a]
imapM = withIndexFirst mapM

imapM_ :: Monad m => (Int -> a -> m b) -> [a] -> m ()
imapM_ = withIndexFirst mapM_

izipWith :: (Int -> a -> a -> a) -> [a] -> [a] -> [a]
izipWith = withIndexFirst zipWith

izipWithM :: Monad m => (Int -> a -> a -> m a) -> [a] -> [a] -> m [a]
izipWithM = withIndexFirst zipWithM

izipWithM_ :: Monad m => (Int -> a -> a -> m b) -> [a] -> [a] -> m ()
izipWithM_ = withIndexFirst zipWithM_

izipWith3 :: (Int -> a -> a -> a -> a) -> [a] -> [a] -> [a] -> [a]
izipWith3 = withIndexFirst zipWith3

ifilter :: (Int -> a -> Bool) -> [a] -> [a]
ifilter f = map snd . withIndexFirst filter f

imapMaybe :: (Int -> a -> Maybe b) -> [a] -> [b]
imapMaybe f = catMaybes . withIndexFirst map f

indexedLeftFold fld f z = fld (uncurry . f) z . zip [0..]

ifoldl :: (a -> Int -> a -> a) -> a -> [a] -> a
ifoldl = indexedLeftFold foldl

iscanl :: (Int -> a -> b -> a) -> a -> [b] -> [a]
iscanl f z = scanl (\a (i, b) -> f i a b) z . zip [0..]

iscanr :: (Int -> a -> b -> b) -> b -> [a] -> [b]
iscanr f z = scanr (uncurry f) z . zip [0..]

ifoldr :: (Int -> a -> b -> b) -> b -> [a] -> b
ifoldr f z = foldr (uncurry f) z . zip [0..]

ifoldM :: Monad m => (b -> Int -> a -> m b) -> b -> [a] -> m b
ifoldM = indexedLeftFold foldM

ifoldrM :: Monad m => (Int -> a -> b -> m b) -> b -> [a] -> m b
ifoldrM f z xs = foldrM (\(i,a) b -> f i a b) z ([0..] `zip` xs)

ifoldM_ :: Monad m => (b -> Int -> a -> m b) -> b -> [a] -> m ()
ifoldM_ = indexedLeftFold foldM_

minIndex :: Ord a => [a] -> Int
minIndex = fst . foldr1 imin . zip [0..]
  where
    imin (i,x) (j,y) | x <= y    = (i,x)
                     | otherwise = (j,y)

maxIndex :: Ord a => [a] -> Int
maxIndex = fst . foldr1 imax . zip [0..]
  where
    imax (i,x) (j,y) | x >= y    = (i,x)
                     | otherwise = (j,y)

iterateNM :: Monad m => Int -> (a -> m a) -> a -> m [a]
iterateNM n f x
    | n <= 0    = return []
    | n == 1    = return [x]
    | otherwise =  do x' <- f x
                      xs <- iterateNM (n-1) f x'
                      return (x : xs)

unfoldrM :: Monad m => (b -> m (Maybe (a,b))) -> b -> m [a]
unfoldrM step b0 = do
    r <- step b0
    case r of
      Nothing    -> return []
      Just (a,b) -> do as <- unfoldrM step b
                       return (a : as)


limitUnfolds f (theirs, ours)
    | ours >= 0
    , Just (out, theirs') <- f theirs = Just (out, (theirs', ours - 1))
    | otherwise                       = Nothing
