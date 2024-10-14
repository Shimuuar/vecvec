-- |
-- Data types which are necessary for defining APIs for solving linear
-- systems etc. But ones that aren't needed in standard
-- workflows. They're placed in separate module to avoid namespace
-- pollution
module Vecvec.LAPACK.LinAlg.Types
  ( -- * Right side of equation
    -- $RHS
    RhsDim(..)
  , RhsStore(..)
  , RhsParser(..)
  , PreparedRHS(..)
  , EquationRHS(..)
  , rhsToMatrix
  , rhsGetSolutions
  ) where

import Control.Monad
import Control.Monad.ST
import Data.Proxy
import Data.Vector               qualified as V
import Data.Vector.Unboxed       qualified as VU
import Data.Vector.Storable      qualified as VS
import Data.Vector.Primitive     qualified as VP
import Data.Vector.Generic       qualified as VG
import Foreign.Storable

import Vecvec.Classes.NDMutable
import Vecvec.LAPACK.Vector         (Vec)
import Vecvec.LAPACK.Matrix         (Matrix,getCol)
import Vecvec.LAPACK.Matrix.Mutable (MMatrix)
import Vecvec.LAPACK.Matrix.Mutable qualified as MMat

-- $RHS
--
-- When solving linear equations it's desirable to batch right hand
-- sides (RHS) for same matrix. Here we have supporting infrastructure
-- for generating isomorphism between haskell types and RHS in matrix
-- form. Each RHS is a column in a matrix.


-- | Data type used for computing dimension of RHS of equations
data RhsDim
  = DimMismatch !Int !Int
  -- ^ There're at least two vectors with different dimension
  | UnknownDim
  -- ^ Dimension is unknown (e.g. RHS is empty)
  | Dim !Int
  -- ^ All vectors on RHS have this dimension.
  deriving (Show)

instance Semigroup RhsDim where
  Dim n           <> Dim k | n == k = Dim n
                           | otherwise = DimMismatch n k
  UnknownDim      <> d               = d
  d               <> UnknownDim      = d
  d@DimMismatch{} <> _               = d
  _               <> d@DimMismatch{} = d

instance Monoid RhsDim where
  mempty = UnknownDim


-- | Builder monoid for writing RHS into predefined buffer.
newtype RhsStore s a = RhsStore (MMatrix s a -> Int -> ST s Int)

instance Semigroup (RhsStore s a) where
  RhsStore fA <> RhsStore fB = RhsStore $ \mat -> fA mat >=> fB mat

instance Monoid (RhsStore s a) where
  mempty = RhsStore $ \_ -> pure


-- | Parser for a right hand side of an equation
newtype RhsParser a r = RhsParser (Matrix a -> Int -> (Int, r))
  deriving stock Functor

instance Applicative (RhsParser a) where
  pure a = RhsParser $ \_ i -> (i,a)
  RhsParser funF <*> RhsParser funA = RhsParser $ \m i ->
    case funF m i of
      (!i', f) -> case funA m i' of
        (!i'', a) -> (i'', f a)

-- | RHS in matrix form. Solver will overwrite buffer for we return
--   mutable RHS
data PreparedRHS s a
  = PreparedRHS (MMatrix s a) -- ^ RHS matrix. 
  | EmptyRHS                  -- ^ We don't have any RHS
  | InvalidRHS !Int !Int      -- ^ RHS have vectors with different dimensions


-- | When solving linear equations like \(Ax=b\) most of the work is
--   spent on factoring matrix. Thus it's computationally advantageous
--   to batch right hand sides of an equation. This type class exists
--   in order to built such batches in form of matrices from haskell
--   data structures
class EquationRHS rhs a where
  -- | Number of right hand sides in a container.
  numberOfRhs :: Proxy a -> rhs -> Int
  -- | Compute dimension of right hand side. Should return @Nothing@
  --   if it's not possible to compute one or right sides have different sizes
  dimensionOfRhs :: Proxy a -> rhs -> RhsDim
  -- | Store RHS to a matrix as columns.
  storeRhsToMatrix :: Storable a => rhs -> RhsStore s a
  -- | Read solution from a matrix. Original right hand side provides
  --   information about shape of RHS.
  loadRhsFromMatrix :: Storable a => rhs -> RhsParser a rhs

-- | Convert right hand of equation to matrix where each \(b\) is
--   arranged as column. We need to create mutable matrix in order
--   to ensure that fresh buffer is allocated since LAPACK routines
--   frequently reuse storage.
rhsToMatrix
  :: forall a rhs s. (EquationRHS rhs a, Storable a)
  => rhs -> ST s (PreparedRHS s a)
rhsToMatrix rhs = do
  case dimensionOfRhs (Proxy @a) rhs of
    UnknownDim -> pure $ EmptyRHS
    Dim k      -> do
      mat <- MMat.new (k, n)
      _   <- case storeRhsToMatrix rhs of
        RhsStore f -> f mat 0
      return $ PreparedRHS mat
    DimMismatch i j -> pure $ InvalidRHS i j
  where
    n = numberOfRhs (Proxy @a) rhs


-- | Extract solutions from matrix. First argument is used to retain
--   information which isn't right hand sides.
rhsGetSolutions
  :: forall a rhs. (EquationRHS rhs a, Storable a)
  => rhs -> Matrix a -> rhs
rhsGetSolutions rhs solution =
  case loadRhsFromMatrix rhs of
    RhsParser fun -> snd $ fun solution 0


----------------------------------------------------------------
-- Instances
----------------------------------------------------------------

instance (EquationRHS r1 a, EquationRHS r2 a) => EquationRHS (r1,r2) a where
  numberOfRhs    p  (r1,r2) = numberOfRhs    p r1 +  numberOfRhs    p r2
  dimensionOfRhs p  (r1,r2) = dimensionOfRhs p r1 <> dimensionOfRhs p r2
  storeRhsToMatrix  (r1,r2) = storeRhsToMatrix r1 <> storeRhsToMatrix r2
  loadRhsFromMatrix (r1,r2) = (,) <$> loadRhsFromMatrix r1 <*> loadRhsFromMatrix r2

instance (EquationRHS r a) => EquationRHS [r] a where
  numberOfRhs    p xs = sum $ numberOfRhs p <$> xs
  dimensionOfRhs p    = foldMap (dimensionOfRhs p)
  storeRhsToMatrix    = foldMap storeRhsToMatrix
  loadRhsFromMatrix   = traverse loadRhsFromMatrix



instance (a ~ a', Storable a) => EquationRHS (Matrix a) a' where
  numberOfRhs    _ = nCols
  dimensionOfRhs _ = Dim . nRows
  --
  storeRhsToMatrix src = RhsStore $ \rhs i -> do
    let n = nCols src
    MMat.copy src (((0,End), (i, Length n)) `slice` rhs)
    return $! i + n
  --
  loadRhsFromMatrix rhs = RhsParser $ \sol i ->
    (i+n, ((0,End), (i, Length n)) `slice` sol)
    where n = nCols rhs


instance (a ~ a', Storable a) => EquationRHS (Vec a) a' where
  numberOfRhs    _ _ = 1
  dimensionOfRhs _   = Dim . VG.length
  storeRhsToMatrix v = RhsStore $ \rhs i -> do
    VG.iforM_ v $ \j a -> writeArr rhs (j,i) a
    return $! i + 1
  loadRhsFromMatrix _ = RhsParser $ \sol i -> (i+1, getCol sol i)

instance (a ~ a', Storable a) => EquationRHS (V.Vector a) a' where
  numberOfRhs    _ _ = 1
  dimensionOfRhs _   = Dim . VG.length
  storeRhsToMatrix v = RhsStore $ \rhs i -> do
    VG.iforM_ v $ \j a -> writeArr rhs (j,i) a
    return $! i + 1
  loadRhsFromMatrix _ = RhsParser $ \sol i -> (i+1, VG.convert $ getCol sol i)

instance (a ~ a', Storable a) => EquationRHS (VS.Vector a) a' where
  numberOfRhs    _ _ = 1
  dimensionOfRhs _   = Dim . VG.length
  storeRhsToMatrix v = RhsStore $ \rhs i -> do
    VG.iforM_ v $ \j a -> writeArr rhs (j,i) a
    return $! i + 1
  loadRhsFromMatrix _ = RhsParser $ \sol i -> (i+1, VG.convert $ getCol sol i)

instance (a ~ a', Storable a, VU.Unbox a) => EquationRHS (VU.Vector a) a' where
  numberOfRhs    _ _ = 1
  dimensionOfRhs _   = Dim . VG.length
  storeRhsToMatrix v = RhsStore $ \rhs i -> do
    VG.iforM_ v $ \j a -> writeArr rhs (j,i) a
    return $! i + 1
  loadRhsFromMatrix _ = RhsParser $ \sol i -> (i+1, VG.convert $ getCol sol i)

instance (a ~ a', Storable a, VP.Prim a) => EquationRHS (VP.Vector a) a' where
  numberOfRhs    _ _ = 1
  dimensionOfRhs _   = Dim . VG.length
  storeRhsToMatrix v = RhsStore $ \rhs i -> do
    VG.iforM_ v $ \j a -> writeArr rhs (j,i) a
    return $! i + 1
  loadRhsFromMatrix _ = RhsParser $ \sol i -> (i+1, VG.convert $ getCol sol i)
