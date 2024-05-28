% hmatrix vs vecvec
% Dominic Steinitz
% 28th May 2024

Introduction
============

Age-Dependent Populations
=========================

McKendrick / von Foerster
-------------------------

> import Prelude hiding ((<>))
> import Numeric.LinearAlgebra
> import Vecvec.LAPACK.LinAlg
> import Vecvec.LAPACK.Matrix.Dense as M
> import Vecvec.LAPACK.FFI
> import Vecvec.Classes            qualified as VV
> import Vecvec.Classes ((.+.))
> import Vecvec.LAPACK.Vector
> import Data.Vector.Generic       qualified as VG

> u = vector [1..5]

> v = vector [10,-3,0,4,5]

    [ghci]

    u + v

    u <.> v

    u - 10

> u' :: Vector Double
> u' = fromList [1..5]

> v' :: Vector Double
> v' = fromList [10,-3,0,4,5]

    [ghci]

    u' .+. v'

> w = vector [1, 0, 1, 0, 1, 0, 1]

    [ghci]
    norm_2 w

> mm :: M.Matrix Double
> mm = fromRowsFF [[1.0, 0.0], [0.0, 1.0]]

    [ghci]
    mm
