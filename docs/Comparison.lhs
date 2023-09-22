% WENO
% Dominic Steinitz
% 15th August 2020

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
> import qualified Vecvec.Classes as VV

> u = vector [1..5]

> v = vector [10,-3,0,4,5]

    [ghci]

    u + v

    u <.> v

    u - 10

> w = vector [1, 0, 1, 0, 1, 0, 1]

    [ghci]
    norm_2 w

> mm :: M.Matrix Double
> mm = fromRowsFF [[1.0, 0.0], [0.0, 1.0]]

    [ghci]
    mm
