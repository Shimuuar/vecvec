> import Prelude hiding ((<>))
> import Numeric.LinearAlgebra
> import Vecvec.LAPACK.LinAlg
> import Vecvec.LAPACK.Matrix.Dense as M
> import Vecvec.LAPACK.FFI
> import qualified Vecvec.Classes as VV

> u = vector [1..5]

> v = vector [10,-3,0,4,5]

> w = u <.> v
