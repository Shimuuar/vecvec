# Introduction

This README is generated via

```
BlogLiterately --ghci docs/Comparison.lhs > docs/Comparison.html
pandoc docs/Comparison.html -o docs/README.md
```

# Age-Dependent Populations

## McKendrick / von Foerster

``` {.sourceCode .haskell}
import Prelude hiding ((<>))
import Numeric.LinearAlgebra
import Vecvec.LAPACK.LinAlg
import Vecvec.LAPACK.Matrix.Dense as M
import Vecvec.LAPACK.FFI
import qualified Vecvec.Classes as VV
```

``` {.sourceCode .haskell}
u = vector [1..5]
```

``` {.sourceCode .haskell}
v = vector [10,-3,0,4,5]
```

    ghci> 
    ghci> u + v
      [11.0,-1.0,3.0,8.0,10.0]

    ghci> 
    ghci> u <.> v
      45.0

    ghci> 
    ghci> u - 10
      [-9.0,-8.0,-7.0,-6.0,-5.0]

``` {.sourceCode .haskell}
w = vector [1, 0, 1, 0, 1, 0, 1]
```

    ghci> norm_2 w
      2.0

``` {.sourceCode .haskell}
mm :: M.Matrix Double
mm = fromRowsFF [[1.0, 0.0], [0.0, 1.0]]
```

    ghci> mm
      [ [1.0,0.0]
      , [0.0,1.0]]
