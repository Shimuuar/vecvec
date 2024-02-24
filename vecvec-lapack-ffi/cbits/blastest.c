#include <cblas.h>
#include <lapacke.h>
#include <stdint.h>
#include <assert.h>


static_assert(
    sizeof(blasint) == sizeof(int),
    "BLAS integer is not of same size as int"
);

static_assert(
    sizeof(lapack_int) == sizeof(int),
    "LAPACK integer is not of same size as int"
);
