#include <cblas.h>
#include <lapacke.h>
#include <stdint.h>
#include <assert.h>

// Define expected size of int used by BLAS
#ifdef VECVEC_BLAS64
typedef int64_t EXPECTED_BLAS;
#else
typedef int EXPECTED_BLAS;
#endif

// Define expected size of int used by LAPACK
#ifdef VECVEC_LAPACK64
typedef int64_t EXPECTED_LAPACK;
#else
typedef int EXPECTED_LAPACK;
#endif

static_assert(
    sizeof(blasint) == sizeof(EXPECTED_BLAS),
    "BLAS integer is not of expected size"
);

static_assert(
    sizeof(lapack_int) == sizeof(int),
    "LAPACK integer is not of expected size"
);
