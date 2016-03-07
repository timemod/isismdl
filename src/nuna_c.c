#include <R.h>
#include <Rinternals.h>

// C function get_NA_real to be called from Fortran
double F77_SUB(get_na_real)(void) {
    return NA_REAL;
}


// C function r_finite to be called from Fortran
int F77_SUB(r_finite)(double *x) {
    return R_FINITE(*x);
}



