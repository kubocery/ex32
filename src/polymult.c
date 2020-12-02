#include <R.h>
#include <Rdefines.h>


SEXP polymult_call(SEXP x, SEXP y) {
  SEXP z;
  int z_len = LENGTH(x) + LENGTH(y) - 1;
  PROTECT(z = NEW_NUMERIC(z_len));
  for (int i=0; i < z_len; i++) {
    NUMERIC_POINTER(z)[i] = 0;
  }
  for (int i=0; i < LENGTH(x); i++) {
    for (int j=0; j < LENGTH(y); j++) {
      NUMERIC_POINTER(z)[i + j] += NUMERIC_POINTER(x)[i] * NUMERIC_POINTER(y)[j];
    }
  }
  UNPROTECT(1);
  return z;
}

void polymult_c(double *x, double *y, int *x_len, int *y_len, double *z) {
  for (int i = 0; i < *x_len; i++) {
    for (int j = 0; j < *y_len; j++){
      z[i + j] += x[i] * y[j];
    }
  }
}

