#ifndef VCTRS_SIZE_H
#define VCTRS_SIZE_H

#include "vctrs.h"
#include "utils.h"

// These versions return NULL and 0 for bare vectors.
// This is useful to distinguish them from 1D arrays.
static inline SEXP vec_bare_dim(SEXP x) {
  return Rf_getAttrib(x, R_DimSymbol);
}
static inline R_len_t vec_bare_dim_n(SEXP x) {
  return Rf_length(vec_bare_dim(x));
}


static inline SEXP vec_dim(SEXP x) {
  SEXP dim = PROTECT(vec_bare_dim(x));

  if (dim == R_NilValue) {
    dim = r_int(Rf_length(x));
  }

  UNPROTECT(1);
  return dim;
}

static inline R_len_t vec_dim_n(SEXP x) {
  return Rf_length(vec_dim(x));
}

#endif
