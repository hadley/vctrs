#ifndef VCTRS_EQUAL_H
#define VCTRS_EQUAL_H

#include "vctrs.h"


static inline bool lgl_scalar_equal_missing(int x) {
  return x == NA_LOGICAL;
}
static inline int lgl_equal_scalar_na_equal(const int* x, const int* y) {
  return *x == *y;
}
static inline int lgl_equal_scalar_na_propagate(const int* x, const int* y) {
  // Storing pointed values on the stack helps performance
  const int xi = *x;
  const int yj = *y;
  return (xi == NA_LOGICAL || yj == NA_LOGICAL) ? NA_LOGICAL : xi == yj;
}
static inline int lgl_equal_scalar(const int* x, const int* y, bool na_equal) {
  if (na_equal) {
    return lgl_equal_scalar_na_equal(x, y);
  } else {
    return lgl_equal_scalar_na_propagate(x, y);
  }
}

static inline bool int_scalar_equal_missing(int x) {
  return x == NA_INTEGER;
}
static inline int int_equal_scalar_na_equal(const int* x, const int* y) {
  return *x == *y;
}
static inline int int_equal_scalar_na_propagate(const int* x, const int* y) {
  const int xi = *x;
  const int yj = *y;
  return (xi == NA_INTEGER || yj == NA_INTEGER) ? NA_LOGICAL : xi == yj;
}
static inline int int_equal_scalar(const int* x, const int* y, bool na_equal) {
  if (na_equal) {
    return int_equal_scalar_na_equal(x, y);
  } else {
    return int_equal_scalar_na_propagate(x, y);
  }
}

static inline bool dbl_scalar_equal_missing(double x) {
  return isnan(x);
}
static inline int dbl_equal_scalar_na_equal(const double* x, const double* y) {
  const double xi = *x;
  const double yj = *y;

  switch (dbl_classify(xi)) {
  case vctrs_dbl_number: break;
  case vctrs_dbl_missing: return dbl_classify(yj) == vctrs_dbl_missing;
  case vctrs_dbl_nan: return dbl_classify(yj) == vctrs_dbl_nan;
  }

  if (isnan(yj)) {
    return false;
  } else {
    return xi == yj;
  }
}
static inline int dbl_equal_scalar_na_propagate(const double* x, const double* y) {
  const double xi = *x;
  const double yj = *y;
  if (dbl_scalar_equal_missing(xi) || dbl_scalar_equal_missing(yj)) {
    return NA_LOGICAL;
  } else {
    return xi == yj;
  }
}
static inline int dbl_equal_scalar(const double* x, const double* y, bool na_equal) {
  if (na_equal) {
    return dbl_equal_scalar_na_equal(x, y);
  } else {
    return dbl_equal_scalar_na_propagate(x, y);
  }
}

static inline bool cpl_scalar_equal_missing(Rcomplex x) {
  return dbl_scalar_equal_missing(x.r) || dbl_scalar_equal_missing(x.i);
}
static inline int cpl_equal_scalar_na_equal(const Rcomplex* x, const Rcomplex* y) {
  Rcomplex xi = *x;
  Rcomplex yj = *y;

  int real_equal = dbl_equal_scalar_na_equal(&xi.r, &yj.r);
  int imag_equal = dbl_equal_scalar_na_equal(&xi.i, &yj.i);

  return real_equal && imag_equal;
}
static inline int cpl_equal_scalar_na_propagate(const Rcomplex* x, const Rcomplex* y) {
  Rcomplex xi = *x;
  Rcomplex yj = *y;

  int real_equal = dbl_equal_scalar_na_propagate(&xi.r, &yj.r);
  int imag_equal = dbl_equal_scalar_na_propagate(&xi.i, &yj.i);

  if (real_equal == NA_LOGICAL || imag_equal == NA_LOGICAL) {
    return NA_LOGICAL;
  } else {
    return real_equal && imag_equal;
  }
}
static inline int cpl_equal_scalar(const Rcomplex* x, const Rcomplex* y, bool na_equal) {
  if (na_equal) {
    return cpl_equal_scalar_na_equal(x, y);
  } else {
    return cpl_equal_scalar_na_propagate(x, y);
  }
}

// UTF-8 translation is successful in these cases:
// - (utf8 + latin1), (unknown + utf8), (unknown + latin1)
// UTF-8 translation fails purposefully in these cases:
// - (bytes + utf8), (bytes + latin1), (bytes + unknown)
// UTF-8 translation is not attempted in these cases:
// - (utf8 + utf8), (latin1 + latin1), (unknown + unknown), (bytes + bytes)

static inline int chr_equal_scalar_impl(const SEXP x, const SEXP y) {
  if (x == y) {
    return 1;
  }

  if (Rf_getCharCE(x) != Rf_getCharCE(y)) {
    const void *vmax = vmaxget();
    int out = !strcmp(Rf_translateCharUTF8(x), Rf_translateCharUTF8(y));
    vmaxset(vmax);
    return out;
  }

  return 0;
}

static inline bool chr_scalar_equal_missing(SEXP x) {
  return x == NA_STRING;
}
static inline int chr_equal_scalar_na_equal(const SEXP* x, const SEXP* y) {
  const SEXP xi = *x;
  const SEXP yj = *y;
  return chr_equal_scalar_impl(xi, yj);
}
static inline int chr_equal_scalar_na_propagate(const SEXP* x, const SEXP* y) {
  const SEXP xi = *x;
  const SEXP yj = *y;
  return (xi == NA_STRING || yj == NA_STRING) ? NA_LOGICAL : chr_equal_scalar_impl(xi, yj);
}
static inline int chr_equal_scalar(const SEXP* x, const SEXP* y, bool na_equal) {
  if (na_equal) {
    return chr_equal_scalar_na_equal(x, y);
  } else {
    return chr_equal_scalar_na_propagate(x, y);
  }
}

static inline bool list_scalar_equal_missing(SEXP x, R_len_t i) {
  return VECTOR_ELT(x, i) == R_NilValue;
}
static inline int list_equal_scalar_na_equal(SEXP x, R_len_t i, SEXP y, R_len_t j) {
  const SEXP xi = VECTOR_ELT(x, i);
  const SEXP yj = VECTOR_ELT(y, j);
  return equal_object(xi, yj);
}
static inline int list_equal_scalar_na_propagate(SEXP x, R_len_t i, SEXP y, R_len_t j) {
  const SEXP xi = VECTOR_ELT(x, i);
  const SEXP yj = VECTOR_ELT(y, j);
  return (xi == R_NilValue || yj == R_NilValue) ? NA_LOGICAL : equal_object(xi, yj);
}
static inline int list_equal_scalar(SEXP x, R_len_t i, SEXP y, R_len_t j, bool na_equal) {
  if (na_equal) {
    return list_equal_scalar_na_equal(x, i, y, j);
  } else {
    return list_equal_scalar_na_propagate(x, i, y, j);
  }
}

// Raw vectors have no notion of missing value
static inline bool raw_scalar_equal_missing(Rbyte x) {
  return false;
}
static inline int raw_equal_scalar(const Rbyte* x, const Rbyte* y, bool na_equal) {
  return *x == *y;
}
static inline int raw_equal_scalar_na_equal(const Rbyte* x, const Rbyte* y) {
  return *x == *y;
}
static inline int raw_equal_scalar_na_propagate(const Rbyte* x, const Rbyte* y) {
  return *x == *y;
}

static inline int df_equal_scalar(SEXP x, R_len_t i, SEXP y, R_len_t j, bool na_equal, int n_col) {
  for (int k = 0; k < n_col; ++k) {
    int eq = equal_scalar(VECTOR_ELT(x, k), i, VECTOR_ELT(y, k), j, na_equal);

    if (eq <= 0) {
      return eq;
    }
  }

  return true;
}



#endif
