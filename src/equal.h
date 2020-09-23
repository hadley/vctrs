#ifndef VCTRS_EQUAL_H
#define VCTRS_EQUAL_H

#include "vctrs.h"

// -----------------------------------------------------------------------------

static inline bool lgl_equal_missing_scalar(int x) {
  return x == NA_LOGICAL;
}
static inline bool int_equal_missing_scalar(int x) {
  return x == NA_INTEGER;
}
static inline bool dbl_equal_missing_scalar(double x) {
  return isnan(x);
}
static inline bool cpl_equal_missing_scalar(Rcomplex x) {
  return dbl_equal_missing_scalar(x.r) || dbl_equal_missing_scalar(x.i);
}
static inline bool chr_equal_missing_scalar(SEXP x) {
  return x == NA_STRING;
}
static inline bool raw_equal_missing_scalar(Rbyte x) {
  return false;
}
static inline bool list_equal_missing_scalar(SEXP x) {
  return x == R_NilValue;
}

// -----------------------------------------------------------------------------

#define P_EQUAL_MISSING_SCALAR(CTYPE, EQUAL_MISSING_SCALAR) do { \
  return EQUAL_MISSING_SCALAR(((const CTYPE*) p_x)[i]);          \
} while (0)

static inline bool p_nil_equal_missing_scalar(const void* p_x, r_ssize i) {
  stop_internal("p_nil_equal_missing_scalar", "Can't check NULL for missingness.");
}
static inline bool p_lgl_equal_missing_scalar(const void* p_x, r_ssize i) {
  P_EQUAL_MISSING_SCALAR(int, lgl_equal_missing_scalar);
}
static inline bool p_int_equal_missing_scalar(const void* p_x, r_ssize i) {
  P_EQUAL_MISSING_SCALAR(int, int_equal_missing_scalar);
}
static inline bool p_dbl_equal_missing_scalar(const void* p_x, r_ssize i) {
  P_EQUAL_MISSING_SCALAR(double, dbl_equal_missing_scalar);
}
static inline bool p_cpl_equal_missing_scalar(const void* p_x, r_ssize i) {
  P_EQUAL_MISSING_SCALAR(Rcomplex, cpl_equal_missing_scalar);
}
static inline bool p_chr_equal_missing_scalar(const void* p_x, r_ssize i) {
  P_EQUAL_MISSING_SCALAR(SEXP, chr_equal_missing_scalar);
}
static inline bool p_raw_equal_missing_scalar(const void* p_x, r_ssize i) {
  P_EQUAL_MISSING_SCALAR(Rbyte, raw_equal_missing_scalar);
}
static inline bool p_list_equal_missing_scalar(const void* p_x, r_ssize i) {
  P_EQUAL_MISSING_SCALAR(SEXP, list_equal_missing_scalar);
}

#undef P_EQUAL_MISSING_SCALAR

static inline bool p_equal_missing_scalar(const void* p_x,
                                          r_ssize i,
                                          const enum vctrs_type type) {
  switch (type) {
  case vctrs_type_logical: return p_lgl_equal_missing_scalar(p_x, i);
  case vctrs_type_integer: return p_int_equal_missing_scalar(p_x, i);
  case vctrs_type_double: return p_dbl_equal_missing_scalar(p_x, i);
  case vctrs_type_complex: return p_cpl_equal_missing_scalar(p_x, i);
  case vctrs_type_character: return p_chr_equal_missing_scalar(p_x, i);
  case vctrs_type_raw: return p_raw_equal_missing_scalar(p_x, i);
  case vctrs_type_list: return p_list_equal_missing_scalar(p_x, i);
  default: stop_unimplemented_vctrs_type("p_equal_missing_scalar", type);
  }
}

// -----------------------------------------------------------------------------

static inline int lgl_equal_scalar_na_equal(int x, int y) {
  return x == y;
}
static inline int int_equal_scalar_na_equal(int x, int y) {
  return x == y;
}
static inline int dbl_equal_scalar_na_equal(double x, double y) {
  switch (dbl_classify(x)) {
  case vctrs_dbl_number: break;
  case vctrs_dbl_missing: return dbl_classify(y) == vctrs_dbl_missing;
  case vctrs_dbl_nan: return dbl_classify(y) == vctrs_dbl_nan;
  }

  return isnan(y) ? false : x == y;
}
static inline int cpl_equal_scalar_na_equal(Rcomplex x, Rcomplex y) {
  return dbl_equal_scalar_na_equal(x.r, y.r) && dbl_equal_scalar_na_equal(x.i, y.i);
}
static inline int chr_equal_scalar_na_equal(SEXP x, SEXP y) {
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
static inline int raw_equal_scalar_na_equal(Rbyte x, Rbyte y) {
  return x == y;
}
static inline int list_equal_scalar_na_equal(SEXP x, SEXP y) {
  return equal_object(x, y);
}

// -----------------------------------------------------------------------------

#define P_EQUAL_SCALAR_NA_EQUAL(CTYPE, EQUAL_SCALAR_NA_EQUAL) do {                \
  return EQUAL_SCALAR_NA_EQUAL(((const CTYPE*) p_x)[i], ((const CTYPE*) p_y)[j]); \
} while (0)

static inline int p_nil_equal_scalar_na_equal(const void* p_x, r_ssize i, const void* p_y, r_ssize j) {
  stop_internal("p_nil_equal_scalar_na_equal", "Can't compare NULL for equality.");
}
static inline int p_lgl_equal_scalar_na_equal(const void* p_x, r_ssize i, const void* p_y, r_ssize j) {
  P_EQUAL_SCALAR_NA_EQUAL(int, lgl_equal_scalar_na_equal);
}
static inline int p_int_equal_scalar_na_equal(const void* p_x, r_ssize i, const void* p_y, r_ssize j) {
  P_EQUAL_SCALAR_NA_EQUAL(int, int_equal_scalar_na_equal);
}
static inline int p_dbl_equal_scalar_na_equal(const void* p_x, r_ssize i, const void* p_y, r_ssize j) {
  P_EQUAL_SCALAR_NA_EQUAL(double, dbl_equal_scalar_na_equal);
}
static inline int p_cpl_equal_scalar_na_equal(const void* p_x, r_ssize i, const void* p_y, r_ssize j) {
  P_EQUAL_SCALAR_NA_EQUAL(Rcomplex, cpl_equal_scalar_na_equal);
}
static inline int p_chr_equal_scalar_na_equal(const void* p_x, r_ssize i, const void* p_y, r_ssize j) {
  P_EQUAL_SCALAR_NA_EQUAL(SEXP, chr_equal_scalar_na_equal);
}
static inline int p_raw_equal_scalar_na_equal(const void* p_x, r_ssize i, const void* p_y, r_ssize j) {
  P_EQUAL_SCALAR_NA_EQUAL(Rbyte, raw_equal_scalar_na_equal);
}
static inline int p_list_equal_scalar_na_equal(const void* p_x, r_ssize i, const void* p_y, r_ssize j) {
  P_EQUAL_SCALAR_NA_EQUAL(SEXP, list_equal_scalar_na_equal);
}

#undef P_EQUAL_SCALAR_NA_EQUAL

static inline bool p_equal_scalar_na_equal(const void* p_x,
                                           r_ssize i,
                                           const void* p_y,
                                           r_ssize j,
                                           const enum vctrs_type type) {
  switch (type) {
  case vctrs_type_logical: return p_lgl_equal_scalar_na_equal(p_x, i, p_y, j);
  case vctrs_type_integer: return p_int_equal_scalar_na_equal(p_x, i, p_y, j);
  case vctrs_type_double: return p_dbl_equal_scalar_na_equal(p_x, i, p_y, j);
  case vctrs_type_complex: return p_cpl_equal_scalar_na_equal(p_x, i, p_y, j);
  case vctrs_type_character: return p_chr_equal_scalar_na_equal(p_x, i, p_y, j);
  case vctrs_type_raw: return p_raw_equal_scalar_na_equal(p_x, i, p_y, j);
  case vctrs_type_list: return p_list_equal_scalar_na_equal(p_x, i, p_y, j);
  default: stop_unimplemented_vctrs_type("p_equal_scalar_na_equal", type);
  }
}

// -----------------------------------------------------------------------------

static inline int lgl_equal_scalar_na_propagate(int x, int y) {
  if (lgl_equal_missing_scalar(x) || lgl_equal_missing_scalar(y)) {
    return NA_LOGICAL;
  } else {
    return lgl_equal_scalar_na_equal(x, y);
  }
}
static inline int int_equal_scalar_na_propagate(int x, int y) {
  if (int_equal_missing_scalar(x) || int_equal_missing_scalar(y)) {
    return NA_LOGICAL;
  } else {
    return int_equal_scalar_na_equal(x, y);
  }
}
static inline int dbl_equal_scalar_na_propagate(double x, double y) {
  if (dbl_equal_missing_scalar(x) || dbl_equal_missing_scalar(y)) {
    return NA_LOGICAL;
  } else {
    // Faster than `dbl_equal_scalar_na_equal()`,
    // which has unneeded missing value checks
    return x == y;
  }
}
static inline int cpl_equal_scalar_na_propagate(Rcomplex x, Rcomplex y) {
  int real_equal = dbl_equal_scalar_na_propagate(x.r, y.r);
  int imag_equal = dbl_equal_scalar_na_propagate(x.i, y.i);

  if (real_equal == NA_LOGICAL || imag_equal == NA_LOGICAL) {
    return NA_LOGICAL;
  } else {
    return real_equal && imag_equal;
  }
}
static inline int chr_equal_scalar_na_propagate(SEXP x, SEXP y) {
  if (chr_equal_missing_scalar(x) || chr_equal_missing_scalar(y)) {
    return NA_LOGICAL;
  } else {
    return chr_equal_scalar_na_equal(x, y);
  }
}
static inline int raw_equal_scalar_na_propagate(Rbyte x, Rbyte y) {
  return raw_equal_scalar_na_equal(x, y);
}
static inline int list_equal_scalar_na_propagate(SEXP x, SEXP y) {
  if (list_equal_missing_scalar(x) || list_equal_missing_scalar(y)) {
    return NA_LOGICAL;
  } else {
    return list_equal_scalar_na_equal(x, y);
  }
}

// -----------------------------------------------------------------------------

#define P_EQUAL_SCALAR_NA_PROPAGATE(CTYPE, EQUAL_SCALAR_NA_PROPAGATE) do {            \
  return EQUAL_SCALAR_NA_PROPAGATE(((const CTYPE*) p_x)[i], ((const CTYPE*) p_y)[j]); \
} while (0)

static inline int p_nil_equal_scalar_na_propagate(const void* p_x, r_ssize i, const void* p_y, r_ssize j) {
  stop_internal("p_nil_equal_scalar_na_propagate", "Can't compare NULL for equality.");
}
static inline int p_lgl_equal_scalar_na_propagate(const void* p_x, r_ssize i, const void* p_y, r_ssize j) {
  P_EQUAL_SCALAR_NA_PROPAGATE(int, lgl_equal_scalar_na_propagate);
}
static inline int p_int_equal_scalar_na_propagate(const void* p_x, r_ssize i, const void* p_y, r_ssize j) {
  P_EQUAL_SCALAR_NA_PROPAGATE(int, int_equal_scalar_na_propagate);
}
static inline int p_dbl_equal_scalar_na_propagate(const void* p_x, r_ssize i, const void* p_y, r_ssize j) {
  P_EQUAL_SCALAR_NA_PROPAGATE(double, dbl_equal_scalar_na_propagate);
}
static inline int p_cpl_equal_scalar_na_propagate(const void* p_x, r_ssize i, const void* p_y, r_ssize j) {
  P_EQUAL_SCALAR_NA_PROPAGATE(Rcomplex, cpl_equal_scalar_na_propagate);
}
static inline int p_chr_equal_scalar_na_propagate(const void* p_x, r_ssize i, const void* p_y, r_ssize j) {
  P_EQUAL_SCALAR_NA_PROPAGATE(SEXP, chr_equal_scalar_na_propagate);
}
static inline int p_raw_equal_scalar_na_propagate(const void* p_x, r_ssize i, const void* p_y, r_ssize j) {
  P_EQUAL_SCALAR_NA_PROPAGATE(Rbyte, raw_equal_scalar_na_propagate);
}
static inline int p_list_equal_scalar_na_propagate(const void* p_x, r_ssize i, const void* p_y, r_ssize j) {
  P_EQUAL_SCALAR_NA_PROPAGATE(SEXP, list_equal_scalar_na_propagate);
}

#undef P_EQUAL_SCALAR_NA_PROPAGATE

static inline bool p_equal_scalar_na_propagate(const void* p_x,
                                               r_ssize i,
                                               const void* p_y,
                                               r_ssize j,
                                               const enum vctrs_type type) {
  switch (type) {
  case vctrs_type_logical: return p_lgl_equal_scalar_na_propagate(p_x, i, p_y, j);
  case vctrs_type_integer: return p_int_equal_scalar_na_propagate(p_x, i, p_y, j);
  case vctrs_type_double: return p_dbl_equal_scalar_na_propagate(p_x, i, p_y, j);
  case vctrs_type_complex: return p_cpl_equal_scalar_na_propagate(p_x, i, p_y, j);
  case vctrs_type_character: return p_chr_equal_scalar_na_propagate(p_x, i, p_y, j);
  case vctrs_type_raw: return p_raw_equal_scalar_na_propagate(p_x, i, p_y, j);
  case vctrs_type_list: return p_list_equal_scalar_na_propagate(p_x, i, p_y, j);
  default: stop_unimplemented_vctrs_type("p_equal_scalar_na_propagate", type);
  }
}

// -----------------------------------------------------------------------------

#define EQUAL_SCALAR(EQUAL_SCALAR_NA_EQUAL, EQUAL_SCALAR_NA_PROPAGATE) do { \
  if (na_equal) {                                                           \
    return EQUAL_SCALAR_NA_EQUAL(x, y);                                     \
  } else {                                                                  \
    return EQUAL_SCALAR_NA_PROPAGATE(x, y);                                 \
  }                                                                         \
} while (0)

static inline int lgl_equal_scalar(int x, int y, bool na_equal) {
  EQUAL_SCALAR(lgl_equal_scalar_na_equal, lgl_equal_scalar_na_propagate);
}
static inline int int_equal_scalar(int x, int y, bool na_equal) {
  EQUAL_SCALAR(int_equal_scalar_na_equal, int_equal_scalar_na_propagate);
}
static inline int dbl_equal_scalar(double x, double y, bool na_equal) {
  EQUAL_SCALAR(dbl_equal_scalar_na_equal, dbl_equal_scalar_na_propagate);
}
static inline int cpl_equal_scalar(Rcomplex x, Rcomplex y, bool na_equal) {
  EQUAL_SCALAR(cpl_equal_scalar_na_equal, cpl_equal_scalar_na_propagate);
}
static inline int chr_equal_scalar(SEXP x, SEXP y, bool na_equal) {
  EQUAL_SCALAR(chr_equal_scalar_na_equal, chr_equal_scalar_na_propagate);
}
static inline int raw_equal_scalar(Rbyte x, Rbyte y, bool na_equal) {
  EQUAL_SCALAR(raw_equal_scalar_na_equal, raw_equal_scalar_na_propagate);
}
static inline int list_equal_scalar(SEXP x, SEXP y, bool na_equal) {
  EQUAL_SCALAR(list_equal_scalar_na_equal, list_equal_scalar_na_propagate);
}

#undef EQUAL_SCALAR

// -----------------------------------------------------------------------------

#endif
