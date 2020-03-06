#include <math.h>
#include "equal.h"
#include "vctrs.h"
#include "utils.h"


// If `x` is a data frame, it must have been recursively proxied
// beforehand
//
// [[ include("vctrs.h") ]]
int equal_scalar(SEXP x, R_len_t i, SEXP y, R_len_t j, bool na_equal) {
  enum vctrs_type proxy_type = vec_proxy_typeof(x);
  const void* x_p = NULL;
  const void* y_p = NULL;

  switch (proxy_type) {
  case vctrs_type_dataframe: {
    int n_col = Rf_length(x);
    if (n_col != Rf_length(y)) {
      Rf_errorcall(R_NilValue, "`x` and `y` must have the same number of columns");
    }
    return df_equal_scalar(x, i, y, j, na_equal, n_col);
  }

  case vctrs_type_logical:   x_p = LOGICAL_RO(x);    y_p = LOGICAL_RO(y);    break;
  case vctrs_type_integer:   x_p = INTEGER_RO(x);    y_p = INTEGER_RO(y);    break;
  case vctrs_type_double:    x_p = REAL_RO(x);       y_p = REAL_RO(y);       break;
  case vctrs_type_complex:   x_p = COMPLEX_RO(x);    y_p = COMPLEX_RO(y);    break;
  case vctrs_type_character: x_p = STRING_PTR_RO(x); y_p = STRING_PTR_RO(y); break;
  case vctrs_type_raw:       x_p = RAW_RO(x);        y_p = RAW_RO(y);        break;
  case vctrs_type_list:      x_p = x;                y_p = y;                break;

  default: vctrs_stop_unsupported_type(vec_typeof(x), "equal_scalar()");
  }

  if (na_equal) {
    return equal_scalar_na_equal_p(proxy_type, x, x_p, i, y, y_p, j);
  } else {
    return equal_scalar_na_propagate_p(proxy_type, x, x_p, i, y, y_p, j);
  }
}

// [[ register() ]]
SEXP vctrs_equal_scalar(SEXP x, SEXP i_, SEXP y, SEXP j_, SEXP na_equal_) {
  R_len_t i = Rf_asInteger(i_);
  R_len_t j = Rf_asInteger(j_);
  bool na_equal = r_bool_as_int(na_equal_);
  return r_lgl(equal_scalar(x, i, y, j, na_equal));
}

// [[ include("vctrs.h") ]]
int equal_scalar_na_equal_p(enum vctrs_type proxy_type,
                            SEXP x, const void* x_p, R_len_t i,
                            SEXP y, const void* y_p, R_len_t j) {
  switch (proxy_type) {
  case vctrs_type_logical: return lgl_equal_scalar_na_equal(((const int*) x_p) + i, ((const int*) y_p) + j);
  case vctrs_type_integer: return int_equal_scalar_na_equal(((const int*) x_p) + i, ((const int*) y_p) + j);
  case vctrs_type_double: return dbl_equal_scalar_na_equal(((const double*) x_p) + i, ((const double*) y_p) + j);
  case vctrs_type_complex: return cpl_equal_scalar_na_equal(((const Rcomplex*) x_p) + i, ((const Rcomplex*) y_p) + j);
  case vctrs_type_character: return chr_equal_scalar_na_equal(((const SEXP*) x_p) + i, ((const SEXP*) y_p) + j);
  case vctrs_type_raw: return raw_equal_scalar_na_equal(((const Rbyte*) x_p) + i, ((const Rbyte*) y_p) + j);
  case vctrs_type_list: return list_equal_scalar_na_equal(((const SEXP) x_p), i, ((const SEXP) y_p), j);
  default: vctrs_stop_unsupported_type(vec_typeof(x), "equal_scalar_na_equal_p()");
  }
}
// [[ include("vctrs.h") ]]
int equal_scalar_na_propagate_p(enum vctrs_type proxy_type,
                                SEXP x, const void* x_p, R_len_t i,
                                SEXP y, const void* y_p, R_len_t j) {
  switch (proxy_type) {
  case vctrs_type_logical: return lgl_equal_scalar_na_propagate(((const int*) x_p) + i, ((const int*) y_p) + j);
  case vctrs_type_integer: return int_equal_scalar_na_propagate(((const int*) x_p) + i, ((const int*) y_p) + j);
  case vctrs_type_double: return dbl_equal_scalar_na_propagate(((const double*) x_p) + i, ((const double*) y_p) + j);
  case vctrs_type_complex: return cpl_equal_scalar_na_propagate(((const Rcomplex*) x_p) + i, ((const Rcomplex*) y_p) + j);
  case vctrs_type_character: return chr_equal_scalar_na_propagate(((const SEXP*) x_p) + i, ((const SEXP*) y_p) + j);
  case vctrs_type_raw: return raw_equal_scalar_na_propagate(((const Rbyte*) x_p) + i, ((const Rbyte*) y_p) + j);
  case vctrs_type_list: return list_equal_scalar_na_propagate(((const SEXP) x_p), i, ((const SEXP) y_p), j);
  default: vctrs_stop_unsupported_type(vec_typeof(x), "equal_scalar_na_propagate_p()");
  }
}

// -----------------------------------------------------------------------------

static SEXP df_equal(SEXP x, SEXP y, bool na_equal, R_len_t n_row);

#define EQUAL(CTYPE, CONST_DEREF, SCALAR_EQUAL)         \
  do {                                                  \
    SEXP out = PROTECT(Rf_allocVector(LGLSXP, size));   \
    int* p_out = LOGICAL(out);                          \
                                                        \
    const CTYPE* p_x = CONST_DEREF(x);                  \
    const CTYPE* p_y = CONST_DEREF(y);                  \
                                                        \
    for (R_len_t i = 0; i < size; ++i, ++p_x, ++p_y) {  \
      p_out[i] = SCALAR_EQUAL(p_x, p_y, na_equal);      \
    }                                                   \
                                                        \
    UNPROTECT(3);                                       \
    return out;                                         \
  }                                                     \
  while (0)

#define EQUAL_BARRIER(SCALAR_EQUAL)                     \
  do {                                                  \
    SEXP out = PROTECT(Rf_allocVector(LGLSXP, size));   \
    int* p_out = LOGICAL(out);                          \
                                                        \
    for (R_len_t i = 0; i < size; ++i) {                \
      p_out[i] = SCALAR_EQUAL(x, i, y, i, na_equal);    \
    }                                                   \
                                                        \
    UNPROTECT(3);                                       \
    return out;                                         \
  }                                                     \
  while (0)

// [[ register() ]]
SEXP vctrs_equal(SEXP x, SEXP y, SEXP na_equal_) {
  x = PROTECT(vec_proxy_equal(x));
  y = PROTECT(vec_proxy_equal(y));

  R_len_t size = vec_size(x);

  enum vctrs_type type = vec_proxy_typeof(x);
  if (type != vec_proxy_typeof(y) || size != vec_size(y)) {
    Rf_errorcall(R_NilValue, "`x` and `y` must have same types and lengths");
  }

  bool na_equal = r_bool_as_int(na_equal_);

  switch (type) {
  case vctrs_type_logical:   EQUAL(int, LOGICAL_RO, lgl_equal_scalar);
  case vctrs_type_integer:   EQUAL(int, INTEGER_RO, int_equal_scalar);
  case vctrs_type_double:    EQUAL(double, REAL_RO, dbl_equal_scalar);
  case vctrs_type_raw:       EQUAL(Rbyte, RAW_RO, raw_equal_scalar);
  case vctrs_type_complex:   EQUAL(Rcomplex, COMPLEX_RO, cpl_equal_scalar);
  case vctrs_type_character: EQUAL(SEXP, STRING_PTR_RO, chr_equal_scalar);
  case vctrs_type_list:      EQUAL_BARRIER(list_equal_scalar);
  case vctrs_type_dataframe: {
    SEXP out = PROTECT(df_equal(x, y, na_equal, size));
    UNPROTECT(3);
    return out;
  }
  case vctrs_type_scalar:    Rf_errorcall(R_NilValue, "Can't compare scalars with `vctrs_equal()`");
  default:                   Rf_error("Unimplemented type in `vctrs_equal()`");
  }
}

#undef EQUAL
#undef EQUAL_BARRIER

// -----------------------------------------------------------------------------

// Missingness is never propagated through objects,
// so `na_equal` is always `true` in these macros

#define EQUAL_ALL(CTYPE, CONST_DEREF, SCALAR_EQUAL)       \
  do {                                                    \
    const CTYPE* p_x = CONST_DEREF(x);                    \
    const CTYPE* p_y = CONST_DEREF(y);                    \
                                                          \
    for (R_len_t i = 0; i < n; ++i, ++p_x, ++p_y) {       \
      if (!SCALAR_EQUAL(p_x, p_y, true)) {                \
        return false;                                     \
      }                                                   \
    }                                                     \
    return true;                                          \
  }                                                       \
  while (0)

#define EQUAL_ALL_BARRIER(SCALAR_EQUAL)                   \
  do {                                                    \
    for (R_len_t i = 0; i < n; ++i) {                     \
      if (!SCALAR_EQUAL(x, i, y, i, true)) {              \
        return false;                                     \
      }                                                   \
    }                                                     \
    return true;                                          \
  }                                                       \
  while (0)

static inline bool vec_equal_attrib(SEXP x, SEXP y);

// [[ include("vctrs.h") ]]
bool equal_object(SEXP x, SEXP y) {
  SEXPTYPE type = TYPEOF(x);

  if (type != TYPEOF(y)) {
    return false;
  }

  // Pointer comparison is all that is required for these types
  switch (type) {
  case NILSXP:
  case SYMSXP:
  case SPECIALSXP:
  case BUILTINSXP:
  case CHARSXP:
  case ENVSXP:
  case EXTPTRSXP:
    return x == y;
  }

  // For other types, try a pointer comparison first before
  // performing an in depth equality check
  if (x == y) {
    return true;
  }

  switch (type) {
  // Handled below
  case LGLSXP:
  case INTSXP:
  case REALSXP:
  case STRSXP:
  case RAWSXP:
  case CPLXSXP:
  case EXPRSXP:
  case VECSXP: break;

  case DOTSXP:
  case LANGSXP:
  case LISTSXP:
  case BCODESXP: {
    if (!equal_object(ATTRIB(x), ATTRIB(y))) {
      return false;
    }

    if (!equal_object(CAR(x), CAR(y))) {
      return false;
    }

    x = CDR(x);
    y = CDR(y);

    if (!equal_object(x, y)) {
      return false;
    }

    return true;
  }

  case CLOSXP:
    if (!equal_object(ATTRIB(x), ATTRIB(y))) {
      return false;
    }
    if (!equal_object(BODY(x), BODY(y))) {
      return false;
    }
    if (!equal_object(CLOENV(x), CLOENV(y))) {
      return false;
    }
    if (!equal_object(FORMALS(x), FORMALS(y))) {
      return false;
    }
    return true;

  case NILSXP:
  case SYMSXP:
  case SPECIALSXP:
  case BUILTINSXP:
  case CHARSXP:
  case ENVSXP:
  case EXTPTRSXP:
    // These are handled above with pointer comparison
    Rf_error("Internal error: Unexpected reference type in `vec_equal()`");

  default:
    Rf_errorcall(R_NilValue, "Unsupported type %s", Rf_type2char(TYPEOF(x)));
  }

  R_len_t n = Rf_length(x);
  if (n != Rf_length(y)) {
    return false;
  }

  if (!vec_equal_attrib(x, y)) {
    return false;
  }

  switch (type) {
  case LGLSXP:  EQUAL_ALL(int, LOGICAL_RO, lgl_equal_scalar);
  case INTSXP:  EQUAL_ALL(int, INTEGER_RO, int_equal_scalar);
  case REALSXP: EQUAL_ALL(double, REAL_RO, dbl_equal_scalar);
  case STRSXP:  EQUAL_ALL(SEXP, STRING_PTR_RO, chr_equal_scalar);
  case RAWSXP:  EQUAL_ALL(Rbyte, RAW_RO, raw_equal_scalar);
  case CPLXSXP: EQUAL_ALL(Rcomplex, COMPLEX_RO, cpl_equal_scalar);
  case EXPRSXP:
  case VECSXP:  EQUAL_ALL_BARRIER(list_equal_scalar);
  default:      Rf_errorcall(R_NilValue, "Internal error: Unexpected type in `equal_object()`");
  }
}

#undef EQUAL_ALL
#undef EQUAL_ALL_BARRIER

// [[ register() ]]
SEXP vctrs_equal_object(SEXP x, SEXP y) {
  return Rf_ScalarLogical(equal_object(x, y));
}

// TODO: Sort attributes by tag before comparison

static inline bool vec_equal_attrib(SEXP x, SEXP y) {
  SEXP x_attrs = ATTRIB(x);
  SEXP y_attrs = ATTRIB(y);

  while (x_attrs != R_NilValue) {
    if (y_attrs == R_NilValue) {
      return false;
    }

    SEXP x_tag = TAG(x_attrs);
    SEXP y_tag = TAG(x_attrs);

    if (x_tag != y_tag) {
      return false;
    }

    if (!equal_object(CAR(x_attrs), CAR(y_attrs))) {
      return false;
    }

    x_attrs = CDR(x_attrs);
    y_attrs = CDR(y_attrs);
  }

  return true;
}


// [[ include("vctrs.h") ]]
bool equal_names(SEXP x, SEXP y) {
  SEXP x_names = PROTECT(Rf_getAttrib(x, R_NamesSymbol));
  SEXP y_names = PROTECT(Rf_getAttrib(y, R_NamesSymbol));

  bool out = equal_object(x_names, y_names);

  UNPROTECT(2);
  return out;
}

// -----------------------------------------------------------------------------

static struct vctrs_df_rowwise_info vec_equal_col(SEXP x,
                                                  SEXP y,
                                                  bool na_equal,
                                                  struct vctrs_df_rowwise_info info,
                                                  R_len_t n_row);

static struct vctrs_df_rowwise_info df_equal_impl(SEXP x,
                                                  SEXP y,
                                                  bool na_equal,
                                                  struct vctrs_df_rowwise_info info,
                                                  R_len_t n_row);

static struct vctrs_df_rowwise_info init_rowwise_equal_info(R_len_t n_row) {
  struct vctrs_df_rowwise_info info;

  // Initialize to "equality" value
  // and only change if we learn that it differs
  info.out = PROTECT(Rf_allocVector(LGLSXP, n_row));
  int* p_out = LOGICAL(info.out);

  for (R_len_t i = 0; i < n_row; ++i) {
    p_out[i] = 1;
  }

  // To begin with, no rows have a known comparison value
  info.row_known = PROTECT(Rf_allocVector(RAWSXP, n_row * sizeof(bool)));
  info.p_row_known = (bool*) RAW(info.row_known);
  memset(info.p_row_known, false, n_row * sizeof(bool));

  info.remaining = n_row;

  UNPROTECT(2);
  return info;
}

static SEXP df_equal(SEXP x, SEXP y, bool na_equal, R_len_t n_row) {
  int nprot = 0;

  struct vctrs_df_rowwise_info info = init_rowwise_equal_info(n_row);
  PROTECT_DF_ROWWISE_INFO(&info, &nprot);

  info = df_equal_impl(x, y, na_equal, info, n_row);

  UNPROTECT(nprot);
  return info.out;
}

static struct vctrs_df_rowwise_info df_equal_impl(SEXP x,
                                                  SEXP y,
                                                  bool na_equal,
                                                  struct vctrs_df_rowwise_info info,
                                                  R_len_t n_row) {
  int n_col = Rf_length(x);

  if (n_col != Rf_length(y)) {
    Rf_errorcall(R_NilValue, "`x` and `y` must have the same number of columns");
  }

  for (R_len_t i = 0; i < n_col; ++i) {
    SEXP x_col = VECTOR_ELT(x, i);
    SEXP y_col = VECTOR_ELT(y, i);

    info = vec_equal_col(x_col, y_col, na_equal, info, n_row);

    // If we know all comparison values, break
    if (info.remaining == 0) {
      break;
    }
  }

  return info;
}

// -----------------------------------------------------------------------------

#define EQUAL_COL(CTYPE, CONST_DEREF, SCALAR_EQUAL)                  \
do {                                                                 \
  int* p_out = LOGICAL(info.out);                                    \
                                                                     \
  const CTYPE* p_x = CONST_DEREF(x);                                 \
  const CTYPE* p_y = CONST_DEREF(y);                                 \
                                                                     \
  for (R_len_t i = 0; i < n_row; ++i, ++p_x, ++p_y) {                \
    if (info.p_row_known[i]) {                                       \
      continue;                                                      \
    }                                                                \
                                                                     \
    int eq = SCALAR_EQUAL(p_x, p_y, na_equal);                       \
                                                                     \
    if (eq <= 0) {                                                   \
      p_out[i] = eq;                                                 \
      info.p_row_known[i] = true;                                    \
      --info.remaining;                                              \
                                                                     \
      if (info.remaining == 0) {                                     \
        break;                                                       \
      }                                                              \
    }                                                                \
  }                                                                  \
                                                                     \
  return info;                                                       \
}                                                                    \
while (0)

#define EQUAL_COL_BARRIER(SCALAR_EQUAL)                \
do {                                                   \
  int* p_out = LOGICAL(info.out);                      \
                                                       \
  for (R_len_t i = 0; i < n_row; ++i) {                \
    if (info.p_row_known[i]) {                         \
      continue;                                        \
    }                                                  \
                                                       \
    int eq = SCALAR_EQUAL(x, i, y, i, na_equal);       \
                                                       \
    if (eq <= 0) {                                     \
      p_out[i] = eq;                                   \
      info.p_row_known[i] = true;                      \
      --info.remaining;                                \
                                                       \
      if (info.remaining == 0) {                       \
        break;                                         \
      }                                                \
    }                                                  \
  }                                                    \
                                                       \
  return info;                                         \
}                                                      \
while (0)

static struct vctrs_df_rowwise_info vec_equal_col(SEXP x,
                                                  SEXP y,
                                                  bool na_equal,
                                                  struct vctrs_df_rowwise_info info,
                                                  R_len_t n_row) {
  switch (vec_proxy_typeof(x)) {
  case vctrs_type_logical:   EQUAL_COL(int, LOGICAL_RO, lgl_equal_scalar);
  case vctrs_type_integer:   EQUAL_COL(int, INTEGER_RO, int_equal_scalar);
  case vctrs_type_double:    EQUAL_COL(double, REAL_RO, dbl_equal_scalar);
  case vctrs_type_raw:       EQUAL_COL(Rbyte, RAW_RO, raw_equal_scalar);
  case vctrs_type_complex:   EQUAL_COL(Rcomplex, COMPLEX_RO, cpl_equal_scalar);
  case vctrs_type_character: EQUAL_COL(SEXP, STRING_PTR_RO, chr_equal_scalar);
  case vctrs_type_list:      EQUAL_COL_BARRIER(list_equal_scalar);
  case vctrs_type_dataframe: return df_equal_impl(x, y, na_equal, info, n_row);
  case vctrs_type_scalar:    Rf_errorcall(R_NilValue, "Can't compare scalars with `vctrs_equal()`");
  default:                   Rf_error("Unimplemented type in `vctrs_equal()`");
  }
}

#undef EQUAL_COL
#undef EQUAL_COL_BARRIER

// -----------------------------------------------------------------------------

static int vec_all_equal_impl(SEXP x, SEXP y, bool na_equal, R_len_t size);

SEXP vec_all_equal(SEXP x, SEXP y, bool na_equal, SEXP ptype) {
  SEXP args = PROTECT(Rf_allocVector(VECSXP, 2));

  SET_VECTOR_ELT(args, 0, x);
  SET_VECTOR_ELT(args, 1, y);

  R_len_t size = vec_size_common(args, 0);

  args = PROTECT(vec_recycle_common(args, size));
  args = PROTECT(vec_cast_common(args, ptype));

  x = VECTOR_ELT(args, 0);
  y = VECTOR_ELT(args, 1);

  x = PROTECT(vec_proxy_equal(x));
  y = PROTECT(vec_proxy_equal(y));

  // Catch when one of the inputs is `NULL`
  if (vec_proxy_typeof(x) != vec_proxy_typeof(y)) {
    Rf_errorcall(R_NilValue, "`x` and `y` must have the same type");
  }

  int out = vec_all_equal_impl(x, y, na_equal, size);

  UNPROTECT(5);
  return Rf_ScalarLogical(out);
}

// [[ export() ]]
SEXP vctrs_all_equal(SEXP x, SEXP y, SEXP na_equal, SEXP ptype) {
  bool na_equal_ = Rf_asLogical(na_equal);
  return vec_all_equal(x, y, na_equal_, ptype);
}

#define ALL_EQUAL(CTYPE, CONST_DEREF, SCALAR_EQUAL)   \
do {                                                  \
  const CTYPE* p_x = CONST_DEREF(x);                  \
  const CTYPE* p_y = CONST_DEREF(y);                  \
                                                      \
  for (R_len_t i = 0; i < size; ++i, ++p_x, ++p_y) {  \
    int eq = SCALAR_EQUAL(p_x, p_y, na_equal);        \
                                                      \
    if (eq <= 0) {                                    \
      out = eq;                                       \
      break;                                          \
    }                                                 \
  }                                                   \
}                                                     \
while (0)

#define ALL_EQUAL_BARRIER(SCALAR_EQUAL)          \
do {                                             \
  for (R_len_t i = 0; i < size; ++i) {           \
    int eq = SCALAR_EQUAL(x, i, y, i, na_equal); \
                                                 \
    if (eq <= 0) {                               \
      out = eq;                                  \
      break;                                     \
    }                                            \
  }                                              \
}                                                \
while (0)

static int df_all_equal(SEXP x, SEXP y, bool na_equal, R_len_t size);

static int vec_all_equal_impl(SEXP x, SEXP y, bool na_equal, R_len_t size) {
  int out = 1;

  switch (vec_proxy_typeof(x)) {
  case vctrs_type_logical:   ALL_EQUAL(int, LOGICAL_RO, lgl_equal_scalar); break;
  case vctrs_type_integer:   ALL_EQUAL(int, INTEGER_RO, int_equal_scalar); break;
  case vctrs_type_double:    ALL_EQUAL(double, REAL_RO, dbl_equal_scalar); break;
  case vctrs_type_raw:       ALL_EQUAL(Rbyte, RAW_RO, raw_equal_scalar); break;
  case vctrs_type_complex:   ALL_EQUAL(Rcomplex, COMPLEX_RO, cpl_equal_scalar); break;
  case vctrs_type_character: ALL_EQUAL(SEXP, STRING_PTR_RO, chr_equal_scalar); break;
  case vctrs_type_list:      ALL_EQUAL_BARRIER(list_equal_scalar); break;
  case vctrs_type_dataframe: out = df_all_equal(x, y, na_equal, size); break;
  case vctrs_type_scalar:    Rf_errorcall(R_NilValue, "Can't detect equality in scalars with `vec_all_equal()`");
  default:                   Rf_error("Unimplemented type in `vec_all_equal()`");
  }

  return out;
}

#undef ALL_EQUAL
#undef ALL_EQUAL_BARRIER

static int df_all_equal(SEXP x, SEXP y, bool na_equal, R_len_t size) {
  int out = 1;

  int n_col = Rf_length(x);

  if (n_col != Rf_length(y)) {
    Rf_errorcall(R_NilValue, "`x` and `y` must have the same number of columns");
  }

  for (R_len_t i = 0; i < n_col; ++i) {
    SEXP x_col = VECTOR_ELT(x, i);
    SEXP y_col = VECTOR_ELT(y, i);

    int eq = vec_all_equal_impl(x_col, y_col, na_equal, size);

    if (eq <= 0) {
      out = eq;
      break;
    }
  }

  return out;
}

// -----------------------------------------------------------------------------

static int lgl_equal_na_scalar(const int* x);
static int int_equal_na_scalar(const int* x);
static int dbl_equal_na_scalar(const double* x);
static int cpl_equal_na_scalar(const Rcomplex* x);
static int chr_equal_na_scalar(const SEXP* x);
static int list_equal_na_scalar(SEXP x, R_len_t i);
static int df_equal_na_scalar(SEXP x, R_len_t i);

// If `x` is a data frame, it must have been recursively proxied
// beforehand so we can safely use `TYPEOF(x)`
int equal_na(SEXP x, R_len_t i) {
  switch (TYPEOF(x)) {
  case LGLSXP: return lgl_equal_na_scalar(LOGICAL(x) + i);
  case INTSXP: return int_equal_na_scalar(INTEGER(x) + i);
  case REALSXP: return dbl_equal_na_scalar(REAL(x) + i);
  case CPLXSXP: return cpl_equal_na_scalar(COMPLEX(x) + i);
  case STRSXP: return chr_equal_na_scalar(STRING_PTR(x) + i);
  default: break;
  }

  switch (vec_proxy_typeof(x)) {
  case vctrs_type_list: return list_equal_na_scalar(x, i);
  case vctrs_type_dataframe: return df_equal_na_scalar(x, i);
  default: break;
  }

  vctrs_stop_unsupported_type(vec_typeof(x), "equal_na()");
}

#define EQUAL_NA(CTYPE, CONST_DEREF, SCALAR_EQUAL_NA)     \
  do {                                                    \
    const CTYPE* xp = CONST_DEREF(x);                     \
                                                          \
    for (R_len_t i = 0; i < n; ++i, ++xp) {               \
      p[i] = SCALAR_EQUAL_NA(xp);                         \
    }                                                     \
  }                                                       \
  while (0)

#define EQUAL_NA_BARRIER(SCALAR_EQUAL_NA)                 \
  do {                                                    \
    for (R_len_t i = 0; i < n; ++i) {                     \
      p[i] = SCALAR_EQUAL_NA(x, i);                       \
    }                                                     \
  }                                                       \
  while (0)

// [[ register() ]]
SEXP vctrs_equal_na(SEXP x) {
  R_len_t n = vec_size(x);
  SEXP out = PROTECT(Rf_allocVector(LGLSXP, n));
  int32_t* p = LOGICAL(out);

  x = PROTECT(vec_proxy_equal(x));

  enum vctrs_type type = vec_proxy_typeof(x);

  switch (type) {
  case vctrs_type_logical:   EQUAL_NA(int, LOGICAL_RO, lgl_equal_na_scalar); break;
  case vctrs_type_integer:   EQUAL_NA(int, INTEGER_RO, int_equal_na_scalar); break;
  case vctrs_type_double:    EQUAL_NA(double, REAL_RO, dbl_equal_na_scalar); break;
  case vctrs_type_complex:   EQUAL_NA(Rcomplex, COMPLEX_RO, cpl_equal_na_scalar); break;
  case vctrs_type_character: EQUAL_NA(SEXP, STRING_PTR_RO, chr_equal_na_scalar); break;
  case vctrs_type_list:      EQUAL_NA_BARRIER(list_equal_na_scalar); break;
  case vctrs_type_dataframe: EQUAL_NA_BARRIER(df_equal_na_scalar); break;
  case vctrs_type_scalar:    Rf_errorcall(R_NilValue, "Can't detect `NA` values in scalars with `vctrs_equal_na()`.");
  default:                   Rf_error("Unimplemented type in `vctrs_equal_na()`.");
  }

  UNPROTECT(2);
  return out;
}

#undef EQUAL_NA
#undef EQUAL_NA_BARRIER

static int lgl_equal_na_scalar(const int* x) {
  return *x == NA_LOGICAL;
}

static int int_equal_na_scalar(const int* x) {
  return *x == NA_INTEGER;
}

static int dbl_equal_na_scalar(const double* x) {
  // is.na(NaN) is TRUE
  // isnan() does not consistently return 1 and 0 on all platforms,
  // but R's ISNAN() does
  return ISNAN(*x);
}

static int cpl_equal_na_scalar(const Rcomplex* x) {
  return ISNAN(x->r) || ISNAN(x->i);
}

static int chr_equal_na_scalar(const SEXP* x) {
  return *x == NA_STRING;
}

static int list_equal_na_scalar(SEXP x, R_len_t i) {
  return Rf_isNull(VECTOR_ELT(x, i));
}

static int df_equal_na_scalar(SEXP x, R_len_t i) {
  int n_col = Rf_length(x);

  for (int k = 0; k < n_col; ++k) {
    if (!equal_na(VECTOR_ELT(x, k), i)) {
      return false;
    }
  }

  return true;
}
