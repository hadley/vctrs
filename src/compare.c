#include "vctrs.h"
#include "utils.h"
#include <strings.h>

static void stop_not_comparable(SEXP x, SEXP y, const char* message) {
  Rf_errorcall(R_NilValue, "`x` and `y` are not comparable: %s", message);
}

// https://stackoverflow.com/questions/10996418
static int icmp(int x, int y) {
  return (x > y) - (x < y);
}

static int dcmp(double x, double y) {
  return (x > y) - (x < y);
}

// UTF-8 translation is successful in these cases:
// - (utf8 + latin1), (unknown + utf8), (unknown + latin1)
// UTF-8 translation fails purposefully in these cases:
// - (bytes + utf8), (bytes + latin1), (bytes + unknown)
// UTF-8 translation is not attempted in these cases:
// - (utf8 + utf8), (latin1 + latin1), (unknown + unknown), (bytes + bytes)

static int scmp(SEXP x, SEXP y) {
  if (x == y) {
    return 0;
  }

  // Same encoding
  if (Rf_getCharCE(x) == Rf_getCharCE(y)) {
    int cmp = strcmp(CHAR(x), CHAR(y));
    return cmp / abs(cmp);
  }

  const void *vmax = vmaxget();
  int cmp = strcmp(Rf_translateCharUTF8(x), Rf_translateCharUTF8(y));
  vmaxset(vmax);

  if (cmp == 0) {
    return cmp;
  } else {
    return cmp / abs(cmp);
  }
}

// -----------------------------------------------------------------------------

static inline int lgl_compare_scalar(const int* x, const int* y, bool na_equal) {
  int xi = *x;
  int yj = *y;

  if (na_equal) {
    return icmp(xi, yj);
  } else {
    return (xi == NA_LOGICAL || yj == NA_LOGICAL) ? NA_INTEGER : icmp(xi, yj);
  }
}

static inline int int_compare_scalar(const int* x, const int* y, bool na_equal) {
  int xi = *x;
  int yj = *y;

  if (na_equal) {
    return icmp(xi, yj);
  } else {
    return (xi == NA_INTEGER || yj == NA_INTEGER) ? NA_INTEGER : icmp(xi, yj);
  }
}

static inline int dbl_compare_scalar(const double* x, const double* y, bool na_equal) {
  double xi = *x;
  double yj = *y;

  if (na_equal) {
    enum vctrs_dbl_class x_class = dbl_classify(xi);
    enum vctrs_dbl_class y_class = dbl_classify(yj);

    switch (x_class) {
    case vctrs_dbl_number: {
      switch (y_class) {
      case vctrs_dbl_number: return dcmp(xi, yj);
      case vctrs_dbl_missing: return 1;
      case vctrs_dbl_nan: return 1;
      }
    }
    case vctrs_dbl_missing: {
      switch (y_class) {
      case vctrs_dbl_number: return -1;
      case vctrs_dbl_missing: return 0;
      case vctrs_dbl_nan: return 1;
      }
    }
    case vctrs_dbl_nan: {
      switch (y_class) {
      case vctrs_dbl_number: return -1;
      case vctrs_dbl_missing: return -1;
      case vctrs_dbl_nan: return 0;
      }
    }
    }
  } else {
    return (isnan(xi) || isnan(yj)) ? NA_INTEGER : dcmp(xi, yj);
  }

  never_reached("dbl_compare_scalar");
}

static inline int chr_compare_scalar(const SEXP* x, const SEXP* y, bool na_equal) {
  const SEXP xi = *x;
  const SEXP yj = *y;

  if (na_equal) {
    if (xi == NA_STRING) {
      return (yj == NA_STRING) ? 0 : -1;
    } else {
      return (yj == NA_STRING) ? 1 : scmp(xi, yj);
    }
  } else {
    return (xi == NA_STRING || yj == NA_STRING) ? NA_INTEGER : scmp(xi, yj);
  }
}

static inline int df_compare_scalar(SEXP x, R_len_t i, SEXP y, R_len_t j, bool na_equal, int n_col) {
  int cmp;

  for (int k = 0; k < n_col; ++k) {
    SEXP col_x = VECTOR_ELT(x, k);
    SEXP col_y = VECTOR_ELT(y, k);

    cmp = compare_scalar(col_x, i, col_y, j, na_equal);

    if (cmp != 0) {
      return cmp;
    }
  }

  return cmp;
}

// -----------------------------------------------------------------------------

// [[ include("vctrs.h") ]]
int compare_scalar(SEXP x, R_len_t i, SEXP y, R_len_t j, bool na_equal) {
  switch (TYPEOF(x)) {
  case LGLSXP: return lgl_compare_scalar(LOGICAL(x) + i, LOGICAL(y) + j, na_equal);
  case INTSXP: return int_compare_scalar(INTEGER(x) + i, INTEGER(y) + j, na_equal);
  case REALSXP: return dbl_compare_scalar(REAL(x) + i, REAL(y) + j, na_equal);
  case STRSXP: return chr_compare_scalar(STRING_PTR(x) + i, STRING_PTR(y) + j, na_equal);
  default: break;
  }

  switch (vec_proxy_typeof(x)) {
  case vctrs_type_list: stop_not_comparable(x, y, "lists are not comparable");
  case vctrs_type_dataframe: {
    int n_col = Rf_length(x);

    if (n_col != Rf_length(y)) {
      stop_not_comparable(x, y, "must have the same number of columns");
    }

    if (n_col == 0) {
      stop_not_comparable(x, y, "data frame with zero columns");
    }

    return df_compare_scalar(x, i, y, j, na_equal, n_col);
  }
  default: break;
  }

  Rf_errorcall(R_NilValue, "Unsupported type %s", Rf_type2char(TYPEOF(x)));
}

// -----------------------------------------------------------------------------

static SEXP df_compare(SEXP x, SEXP y, bool na_equal, R_len_t n_row);

#define COMPARE(CTYPE, CONST_DEREF, SCALAR_COMPARE)     \
do {                                                    \
  SEXP out = PROTECT(Rf_allocVector(INTSXP, size));     \
  int* p_out = INTEGER(out);                            \
                                                        \
  const CTYPE* p_x = CONST_DEREF(x);                    \
  const CTYPE* p_y = CONST_DEREF(y);                    \
                                                        \
  for (R_len_t i = 0; i < size; ++i, ++p_x, ++p_y) {    \
    p_out[i] = SCALAR_COMPARE(p_x, p_y, na_equal);      \
  }                                                     \
                                                        \
  UNPROTECT(1);                                         \
  return out;                                           \
}                                                       \
while (0)

// [[ register() ]]
SEXP vctrs_compare(SEXP x, SEXP y, SEXP na_equal_) {
  bool na_equal = Rf_asLogical(na_equal_);

  R_len_t size = vec_size(x);

  enum vctrs_type type = vec_proxy_typeof(x);
  if (type != vec_proxy_typeof(y) || size != vec_size(y)) {
    stop_not_comparable(x, y, "must have the same types and lengths");
  }

  switch (type) {
  case vctrs_type_logical:   COMPARE(int, LOGICAL_RO, lgl_compare_scalar);
  case vctrs_type_integer:   COMPARE(int, INTEGER_RO, int_compare_scalar);
  case vctrs_type_double:    COMPARE(double, REAL_RO, dbl_compare_scalar);
  case vctrs_type_character: COMPARE(SEXP, STRING_PTR_RO, chr_compare_scalar);
  case vctrs_type_dataframe: return df_compare(x, y, na_equal, size);
  case vctrs_type_scalar:    Rf_errorcall(R_NilValue, "Can't compare scalars with `vctrs_compare()`");
  case vctrs_type_list:      Rf_errorcall(R_NilValue, "Can't compare lists with `vctrs_compare()`");
  default:                   Rf_error("Unimplemented type in `vctrs_compare()`");
  }
}

#undef COMPARE

// -----------------------------------------------------------------------------

/**
 * @member out An integer vector of size `n_row` containing the output of the
 *   row wise data frame comparison.
 * @member row_known A logical vector of size `n_row`. Initially, all values
 *   are initialized to `FALSE`. As we iterate along the columns, we flip the
 *   corresponding row's `row_known` value to `TRUE` if we can determine it
 *   from the current column comparison (i.e. the comparison returns less than
 *   or greater than). Once a row's `row_known` value is `TRUE`, we never check
 *   that row again as we continue through the columns.
 * @member remaining The number of `row_known` values that are still `FALSE`.
 *   If this hits `0` before we traverse the entire data frame, we can exit
 *   immediately because all comparison values are already known.
 */
struct vctrs_df_compare_info {
  SEXP out;
  SEXP row_known;
  R_len_t remaining;
};

#define PROTECT_DF_COMPARE_INFO(info, n) do {  \
  PROTECT((info)->out);                        \
  PROTECT((info)->row_known);                  \
  *n += 2;                                     \
} while (0)

static struct vctrs_df_compare_info init_compare_info(R_len_t n_row) {
  struct vctrs_df_compare_info info;

  // Initialize to "equality" value
  // and only change if we learn that it differs
  info.out = PROTECT(Rf_allocVector(INTSXP, n_row));
  int* p_out = INTEGER(info.out);
  memset(p_out, 0, n_row * sizeof(int));

  // To begin with, no rows have a known comparison value
  info.row_known = PROTECT(Rf_allocVector(LGLSXP, n_row));
  int* p_row_known = LOGICAL(info.row_known);
  memset(p_row_known, 0, n_row * sizeof(int));

  info.remaining = n_row;

  UNPROTECT(2);
  return info;
}

// -----------------------------------------------------------------------------

static struct vctrs_df_compare_info vec_compare_col(SEXP x,
                                                    SEXP y,
                                                    bool na_equal,
                                                    struct vctrs_df_compare_info info,
                                                    R_len_t n_row);

static struct vctrs_df_compare_info df_compare_impl(SEXP x,
                                                    SEXP y,
                                                    bool na_equal,
                                                    struct vctrs_df_compare_info info,
                                                    R_len_t n_row);

static SEXP df_compare(SEXP x, SEXP y, bool na_equal, R_len_t n_row) {
  int nprot = 0;

  struct vctrs_df_compare_info info = init_compare_info(n_row);
  PROTECT_DF_COMPARE_INFO(&info, &nprot);

  info = df_compare_impl(x, y, na_equal, info, n_row);

  UNPROTECT(nprot);
  return info.out;
}

static struct vctrs_df_compare_info df_compare_impl(SEXP x,
                                                    SEXP y,
                                                    bool na_equal,
                                                    struct vctrs_df_compare_info info,
                                                    R_len_t n_row) {
  int n_col = Rf_length(x);

  if (n_col == 0) {
    stop_not_comparable(x, y, "data frame with zero columns");
  }

  if (n_col != Rf_length(y)) {
    stop_not_comparable(x, y, "must have the same number of columns");
  }

  for (R_len_t i = 0; i < n_col; ++i) {
    SEXP x_col = VECTOR_ELT(x, i);
    SEXP y_col = VECTOR_ELT(y, i);

    info = vec_compare_col(x_col, y_col, na_equal, info, n_row);

    // If we know all comparison values, break
    if (info.remaining == 0) {
      break;
    }
  }

  return info;
}

// -----------------------------------------------------------------------------

#define COMPARE_COL(CTYPE, CONST_DEREF, SCALAR_COMPARE)              \
do {                                                                 \
  int* p_out = INTEGER(info.out);                                    \
  int* p_row_known = LOGICAL(info.row_known);                        \
                                                                     \
  const CTYPE* p_x = CONST_DEREF(x);                                 \
  const CTYPE* p_y = CONST_DEREF(y);                                 \
                                                                     \
  for (R_len_t i = 0; i < n_row; ++i, ++p_row_known, ++p_x, ++p_y) { \
    if (*p_row_known) {                                              \
      continue;                                                      \
    }                                                                \
                                                                     \
    int cmp = SCALAR_COMPARE(p_x, p_y, na_equal);                    \
                                                                     \
    if (cmp != 0) {                                                  \
      p_out[i] = cmp;                                                \
      *p_row_known = true;                                           \
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

static struct vctrs_df_compare_info vec_compare_col(SEXP x,
                                                    SEXP y,
                                                    bool na_equal,
                                                    struct vctrs_df_compare_info info,
                                                    R_len_t n_row) {
  switch (vec_proxy_typeof(x)) {
  case vctrs_type_logical:   COMPARE_COL(int, LOGICAL_RO, lgl_compare_scalar);
  case vctrs_type_integer:   COMPARE_COL(int, INTEGER_RO, int_compare_scalar);
  case vctrs_type_double:    COMPARE_COL(double, REAL_RO, dbl_compare_scalar);
  case vctrs_type_character: COMPARE_COL(SEXP, STRING_PTR_RO, chr_compare_scalar);
  case vctrs_type_dataframe: return df_compare_impl(x, y, na_equal, info, n_row);
  case vctrs_type_scalar:    Rf_errorcall(R_NilValue, "Can't compare scalars with `vctrs_compare()`");
  case vctrs_type_list:      Rf_errorcall(R_NilValue, "Can't compare lists with `vctrs_compare()`");
  default:                   Rf_error("Unimplemented type in `vctrs_compare()`");
  }
}

#undef COMPARE_COL
