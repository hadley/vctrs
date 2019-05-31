#include "vctrs.h"
#include "utils.h"


// [[ include("utils.h") ]]
SEXP new_data_frame(SEXP x, R_len_t n) {
  x = PROTECT(r_maybe_duplicate(x));
  init_data_frame(x, n);

  UNPROTECT(1);
  return x;
}

// [[ include("utils.h") ]]
bool is_compact_rownames(SEXP x) {
  return Rf_length(x) == 2 && INTEGER(x)[0] == NA_INTEGER;
}
// [[ include("utils.h") ]]
R_len_t compact_rownames_length(SEXP x) {
  return abs(INTEGER(x)[1]);
}

static void init_bare_data_frame(SEXP x, R_len_t n);
static void init_compact_rownames(SEXP x, R_len_t n);
static SEXP new_compact_rownames(R_len_t n);

// [[ include("utils.h") ]]
void init_data_frame(SEXP x, R_len_t n) {
  Rf_setAttrib(x, R_ClassSymbol, classes_data_frame);
  init_bare_data_frame(x, n);
}
// [[ include("utils.h") ]]
void init_tibble(SEXP x, R_len_t n) {
  Rf_setAttrib(x, R_ClassSymbol, classes_tibble);
  init_bare_data_frame(x, n);
}

static void init_bare_data_frame(SEXP x, R_len_t n) {
  if (Rf_length(x) == 0) {
    Rf_setAttrib(x, R_NamesSymbol, vctrs_shared_empty_chr);
  }

  init_compact_rownames(x, n);
}

static void init_compact_rownames(SEXP x, R_len_t n) {
  SEXP rn = PROTECT(new_compact_rownames(n));
  Rf_setAttrib(x, R_RowNamesSymbol, rn);
  UNPROTECT(1);
}
static SEXP new_compact_rownames(R_len_t n) {
  if (n <= 0) {
    return vctrs_shared_empty_int;
  }

  SEXP out = Rf_allocVector(INTSXP, 2);
  int* out_data = INTEGER(out);
  out_data[0] = NA_INTEGER;
  out_data[1] = -n;
  return out;
}
