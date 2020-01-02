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

// [[ include("utils.h") ]]
void init_compact_rownames(SEXP x, R_len_t n) {
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

// [[ include("utils.h") ]]
SEXP get_rownames(SEXP x) {
  // Required, because getAttrib() already does the transformation to a vector,
  // and getAttrib0() is hidden
  SEXP node = ATTRIB(x);

  while (node != R_NilValue) {
    SEXP tag = TAG(node);

    if (tag == R_RowNamesSymbol) {
      return CAR(node);
    }

    node = CDR(node);
  }

  return R_NilValue;
}

// If negative index, value is appended
SEXP df_poke(SEXP x, R_len_t i, SEXP value) {
  if (i >= 0) {
    SET_VECTOR_ELT(x, i, value);
    return x;
  }

  R_len_t ncol = Rf_length(x);

  SEXP tmp = PROTECT(r_resize(x, ncol + 1));
  Rf_copyMostAttrib(x, tmp);
  x = tmp;

  SET_VECTOR_ELT(x, ncol, value);

  UNPROTECT(1);
  return x;
}
SEXP df_poke_at(SEXP x, SEXP name, SEXP value) {
  SEXP names = PROTECT(r_names(x));
  R_len_t i = r_chr_find(names, name);
  UNPROTECT(1);

  x = PROTECT(df_poke(x, i, value));

  if (i < 0) {
    SEXP names = PROTECT(r_names(x));
    SET_STRING_ELT(names, Rf_length(x) - 1, name);
    UNPROTECT(1);
  }

  UNPROTECT(1);
  return x;
}
