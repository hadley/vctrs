#include "vctrs.h"
#include "utils.h"
#include "altrep-rep.h"

// Initialised at load time
static SEXP unspecified_attrib = NULL;
SEXP vctrs_shared_empty_uns = NULL;


// [[ include("vctrs.h") ]]
SEXP vec_unspecified(R_len_t n) {
  SEXP out = PROTECT(Rf_allocVector(LGLSXP, n));

  r_lgl_fill(out, NA_LOGICAL, n);
  SET_ATTRIB(out, unspecified_attrib);
  SET_OBJECT(out, 1);

  UNPROTECT(1);
  return out;
}

// [[ register ]]
SEXP vctrs_unspecified(SEXP n) {
  if (Rf_length(n) != 1) {
    Rf_errorcall(R_NilValue, "`n` must be a single number");
  }
  if (TYPEOF(n) != INTSXP) {
    n = vec_cast(n, vctrs_shared_empty_int, args_empty, args_empty);
  }
  int len = INTEGER(n)[0];
  return vec_unspecified(len);
}

// [[ include("vctrs.h") ]]
bool vec_is_unspecified(SEXP x) {
  if (TYPEOF(x) != LGLSXP) {
    return false;
  }

  SEXP attrib = ATTRIB(x);

  if (attrib == unspecified_attrib) {
    return true;
  }

  if (attrib != R_NilValue) {
    // The unspecified vector might have been created outside the
    // session (e.g. serialisation)
    if (Rf_inherits(x, "vctrs_unspecified")) {
      return true;
    }
    if (OBJECT(x)) {
      return false;
    }
    if (has_dim(x)) {
      return false;
    }
  }

  R_len_t n = Rf_length(x);
  if (n == 0) {
    return false;
  }

  // Avoid compact_rep expansion from calling LOGICAL()
  if (vec_is_vctrs_compact_rep_lgl(x)) {
    return !LOGICAL_NO_NA(x);
  }

  R_len_t* p_x = LOGICAL(x);
  for (R_len_t i = 0; i < n; ++i) {
    if (p_x[i] != NA_LOGICAL) {
      return false;
    }
  }

  return true;
}

// [[ register ]]
SEXP vctrs_is_unspecified(SEXP x) {
  return Rf_ScalarLogical(vec_is_unspecified(x));
}


void vctrs_init_unspecified(SEXP ns) {
  {
    SEXP unspecified_class = PROTECT(Rf_allocVector(STRSXP, 1));
    SET_STRING_ELT(unspecified_class, 0, Rf_mkChar("vctrs_unspecified"));

    unspecified_attrib = Rf_cons(unspecified_class, R_NilValue);
    R_PreserveObject(unspecified_attrib);
    SET_TAG(unspecified_attrib, R_ClassSymbol);

    UNPROTECT(1);
  }

  vctrs_shared_empty_uns = vec_unspecified(0);
  R_PreserveObject(vctrs_shared_empty_uns);
  MARK_NOT_MUTABLE(vctrs_shared_empty_uns);
}
