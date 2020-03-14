#include "vctrs.h"
#include "altrep.h"
#include "altrep-rep.h"
#include "altrep-rep-internal.h"

// -----------------------------------------------------------------------------
#if HAS_ALTREP

// [[ include("altrep-rep.h") ]]
bool vec_is_vctrs_compact_rep(SEXP x) {
  if (!ALTREP(x)) {
    return false;
  }

  SEXP cls = ALTREP_CLASS(x);

  bool out =
    cls == vctrs_compact_rep_int_class_sexp ||
    cls == vctrs_compact_rep_dbl_class_sexp ||
    cls == vctrs_compact_rep_chr_class_sexp;

#if HAS_ALTREP_3_6
  out = out ||
    cls == vctrs_compact_rep_lgl_class_sexp;
#endif

  return out;
}

// [[ register() ]]
SEXP vctrs_is_vctrs_compact_rep(SEXP x) {
  return Rf_ScalarLogical(vec_is_vctrs_compact_rep(x));
}


// [[ register() ]]
SEXP vctrs_is_vctrs_compact_rep_compact(SEXP x) {
  if (!vec_is_vctrs_compact_rep(x)) {
    Rf_errorcall(R_NilValue, "Internal error: `x` is not an ALTREP compact rep");
  }

  bool out = VCTRS_COMPACT_REP_IS_COMPACT(x);

  return Rf_ScalarLogical(out);
}


// [[ init() ]]
void vctrs_init_vctrs_compact_rep(DllInfo* dll) {
  vctrs_init_vctrs_compact_rep_int(dll);
  vctrs_init_vctrs_compact_rep_dbl(dll);
  vctrs_init_vctrs_compact_rep_chr(dll);

#if HAS_ALTREP_3_6
  vctrs_init_vctrs_compact_rep_lgl(dll);
#endif
}

// -----------------------------------------------------------------------------
#else

// [[ include("altrep-rep.h") ]]
bool vec_is_vctrs_compact_rep(SEXP x) { return false; }

// [[ register() ]]
SEXP vctrs_is_vctrs_compact_rep(SEXP x) {
  Rf_errorcall(R_NilValue, "Need R 3.5+ for ALTREP support");
  return R_NilValue;
}


// [[ register() ]]
SEXP vctrs_is_vctrs_compact_rep_compact(SEXP x) {
  Rf_errorcall(R_NilValue, "Need R 3.5+ for ALTREP support");
  return R_NilValue;
}


// For DllInfo on R < 3.4
#include <R_ext/Rdynload.h>

// [[ init() ]]
void vctrs_init_vctrs_compact_rep(DllInfo* dll) { }

#endif
