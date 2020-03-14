#include "vctrs.h"
#include "altrep.h"
#include "altrep-rep.h"
#include "altrep-rep-internal.h"
#include "altrep-rep-macros.h"

#if !HAS_ALTREP
// -----------------------------------------------------------------------------
// Non-ALTREP support

// [[ include("altrep-rep.h") ]]
SEXP new_vctrs_compact_rep_dbl(double value, R_xlen_t size) {
  Rf_errorcall(R_NilValue, "Need R 3.5+ for ALTREP support");
  return R_NilValue;
}

// [[ register() ]]
SEXP vctrs_new_vctrs_compact_rep_dbl(SEXP value, SEXP size) {
  Rf_errorcall(R_NilValue, "Need R 3.5+ for ALTREP support");
  return R_NilValue;
}

#else
// -----------------------------------------------------------------------------
// ALTREP implementation

struct vctrs_compact_rep_dbl_info {
  double value;
  R_xlen_t size;
};

// [[ include("altrep-rep.h") ]]
SEXP new_vctrs_compact_rep_dbl(double value, R_xlen_t size) {
  NEW_VCTRS_COMPACT_REP(value, size, dbl);
}

// [[ register() ]]
SEXP vctrs_new_vctrs_compact_rep_dbl(SEXP value, SEXP size) {
  VCTRS_NEW_VCTRS_COMPACT_REP(value, size, double, REAL_RO, dbl);
}

// -----------------------------------------------------------------------------

#define VCTRS_COMPACT_REP_DBL_VALUE(info) VCTRS_COMPACT_REP_VALUE(info, dbl)
#define VCTRS_COMPACT_REP_DBL_VALUE_DATA(info) VCTRS_COMPACT_REP_DBL_VALUE(info)
#define VCTRS_COMPACT_REP_DBL_SIZE(info) VCTRS_COMPACT_REP_SIZE(info, dbl)

static SEXP vctrs_compact_rep_dbl_materialize(SEXP x) {
  VCTRS_COMPACT_REP_MATERIALIZE(x, double, REAL, REALSXP, DBL);
}

static SEXP vctrs_compact_rep_dbl_serialized_state(SEXP x) {
  VCTRS_COMPACT_REP_SERIALIZED_STATE(x);
}

static SEXP vctrs_compact_rep_dbl_unserialize(SEXP cls, SEXP state) {
  VCTRS_COMPACT_REP_UNSERIALIZE(cls, state, double, DBL, dbl);
}

static SEXP vctrs_compact_rep_dbl_duplicate(SEXP x, Rboolean deep) {
  VCTRS_COMPACT_REP_DUPLICATE(x, deep, dbl);
}

static SEXP vctrs_compact_rep_dbl_coerce(SEXP x, int type) {
  VCTRS_COMPACT_REP_COERCE(x, type);
}

static Rboolean vctrs_compact_rep_dbl_inspect(SEXP x,
                                              int pre,
                                              int deep,
                                              int pvec,
                                              void (*inspect_subtree)(SEXP, int, int, int)) {
  VCTRS_COMPACT_REP_INSPECT(
    x, pre, deep, pvec, inspect_subtree,
    double, "f", "dbl", DBL
  );
}

static R_xlen_t vctrs_compact_rep_dbl_length(SEXP x) {
  VCTRS_COMPACT_REP_LENGTH(x, DBL);
}

static void* vctrs_compact_rep_dbl_dataptr(SEXP x, Rboolean writeable) {
  VCTRS_COMPACT_REP_DATAPTR(x, writeable, dbl);
}

static const void* vctrs_compact_rep_dbl_dataptr_or_null(SEXP x) {
  VCTRS_COMPACT_REP_DATAPTR_OR_NULL(x, dbl);
}

#define COMPACT_REP_DBL_EXTRACT_SUBSET_LOOP(CTYPE, CONST_DEREF, LOC_IS_FINITE) do { \
  const CTYPE* p_indx = CONST_DEREF(indx);                                          \
                                                                                    \
  for (R_xlen_t i = 0; i < out_size; ++i) {                                         \
    const CTYPE loc = p_indx[i];                                                    \
                                                                                    \
    if (LOC_IS_FINITE && 0 < loc && loc <= size) {                                  \
      p_out[i] = value;                                                             \
    } else {                                                                        \
      p_out[i] = NA_REAL;                                                           \
    }                                                                               \
  }                                                                                 \
} while(0)

static SEXP vctrs_compact_rep_dbl_extract_subset(SEXP x, SEXP indx, SEXP call) {
  const SEXP info = VCTRS_COMPACT_REP_INFO(x);
  const double value = VCTRS_COMPACT_REP_DBL_VALUE(info);
  const R_xlen_t size = VCTRS_COMPACT_REP_DBL_SIZE(info);

  const R_xlen_t out_size = Rf_xlength(indx);

  SEXP out = PROTECT(Rf_allocVector(REALSXP, out_size));
  double* p_out = REAL(out);

  switch (TYPEOF(indx)) {
  case INTSXP: COMPACT_REP_DBL_EXTRACT_SUBSET_LOOP(int, INTEGER_RO, true); break;
  case REALSXP: COMPACT_REP_DBL_EXTRACT_SUBSET_LOOP(double, REAL_RO, R_FINITE(loc)); break;
  }

  UNPROTECT(1);
  return out;
}

#undef COMPACT_REP_DBL_EXTRACT_SUBSET_LOOP

// I believe we should expect that *_ELT() methods will never contain
// an `NA` index. I assumed this from how ExtractSubset() works and from
// how compact_intseq_Elt() is implemented
static double vctrs_compact_rep_dbl_elt(SEXP x, R_xlen_t i) {
  SEXP info = VCTRS_COMPACT_REP_INFO(x);
  return VCTRS_COMPACT_REP_DBL_VALUE(info);
}

static int vctrs_compact_rep_dbl_no_na(SEXP x) {
  SEXP info = VCTRS_COMPACT_REP_INFO(x);
  return !ISNAN(VCTRS_COMPACT_REP_DBL_VALUE(info));
}

static R_xlen_t vctrs_compact_rep_dbl_get_region(SEXP x, R_xlen_t i, R_xlen_t n, double* buf) {
  SEXP info = VCTRS_COMPACT_REP_INFO(x);
  double value = VCTRS_COMPACT_REP_DBL_VALUE(info);
  R_xlen_t size = VCTRS_COMPACT_REP_DBL_SIZE(info);

  R_xlen_t n_capped = size - i > n ? n : size - i;

  for (R_xlen_t k = 0; k < n_capped; ++k) {
    buf[k] = value;
  }

  return n_capped;
}

#undef VCTRS_COMPACT_REP_DBL_VALUE
#undef VCTRS_COMPACT_REP_DBL_VALUE_DATA
#undef VCTRS_COMPACT_REP_DBL_SIZE

// -----------------------------------------------------------------------------

SEXP vctrs_compact_rep_dbl_class_sexp = NULL;
R_altrep_class_t vctrs_compact_rep_dbl_class;

void vctrs_init_vctrs_compact_rep_dbl(DllInfo* dll) {
  vctrs_compact_rep_dbl_class = R_make_altreal_class("vctrs_compact_rep_dbl", "vctrs", dll);

  vctrs_compact_rep_dbl_class_sexp = R_SEXP(vctrs_compact_rep_dbl_class);
  R_PreserveObject(vctrs_compact_rep_dbl_class_sexp);

  // ALTREP methods
  R_set_altrep_Serialized_state_method(vctrs_compact_rep_dbl_class, vctrs_compact_rep_dbl_serialized_state);
  R_set_altrep_Unserialize_method(vctrs_compact_rep_dbl_class, vctrs_compact_rep_dbl_unserialize);
  R_set_altrep_Duplicate_method(vctrs_compact_rep_dbl_class, vctrs_compact_rep_dbl_duplicate);
  R_set_altrep_Coerce_method(vctrs_compact_rep_dbl_class, vctrs_compact_rep_dbl_coerce);
  R_set_altrep_Inspect_method(vctrs_compact_rep_dbl_class, vctrs_compact_rep_dbl_inspect);
  R_set_altrep_Length_method(vctrs_compact_rep_dbl_class, vctrs_compact_rep_dbl_length);

  // ALTVEC methods
  R_set_altvec_Dataptr_method(vctrs_compact_rep_dbl_class, vctrs_compact_rep_dbl_dataptr);
  R_set_altvec_Dataptr_or_null_method(vctrs_compact_rep_dbl_class, vctrs_compact_rep_dbl_dataptr_or_null);
  R_set_altvec_Extract_subset_method(vctrs_compact_rep_dbl_class, vctrs_compact_rep_dbl_extract_subset);

  // ALTREAL methods
  R_set_altreal_Elt_method(vctrs_compact_rep_dbl_class, vctrs_compact_rep_dbl_elt);
  R_set_altreal_No_NA_method(vctrs_compact_rep_dbl_class, vctrs_compact_rep_dbl_no_na);
  R_set_altreal_Get_region_method(vctrs_compact_rep_dbl_class, vctrs_compact_rep_dbl_get_region);
}

#endif
