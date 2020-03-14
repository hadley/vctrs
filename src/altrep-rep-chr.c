#include "vctrs.h"
#include "altrep.h"
#include "altrep-rep.h"
#include "altrep-rep-internal.h"
#include "altrep-rep-macros.h"

#if !HAS_ALTREP
// -----------------------------------------------------------------------------
// Non-ALTREP support

// [[ include("altrep-rep.h") ]]
SEXP new_vctrs_compact_rep_chr(SEXP value, R_xlen_t size) {
  Rf_errorcall(R_NilValue, "Need R 3.5+ for ALTREP support");
  return R_NilValue;
}

// [[ register() ]]
SEXP vctrs_new_vctrs_compact_rep_chr(SEXP value, SEXP size) {
  Rf_errorcall(R_NilValue, "Need R 3.5+ for ALTREP support");
  return R_NilValue;
}

#else
// -----------------------------------------------------------------------------
// ALTREP implementation

struct vctrs_compact_rep_chr_info {
  SEXP value;
  R_xlen_t size;
};

// [[ include("altrep-rep.h") ]]
SEXP new_vctrs_compact_rep_chr(SEXP value, R_xlen_t size) {
  NEW_VCTRS_COMPACT_REP(value, size, chr);
}

// [[ register() ]]
SEXP vctrs_new_vctrs_compact_rep_chr(SEXP value, SEXP size) {
  VCTRS_NEW_VCTRS_COMPACT_REP(value, size, SEXP, STRING_PTR_RO, chr);
}

// -----------------------------------------------------------------------------

#define VCTRS_COMPACT_REP_CHR_VALUE(info) VCTRS_COMPACT_REP_VALUE(info, chr)
#define VCTRS_COMPACT_REP_CHR_VALUE_DATA(info) CHAR(VCTRS_COMPACT_REP_CHR_VALUE(info))
#define VCTRS_COMPACT_REP_CHR_SIZE(info) VCTRS_COMPACT_REP_SIZE(info, chr)

static SEXP vctrs_compact_rep_chr_materialize(SEXP x) {
  VCTRS_COMPACT_REP_MATERIALIZE(x, SEXP, STRING_PTR, STRSXP, CHR);
}

static SEXP vctrs_compact_rep_chr_serialized_state(SEXP x) {
  VCTRS_COMPACT_REP_SERIALIZED_STATE(x);
}

static SEXP vctrs_compact_rep_chr_unserialize(SEXP cls, SEXP state) {
  VCTRS_COMPACT_REP_UNSERIALIZE(cls, state, SEXP, CHR, chr);
}

static SEXP vctrs_compact_rep_chr_duplicate(SEXP x, Rboolean deep) {
  VCTRS_COMPACT_REP_DUPLICATE(x, deep, chr);
}

static SEXP vctrs_compact_rep_chr_coerce(SEXP x, int type) {
  VCTRS_COMPACT_REP_COERCE(x, type);
}

static Rboolean vctrs_compact_rep_chr_inspect(SEXP x,
                                              int pre,
                                              int deep,
                                              int pvec,
                                              void (*inspect_subtree)(SEXP, int, int, int)) {
  VCTRS_COMPACT_REP_INSPECT(
    x, pre, deep, pvec, inspect_subtree,
    char*, "s", "chr", CHR
  );
}

static R_xlen_t vctrs_compact_rep_chr_length(SEXP x) {
  VCTRS_COMPACT_REP_LENGTH(x, CHR);
}

static void* vctrs_compact_rep_chr_dataptr(SEXP x, Rboolean writeable) {
  VCTRS_COMPACT_REP_DATAPTR(x, writeable, chr);
}

static const void* vctrs_compact_rep_chr_dataptr_or_null(SEXP x) {
  VCTRS_COMPACT_REP_DATAPTR_OR_NULL(x, chr);
}

#define COMPACT_REP_CHR_EXTRACT_SUBSET_LOOP(CTYPE, CONST_DEREF, LOC_IS_FINITE) do { \
  const CTYPE* p_indx = CONST_DEREF(indx);                                          \
                                                                                    \
  for (R_xlen_t i = 0; i < out_size; ++i) {                                         \
    const CTYPE loc = p_indx[i];                                                    \
                                                                                    \
    if (LOC_IS_FINITE && 0 < loc && loc <= size) {                                  \
      p_out[i] = value;                                                             \
    } else {                                                                        \
      p_out[i] = NA_STRING;                                                         \
    }                                                                               \
  }                                                                                 \
} while(0)

static SEXP vctrs_compact_rep_chr_extract_subset(SEXP x, SEXP indx, SEXP call) {
  const SEXP info = VCTRS_COMPACT_REP_INFO(x);
  const SEXP value = VCTRS_COMPACT_REP_CHR_VALUE(info);
  const R_xlen_t size = VCTRS_COMPACT_REP_CHR_SIZE(info);

  const R_xlen_t out_size = Rf_xlength(indx);

  SEXP out = PROTECT(Rf_allocVector(STRSXP, out_size));
  SEXP* p_out = STRING_PTR(out);

  switch (TYPEOF(indx)) {
  case INTSXP: COMPACT_REP_CHR_EXTRACT_SUBSET_LOOP(int, INTEGER_RO, true); break;
  case REALSXP: COMPACT_REP_CHR_EXTRACT_SUBSET_LOOP(double, REAL_RO, R_FINITE(loc)); break;
  }

  UNPROTECT(1);
  return out;
}

#undef COMPACT_REP_CHR_EXTRACT_SUBSET_LOOP

// I believe we should expect that *_ELT() methods will never contain
// an `NA` index. I assumed this from how ExtractSubset() works and from
// how compact_intseq_Elt() is implemented
static SEXP vctrs_compact_rep_chr_elt(SEXP x, R_xlen_t i) {
  SEXP info = VCTRS_COMPACT_REP_INFO(x);
  return VCTRS_COMPACT_REP_CHR_VALUE(info);
}

static int vctrs_compact_rep_chr_no_na(SEXP x) {
  SEXP info = VCTRS_COMPACT_REP_INFO(x);
  return VCTRS_COMPACT_REP_CHR_VALUE(info) != NA_STRING;
}

// No altstring_Get_region method as of R 3.6.0
// static R_xlen_t vctrs_compact_rep_chr_get_region(SEXP x, R_xlen_t i, R_xlen_t n, SEXP* buf) {
//   SEXP info = VCTRS_COMPACT_REP_INFO(x);
//   SEXP value = VCTRS_COMPACT_REP_CHR_VALUE(info);
//   R_xlen_t size = VCTRS_COMPACT_REP_CHR_SIZE(info);
//
//   R_xlen_t n_capped = size - i > n ? n : size - i;
//
//   for (R_xlen_t k = 0; k < n_capped; ++k) {
//     buf[k] = value;
//   }
//
//   return n_capped;
// }

#undef VCTRS_COMPACT_REP_CHR_VALUE
#undef VCTRS_COMPACT_REP_CHR_VALUE_DATA
#undef VCTRS_COMPACT_REP_CHR_SIZE

// -----------------------------------------------------------------------------

SEXP vctrs_compact_rep_chr_class_sexp = NULL;
R_altrep_class_t vctrs_compact_rep_chr_class;

void vctrs_init_vctrs_compact_rep_chr(DllInfo* dll) {
  vctrs_compact_rep_chr_class = R_make_altstring_class("vctrs_compact_rep_chr", "vctrs", dll);

  vctrs_compact_rep_chr_class_sexp = R_SEXP(vctrs_compact_rep_chr_class);
  R_PreserveObject(vctrs_compact_rep_chr_class_sexp);

  // ALTREP methods
  R_set_altrep_Serialized_state_method(vctrs_compact_rep_chr_class, vctrs_compact_rep_chr_serialized_state);
  R_set_altrep_Unserialize_method(vctrs_compact_rep_chr_class, vctrs_compact_rep_chr_unserialize);
  R_set_altrep_Duplicate_method(vctrs_compact_rep_chr_class, vctrs_compact_rep_chr_duplicate);
  R_set_altrep_Coerce_method(vctrs_compact_rep_chr_class, vctrs_compact_rep_chr_coerce);
  R_set_altrep_Inspect_method(vctrs_compact_rep_chr_class, vctrs_compact_rep_chr_inspect);
  R_set_altrep_Length_method(vctrs_compact_rep_chr_class, vctrs_compact_rep_chr_length);

  // ALTVEC methods
  R_set_altvec_Dataptr_method(vctrs_compact_rep_chr_class, vctrs_compact_rep_chr_dataptr);
  R_set_altvec_Dataptr_or_null_method(vctrs_compact_rep_chr_class, vctrs_compact_rep_chr_dataptr_or_null);
  R_set_altvec_Extract_subset_method(vctrs_compact_rep_chr_class, vctrs_compact_rep_chr_extract_subset);

  // ALTSTRING methods
  R_set_altstring_Elt_method(vctrs_compact_rep_chr_class, vctrs_compact_rep_chr_elt);
  R_set_altstring_No_NA_method(vctrs_compact_rep_chr_class, vctrs_compact_rep_chr_no_na);
  // No altstring_Get_region method as of R 3.6.0
  // R_set_altstring_Get_region_method(vctrs_compact_rep_chr_class, vctrs_compact_rep_chr_get_region);
}

#endif
