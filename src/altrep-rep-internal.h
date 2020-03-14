#ifndef VCTRS_ALTREP_REP_INTERNAL_H
#define VCTRS_ALTREP_REP_INTERNAL_H

// -----------------------------------------------------------------------------
#if HAS_ALTREP

// Used with all compact rep types
#define VCTRS_COMPACT_REP_INFO(x) R_altrep_data1(x)
#define VCTRS_COMPACT_REP_DATA(x) R_altrep_data2(x)
#define VCTRS_COMPACT_REP_IS_COMPACT(x) (VCTRS_COMPACT_REP_DATA(x) == R_NilValue)
#define VCTRS_COMPACT_REP_SET_DATA(x, data) R_set_altrep_data2(x, data)

extern SEXP vctrs_compact_rep_int_class_sexp;
extern SEXP vctrs_compact_rep_dbl_class_sexp;
extern SEXP vctrs_compact_rep_chr_class_sexp;

extern R_altrep_class_t vctrs_compact_rep_int_class;
extern R_altrep_class_t vctrs_compact_rep_dbl_class;
extern R_altrep_class_t vctrs_compact_rep_chr_class;

void vctrs_init_vctrs_compact_rep_int(DllInfo* dll);
void vctrs_init_vctrs_compact_rep_dbl(DllInfo* dll);
void vctrs_init_vctrs_compact_rep_chr(DllInfo* dll);

#endif

// -----------------------------------------------------------------------------
#if HAS_ALTREP_3_6

extern SEXP vctrs_compact_rep_lgl_class_sexp;

extern R_altrep_class_t vctrs_compact_rep_lgl_class;

void vctrs_init_vctrs_compact_rep_lgl(DllInfo* dll);

#endif

#endif
