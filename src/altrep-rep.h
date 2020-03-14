#ifndef VCTRS_ALTREP_REP_H
#define VCTRS_ALTREP_REP_H

bool vec_is_vctrs_compact_rep(SEXP x);
bool vec_is_vctrs_compact_rep_lgl(SEXP x);

SEXP new_vctrs_compact_rep_lgl(int value, R_xlen_t size);
SEXP new_vctrs_compact_rep_int(int value, R_xlen_t size);
SEXP new_vctrs_compact_rep_dbl(double value, R_xlen_t size);
SEXP new_vctrs_compact_rep_chr(SEXP value, R_xlen_t size);

#endif
