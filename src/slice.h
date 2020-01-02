#ifndef VCTRS_SLICE_H
#define VCTRS_SLICE_H


struct vec_as_index_options {
  bool convert_negative;
};

SEXP vec_as_index(SEXP i, R_len_t n, SEXP names);
SEXP vec_as_index_opts(SEXP i, R_len_t n, SEXP names, struct vec_as_index_options* opts);

SEXP list_slice(SEXP x, SEXP index);
SEXP slice_names(SEXP names, SEXP index);


#endif
