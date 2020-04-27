#ifndef VCTRS_CAST_H
#define VCTRS_CAST_H


struct cast_opts {
  SEXP x;
  SEXP to;
  struct vctrs_arg* x_arg;
  struct vctrs_arg* to_arg;
  bool df_fallback;
};

SEXP df_cast_opts(const struct cast_opts* opts);

// Defined in type-data-frame.c
static inline
SEXP df_cast(SEXP x, SEXP to, struct vctrs_arg* x_arg, struct vctrs_arg* to_arg) {
  const struct cast_opts opts = {
    .x = x,
    .to = to,
    .x_arg = x_arg,
    .to_arg = to_arg
  };
  return df_cast_opts(&opts);
}

SEXP vec_cast_opts(const struct cast_opts* opts);

static inline
SEXP vec_cast_params(SEXP x,
                     SEXP to,
                     struct vctrs_arg* x_arg,
                     struct vctrs_arg* to_arg,
                     bool df_fallback) {
  const struct cast_opts opts = {
    .x = x,
    .to = to,
    .x_arg = x_arg,
    .to_arg = to_arg,
    .df_fallback = df_fallback
  };
  return vec_cast_opts(&opts);
}

SEXP vec_cast_dispatch(const struct cast_opts* opts,
                       enum vctrs_type x_type,
                       enum vctrs_type to_type,
                       bool* lossy);

SEXP vec_cast_e(const struct cast_opts* opts,
                ERR* err);

// Defined in cast-bare.c
SEXP int_as_double(SEXP x, bool* lossy);
SEXP lgl_as_double(SEXP x, bool* lossy);
SEXP dbl_as_integer(SEXP x, bool* lossy);
SEXP lgl_as_integer(SEXP x, bool* lossy);
SEXP chr_as_logical(SEXP x, bool* lossy);
SEXP dbl_as_logical(SEXP x, bool* lossy);
SEXP int_as_logical(SEXP x, bool* lossy);


#endif
