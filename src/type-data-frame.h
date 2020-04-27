#ifndef VCTRS_TYPE_DATA_FRAME_H
#define VCTRS_TYPE_DATA_FRAME_H

#include "arg.h"
#include "names.h"
#include "ptype2.h"


SEXP new_data_frame(SEXP x, R_len_t n);
void init_data_frame(SEXP x, R_len_t n);
void init_tibble(SEXP x, R_len_t n);
void init_compact_rownames(SEXP x, R_len_t n);
SEXP df_rownames(SEXP x);

bool is_native_df(SEXP x);
SEXP df_poke(SEXP x, R_len_t i, SEXP value);
SEXP df_poke_at(SEXP x, SEXP name, SEXP value);
R_len_t df_flat_width(SEXP x);
SEXP df_flatten(SEXP x);
SEXP df_repair_names(SEXP x, struct name_repair_opts* name_repair);

static inline
SEXP df_cast(SEXP x, SEXP to, struct vctrs_arg* x_arg, struct vctrs_arg* to_arg);

enum rownames_type {
  ROWNAMES_AUTOMATIC,
  ROWNAMES_AUTOMATIC_COMPACT,
  ROWNAMES_IDENTIFIERS
};
enum rownames_type rownames_type(SEXP rn);
R_len_t rownames_size(SEXP rn);


SEXP df_ptype2(const struct ptype2_opts* opts);

static inline
SEXP df_ptype2_params(SEXP x,
                      SEXP y,
                      struct vctrs_arg* x_arg,
                      struct vctrs_arg* y_arg,
                      bool df_fallback) {
  const struct ptype2_opts opts = {
    .x = x,
    .y = y,
    .x_arg = x_arg,
    .y_arg = y_arg,
    .df_fallback = df_fallback
  };
  return df_ptype2(&opts);
}


#endif
