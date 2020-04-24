#ifndef VCTRS_TYPE_DATA_FRAME_H
#define VCTRS_TYPE_DATA_FRAME_H


SEXP new_data_frame(SEXP x, R_len_t size);
void init_data_frame(SEXP x, R_len_t size);
void init_tibble(SEXP x, R_len_t size);
void init_compact_rownames(SEXP x, R_len_t size);
SEXP df_rownames(SEXP x);

bool is_native_df(SEXP x);
SEXP df_poke(SEXP x, R_len_t i, SEXP value);
SEXP df_poke_at(SEXP x, SEXP name, SEXP value);
R_len_t df_flat_width(SEXP x);
SEXP df_flatten(SEXP x);
SEXP df_repair_names(SEXP x, struct name_repair_opts* name_repair);

SEXP df_cast(SEXP x, SEXP to, struct vctrs_arg* x_arg, struct vctrs_arg* to_arg);

enum rownames_type {
  ROWNAMES_AUTOMATIC,
  ROWNAMES_AUTOMATIC_COMPACT,
  ROWNAMES_IDENTIFIERS
};
enum rownames_type rownames_type(SEXP rn);
R_len_t rownames_size(SEXP rn);


#endif
