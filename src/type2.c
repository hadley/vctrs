#include "vctrs.h"
#include "utils.h"


// Initialised at load time
static SEXP fns_vec_type2_dispatch = NULL;
static SEXP syms_vec_type2_dispatch = NULL;

static SEXP vctrs_type2_dispatch(SEXP x,
                                 SEXP y,
                                 struct vctrs_arg* x_arg,
                                 struct vctrs_arg* y_arg) {
  SEXP x_arg_chr = PROTECT(vctrs_arg(x_arg));
  SEXP y_arg_chr = PROTECT(vctrs_arg(y_arg));

  SEXP syms[5] = { syms_x, syms_y, syms_x_arg, syms_y_arg, NULL };
  SEXP args[5] = {      x,      y,  x_arg_chr,  y_arg_chr, NULL };

  SEXP out = vctrs_dispatch_n(syms_vec_type2_dispatch, fns_vec_type2_dispatch,
                              syms, args);

  UNPROTECT(2);
  return out;
}


static SEXP df_type2(SEXP x, SEXP y, struct vctrs_arg* x_arg, struct vctrs_arg* y_arg);

// [[ include("vctrs.h") ]]
SEXP vec_type2(SEXP x, SEXP y,
               struct vctrs_arg* x_arg,
               struct vctrs_arg* y_arg,
               int* left) {
  if (x == R_NilValue) {
    if (!vec_is_partial(y)) {
      vec_assert(y, y_arg);
    }
    *left = y == R_NilValue;
    return vec_type(y);
  }
  if (y == R_NilValue) {
    if (!vec_is_partial(x)) {
      vec_assert(x, x_arg);
    }
    *left = x == R_NilValue;
    return vec_type(x);
  }

  if (has_dim(x) || has_dim(y)) {
    return vctrs_type2_dispatch(x, y, x_arg, y_arg);
  }

  enum vctrs_type type_x = vec_typeof(x);
  enum vctrs_type type_y = vec_typeof(y);

  if (type_x == vctrs_type_scalar) {
    stop_scalar_type(x, x_arg);
  }
  if (type_y == vctrs_type_scalar) {
    stop_scalar_type(y, y_arg);
  }

  enum vctrs_type2 type2 = vec_typeof2_impl(type_x, type_y, left);

  switch (type2) {
  case vctrs_type2_null_null:
    return R_NilValue;

  case vctrs_type2_logical_logical:
    return vctrs_shared_empty_lgl;

  case vctrs_type2_logical_integer:
  case vctrs_type2_integer_integer:
    return vctrs_shared_empty_int;

  case vctrs_type2_logical_double:
  case vctrs_type2_integer_double:
  case vctrs_type2_double_double:
    return vctrs_shared_empty_dbl;

  case vctrs_type2_character_character:
    return vctrs_shared_empty_chr;

  case vctrs_type2_raw_raw:
    return vctrs_shared_empty_raw;

  case vctrs_type2_list_list:
    return vctrs_shared_empty_list;

  case vctrs_type2_dataframe_dataframe:
    return df_type2(x, y, x_arg, y_arg);

  default: {
    x = PROTECT(vec_type(x));
    y = PROTECT(vec_type(y));
    SEXP out = vctrs_type2_dispatch(x, y, x_arg, y_arg);

    UNPROTECT(2);
    return out;
  }}
}


// From dictionary.c
SEXP vctrs_match(SEXP needles, SEXP haystack);

SEXP df_type2(SEXP x, SEXP y, struct vctrs_arg* x_arg, struct vctrs_arg* y_arg) {
  SEXP x_names = PROTECT(r_names(x));
  SEXP y_names = PROTECT(r_names(y));

  SEXP x_dups_pos = PROTECT(vctrs_match(x_names, y_names));
  SEXP y_dups_pos = PROTECT(vctrs_match(y_names, x_names));

  int* x_dups_pos_data = INTEGER(x_dups_pos);
  int* y_dups_pos_data = INTEGER(y_dups_pos);

  R_len_t x_len = Rf_length(x_dups_pos);
  R_len_t y_len = Rf_length(y_dups_pos);

  // Count columns that are only in `y`
  R_len_t rest_len = 0;
  for (R_len_t i = 0; i < y_len; ++i) {
    if (y_dups_pos_data[i] == NA_INTEGER) {
      ++rest_len;
    }
  }

  R_len_t out_len = x_len + rest_len;
  SEXP out = PROTECT(Rf_allocVector(VECSXP, out_len));
  SEXP nms = PROTECT(Rf_allocVector(STRSXP, out_len));
  Rf_setAttrib(out, R_NamesSymbol, nms);

  R_len_t i = 0;

  // Fill in prototypes of all the columns that are in `x`, in order
  for (; i < x_len; ++i) {
    R_len_t dup = x_dups_pos_data[i];

    SEXP type;
    if (dup == NA_INTEGER) {
      type = vec_type(VECTOR_ELT(x, i));
    } else {
      --dup; // 1-based index
      struct arg_data_index x_arg_data = new_index_arg_data(r_chr_get_c_string(x_names, i), x_arg);
      struct arg_data_index y_arg_data = new_index_arg_data(r_chr_get_c_string(y_names, dup), y_arg);
      struct vctrs_arg named_x_arg = new_index_arg(x_arg, &x_arg_data);
      struct vctrs_arg named_y_arg = new_index_arg(y_arg, &y_arg_data);
      int _left;
      type = vec_type2(VECTOR_ELT(x, i),
                       VECTOR_ELT(y, dup),
                       &named_x_arg,
                       &named_y_arg,
                       &_left);
    }

    SET_VECTOR_ELT(out, i, type);
    SET_STRING_ELT(nms, i, STRING_ELT(x_names, i));
  }

  // Fill in prototypes of the columns that are only in `y`
  for (R_len_t j = 0; i < out_len; ++j) {
    R_len_t dup = y_dups_pos_data[j];
    if (dup == NA_INTEGER) {
      SET_VECTOR_ELT(out, i, vec_type(VECTOR_ELT(y, j)));
      SET_STRING_ELT(nms, i, STRING_ELT(y_names, j));
      ++i;
    }
  }

  init_data_frame(out, 0);

  UNPROTECT(6);
  return out;
}

// [[ register() ]]
SEXP vctrs_type2(SEXP x, SEXP y, SEXP x_arg, SEXP y_arg) {
  if (!r_is_string(x_arg)) {
    Rf_errorcall(R_NilValue, "`x_arg` must be a string");
  }
  if (!r_is_string(y_arg)) {
    Rf_errorcall(R_NilValue, "`y_arg` must be a string");
  }

  struct vctrs_arg x_arg_ = new_wrapper_arg(NULL, r_chr_get_c_string(x_arg, 0));
  struct vctrs_arg y_arg_ = new_wrapper_arg(NULL, r_chr_get_c_string(y_arg, 0));

  int _left;
  return vec_type2(x, y, &x_arg_, &y_arg_, &_left);
}

// [[ register() ]]
SEXP vctrs_type2_df_df(SEXP x, SEXP y, SEXP x_arg, SEXP y_arg) {
  if (!r_is_string(x_arg)) {
    Rf_errorcall(R_NilValue, "`x_arg` must be a string");
  }
  if (!r_is_string(y_arg)) {
    Rf_errorcall(R_NilValue, "`y_arg` must be a string");
  }

  struct vctrs_arg x_arg_ = new_wrapper_arg(NULL, r_chr_get_c_string(x_arg, 0));
  struct vctrs_arg y_arg_ = new_wrapper_arg(NULL, r_chr_get_c_string(y_arg, 0));

  return df_type2(x, y, &x_arg_, &y_arg_);
}


void vctrs_init_type2(SEXP ns) {
  syms_vec_type2_dispatch = Rf_install("vec_type2_dispatch");
  fns_vec_type2_dispatch = Rf_findVar(syms_vec_type2_dispatch, ns);
}
