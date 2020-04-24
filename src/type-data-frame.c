#include "vctrs.h"
#include "type-data-frame.h"
#include "utils.h"

static SEXP syms_df_lossy_cast = NULL;
static SEXP fns_df_lossy_cast = NULL;

static SEXP new_compact_rownames(R_len_t size);


// [[ include("type-data-frame.h") ]]
bool is_data_frame(SEXP x) {
  if (TYPEOF(x) != VECSXP) {
    return false;
  }

  enum vctrs_class_type type = class_type(x);
  return
    type == vctrs_class_bare_data_frame ||
    type == vctrs_class_bare_tibble ||
    type == vctrs_class_data_frame;
}

// [[ include("type-data-frame.h") ]]
bool is_native_df(SEXP x) {
  enum vctrs_class_type type = class_type(x);
  return
    type == vctrs_class_bare_data_frame ||
    type == vctrs_class_bare_tibble;
}

// [[ include("type-data-frame.h") ]]
bool is_bare_data_frame(SEXP x) {
  return class_type(x) == vctrs_class_bare_data_frame;
}

// [[ include("type-data-frame.h") ]]
bool is_bare_tibble(SEXP x) {
  return class_type(x) == vctrs_class_bare_tibble;
}

// [[ include("type-data-frame.h") ]]
SEXP new_data_frame(SEXP x, R_len_t size) {
  x = PROTECT(r_maybe_duplicate(x));
  init_data_frame(x, size);

  UNPROTECT(1);
  return x;
}

static R_len_t df_size_from_list(SEXP x, SEXP size);
static SEXP c_data_frame_class(SEXP cls);

// [[ register() ]]
SEXP vctrs_new_data_frame(SEXP args) {
  args = CDR(args);

  SEXP x = CAR(args); args = CDR(args);
  SEXP size = CAR(args); args = CDR(args);
  SEXP cls = CAR(args); args = CDR(args);
  SEXP attrib = args;

  PROTECT_INDEX pi;
  PROTECT_WITH_INDEX(attrib, &pi);

  if (TYPEOF(x) != VECSXP) {
    Rf_errorcall(R_NilValue, "`.x` must be a list");
  }

  bool has_names = false;
  bool has_rownames = false;
  R_len_t x_size = df_size_from_list(x, size);

  SEXP out = PROTECT(r_maybe_duplicate(x));

  for (SEXP node = attrib; node != R_NilValue; node = CDR(node)) {
    SEXP tag = TAG(node);

    // We might add dynamic dots later on
    if (tag == R_ClassSymbol) {
      Rf_error("Internal error in `new_data_frame()`: Can't supply `class` in `...`.");
    }

    if (tag == R_NamesSymbol) {
      has_names = true;
      continue;
    }

    if (tag == R_RowNamesSymbol) {
      // "row.names" is checked for consistency with `.size` (if provided)
      if (x_size != rownames_size(CAR(node)) && size != R_NilValue) {
        Rf_errorcall(R_NilValue, "`.size` and `row.names` must be consistent.");
      }

      has_rownames = true;
      continue;
    }
  }

  // Take names from `x` if `attrib` doesn't have any
  if (!has_names) {
    SEXP nms = vctrs_shared_empty_chr;
    if (Rf_length(out)) {
      nms = r_names(out);
    }
    PROTECT(nms);

    if (nms != R_NilValue) {
      attrib = Rf_cons(nms, attrib);
      SET_TAG(attrib, R_NamesSymbol);
      REPROTECT(attrib, pi);
    }

    UNPROTECT(1);
  }

  if (!has_rownames) {
    SEXP rn = PROTECT(new_compact_rownames(x_size));
    attrib = Rf_cons(rn, attrib);
    SET_TAG(attrib, R_RowNamesSymbol);

    UNPROTECT(1);
    REPROTECT(attrib, pi);
  }

  if (cls == R_NilValue) {
    cls = classes_data_frame;
  } else {
    cls = c_data_frame_class(cls);
  }
  PROTECT(cls);

  attrib = Rf_cons(cls, attrib);
  SET_TAG(attrib, R_ClassSymbol);

  UNPROTECT(1);
  REPROTECT(attrib, pi);


  SET_ATTRIB(out, attrib);
  SET_OBJECT(out, 1);

  UNPROTECT(2);
  return out;
}

static R_len_t df_size_from_size(SEXP size);

static R_len_t df_size_from_list(SEXP x, SEXP size) {
  if (size == R_NilValue) {
    return df_raw_size_from_list(x);
  }
  return df_size_from_size(size);
}

static R_len_t df_size_from_size(SEXP size) {
  if (TYPEOF(size) != INTSXP || Rf_length(size) != 1) {
    Rf_errorcall(R_NilValue, "`.size` must be an integer of size 1");
  }

  return r_int_get(size, 0);
}

static SEXP c_data_frame_class(SEXP cls) {
  if (TYPEOF(cls) != STRSXP) {
    Rf_errorcall(R_NilValue, "`class` must be NULL or a character vector");
  }
  if (Rf_length(cls) == 0) {
    return classes_data_frame;
  }

  SEXP args = PROTECT(Rf_allocVector(VECSXP, 2));
  SET_VECTOR_ELT(args, 0, cls);
  SET_VECTOR_ELT(args, 1, classes_data_frame);

  SEXP out = vec_c(
    args,
    vctrs_shared_empty_chr,
    R_NilValue,
    NULL
  );

  UNPROTECT(1);
  return out;
}

// [[ include("type-data-frame.h") ]]
enum rownames_type rownames_type(SEXP x) {
  switch (TYPEOF(x)) {
  case STRSXP:
    return ROWNAMES_IDENTIFIERS;
  case INTSXP:
    if (Rf_length(x) == 2 && INTEGER(x)[0] == NA_INTEGER) {
      return ROWNAMES_AUTOMATIC_COMPACT;
    } else {
      return ROWNAMES_AUTOMATIC;
    }
  default:
    Rf_error("Corrupt data in `rownames_type()`: Unexpected type `%s`.",
             Rf_type2char(TYPEOF(x)));
  }
}

static R_len_t compact_rownames_length(SEXP x) {
  return abs(INTEGER(x)[1]);
}

// [[ include("type-data-frame.h") ]]
R_len_t rownames_size(SEXP rn) {
  switch (rownames_type(rn)) {
  case ROWNAMES_IDENTIFIERS:
  case ROWNAMES_AUTOMATIC:
    return Rf_length(rn);
  case ROWNAMES_AUTOMATIC_COMPACT:
    return compact_rownames_length(rn);
  }

  never_reached("rownames_size");
}

static void init_bare_data_frame(SEXP x, R_len_t size);

// [[ include("type-data-frame.h") ]]
void init_data_frame(SEXP x, R_len_t size) {
  Rf_setAttrib(x, R_ClassSymbol, classes_data_frame);
  init_bare_data_frame(x, size);
}
// [[ include("type-data-frame.h") ]]
void init_tibble(SEXP x, R_len_t size) {
  Rf_setAttrib(x, R_ClassSymbol, classes_tibble);
  init_bare_data_frame(x, size);
}

static void init_bare_data_frame(SEXP x, R_len_t size) {
  if (Rf_length(x) == 0) {
    Rf_setAttrib(x, R_NamesSymbol, vctrs_shared_empty_chr);
  }

  init_compact_rownames(x, size);
}

// [[ include("type-data-frame.h") ]]
void init_compact_rownames(SEXP x, R_len_t size) {
  SEXP rn = PROTECT(new_compact_rownames(size));
  Rf_setAttrib(x, R_RowNamesSymbol, rn);
  UNPROTECT(1);
}

static SEXP new_compact_rownames(R_len_t size) {
  if (size <= 0) {
    return vctrs_shared_empty_int;
  }

  SEXP out = Rf_allocVector(INTSXP, 2);
  int* out_data = INTEGER(out);
  out_data[0] = NA_INTEGER;
  out_data[1] = -size;
  return out;
}

// [[ include("type-data-frame.h") ]]
SEXP df_rownames(SEXP x) {
  // Required, because getAttrib() already does the transformation to a vector,
  // and getAttrib0() is hidden
  SEXP node = ATTRIB(x);

  while (node != R_NilValue) {
    SEXP tag = TAG(node);

    if (tag == R_RowNamesSymbol) {
      return CAR(node);
    }

    node = CDR(node);
  }

  return R_NilValue;
}


// vctrs type methods ------------------------------------------------

// [[ register() ]]
SEXP vctrs_df_ptype2(SEXP x, SEXP y, SEXP x_arg, SEXP y_arg) {
  struct vctrs_arg x_arg_ = vec_as_arg(x_arg);
  struct vctrs_arg y_arg_ = vec_as_arg(y_arg);;

  return df_ptype2(x, y, &x_arg_, &y_arg_);
}

static SEXP df_ptype2_impl(SEXP x, SEXP y, SEXP x_names, SEXP y_names,
                           struct vctrs_arg* x_arg, struct vctrs_arg* y_arg);

static SEXP df_ptype2_sequential(SEXP x, SEXP y, SEXP names,
                                 struct vctrs_arg* x_arg, struct vctrs_arg* y_arg);

// [[ include("vctrs.h") ]]
SEXP df_ptype2(SEXP x, SEXP y, struct vctrs_arg* x_arg, struct vctrs_arg* y_arg) {
  SEXP x_names = PROTECT(r_names(x));
  SEXP y_names = PROTECT(r_names(y));

  SEXP out;

  if (equal_object(x_names, y_names)) {
    out = df_ptype2_sequential(x, y, x_names, x_arg, y_arg);
  } else {
    out = df_ptype2_impl(x, y, x_names, y_names, x_arg, y_arg);
  }

  UNPROTECT(2);
  return out;
}

SEXP df_ptype2_impl(SEXP x, SEXP y, SEXP x_names, SEXP y_names,
                    struct vctrs_arg* x_arg, struct vctrs_arg* y_arg) {
  SEXP x_dups_pos = PROTECT(vec_match(x_names, y_names));
  SEXP y_dups_pos = PROTECT(vec_match(y_names, x_names));

  int* x_dups_pos_data = INTEGER(x_dups_pos);
  int* y_dups_pos_data = INTEGER(y_dups_pos);

  R_len_t x_len = Rf_length(x_names);
  R_len_t y_len = Rf_length(y_names);

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

    struct arg_data_index x_arg_data = new_index_arg_data(r_chr_get_c_string(x_names, i), x_arg);
    struct vctrs_arg named_x_arg = new_index_arg(x_arg, &x_arg_data);

    SEXP type;
    if (dup == NA_INTEGER) {
      type = vec_ptype(VECTOR_ELT(x, i), &named_x_arg);
    } else {
      --dup; // 1-based index

      struct arg_data_index y_arg_data = new_index_arg_data(r_chr_get_c_string(y_names, dup), y_arg);
      struct vctrs_arg named_y_arg = new_index_arg(y_arg, &y_arg_data);

      int _left;

      type = vec_ptype2(VECTOR_ELT(x, i),
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
      struct arg_data_index y_arg_data = new_index_arg_data(r_chr_get_c_string(y_names, j), y_arg);
      struct vctrs_arg named_y_arg = new_index_arg(y_arg, &y_arg_data);

      SET_VECTOR_ELT(out, i, vec_ptype(VECTOR_ELT(y, j), &named_y_arg));
      SET_STRING_ELT(nms, i, STRING_ELT(y_names, j));
      ++i;
    }
  }

  init_data_frame(out, 0);

  UNPROTECT(4);
  return out;
}

SEXP df_ptype2_sequential(SEXP x, SEXP y, SEXP names,
                          struct vctrs_arg* x_arg, struct vctrs_arg* y_arg) {
  R_len_t len = Rf_length(names);

  SEXP out = PROTECT(Rf_allocVector(VECSXP, len));
  Rf_setAttrib(out, R_NamesSymbol, names);

  for (R_len_t i = 0; i < len; ++i) {
    const char* name = r_chr_get_c_string(names, i);

    struct arg_data_index x_arg_data = new_index_arg_data(name, x_arg);
    struct arg_data_index y_arg_data = new_index_arg_data(name, y_arg);
    struct vctrs_arg named_x_arg = new_index_arg(x_arg, &x_arg_data);
    struct vctrs_arg named_y_arg = new_index_arg(y_arg, &y_arg_data);

    int _left;

    SEXP type = vec_ptype2(VECTOR_ELT(x, i),
                           VECTOR_ELT(y, i),
                           &named_x_arg,
                           &named_y_arg,
                           &_left);

    SET_VECTOR_ELT(out, i, type);
  }

  init_data_frame(out, 0);

  UNPROTECT(1);
  return out;
}

// [[ register() ]]
SEXP vctrs_df_cast(SEXP x, SEXP to, SEXP x_arg_, SEXP to_arg_) {
  struct vctrs_arg x_arg = vec_as_arg(x_arg_);
  struct vctrs_arg to_arg = vec_as_arg(to_arg_);;

  return df_cast(x, to, &x_arg, &to_arg);
}

// Take all columns of `to` and preserve the order. Common columns are
// cast to their types in `to`. Extra `x` columns are dropped and
// cause a lossy cast. Extra `to` columns are filled with missing
// values.
// [[ include(c("type-data-frame.h", "cast.h")) ]]
SEXP df_cast(SEXP x, SEXP to, struct vctrs_arg* x_arg, struct vctrs_arg* to_arg) {
  SEXP x_names = PROTECT(r_names(x));
  SEXP to_names = PROTECT(r_names(to));

  if (x_names == R_NilValue || to_names == R_NilValue) {
    Rf_error("Internal error in `df_cast()`: Data frame must have names.");
  }

  SEXP to_dups_pos = PROTECT(vec_match(to_names, x_names));
  int* to_dups_pos_data = INTEGER(to_dups_pos);

  R_len_t to_len = Rf_length(to_dups_pos);
  SEXP out = PROTECT(Rf_allocVector(VECSXP, to_len));
  Rf_setAttrib(out, R_NamesSymbol, to_names);

  R_len_t size = df_size(x);
  R_len_t common_len = 0;

  for (R_len_t i = 0; i < to_len; ++i) {
    R_len_t pos = to_dups_pos_data[i];

    SEXP col;
    if (pos == NA_INTEGER) {
      col = vec_init(VECTOR_ELT(to, i), size);
    } else {
      --pos; // 1-based index
      struct arg_data_index x_arg_data = new_index_arg_data(r_chr_get_c_string(x_names, pos), x_arg);
      struct arg_data_index to_arg_data = new_index_arg_data(r_chr_get_c_string(to_names, i), to_arg);
      struct vctrs_arg named_x_arg = new_index_arg(x_arg, &x_arg_data);
      struct vctrs_arg named_to_arg = new_index_arg(to_arg, &to_arg_data);
      ++common_len;
      col = vec_cast(VECTOR_ELT(x, pos), VECTOR_ELT(to, i), &named_x_arg, &named_to_arg);
    }

    SET_VECTOR_ELT(out, i, col);
  }

  // Restore data frame size before calling `vec_restore()`. `x` and
  // `to` might not have any columns to compute the original size.
  init_data_frame(out, size);
  Rf_setAttrib(out, R_RowNamesSymbol, df_rownames(x));

  R_len_t extra_len = Rf_length(x) - common_len;
  if (extra_len) {
    out = vctrs_dispatch3(syms_df_lossy_cast, fns_df_lossy_cast,
                          syms_out, out,
                          syms_x, x,
                          syms_to, to);
  }

  UNPROTECT(4);
  return out;
}



// If negative index, value is appended
SEXP df_poke(SEXP x, R_len_t i, SEXP value) {
  if (i >= 0) {
    SET_VECTOR_ELT(x, i, value);
    return x;
  }

  R_len_t ncol = Rf_length(x);

  SEXP tmp = PROTECT(r_resize(x, ncol + 1));
  Rf_copyMostAttrib(x, tmp);
  x = tmp;

  SET_VECTOR_ELT(x, ncol, value);

  UNPROTECT(1);
  return x;
}
SEXP df_poke_at(SEXP x, SEXP name, SEXP value) {
  SEXP names = PROTECT(r_names(x));
  R_len_t i = r_chr_find(names, name);
  UNPROTECT(1);

  x = PROTECT(df_poke(x, i, value));

  if (i < 0) {
    SEXP names = PROTECT(r_names(x));
    SET_STRING_ELT(names, Rf_length(x) - 1, name);
    UNPROTECT(1);
  }

  UNPROTECT(1);
  return x;
}

// [[ include("type-data-frame.h") ]]
R_len_t df_flat_width(SEXP x) {
  R_len_t n = Rf_length(x);
  R_len_t out = n;

  for (R_len_t i = 0; i < n; ++i) {
    SEXP col = VECTOR_ELT(x, i);
    if (is_data_frame(col)) {
      out = out + df_flat_width(col) - 1;
    }
  }

  return out;
}

// [[ register() ]]
SEXP vctrs_df_flat_width(SEXP x) {
  return r_int(df_flat_width(x));
}


static R_len_t df_flatten_loop(SEXP x, SEXP out, SEXP out_names, R_len_t counter);

// Might return duplicate names. Currently only used for equality
// proxy so this doesn't matter. A less bare bone version would repair
// names.
//
// [[ register(); include("type-data-frame.h") ]]
SEXP df_flatten(SEXP x) {
  R_len_t width = df_flat_width(x);
  SEXP out = PROTECT(Rf_allocVector(VECSXP, width));
  SEXP out_names = PROTECT(Rf_allocVector(STRSXP, width));
  r_poke_names(out, out_names);

  df_flatten_loop(x, out, out_names, 0);
  init_data_frame(out, df_size(x));

  UNPROTECT(2);
  return out;
}

static R_len_t df_flatten_loop(SEXP x, SEXP out, SEXP out_names, R_len_t counter) {
  R_len_t n = Rf_length(x);
  SEXP x_names = PROTECT(r_names(x));

  for (R_len_t i = 0; i < n; ++i) {
    SEXP col = VECTOR_ELT(x, i);

    if (is_data_frame(col)) {
      counter = df_flatten_loop(col, out, out_names, counter);
    } else {
      SET_VECTOR_ELT(out, counter, col);
      SET_STRING_ELT(out_names, counter, STRING_ELT(x_names, i));
      ++counter;
    }
  }

  UNPROTECT(1);
  return counter;
}

SEXP df_repair_names(SEXP x, struct name_repair_opts* name_repair) {
  SEXP nms = PROTECT(r_names(x));
  SEXP repaired = PROTECT(vec_as_names(nms, name_repair));

  // Should this go through proxy and restore so that classes can
  // update metadata and check invariants when special columns are
  // renamed?
  if (nms != repaired) {
    x = PROTECT(r_maybe_duplicate(x));
    r_poke_names(x, repaired);
    UNPROTECT(1);
  }

  UNPROTECT(2);
  return x;
}


void vctrs_init_type_data_frame(SEXP ns) {
  syms_df_lossy_cast = Rf_install("df_lossy_cast");
  fns_df_lossy_cast = Rf_findVar(syms_df_lossy_cast, ns);
}
