#include "vctrs.h"
#include "utils.h"


static SEXP vec_rbind(SEXP xs, SEXP ptype, SEXP id, enum name_repair_arg name_repair);
static SEXP as_df_row(SEXP x, enum name_repair_arg name_repair, bool quiet);
static SEXP as_df_row_impl(SEXP x, enum name_repair_arg name_repair, bool quiet);
enum name_repair_arg validate_bind_name_repair(SEXP name_repair, bool allow_minimal);

// [[ register(external = TRUE) ]]
SEXP vctrs_rbind(SEXP call, SEXP op, SEXP args, SEXP env) {
  args = CDR(args);

  SEXP xs = PROTECT(rlang_env_dots_list(env));
  SEXP ptype = PROTECT(Rf_eval(CAR(args), env)); args = CDR(args);
  SEXP names_to = PROTECT(Rf_eval(CAR(args), env)); args = CDR(args);
  SEXP name_repair = PROTECT(Rf_eval(CAR(args), env));

  if (names_to != R_NilValue) {
    if (!r_is_string(names_to)) {
      Rf_errorcall(R_NilValue, "`.names_to` must be `NULL` or a string.");
    }
    names_to = r_chr_get(names_to, 0);
  }

  enum name_repair_arg repair_arg = validate_bind_name_repair(name_repair, false);
  SEXP out = vec_rbind(xs, ptype, names_to, repair_arg);

  UNPROTECT(4);
  return out;
}


// From type.c
SEXP vctrs_type_common_impl(SEXP dots, SEXP ptype);

static SEXP vec_rbind(SEXP xs, SEXP ptype, SEXP names_to, enum name_repair_arg name_repair) {
  int nprot = 0;
  R_len_t n = Rf_length(xs);

  for (R_len_t i = 0; i < n; ++i) {
    SET_VECTOR_ELT(xs, i, as_df_row(VECTOR_ELT(xs, i), name_repair, false));
  }

  // The common type holds information about common column names,
  // types, etc. Each element of `xs` needs to be cast to that type
  // before assignment.
  ptype = PROTECT_N(vctrs_type_common_impl(xs, ptype), &nprot);

  if (ptype == R_NilValue) {
    UNPROTECT(nprot);
    return new_data_frame(vctrs_shared_empty_list, 0);
  }
  if (TYPEOF(ptype) == LGLSXP && !Rf_length(ptype)) {
    ptype = as_df_row_impl(vctrs_shared_na_lgl, name_repair, false);
    PROTECT_N(ptype, &nprot);
  }
  if (!is_data_frame(ptype)) {
    Rf_errorcall(R_NilValue, "Can't bind objects that are not coercible to a data frame.");
  }

  // Find individual input sizes and total size of output
  R_len_t nrow = 0;

  SEXP ns_placeholder = PROTECT_N(Rf_allocVector(INTSXP, n), &nprot);
  int* ns = INTEGER(ns_placeholder);

  for (R_len_t i = 0; i < n; ++i) {
    SEXP elt = VECTOR_ELT(xs, i);
    R_len_t size = (elt == R_NilValue) ? 0 : vec_size(elt);
    nrow += size;
    ns[i] = size;
  }

  SEXP out = PROTECT_N(vec_init(ptype, nrow), &nprot);
  SEXP idx = PROTECT_N(compact_seq(0, 0, true), &nprot);
  int* idx_ptr = INTEGER(idx);

  SEXP names_to_col = R_NilValue;
  SEXPTYPE names_to_type = 99;
  void* names_to_p = NULL;
  const void* index_p = NULL;

  if (names_to != R_NilValue) {
    SEXP index = PROTECT_N(r_names(xs), &nprot);
    if (index == R_NilValue) {
      index = PROTECT_N(Rf_allocVector(INTSXP, n), &nprot);
      r_int_fill_seq(index, 1, n);
    }
    index_p = r_vec_const_deref(index);

    names_to_type = TYPEOF(index);
    names_to_col = PROTECT_N(Rf_allocVector(names_to_type, nrow), &nprot);
    names_to_p = r_vec_deref(names_to_col);
  }

  // Compact sequences use 0-based counters
  R_len_t counter = 0;

  for (R_len_t i = 0; i < n; ++i) {
    R_len_t size = ns[i];
    if (!size) {
      continue;
    }

    SEXP tbl = PROTECT(vec_cast(VECTOR_ELT(xs, i), ptype, args_empty, args_empty));
    init_compact_seq(idx_ptr, counter, size, true);
    df_assign(out, idx, tbl, false);

    // Assign current name to group vector, if supplied
    if (names_to != R_NilValue) {
      r_vec_fill(names_to_type, names_to_p, index_p, i, size);
      r_vec_ptr_inc(names_to_type, &names_to_p, size);
    }

    counter += size;
    UNPROTECT(1);
  }

  if (names_to != R_NilValue) {
    out = df_poke_at(out, names_to, names_to_col);
  }

  UNPROTECT(nprot);
  return out;
}

static SEXP as_df_row(SEXP x, enum name_repair_arg name_repair, bool quiet) {
  if (vec_is_unspecified(x) && r_names(x) == R_NilValue) {
    return x;
  } else {
    return as_df_row_impl(x, name_repair, quiet);
  }
}

static SEXP as_df_row_impl(SEXP x, enum name_repair_arg name_repair, bool quiet) {
  if (x == R_NilValue) {
    return x;
  }
  if (is_data_frame(x)) {
    return x;
  }

  int nprot = 0;

  R_len_t ndim = vec_dim_n(x);
  if (ndim > 2) {
    Rf_errorcall(R_NilValue, "Can't bind arrays.");
  }
  if (ndim == 2) {
    SEXP names = PROTECT_N(vec_unique_colnames(x, false), &nprot);
    SEXP out = PROTECT_N(r_as_data_frame(x), &nprot);
    r_poke_names(out, names);
    UNPROTECT(nprot);
    return out;
  }

  SEXP nms = PROTECT_N(vec_names(x), &nprot);

  // Remove names as they are promoted to data frame column names
  if (nms != R_NilValue) {
    x = PROTECT_N(r_maybe_duplicate(x), &nprot);
    r_poke_names(x, R_NilValue);
  }

  if (nms == R_NilValue) {
    nms = PROTECT_N(vec_unique_names(x, quiet), &nprot);
  } else {
    nms = PROTECT_N(vec_as_names(nms, name_repair, quiet), &nprot);
  }

  x = PROTECT_N(vec_chop(x, R_NilValue), &nprot);

  r_poke_names(x, nms);

  x = new_data_frame(x, 1);

  UNPROTECT(nprot);
  return x;
}

// [[ register() ]]
SEXP vctrs_as_df_row(SEXP x, SEXP quiet) {
  return as_df_row(x, name_repair_unique, LOGICAL(quiet)[0]);
}

static SEXP as_df_col(SEXP x, SEXP outer, bool* allow_pack);
static SEXP vec_cbind(SEXP xs, SEXP ptype, SEXP size, enum name_repair_arg name_repair);
static SEXP cbind_container_type(SEXP x);

// [[ register(external = TRUE) ]]
SEXP vctrs_cbind(SEXP call, SEXP op, SEXP args, SEXP env) {
  args = CDR(args);

  SEXP xs = PROTECT(rlang_env_dots_list(env));
  SEXP ptype = PROTECT(Rf_eval(CAR(args), env)); args = CDR(args);
  SEXP size = PROTECT(Rf_eval(CAR(args), env)); args = CDR(args);
  SEXP name_repair = PROTECT(Rf_eval(CAR(args), env));

  enum name_repair_arg repair_arg = validate_bind_name_repair(name_repair, true);
  SEXP out = vec_cbind(xs, ptype, size, repair_arg);

  UNPROTECT(4);
  return out;
}

static SEXP vec_cbind(SEXP xs, SEXP ptype, SEXP size, enum name_repair_arg name_repair) {
  R_len_t n = Rf_length(xs);

  // Find the common container type of inputs
  SEXP containers = PROTECT(map(xs, &cbind_container_type));
  ptype = PROTECT(cbind_container_type(ptype));

  SEXP type = PROTECT(tbl_ptype_common(containers, ptype));
  if (type == R_NilValue) {
    type = new_data_frame(vctrs_shared_empty_list, 0);
  } else if (!is_data_frame(type)) {
    type = r_as_data_frame(type);
  }
  UNPROTECT(1);
  PROTECT(type);


  R_len_t nrow;
  if (size == R_NilValue) {
    nrow = vec_size_common(xs, 0);
  } else {
    nrow = size_validate(size, ".size");
  }


  // Convert inputs to data frames, validate, and collect total number of columns
  SEXP xs_names = PROTECT(r_names(xs));
  bool has_names = xs_names != R_NilValue;
  SEXP* xs_names_p = has_names ? STRING_PTR(xs_names) : NULL;

  R_len_t ncol = 0;
  for (R_len_t i = 0; i < n; ++i) {
    SEXP x = VECTOR_ELT(xs, i);

    if (x == R_NilValue) {
      continue;
    }

    x = PROTECT(vec_recycle(x, nrow));

    SEXP outer_name = has_names ? xs_names_p[i] : strings_empty;
    bool allow_packing;
    x = PROTECT(as_df_col(x, outer_name, &allow_packing));

    // Remove outer name of column vectors because they shouldn't be repacked
    if (has_names && !allow_packing) {
      SET_STRING_ELT(xs_names, i, strings_empty);
    }

    SET_VECTOR_ELT(xs, i, x);
    UNPROTECT(2);

    // Named inputs are packed in a single column
    R_len_t x_ncol = outer_name == strings_empty ? Rf_length(x) : 1;
    ncol += x_ncol;
  }


  // Fill in columns
  SEXP out = PROTECT(Rf_allocVector(VECSXP, ncol));
  SEXP names = PROTECT(Rf_allocVector(STRSXP, ncol));

  SEXP idx = PROTECT(compact_seq(0, 0, true));
  int* idx_ptr = INTEGER(idx);

  R_len_t counter = 0;

  for (R_len_t i = 0; i < n; ++i) {
    SEXP x = VECTOR_ELT(xs, i);

    if (x == R_NilValue) {
      continue;
    }

    SEXP outer_name = has_names ? xs_names_p[i] : strings_empty;
    if (outer_name != strings_empty) {
      SET_VECTOR_ELT(out, counter, x);
      SET_STRING_ELT(names, counter, outer_name);
      ++counter;
      continue;
    }

    R_len_t xn = Rf_length(x);
    init_compact_seq(idx_ptr, counter, xn, true);
    list_assign(out, idx, x, false);

    SEXP xnms = PROTECT(r_names(x));
    if (xnms != R_NilValue) {
      chr_assign(names, idx, xnms, false);
    }

    counter += xn;
    UNPROTECT(1);
  }

  names = PROTECT(vec_as_names(names, name_repair, false));
  Rf_setAttrib(out, R_NamesSymbol, names);

  out = vec_restore(out, type, R_NilValue);

  UNPROTECT(8);
  return out;
}

static SEXP cbind_container_type(SEXP x) {
  if (is_data_frame(x)) {
    return df_container_type(x);
  } else {
    return R_NilValue;
  }
}


static SEXP shaped_as_df_col(SEXP x, SEXP outer);
static SEXP vec_as_df_col(SEXP x, SEXP outer);

// [[ register() ]]
SEXP vctrs_as_df_col(SEXP x, SEXP outer) {
  bool allow_pack;
  return as_df_col(x, r_chr_get(outer, 0), &allow_pack);
}
static SEXP as_df_col(SEXP x, SEXP outer, bool* allow_pack) {
  if (is_data_frame(x)) {
    *allow_pack = true;
    return Rf_shallow_duplicate(x);
  }

  R_len_t ndim = vec_bare_dim_n(x);
  if (ndim > 2) {
    Rf_errorcall(R_NilValue, "Can't bind arrays.");
  }
  if (ndim > 0) {
    *allow_pack = true;
    return shaped_as_df_col(x, outer);
  }

  *allow_pack = false;
  return vec_as_df_col(x, outer);
}

static SEXP shaped_as_df_col(SEXP x, SEXP outer) {
  // If packed, store array as a column
  if (outer != strings_empty) {
    return x;
  }

  // If unpacked, transform to data frame first. We repair names
  // after unpacking and concatenation.
  SEXP out = PROTECT(r_as_data_frame(x));

  // Remove names if they were repaired by `as.data.frame()`
  if (colnames(x) == R_NilValue) {
    r_poke_names(out, R_NilValue);
  }

  UNPROTECT(1);
  return out;
}

static SEXP vec_as_df_col(SEXP x, SEXP outer) {
  SEXP out = PROTECT(Rf_allocVector(VECSXP, 1));
  SET_VECTOR_ELT(out, 0, x);

  if (outer != strings_empty) {
    SEXP names = PROTECT(r_str_as_character(outer));
    Rf_setAttrib(out, R_NamesSymbol, names);
    UNPROTECT(1);
  }

  init_data_frame(out, Rf_length(x));

  UNPROTECT(1);
  return out;
}

enum name_repair_arg validate_bind_name_repair(SEXP name_repair, bool allow_minimal) {
  enum name_repair_arg arg = validate_name_repair(name_repair);

  switch (arg) {
  case name_repair_unique: break;
  case name_repair_universal: break;
  case name_repair_check_unique: break;
  case name_repair_minimal: if (allow_minimal) break;
  default:
    if (allow_minimal) {
      Rf_errorcall(R_NilValue,
                   "`.name_repair` can't be `\"%s\"`.\n"
                   "It must be one of `\"unique\"`, `\"universal\"`, `\"check_unique\"`, or `\"minimal\"`.",
                   name_repair_arg_as_c_string(arg));
    } else {
      Rf_errorcall(R_NilValue,
                   "`.name_repair` can't be `\"%s\"`.\n"
                   "It must be one of `\"unique\"`, `\"universal\"`, or `\"check_unique\"`.",
                   name_repair_arg_as_c_string(arg));
    }
  }

  return arg;
}
