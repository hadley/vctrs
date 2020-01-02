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

  SEXP proxy = PROTECT_N(vec_proxy(ptype), &nprot);

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

  SEXP out = PROTECT_N(vec_init(proxy, nrow), &nprot);
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

    // First cast, then take proxy, to give a chance to proxy methods
    // to instrument elements (e.g. wrap in data frame columns with meta-data)
    SEXP tbl = PROTECT(vec_cast(VECTOR_ELT(xs, i), ptype, args_empty, args_empty));
    tbl = PROTECT(vec_proxy(tbl));

    init_compact_seq(idx_ptr, counter, size, true);
    df_assign(out, idx, tbl, false);

    // Assign current name to group vector, if supplied
    if (names_to != R_NilValue) {
      r_vec_fill(names_to_type, names_to_p, index_p, i, size);
      r_vec_ptr_inc(names_to_type, &names_to_p, size);
    }

    counter += size;
    UNPROTECT(2);
  }

  if (names_to != R_NilValue) {
    out = PROTECT_N(df_poke_at(out, names_to, names_to_col), &nprot);
  }

  out = vec_restore(out, ptype, R_NilValue);

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

static SEXP as_df_col(SEXP x, SEXP outer);
static SEXP vec_cbind(SEXP xs, SEXP ptype, SEXP size, enum name_repair_arg name_repair);

// [[ register(external = TRUE) ]]
SEXP vctrs_cbind(SEXP call, SEXP op, SEXP args, SEXP env) {
  args = CDR(args);

  SEXP xs = PROTECT(rlang_env_dots_list(env));
  SEXP ptype = PROTECT(Rf_eval(CAR(args), env)); args = CDR(args);
  SEXP size = PROTECT(Rf_eval(CAR(args), env)); args = CDR(args);
  SEXP name_repair = PROTECT(Rf_eval(CAR(args), env));

  enum name_repair_arg repair_arg = validate_bind_name_repair(name_repair, true);
  SEXP out = vec_cbind(xs, ptype, size, repair_arg);

  // Reset visibility
  Rf_eval(R_NilValue, R_EmptyEnv);

  UNPROTECT(4);
  return out;
}

static SEXP vec_cbind(SEXP xs, SEXP ptype, SEXP size, enum name_repair_arg name_repair) {
  R_len_t n = Rf_length(xs);

  // Transform all inputs to data frames. Inputs are packed in a
  // df-col if a name was supplied.
  SEXP dfs;
  SEXP xs_names = PROTECT(Rf_shallow_duplicate(r_names(xs)));
  dfs = PROTECT(map2(xs, xs_names, &as_df_col));

  PROTECT_INDEX ptype_pi;
  PROTECT_WITH_INDEX(ptype, &ptype_pi);

  if (ptype != R_NilValue) {
    // FIXME: is this needed?
    ptype = as_df_col(ptype, R_NilValue);
    REPROTECT(ptype, ptype_pi);
  }
  ptype = PROTECT(tbl_ptype_common(dfs, ptype));

  if (ptype == R_NilValue) {
    ptype = new_data_frame(vctrs_shared_empty_list, 0);
  } else if (!is_data_frame(ptype)) {
    ptype = r_as_data_frame(ptype);
  }
  REPROTECT(ptype, ptype_pi);

  R_len_t n_rows;
  if (size == R_NilValue) {
    n_rows = vec_size_common(xs, 0);
  } else {
    n_rows = size_validate(size, ".size");
  }

  ptype = vec_recycle(ptype, n_rows);
  REPROTECT(ptype, ptype_pi);

  SEXP proxies = PROTECT(Rf_allocVector(VECSXP, n));

  PROTECT_INDEX vcols_pi;
  SEXP vcols = R_NilValue;
  PROTECT_WITH_INDEX(vcols, &vcols_pi);

  R_len_t n_cols = 0;
  R_len_t n_vcols = 0;

  // In the first pass we cast to the common tabular type and collect
  // the number of columns of the output and the table proxies
  for (R_len_t i = 0; i < n; ++i) {
    SEXP df = VECTOR_ELT(dfs, i);
    if (df == R_NilValue) {
      continue;
    }

    df = PROTECT(tbl_cast(df, ptype, args_empty, args_empty));

    SEXP proxy = PROTECT(vec_proxy(df));
    SEXP proxy_vcols = PROTECT(vec_proxy_pop_vcols(&proxy));
    PROTECT(proxy);

    if (proxy_vcols != R_NilValue) {
      r_node_push(&vcols, proxy_vcols, vcols_pi);
      n_vcols += Rf_length(proxy_vcols);
    }

    SET_VECTOR_ELT(proxies, i, proxy);

    n_cols += Rf_length(proxy);
    UNPROTECT(4);
  }

  if (vcols != R_NilValue) {
    n_cols += 1;
  }

  // Fill in columns
  SEXP out = PROTECT(Rf_allocVector(VECSXP, n_cols));
  SEXP names = PROTECT(Rf_allocVector(STRSXP, n_cols));

  SEXP idx = PROTECT(compact_seq(0, 0, true));
  int* idx_ptr = INTEGER(idx);

  R_len_t counter = 0;

  for (R_len_t i = 0; i < n; ++i) {
    SEXP proxy = VECTOR_ELT(proxies, i);
    if (proxy == R_NilValue) {
      continue;
    }

    R_len_t proxy_n = Rf_length(proxy);

    init_compact_seq(idx_ptr, counter, proxy_n, true);
    list_assign(out, idx, proxy, false);

    SEXP proxy_names = PROTECT(r_names(proxy));
    if (proxy_names != R_NilValue) {
      chr_assign(names, idx, proxy_names, false);
    }
    UNPROTECT(1);

    counter += proxy_n;
  }

  if (vcols != R_NilValue) {
    vcols = r_node_reverse(vcols);
    REPROTECT(vcols, vcols_pi);

    SEXP new_vcols = PROTECT(Rf_allocVector(VECSXP, n_vcols));
    SEXP new_vcols_names = PROTECT(Rf_allocVector(STRSXP, n_vcols));
    Rf_setAttrib(new_vcols, R_NamesSymbol, new_vcols_names);
    init_data_frame(new_vcols, n_rows);

    R_len_t i = 0;
    while (vcols != R_NilValue) {
      SEXP vcols_elt = CAR(vcols);
      SEXP vcols_elt_names = PROTECT(r_names(vcols_elt));

      R_len_t n_elt = Rf_length(vcols_elt);
      for (R_len_t j = 0; j < n_elt; ++j) {
        SET_VECTOR_ELT(new_vcols, i, VECTOR_ELT(vcols_elt, j));
        SET_STRING_ELT(new_vcols_names, i, STRING_ELT(vcols_elt_names, j));
        ++i;
      }

      UNPROTECT(1);
      vcols = CDR(vcols);
    }

    SET_VECTOR_ELT(out, n_cols - 1, new_vcols);
    SET_STRING_ELT(names, n_cols - 1, strings_vcols);
    UNPROTECT(2);
  }

  names = PROTECT(vec_as_names(names, name_repair, false));
  Rf_setAttrib(out, R_NamesSymbol, names);
  out = vec_restore(out, ptype, R_NilValue);

  UNPROTECT(10);
  return out;
}

static SEXP packed_as_df_col(SEXP x, SEXP outer);
static SEXP mat_as_data_frame(SEXP x, SEXP outer);
static SEXP vec_as_df_col(SEXP x, SEXP outer);

// [[ register() ]]
SEXP vctrs_as_df_col(SEXP x, SEXP outer) {
  return as_df_col(x, r_chr_get(outer, 0));
}
static SEXP as_df_col(SEXP x, SEXP outer) {
  if (x == R_NilValue) {
    return x;
  }

  bool is_df = is_data_frame(x);
  R_len_t ndim = vec_bare_dim_n(x);
  bool is_vec = !is_df && ndim == 0;

  if (ndim > 2) {
    Rf_errorcall(R_NilValue, "Can't bind arrays.");
  }

  // Packed inputs, including matrices and data frames, are stored as column
  outer = (outer == R_NilValue) ? strings_empty : outer;
  bool packed = (outer != strings_empty);

  if (is_vec || packed) {
    if (is_df || ndim > 0) {
      return packed_as_df_col(x, outer);
    } else {
      return vec_as_df_col(x, outer);
    }
  } else {
    if (is_df) {
      return x;
    } else if (ndim > 0) {
      return mat_as_data_frame(x, outer);
    } else {
      Rf_error("Internal error: Bare vector should have been packed.");
    }
  }
}

static SEXP mat_as_data_frame(SEXP x, SEXP outer) {
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

static SEXP packed_as_df_col(SEXP x, SEXP outer) {
  SEXP out = PROTECT(Rf_allocVector(VECSXP, 1));
  SET_VECTOR_ELT(out, 0, x);

  SEXP names = PROTECT(r_str_as_character(outer));
  Rf_setAttrib(out, R_NamesSymbol, names);

  init_data_frame(out, vec_size(x));

  UNPROTECT(2);
  return out;
}

static SEXP vec_as_df_col(SEXP x, SEXP outer) {
  SEXP out = PROTECT(Rf_allocVector(VECSXP, 1));
  SET_VECTOR_ELT(out, 0, x);

  SEXP names = PROTECT(r_str_as_character(outer));
  Rf_setAttrib(out, R_NamesSymbol, names);

  init_data_frame(out, Rf_length(x));

  UNPROTECT(2);
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
