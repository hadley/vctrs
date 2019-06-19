#include "vctrs.h"
#include "utils.h"

// From type.c
SEXP vctrs_type_common_impl(SEXP dots, SEXP ptype);

// From slice-assign.c
SEXP vec_assign_impl(SEXP proxy, SEXP index, SEXP value, bool clone);

static SEXP vec_c(SEXP xs, SEXP ptype, enum name_repair_arg name_repair);
static bool list_has_inner_names(SEXP xs);


// [[ register(external = TRUE) ]]
SEXP vctrs_c(SEXP call, SEXP op, SEXP args, SEXP env) {
  args = CDR(args);

  SEXP xs = PROTECT(rlang_env_dots_list(env));
  SEXP ptype = PROTECT(Rf_eval(CAR(args), env)); args = CDR(args);
  SEXP name_repair = PROTECT(Rf_eval(CAR(args), env));

  enum name_repair_arg repair_arg = validate_name_repair(name_repair);
  SEXP out = vec_c(xs, ptype, repair_arg);

  UNPROTECT(3);
  return out;
}

static SEXP vec_c(SEXP xs, SEXP ptype, enum name_repair_arg name_repair) {
  R_len_t n = Rf_length(xs);

  ptype = PROTECT(vctrs_type_common_impl(xs, ptype));

  if (ptype == R_NilValue) {
    UNPROTECT(1);
    return R_NilValue;
  }

  // Find individual input sizes and total size of output
  R_len_t out_size = 0;

  SEXP ns_placeholder = PROTECT(Rf_allocVector(INTSXP, n));
  int* ns = INTEGER(ns_placeholder);

  for (R_len_t i = 0; i < n; ++i) {
    SEXP elt = VECTOR_ELT(xs, i);
    R_len_t size = (elt == R_NilValue) ? 0 : vec_size(elt);
    out_size += size;
    ns[i] = size;
  }

  PROTECT_INDEX out_pi;
  SEXP out = vec_na(ptype, out_size);
  PROTECT_WITH_INDEX(out, &out_pi);
  out = vctrs_vec_proxy(out);
  REPROTECT(out, out_pi);

  SEXP idx = PROTECT(compact_seq(0, 0));
  int* idx_ptr = INTEGER(idx);

  SEXP xs_names = PROTECT(r_names(xs));
  bool has_names = xs_names != R_NilValue || list_has_inner_names(xs);
  SEXP out_names = has_names ? Rf_allocVector(STRSXP, out_size) : R_NilValue;
  PROTECT(out_names);

  bool is_shaped = has_dim(ptype);

  // Compact sequences use 0-based counters
  R_len_t counter = 0;

  for (R_len_t i = 0; i < n; ++i) {
    R_len_t size = ns[i];
    if (!size) {
      continue;
    }

    // TODO
    SEXP x = VECTOR_ELT(xs, i);
    SEXP elt = PROTECT(vec_cast(x, ptype, args_empty, args_empty));

    init_compact_seq(idx_ptr, counter, counter + size);

    if (is_shaped) {
      SEXP idx = PROTECT(r_seq(counter + 1, counter + size + 1));
      out = vctrs_vec_assign(out, idx, elt);
      REPROTECT(out, out_pi);
      UNPROTECT(1);
    } else {
      vec_assign_impl(out, idx, elt, false);
    }

    if (has_names) {
      SEXP outer = xs_names == R_NilValue ? R_NilValue : STRING_ELT(xs_names, i);
      SEXP x_nms = outer_names(PROTECT(vctrs_vec_names(x)), outer, size);
      if (x_nms != R_NilValue) {
        vec_assign_impl(out_names, idx, x_nms, false);
      }
      UNPROTECT(1);
    }

    counter += size;
    UNPROTECT(1);
  }

  if (has_names) {
    out_names = PROTECT(vec_as_names(out_names, name_repair, false));

    if (is_shaped) {
      out = set_rownames(out, out_names);
      REPROTECT(out, out_pi);
    } else {
      Rf_setAttrib(out, R_NamesSymbol, out_names);
    }

    UNPROTECT(1);
  }

  out = vctrs_vec_restore(out, ptype, R_NilValue);

  UNPROTECT(6);
  return out;
}


static bool list_has_inner_names(SEXP xs) {
  R_len_t n = Rf_length(xs);

  for (R_len_t i = 0; i < n; ++i) {
    SEXP elt = VECTOR_ELT(xs, i);
    if (vctrs_vec_names(elt) != R_NilValue) {
      return true;
    }
  }

  return false;
}
