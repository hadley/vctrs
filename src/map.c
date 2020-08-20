#include "vctrs.h"
#include "slice.h"
#include "utils.h"

// Defined at load time
SEXP vec_map_call = NULL;

static SEXP atomic_map(SEXP x, SEXP env, SEXP ptype);
static SEXP list_map(SEXP x, SEXP env, SEXP ptype);


SEXP vctrs_map(SEXP args) {
    args = CDR(args);
    SEXP x = CAR(args); args = CDR(args);
    SEXP env = CAR(args); args = CDR(args);
    SEXP ptype = CAR(args);

    if (ptype == R_NilValue) {
      r_abort("`.ptype` can't be NULL.");
    }

    SEXP orig = x;
    bool list_input = vec_is_list(orig);
    bool list_output = vec_is_list(ptype);

    // Instead of using `[[` or equivalent to access the elements of
    // atomic inputs, we chop them into a list
    if (!list_input) {
      x = vec_chop2(x);
    }
    PROTECT(x);

    SEXP out;
    if (list_output) {
      out = list_map(x, env, ptype);
    } else {
      out = atomic_map(x, env, ptype);
    }
    PROTECT(out);

    SEXP names = PROTECT(vec_names(orig));
    if (names != R_NilValue) {
      vec_set_names(out, names);
    }

    UNPROTECT(3);
    return out;
}

static
SEXP list_map(SEXP x, SEXP env, SEXP ptype) {
  // When mapping to a list, we update the input list with the results
  // inplace. We first zap the attributes of this list because it's
  // cast to the target prototype later on.
  SEXP out = PROTECT(r_clone_referenced(x));
  SET_ATTRIB(out, R_NilValue);
  SET_OBJECT(out, 0);

  r_ssize n = r_length(x);
  const SEXP* p_x = VECTOR_PTR_RO(x);

  for (r_ssize i = 0; i < n; ++i) {
    r_env_poke(env, syms_dot_elt, p_x[i]);
    SET_VECTOR_ELT(out, i, r_eval_force(vec_map_call, env));
  }

  // Genericity is accomplished by casting the complete list of
  // outputs to the target prototype. Should use a ptype identity
  // check before casting. Probably `vec_cast()` should make that
  // check.
  if (OBJECT(ptype)) {
    out = vec_cast(out, ptype, NULL, NULL);
  }

  UNPROTECT(1);
  return out;
}

static
SEXP atomic_map(SEXP x, SEXP env, SEXP ptype) {
  r_ssize n = r_length(x);

  // Genericity is handled in a typical fashion when mapping to an
  // atomic vector. We initialise the target prototype and coerce each
  // element to that target before assigning.
  SEXP out = PROTECT(vec_init(ptype, n));

  SEXP out_proxy = vec_proxy(out);
  PROTECT_INDEX out_proxy_pi;
  PROTECT_WITH_INDEX(out_proxy, &out_proxy_pi);

  SEXP loc = PROTECT(compact_seq(0, 0, true));
  int* p_loc = INTEGER(loc);

  const SEXP* p_x = VECTOR_PTR_RO(x);

  for (r_ssize i = 0; i < n; ++i) {
    r_env_poke(env, syms_dot_elt, p_x[i]);

    SEXP elt_out = PROTECT(r_eval_force(vec_map_call, env));
    if (vec_size(elt_out) != 1) {
      r_abort("Mapped function must return a size 1 vector.");
    }

    elt_out = PROTECT(vec_cast(elt_out, ptype, NULL, NULL));

    init_compact_seq(p_loc, i, 1, true);
    out_proxy = vec_proxy_assign(out_proxy, loc, elt_out);

    UNPROTECT(2);
    REPROTECT(out_proxy, out_proxy_pi);
  }

  out = vec_restore(out_proxy, ptype, R_NilValue, VCTRS_OWNED_true);

  UNPROTECT(3);
  return out;
}


void vctrs_init_map(SEXP ns) {
  vec_map_call = r_parse(".fn(.elt, ...)");
  R_PreserveObject(vec_map_call);
}
