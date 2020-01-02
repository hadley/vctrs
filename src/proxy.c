#include "vctrs.h"
#include "utils.h"

// Initialised at load time
SEXP syms_vec_proxy_dispatch = NULL;
SEXP syms_vec_proxy_equal_dispatch = NULL;
SEXP fns_vec_proxy_equal_dispatch = NULL;

// Defined below
SEXP vec_proxy_method(SEXP x);
SEXP vec_proxy_invoke(SEXP x);


// [[ register(); include("vctrs.h") ]]
SEXP vec_proxy(SEXP x) {
  int nprot = 0;
  struct vctrs_type_info info = vec_type_info(x);
  PROTECT_TYPE_INFO(&info, &nprot);

  SEXP out;
  if (info.type == vctrs_type_s3) {
    out = vec_proxy_invoke(x);
  } else {
    out = x;
  }

  UNPROTECT(nprot);
  return out;
}

// [[ register(); include("vctrs.h") ]]
SEXP vec_proxy_equal(SEXP x) {
  return vec_proxy_recursive(x, vctrs_proxy_equal);
}
SEXP vec_proxy_equal_dispatch(SEXP x) {
  if (vec_typeof(x) == vctrs_type_s3) {
    return vctrs_dispatch1(syms_vec_proxy_equal_dispatch, fns_vec_proxy_equal_dispatch,
                           syms_x, x);
  } else {
    return x;
  }
}

// [[ include("vctrs.h") ]]
SEXP vec_proxy_recursive(SEXP x, enum vctrs_proxy_kind kind) {
  switch (kind) {
  case vctrs_proxy_default: x = PROTECT(vec_proxy(x)); break;
  case vctrs_proxy_equal: x = PROTECT(vec_proxy_equal_dispatch(x)); break;
  case vctrs_proxy_compare: Rf_error("Internal error: Unimplemented proxy kind");
  }

  if (is_data_frame(x)) {
    x = PROTECT(r_maybe_duplicate(x));
    R_len_t n = Rf_length(x);

    for (R_len_t i = 0; i < n; ++i) {
      SEXP col = vec_proxy_recursive(VECTOR_ELT(x, i), kind);
      SET_VECTOR_ELT(x, i, col);
    }

    UNPROTECT(1);
  }

  UNPROTECT(1);
  return x;
}

// [[ register() ]]
SEXP vctrs_proxy_recursive(SEXP x, SEXP kind_) {
  enum vctrs_proxy_kind kind;
  if (kind_ == Rf_install("default")) {
    kind = vctrs_proxy_default;
  } else if (kind_ == Rf_install("equal")) {
    kind = vctrs_proxy_equal;
  } else if (kind_ == Rf_install("compare")) {
    kind = vctrs_proxy_compare;
  } else {
    Rf_error("Internal error: Unexpected proxy kind `%s`.", CHAR(PRINTNAME(kind_)));
  }

  return vec_proxy_recursive(x, kind);
}

SEXP vec_proxy_method(SEXP x) {
  return s3_find_method("vec_proxy", x);
}

SEXP vec_proxy_invoke(SEXP x) {
  return vctrs_eval_mask1(syms_vec_proxy_dispatch, syms_x, x, vctrs_ns_env);
}


SEXP vec_proxy_pop_vcols(SEXP* proxy_out) {
  SEXP proxy = *proxy_out;

  SEXP names = PROTECT(r_names(proxy));
  if (names == R_NilValue) {
    UNPROTECT(1);
    return R_NilValue;
  }

  R_len_t n = Rf_length(proxy);
  if (STRING_ELT(names, n - 1) != strings_vcols) {
    UNPROTECT(1);
    return R_NilValue;
  }

  SEXP vcols = PROTECT(VECTOR_ELT(proxy, n - 1));

  SEXP shrunk = PROTECT(Rf_lengthgets(proxy, n - 1));
  Rf_copyMostAttrib(shrunk, proxy);

  UNPROTECT(3);
  *proxy_out = shrunk;
  return vcols;
}


void vctrs_init_data(SEXP ns) {
  syms_vec_proxy_dispatch = Rf_install("vec_proxy_dispatch");
  syms_vec_proxy_equal_dispatch = Rf_install("vec_proxy_equal_dispatch");

  fns_vec_proxy_equal_dispatch = r_env_get(ns, syms_vec_proxy_equal_dispatch);
}
