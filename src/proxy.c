#include <rlang.h>
#include "vctrs.h"
#include "type-data-frame.h"
#include "dim.h"
#include "utils.h"
#include "equal.h"

// Initialised at load time
SEXP syms_vec_proxy = NULL;
SEXP syms_vec_proxy_equal = NULL;
SEXP syms_vec_proxy_equal_array = NULL;
SEXP syms_vec_proxy_compare = NULL;
SEXP syms_vec_proxy_compare_array = NULL;
SEXP syms_vec_proxy_order = NULL;
SEXP syms_vec_proxy_order_array = NULL;

SEXP fns_vec_proxy_equal_array = NULL;
SEXP fns_vec_proxy_compare_array = NULL;
SEXP fns_vec_proxy_order_array = NULL;

static SEXP vec_proxy_unwrap(SEXP x);

SEXP vec_proxy_method(SEXP x);
SEXP vec_proxy_invoke(SEXP x, SEXP method);

// [[ register(); include("vctrs.h") ]]
SEXP vec_proxy(SEXP x) {
  int nprot = 0;
  struct vctrs_type_info info = vec_type_info(x);
  PROTECT_TYPE_INFO(&info, &nprot);

  SEXP out;
  if (info.type == vctrs_type_s3) {
    out = vec_proxy_invoke(x, info.proxy_method);
  } else {
    out = x;
  }

  UNPROTECT(nprot);
  return out;
}

static inline SEXP vec_proxy_equal_method(SEXP x);
static inline SEXP vec_proxy_equal_invoke(SEXP x, SEXP method);

// [[ register(); include("vctrs.h") ]]
SEXP vec_proxy_equal(SEXP x) {
  SEXP method = PROTECT(vec_proxy_equal_method(x));
  SEXP out = vec_proxy_equal_invoke(x, method);
  UNPROTECT(1);
  return out;
}

static inline SEXP vec_proxy_compare_method(SEXP x);
static inline SEXP vec_proxy_compare_invoke(SEXP x, SEXP method);

// [[ register(); include("vctrs.h") ]]
SEXP vec_proxy_compare(SEXP x) {
  SEXP method = PROTECT(vec_proxy_compare_method(x));
  SEXP out = vec_proxy_compare_invoke(x, method);
  UNPROTECT(1);
  return out;
}

static inline SEXP vec_proxy_order_method(SEXP x);
static inline SEXP vec_proxy_order_invoke(SEXP x, SEXP method);

// [[ register(); include("vctrs.h") ]]
SEXP vec_proxy_order(SEXP x) {
  SEXP method = PROTECT(vec_proxy_order_method(x));
  SEXP out = vec_proxy_order_invoke(x, method);
  UNPROTECT(1);
  return out;
}


static r_obj* df_joint_proxy_order(r_obj* x, r_obj* y);
static r_obj* list_joint_proxy_order(r_obj* x, r_obj* y, r_obj* method);

/*
 * Specialized internal variant of `vec_proxy_order()` used in `vec_matches()`.
 * It generally just calls `vec_proxy_order()`, except in the case where we
 * are taking the order-proxy of a list. This generates a proxy that orders
 * by first appearance, so we need to combine `x` and `y` to jointly compute
 * the proxy for comparisons to be correct in `vec_matches()`.
 *
 * For example
 * x <- list(1.5, 2)
 * y <- list(2, 1.5)
 * vec_proxy_order(x)
 * # [1] 1 2
 * vec_proxy_order(y) # can't compare proxies when taken individually
 * # [1] 1 2
 * vec_proxy_order(c(x, y)) # jointly comparable
 * # [1] 1 2 2 1
 *
 * We don't anticipate any other classes having this issue. Most order-proxies
 * should be orderable by typical comparison operators.
 * If that assumption proves incorrect, we may need to expose a new proxy
 * for this.
 */
// [[ include("vctrs.h") ]]
r_obj* vec_joint_proxy_order(r_obj* x, r_obj* y) {
  if (vec_typeof(x) != vec_typeof(y)) {
    r_stop_internal("vec_joint_proxy_order", "`x` and `y` should have the same type.");
  }

  if (is_data_frame(x)) {
    return df_joint_proxy_order(x, y);
  }

  r_obj* method = KEEP(vec_proxy_order_method(x));
  r_obj* vec_proxy_order_list = KEEP(vec_proxy_order_method(vctrs_shared_empty_list));

  if (method == vec_proxy_order_list) {
    // Special case if the chosen proxy method is `vec_proxy_order.list()`
    r_obj* out = list_joint_proxy_order(x, y, method);
    FREE(2);
    return out;
  }

  r_obj* out = KEEP(r_alloc_list(2));
  r_list_poke(out, 0, vec_proxy_order_invoke(x, method));
  r_list_poke(out, 1, vec_proxy_order_invoke(y, method));

  FREE(3);
  return out;
}

static
r_obj* df_joint_proxy_order(r_obj* x, r_obj* y) {
  x = KEEP(r_clone_referenced(x));
  y = KEEP(r_clone_referenced(y));

  r_ssize n_cols = r_length(x);
  if (n_cols != r_length(y)) {
    r_stop_internal(
      "df_joint_proxy_order",
      "`x` and `y` must have the same number of columns."
    );
  }

  r_obj* const* v_x = r_list_cbegin(x);
  r_obj* const* v_y = r_list_cbegin(y);

  for (r_ssize i = 0; i < n_cols; ++i) {
    r_obj* proxies = vec_joint_proxy_order(v_x[i], v_y[i]);
    r_list_poke(x, i, r_list_get(proxies, 0));
    r_list_poke(y, i, r_list_get(proxies, 1));
  }

  x = KEEP(df_flatten(x));
  x = KEEP(vec_proxy_unwrap(x));

  y = KEEP(df_flatten(y));
  y = KEEP(vec_proxy_unwrap(y));

  r_obj* out = KEEP(r_alloc_list(2));
  r_list_poke(out, 0, x);
  r_list_poke(out, 1, y);

  FREE(7);
  return out;
}

static
r_obj* list_joint_proxy_order(r_obj* x, r_obj* y, r_obj* method) {
  r_ssize x_size = vec_size(x);
  r_ssize y_size = vec_size(y);

  r_obj* x_slicer = KEEP(compact_seq(0, x_size, true));
  r_obj* y_slicer = KEEP(compact_seq(x_size, y_size, true));

  r_obj* zap = KEEP(r_alloc_list(0));
  r_poke_class(zap, r_chr("rlang_zap"));

  r_obj* ptype = KEEP(vec_ptype(x, args_empty));

  r_obj* out = KEEP(r_alloc_list(2));
  r_list_poke(out, 0, x);
  r_list_poke(out, 1, y);

  // Combine
  // NOTE: Without long vector support, this limits the maximum allowed
  // size of `vec_matches()` input to `vec_size(x) + vec_size(y) <= INT_MAX`
  // when list columns are used. This should be incredibly rare.
  r_obj* combined = KEEP(vec_c(out, ptype, zap, p_no_repair_silent_ops));

  // Compute order-proxy
  r_obj* proxy = KEEP(vec_proxy_order_invoke(combined, method));

  // Separate and store back in `out`
  r_list_poke(out, 0, vec_slice_impl(proxy, x_slicer));
  r_list_poke(out, 1, vec_slice_impl(proxy, y_slicer));

  FREE(7);
  return out;
}


SEXP vec_proxy_method(SEXP x) {
  return s3_find_method("vec_proxy", x, vctrs_method_table);
}

// This should be faster than normal dispatch but also means that
// proxy methods can't call `NextMethod()`. This could be changed if
// it turns out a problem.
SEXP vec_proxy_invoke(SEXP x, SEXP method) {
  if (method == R_NilValue) {
    return x;
  } else {
    return vctrs_dispatch1(syms_vec_proxy, method, syms_x, x);
  }
}

static inline
SEXP vec_proxy_method_impl(SEXP x, const char* generic, SEXP fn_proxy_array) {
  SEXP cls = PROTECT(s3_get_class(x));
  SEXP method = s3_class_find_method(generic, cls, vctrs_method_table);

  if (method != R_NilValue) {
    UNPROTECT(1);
    return method;
  }

  /* FIXME: Stopgap check for bare arrays */
  /* which equality functions don't handle well */
  if (vec_dim_n(x) > 1) {
    UNPROTECT(1);
    return fn_proxy_array;
  }

  UNPROTECT(1);
  return R_NilValue;
}

static inline
SEXP vec_proxy_equal_method(SEXP x) {
  return vec_proxy_method_impl(x, "vec_proxy_equal", fns_vec_proxy_equal_array);
}
static inline
SEXP vec_proxy_compare_method(SEXP x) {
  return vec_proxy_method_impl(x, "vec_proxy_compare", fns_vec_proxy_compare_array);
}
static inline
SEXP vec_proxy_order_method(SEXP x) {
  return vec_proxy_method_impl(x, "vec_proxy_order", fns_vec_proxy_order_array);
}

static inline
SEXP vec_proxy_invoke_impl(SEXP x,
                           SEXP method,
                           SEXP vec_proxy_sym,
                           SEXP (*vec_proxy_fn)(SEXP)) {
  if (method != R_NilValue) {
    return vctrs_dispatch1(vec_proxy_sym, method, syms_x, x);
  }

  /* Fallback on S3 objects with no proxy */
  if (vec_typeof(x) == vctrs_type_s3) {
    return vec_proxy_fn(x);
  } else {
    return x;
  }
}

static inline
SEXP vec_proxy_equal_invoke(SEXP x, SEXP method) {
  return vec_proxy_invoke_impl(x, method, syms_vec_proxy_equal, vec_proxy);
}
static inline
SEXP vec_proxy_compare_invoke(SEXP x, SEXP method) {
  return vec_proxy_invoke_impl(x, method, syms_vec_proxy_compare, &vec_proxy_equal);
}
static inline
SEXP vec_proxy_order_invoke(SEXP x, SEXP method) {
  return vec_proxy_invoke_impl(x, method, syms_vec_proxy_order, &vec_proxy_compare);
}


#define DF_PROXY(PROXY) do {                                   \
  R_len_t n_cols = Rf_length(x);                               \
                                                               \
  for (R_len_t i = 0; i < n_cols; ++i) {                       \
    SEXP col = VECTOR_ELT(x, i);                               \
    SET_VECTOR_ELT(x, i, PROXY(col));                          \
  }                                                            \
} while (0)

static
SEXP df_proxy(SEXP x, enum vctrs_proxy_kind kind) {
  x = PROTECT(r_clone_referenced(x));

  switch (kind) {
  case VCTRS_PROXY_KIND_default: DF_PROXY(vec_proxy); break;
  case VCTRS_PROXY_KIND_equal: DF_PROXY(vec_proxy_equal); break;
  case VCTRS_PROXY_KIND_compare: DF_PROXY(vec_proxy_compare); break;
  case VCTRS_PROXY_KIND_order: DF_PROXY(vec_proxy_order); break;
  }

  x = PROTECT(df_flatten(x));
  x = vec_proxy_unwrap(x);

  UNPROTECT(2);
  return x;
}

#undef DF_PROXY

// [[ register() ]]
SEXP vctrs_df_proxy(SEXP x, SEXP kind) {
  if (!r_is_number(kind)) {
    r_stop_internal("vctrs_df_proxy", "`kind` must be a single integer.");
  }

  enum vctrs_proxy_kind c_kind = r_int_get(kind, 0);

  return df_proxy(x, c_kind);
}


static
SEXP vec_proxy_unwrap(SEXP x) {
  if (TYPEOF(x) == VECSXP && XLENGTH(x) == 1 && is_data_frame(x)) {
    x = vec_proxy_unwrap(VECTOR_ELT(x, 0));
  }
  return x;
}


// [[ register() ]]
SEXP vctrs_unset_s4(SEXP x) {
  x = r_clone_referenced(x);
  r_unmark_s4(x);
  return x;
}


void vctrs_init_data(SEXP ns) {
  syms_vec_proxy = Rf_install("vec_proxy");

  syms_vec_proxy_equal = Rf_install("vec_proxy_equal");
  syms_vec_proxy_equal_array = Rf_install("vec_proxy_equal.array");

  syms_vec_proxy_compare = Rf_install("vec_proxy_compare");
  syms_vec_proxy_compare_array = Rf_install("vec_proxy_compare.array");

  syms_vec_proxy_order = Rf_install("vec_proxy_order");
  syms_vec_proxy_order_array = Rf_install("vec_proxy_order.array");

  fns_vec_proxy_equal_array = r_env_get(ns, syms_vec_proxy_equal_array);
  fns_vec_proxy_compare_array = r_env_get(ns, syms_vec_proxy_compare_array);
  fns_vec_proxy_order_array = r_env_get(ns, syms_vec_proxy_order_array);
}
