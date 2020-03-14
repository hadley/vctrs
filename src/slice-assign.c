#include "vctrs.h"
#include "subscript-loc.h"
#include "utils.h"
#include "altrep-rep.h"

// Initialised at load time
SEXP syms_vec_assign_fallback = NULL;
SEXP fns_vec_assign_fallback = NULL;

// Defined in slice.c
SEXP vec_as_location(SEXP i, R_len_t n, SEXP names);

static SEXP vec_assign_fallback(SEXP x, SEXP index, SEXP value);
static SEXP lgl_assign(SEXP x, SEXP index, SEXP value);
static SEXP int_assign(SEXP x, SEXP index, SEXP value);
static SEXP dbl_assign(SEXP x, SEXP index, SEXP value);
static SEXP cpl_assign(SEXP x, SEXP index, SEXP value);
SEXP chr_assign(SEXP x, SEXP index, SEXP value);
static SEXP raw_assign(SEXP x, SEXP index, SEXP value);
SEXP list_assign(SEXP x, SEXP index, SEXP value);
SEXP df_assign(SEXP x, SEXP index, SEXP value);

// [[ register(); include("vctrs.h") ]]
SEXP vec_assign(SEXP x, SEXP index, SEXP value) {
  if (x == R_NilValue) {
    return R_NilValue;
  }

  struct vctrs_arg x_arg = new_wrapper_arg(NULL, "x");
  struct vctrs_arg value_arg = new_wrapper_arg(NULL, "value");
  vec_assert(x, &x_arg);
  vec_assert(value, &value_arg);

  index = PROTECT(vec_as_location_opts(index,
                                       vec_size(x),
                                       PROTECT(vec_names(x)),
                                       vec_as_location_default_assign_opts,
                                       NULL));

  // Cast and recycle `value`
  value = PROTECT(vec_coercible_cast(value, x, &value_arg, &x_arg));
  value = PROTECT(vec_recycle(value, vec_size(index), &value_arg));

  SEXP proxy = PROTECT(vec_proxy(x));

  proxy = PROTECT(vec_proxy_assign(proxy, index, value));

  SEXP out = vec_restore(proxy, x, R_NilValue);

  UNPROTECT(6);
  return out;
}

SEXP vec_assign_switch(SEXP proxy, SEXP index, SEXP value) {
  switch (vec_proxy_typeof(proxy)) {
  case vctrs_type_logical:     return lgl_assign(proxy, index, value);
  case vctrs_type_integer:     return int_assign(proxy, index, value);
  case vctrs_type_double:      return dbl_assign(proxy, index, value);
  case vctrs_type_complex:     return cpl_assign(proxy, index, value);
  case vctrs_type_character:   return chr_assign(proxy, index, value);
  case vctrs_type_raw:         return raw_assign(proxy, index, value);
  case vctrs_type_list:        return list_assign(proxy, index, value);
  case vctrs_type_dataframe:   return df_assign(proxy, index, value);
  case vctrs_type_null:
  case vctrs_type_unspecified:
  case vctrs_type_s3:
                               Rf_error("Internal error in `vec_assign_switch()`: Unexpected type %s.",
                                        vec_type_as_str(vec_typeof(proxy)));
  case vctrs_type_scalar:      stop_scalar_type(proxy, args_empty);
  }
  never_reached("vec_assign_switch");
}

// `vec_proxy_assign()` will duplicate the `proxy` if it is referenced or
// marked as not mutable. Otherwise, `vec_proxy_assign()` will assign
// directly into the `proxy`. Even though it can directly assign, the safe
// way to call `vec_proxy_assign()` is to catch and protect its output rather
// than relying on it to assign directly.

/*
 * @param proxy The proxy of the output container
 * @param index The locations to assign `value` to
 * @param value The value to assign into the proxy. Must already be
 *   cast to the type of the true output container, and have been
 *   recycled to the correct size. Should not be proxied, in case
 *   we have to fallback.
 */
SEXP vec_proxy_assign(SEXP proxy, SEXP index, SEXP value) {
  struct vctrs_proxy_info info = vec_proxy_info(value);

  // If a fallback is required, the `proxy` is identical to the output container
  // because no proxy method was called
  if (vec_requires_fallback(value, info) || has_dim(proxy)) {
    index = PROTECT(compact_materialize(index));
    SEXP out = vec_assign_fallback(proxy, index, value);
    UNPROTECT(1);
    return out;
  }

  return vec_assign_switch(proxy, index, info.proxy);
}


#define ASSIGN_ALTREP_VCTRS_COMPACT_REP(CTYPE, DEREF, ELT)       \
  R_len_t n = Rf_length(index);                                  \
  int* index_data = INTEGER(index);                              \
                                                                 \
  if (n != Rf_length(value)) {                                   \
    Rf_error("Internal error in `vec_assign()`: "                \
             "`value` should have been recycled to fit `x`.");   \
  }                                                              \
                                                                 \
  SEXP out = PROTECT(r_maybe_duplicate(x));                      \
  CTYPE* out_data = DEREF(out);                                  \
                                                                 \
  CTYPE elt;                                                     \
  if (n > 0) {                                                   \
    elt = ELT(value, 0);                                         \
  }                                                              \
                                                                 \
  for (R_len_t i = 0; i < n; ++i) {                              \
    int j = index_data[i];                                       \
                                                                 \
    if (j != NA_INTEGER) {                                       \
      out_data[j - 1] = elt;                                     \
    }                                                            \
  }                                                              \
                                                                 \
  UNPROTECT(1);                                                  \
  return out;

#define ASSIGN_ALTREP_VCTRS_COMPACT_REP_COMPACT(CTYPE, DEREF, ELT) \
  int* index_data = INTEGER(index);                                \
  R_len_t start = index_data[0];                                   \
  R_len_t n = index_data[1];                                       \
  R_len_t step = index_data[2];                                    \
                                                                   \
  if (n != Rf_length(value)) {                                     \
    Rf_error("Internal error in `vec_assign()`: "                  \
             "`value` should have been recycled to fit `x`.");     \
  }                                                                \
                                                                   \
  CTYPE elt;                                                       \
  if (n > 0) {                                                     \
    elt = ELT(value, 0);                                           \
  }                                                                \
                                                                   \
  SEXP out = PROTECT(r_maybe_duplicate(x));                        \
  CTYPE* out_data = DEREF(out) + start;                            \
                                                                   \
  for (int i = 0; i < n; ++i, out_data += step) {                  \
    *out_data = elt;                                               \
  }                                                                \
                                                                   \
  UNPROTECT(1);                                                    \
  return out;

#define ASSIGN_INDEX(CTYPE, DEREF, CONST_DEREF)                 \
  R_len_t n = Rf_length(index);                                 \
  int* index_data = INTEGER(index);                             \
                                                                \
  if (n != Rf_length(value)) {                                  \
    Rf_error("Internal error in `vec_assign()`: "               \
             "`value` should have been recycled to fit `x`.");  \
  }                                                             \
                                                                \
  const CTYPE* value_data = CONST_DEREF(value);                 \
  SEXP out = PROTECT(r_maybe_duplicate(x));                     \
  CTYPE* out_data = DEREF(out);                                 \
                                                                \
  for (R_len_t i = 0; i < n; ++i) {                             \
    int j = index_data[i];                                      \
    if (j != NA_INTEGER) {                                      \
      out_data[j - 1] = value_data[i];                          \
    }                                                           \
  }                                                             \
                                                                \
  UNPROTECT(1);                                                 \
  return out

#define ASSIGN_COMPACT(CTYPE, DEREF, CONST_DEREF)               \
  int* index_data = INTEGER(index);                             \
  R_len_t start = index_data[0];                                \
  R_len_t n = index_data[1];                                    \
  R_len_t step = index_data[2];                                 \
                                                                \
  if (n != Rf_length(value)) {                                  \
    Rf_error("Internal error in `vec_assign()`: "               \
             "`value` should have been recycled to fit `x`.");  \
  }                                                             \
                                                                \
  const CTYPE* value_data = CONST_DEREF(value);                 \
  SEXP out = PROTECT(r_maybe_duplicate(x));                     \
  CTYPE* out_data = DEREF(out) + start;                         \
                                                                \
  for (int i = 0; i < n; ++i, out_data += step, ++value_data) { \
    *out_data = *value_data;                                    \
  }                                                             \
                                                                \
  UNPROTECT(1);                                                 \
  return out

#define ASSIGN(CTYPE, DEREF, CONST_DEREF, ELT)                     \
  if (vec_is_vctrs_compact_rep(value)) {                           \
    if (is_compact_seq(index)) {                                   \
      ASSIGN_ALTREP_VCTRS_COMPACT_REP_COMPACT(CTYPE, DEREF, ELT);  \
    } else {                                                       \
      ASSIGN_ALTREP_VCTRS_COMPACT_REP(CTYPE, DEREF, ELT);          \
    }                                                              \
  } else if (is_compact_seq(index)) {                              \
    ASSIGN_COMPACT(CTYPE, DEREF, CONST_DEREF);                     \
  } else {                                                         \
    ASSIGN_INDEX(CTYPE, DEREF, CONST_DEREF);                       \
  }

static SEXP lgl_assign(SEXP x, SEXP index, SEXP value) {
  ASSIGN(int, LOGICAL, LOGICAL_RO, LOGICAL_ELT);
}
static SEXP int_assign(SEXP x, SEXP index, SEXP value) {
  ASSIGN(int, INTEGER, INTEGER_RO, INTEGER_ELT);
}
static SEXP dbl_assign(SEXP x, SEXP index, SEXP value) {
  ASSIGN(double, REAL, REAL_RO, REAL_ELT);
}
static SEXP cpl_assign(SEXP x, SEXP index, SEXP value) {
  ASSIGN(Rcomplex, COMPLEX, COMPLEX_RO, COMPLEX_ELT);
}
SEXP chr_assign(SEXP x, SEXP index, SEXP value) {
  ASSIGN(SEXP, STRING_PTR, STRING_PTR_RO, STRING_ELT);
}
static SEXP raw_assign(SEXP x, SEXP index, SEXP value) {
  ASSIGN(Rbyte, RAW, RAW_RO, RAW_ELT);
}

#undef ASSIGN
#undef ASSIGN_INDEX
#undef ASSIGN_COMPACT
#undef ASSIGN_ALTREP_VCTRS_COMPACT_REP
#undef ASSIGN_ALTREP_VCTRS_COMPACT_REP_COMPACT


#define ASSIGN_BARRIER_INDEX(GET, SET)                          \
  R_len_t n = Rf_length(index);                                 \
  int* index_data = INTEGER(index);                             \
                                                                \
  if (n != Rf_length(value)) {                                  \
    Rf_error("Internal error in `vec_assign()`: "               \
             "`value` should have been recycled to fit `x`.");  \
  }                                                             \
                                                                \
  SEXP out = PROTECT(r_maybe_duplicate(x));                     \
                                                                \
  for (R_len_t i = 0; i < n; ++i) {                             \
    int j = index_data[i];                                      \
    if (j != NA_INTEGER) {                                      \
      SET(out, j - 1, GET(value, i));                           \
    }                                                           \
  }                                                             \
                                                                \
  UNPROTECT(1);                                                 \
  return out

#define ASSIGN_BARRIER_COMPACT(GET, SET)                        \
  int* index_data = INTEGER(index);                             \
  R_len_t start = index_data[0];                                \
  R_len_t n = index_data[1];                                    \
  R_len_t step = index_data[2];                                 \
                                                                \
  if (n != Rf_length(value)) {                                  \
    Rf_error("Internal error in `vec_assign()`: "               \
             "`value` should have been recycled to fit `x`.");  \
  }                                                             \
                                                                \
  SEXP out = PROTECT(r_maybe_duplicate(x));                     \
                                                                \
  for (R_len_t i = 0; i < n; ++i, start += step) {              \
    SET(out, start, GET(value, i));                             \
  }                                                             \
                                                                \
  UNPROTECT(1);                                                 \
  return out

#define ASSIGN_BARRIER(GET, SET)                \
  if (is_compact_seq(index)) {                  \
    ASSIGN_BARRIER_COMPACT(GET, SET);           \
  } else {                                      \
    ASSIGN_BARRIER_INDEX(GET, SET);             \
  }

SEXP list_assign(SEXP x, SEXP index, SEXP value) {
  ASSIGN_BARRIER(VECTOR_ELT, SET_VECTOR_ELT);
}

#undef ASSIGN_BARRIER
#undef ASSIGN_BARRIER_INDEX
#undef ASSIGN_BARRIER_COMPACT


/**
 * - `out` and `value` must be rectangular lists.
 * - `value` must have the same size as `index`.
 *
 * [[ include("vctrs.h") ]]
 */
SEXP df_assign(SEXP x, SEXP index, SEXP value) {
  SEXP out = PROTECT(r_maybe_duplicate(x));
  R_len_t n = Rf_length(out);

  for (R_len_t i = 0; i < n; ++i) {
    SEXP out_elt = VECTOR_ELT(out, i);
    SEXP value_elt = VECTOR_ELT(value, i);

    // No need to cast or recycle because those operations are
    // recursive and have already been performed. However, proxy and
    // restore are not recursive so need to be done for each element
    // we recurse into. `vec_proxy_assign()` will proxy the `value_elt`.
    SEXP proxy_elt = PROTECT(vec_proxy(out_elt));

    SEXP assigned = PROTECT(vec_proxy_assign(proxy_elt, index, value_elt));
    assigned = vec_restore(assigned, out_elt, R_NilValue);

    SET_VECTOR_ELT(out, i, assigned);
    UNPROTECT(2);
  }

  UNPROTECT(1);
  return out;
}

static SEXP vec_assign_fallback(SEXP x, SEXP index, SEXP value) {
  return vctrs_dispatch3(syms_vec_assign_fallback, fns_vec_assign_fallback,
                         syms_x, x,
                         syms_i, index,
                         syms_value, value);
}


void vctrs_init_slice_assign(SEXP ns) {
  syms_vec_assign_fallback = Rf_install("vec_assign_fallback");
  fns_vec_assign_fallback = Rf_findVar(syms_vec_assign_fallback, ns);
}
