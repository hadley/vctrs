#include "vctrs.h"
#include "utils.h"

// Initialised at load time
SEXP syms_vec_assign_fallback = NULL;
SEXP fns_vec_assign_fallback = NULL;

// Defined in slice.c
SEXP vec_as_index(SEXP i, SEXP x);

static SEXP vec_assign_fallback(SEXP x, SEXP index, SEXP value);
static SEXP vec_assign_impl(SEXP x, SEXP index, SEXP value);
static SEXP lgl_assign(SEXP x, SEXP index, SEXP value);
static SEXP int_assign(SEXP x, SEXP index, SEXP value);
static SEXP dbl_assign(SEXP x, SEXP index, SEXP value);
static SEXP cpl_assign(SEXP x, SEXP index, SEXP value);
static SEXP chr_assign(SEXP x, SEXP index, SEXP value);
static SEXP raw_assign(SEXP x, SEXP index, SEXP value);
static SEXP list_assign(SEXP x, SEXP index, SEXP value);
static SEXP df_assign(SEXP x, SEXP index, SEXP value);
static SEXP vec_assign_fallback(SEXP x, SEXP index, SEXP value);


SEXP vec_assign(SEXP x, SEXP index, SEXP value) {
  if (x == R_NilValue) {
    return R_NilValue;
  }

  struct vctrs_arg x_arg = new_wrapper_arg(NULL, "x");
  struct vctrs_arg value_arg = new_wrapper_arg(NULL, "value");
  vec_assert(x, &x_arg);

  // Take the proxy of the RHS before coercing and recycling
  value = PROTECT(vec_coercible_cast(value, x, &value_arg, &x_arg));
  SEXP value_proxy = PROTECT(vec_proxy(value));

  index = PROTECT(vec_as_index(index, x));
  value_proxy = PROTECT(vec_recycle(value_proxy, vec_size(index)));

  struct vctrs_proxy_info info = vec_proxy_info(x);

  SEXP out;
  if ((OBJECT(x) && info.proxy_method == R_NilValue) || has_dim(x)) {
    // Restore the value before falling back to `[<-`
    value = PROTECT(vec_restore(value_proxy, value, R_NilValue));
    out = vec_assign_fallback(x, index, value_proxy);
    UNPROTECT(1);
  } else {
    out = PROTECT(vec_assign_impl(info.proxy, index, value_proxy));
    out = vec_restore(out, x, R_NilValue);
    UNPROTECT(1);
  }

  UNPROTECT(4);
  return out;
}

static SEXP vec_assign_impl(SEXP proxy, SEXP index, SEXP value) {
  switch (vec_proxy_typeof(proxy)) {
  case vctrs_type_logical:   return lgl_assign(proxy, index, value);
  case vctrs_type_integer:   return int_assign(proxy, index, value);
  case vctrs_type_double:    return dbl_assign(proxy, index, value);
  case vctrs_type_complex:   return cpl_assign(proxy, index, value);
  case vctrs_type_character: return chr_assign(proxy, index, value);
  case vctrs_type_raw:       return raw_assign(proxy, index, value);
  case vctrs_type_list:      return list_assign(proxy, index, value);
  case vctrs_type_dataframe: return df_assign(proxy, index, value);
  case vctrs_type_s3:
  case vctrs_type_null:      Rf_error("Internal error in `vec_assign_impl()`: Unexpected type %s.",
                                      vec_type_as_str(vec_typeof(proxy)));
  case vctrs_type_scalar:    stop_scalar_type(proxy, args_empty);
  }
  never_reached("vec_assign_impl");
}

#define ASSIGN(CTYPE, DEREF, CONST_DEREF)                       \
  R_len_t n = Rf_length(index);                                 \
  int* index_data = INTEGER(index);                             \
                                                                \
  if (n != Rf_length(value)) {                                  \
    Rf_error("Internal error in `vec_assign()`: "               \
             "`value` should have been recycled to fit `x`.");  \
  }                                                             \
                                                                \
  const CTYPE* value_data = CONST_DEREF(value);                 \
  SEXP out = PROTECT(Rf_shallow_duplicate(x));                  \
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

static SEXP lgl_assign(SEXP x, SEXP index, SEXP value) {
  ASSIGN(int, LOGICAL, LOGICAL_RO);
}
static SEXP int_assign(SEXP x, SEXP index, SEXP value) {
  ASSIGN(int, INTEGER, INTEGER_RO);
}
static SEXP dbl_assign(SEXP x, SEXP index, SEXP value) {
  ASSIGN(double, REAL, REAL_RO);
}
static SEXP cpl_assign(SEXP x, SEXP index, SEXP value) {
  ASSIGN(Rcomplex, COMPLEX, COMPLEX_RO);
}
static SEXP chr_assign(SEXP x, SEXP index, SEXP value) {
  ASSIGN(SEXP, STRING_PTR, STRING_PTR_RO);
}
static SEXP raw_assign(SEXP x, SEXP index, SEXP value) {
  ASSIGN(Rbyte, RAW, RAW_RO);
}

#undef ASSIGN


#define ASSIGN_BARRIER(GET, SET)                                \
  R_len_t n = Rf_length(index);                                 \
  int* index_data = INTEGER(index);                             \
                                                                \
  if (n != Rf_length(value)) {                                  \
    Rf_error("Internal error in `vec_assign()`: "               \
             "`value` should have been recycled to fit `x`.");  \
  }                                                             \
                                                                \
  SEXP out = PROTECT(Rf_shallow_duplicate(x));                  \
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

static SEXP list_assign(SEXP x, SEXP index, SEXP value) {
  ASSIGN_BARRIER(VECTOR_ELT, SET_VECTOR_ELT);
}

#undef ASSIGN_BARRIER


static SEXP df_assign(SEXP x, SEXP index, SEXP value) {
  R_len_t n = Rf_length(x);
  SEXP out = PROTECT(Rf_shallow_duplicate(x));

  for (R_len_t i = 0; i < n; ++i) {
    SEXP out_elt = VECTOR_ELT(x, i);
    SEXP value_elt = VECTOR_ELT(value, i);

    // No need to cast or recycle because those operations are
    // recursive and have already been performed. However, proxy and
    // restore are not recursive so need to be done for each element
    // we recurse into.
    SEXP proxy_elt = PROTECT(vec_proxy(out_elt));
    value_elt = PROTECT(vec_proxy(value_elt));

    SEXP assigned = PROTECT(vec_assign_impl(proxy_elt, index, value_elt));

    assigned = vec_restore(assigned, out_elt, R_NilValue);

    SET_VECTOR_ELT(out, i, assigned);
    UNPROTECT(3);
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
