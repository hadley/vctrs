#ifndef VCTRS_ALTREP_REP_MACROS_H
#define VCTRS_ALTREP_REP_MACROS_H

#if HAS_ALTREP

// -----------------------------------------------------------------------------
// `new_vctrs_compact_rep_*()`

#define NEW_VCTRS_COMPACT_REP(VALUE, SIZE, VEC) {                                                      \
  SEXP info = PROTECT(Rf_allocVector(RAWSXP, sizeof(struct vctrs_compact_rep_##VEC##_info)));          \
  struct vctrs_compact_rep_##VEC##_info* p_info = (struct vctrs_compact_rep_##VEC##_info*) RAW0(info); \
                                                                                                       \
  p_info->value = VALUE;                                                                               \
  p_info->size = SIZE;                                                                                 \
                                                                                                       \
  SEXP out = R_new_altrep(vctrs_compact_rep_##VEC##_class, info, R_NilValue);                          \
                                                                                                       \
  /* Force duplicate on modify */                                                                      \
  MARK_NOT_MUTABLE(out);                                                                               \
                                                                                                       \
  UNPROTECT(1);                                                                                        \
  return out;                                                                                          \
}

#define VCTRS_NEW_VCTRS_COMPACT_REP(VALUE, SIZE, CTYPE, CONST_DEREF, VEC) { \
  const CTYPE value_ = CONST_DEREF(VALUE)[0];                               \
  R_xlen_t size_ = (R_xlen_t) REAL_RO(SIZE)[0];                             \
                                                                            \
  return new_vctrs_compact_rep_##VEC(value_, size_);                        \
}

// -----------------------------------------------------------------------------
// value / size accessors

#define VCTRS_COMPACT_REP_VALUE(INFO, VEC) (((struct vctrs_compact_rep_##VEC##_info*) RAW0(INFO))->value)
#define VCTRS_COMPACT_REP_SIZE(INFO, VEC) (((struct vctrs_compact_rep_##VEC##_info*) RAW0(INFO))->size)

// -----------------------------------------------------------------------------
// `vctrs_compact_rep_*_materialize()`

// Materialize the full vector

#define VCTRS_COMPACT_REP_MATERIALIZE(X, CTYPE, DEREF, SEXPTYPE, VEC) { \
  const SEXP info = VCTRS_COMPACT_REP_INFO(X);                          \
  const CTYPE value = VCTRS_COMPACT_REP_##VEC##_VALUE(info);            \
  const R_xlen_t size = VCTRS_COMPACT_REP_##VEC##_SIZE(info);           \
                                                                        \
  SEXP out = PROTECT(Rf_allocVector(SEXPTYPE, size));                   \
  CTYPE* p_out = DEREF(out);                                            \
                                                                        \
  for (R_xlen_t i = 0; i < size; ++i) {                                 \
    p_out[i] = value;                                                   \
  }                                                                     \
                                                                        \
  UNPROTECT(1);                                                         \
  return out;                                                           \
}

// -----------------------------------------------------------------------------
// `vctrs_compact_rep_*_serialized_state()`

#define VCTRS_COMPACT_REP_SERIALIZED_STATE(X) { \
  return VCTRS_COMPACT_REP_INFO(X);             \
}

// -----------------------------------------------------------------------------
// `vctrs_compact_rep_*_unserialize()`

#define VCTRS_COMPACT_REP_UNSERIALIZE(CLS, STATE, CTYPE, VEC_UPPER, VEC_LOWER) { \
  const SEXP info = STATE;                                                       \
  const CTYPE value = VCTRS_COMPACT_REP_##VEC_UPPER##_VALUE(info);               \
  const R_xlen_t size = VCTRS_COMPACT_REP_##VEC_UPPER##_SIZE(info);              \
                                                                                 \
  return new_vctrs_compact_rep_##VEC_LOWER(value, size);                         \
}

// -----------------------------------------------------------------------------
// `vctrs_compact_rep_*_duplicate()`

// TODO: What if `deep = false`? vroom dttm duplicates the altrep object
// but compact_intseq objects always materialize

#define VCTRS_COMPACT_REP_DUPLICATE(X, DEEP, VEC) { \
  return vctrs_compact_rep_##VEC##_materialize(X);  \
}

// -----------------------------------------------------------------------------
// `vctrs_compact_rep_*_coerce()`

// Drop through to standard coercion methods for now.
// We could coerce from one compact rep type to another.

#define VCTRS_COMPACT_REP_COERCE(X, TYPE) { \
  return NULL;                              \
}

// -----------------------------------------------------------------------------
// `vctrs_compact_rep_*_inspect()`

// Many of the arguments are not currently used, but may be one day.
// Passing them through like this should make it easier to use them later on,
// if required.

#define VCTRS_COMPACT_REP_INSPECT(X, PRE, DEEP, PVEC, INSPECT_SUBTREE, CTYPE, SPECIFIER, VEC_LOWER, VEC_UPPER) { \
  const SEXP info = VCTRS_COMPACT_REP_INFO(X);                                                                   \
  const CTYPE value_data = VCTRS_COMPACT_REP_##VEC_UPPER##_VALUE_DATA(info);                                     \
  const R_xlen_t size = VCTRS_COMPACT_REP_##VEC_UPPER##_SIZE(info);                                              \
  const char* state = VCTRS_COMPACT_REP_IS_COMPACT(x) ? "compact" : "expanded";                                  \
                                                                                                                 \
  Rprintf(                                                                                                       \
    "vctrs_compact_rep_" VEC_LOWER "(value: %" SPECIFIER ", size: %td, state: %s)\n",                            \
    value_data,                                                                                                  \
    size,                                                                                                        \
    state                                                                                                        \
  );                                                                                                             \
                                                                                                                 \
  return TRUE;                                                                                                   \
}

// -----------------------------------------------------------------------------
// `vctrs_compact_rep_*_length()`

#define VCTRS_COMPACT_REP_LENGTH(X, VEC) {     \
  const SEXP info = VCTRS_COMPACT_REP_INFO(X); \
  return VCTRS_COMPACT_REP_##VEC##_SIZE(info); \
}

// -----------------------------------------------------------------------------
// `vctrs_compact_rep_*_dataptr()`

#define VCTRS_COMPACT_REP_DATAPTR(X, WRITEABLE, VEC) {    \
  if (VCTRS_COMPACT_REP_IS_COMPACT(X)) {                  \
    SEXP data = vctrs_compact_rep_##VEC##_materialize(X); \
    VCTRS_COMPACT_REP_SET_DATA(X, data);                  \
  }                                                       \
                                                          \
  return DATAPTR(VCTRS_COMPACT_REP_DATA(X));              \
}

// -----------------------------------------------------------------------------
// `vctrs_compact_rep_*_dataptr_or_null()`

#define VCTRS_COMPACT_REP_DATAPTR_OR_NULL(X, VEC) {     \
  if (VCTRS_COMPACT_REP_IS_COMPACT(X)) {                \
    return NULL;                                        \
  } else {                                              \
    return vctrs_compact_rep_##VEC##_dataptr(X, FALSE); \
  }                                                     \
}

// -----------------------------------------------------------------------------

#endif

#endif
