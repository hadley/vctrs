#include "vctrs.h"
#include "dictionary.h"
#include "equal.h"
#include "hash.h"
#include "ptype2.h"
#include "utils.h"

// Initialised at load time
struct vctrs_arg args_needles;
struct vctrs_arg args_haystack;


// http://graphics.stanford.edu/~seander/bithacks.html#RoundUpPowerOf2
int32_t ceil2(int32_t x) {
  x--;
  x |= x >> 1;
  x |= x >> 2;
  x |= x >> 4;
  x |= x >> 8;
  x |= x >> 16;
  x++;
  return x;
}

// Dictonary object ------------------------------------------------------------

static struct dictionary* new_dictionary_opts(SEXP x, struct dictionary_opts* opts);


static int nil_p_equal(const void* x, R_len_t i, const void* y, R_len_t j) {
  stop_internal("nil_p_equal", "Can't compare NULL in dictionary.");
}
static int nil_p_equal_missing(const void* x, R_len_t i) {
  stop_internal("nil_p_equal_missing", "Can't compare NULL in dictionary.");
}

static int lgl_p_equal(const void* x, R_len_t i, const void* y, R_len_t j) {
  return lgl_equal_scalar_na_equal(((const int*) x) + i, ((const int*) y) + j);
}
static int lgl_p_equal_missing(const void* x, R_len_t i) {
  return lgl_equal_scalar_na_equal(((const int*) x) + i, &NA_LOGICAL);
}

static int int_p_equal(const void* x, R_len_t i, const void* y, R_len_t j) {
  return int_equal_scalar_na_equal(((const int*) x) + i, ((const int*) y) + j);
}
static int int_p_equal_missing(const void* x, R_len_t i) {
  return int_equal_scalar_na_equal(((const int*) x) + i, &NA_INTEGER);
}

static int dbl_p_equal(const void* x, R_len_t i, const void* y, R_len_t j) {
  return dbl_equal_scalar_na_equal(((const double*) x) + i, ((const double*) y) + j);
}
static int dbl_p_equal_missing(const void* x, R_len_t i) {
  return dbl_equal_scalar_na_equal(((const double*) x) + i, &NA_REAL);
}

static int cpl_p_equal(const void* x, R_len_t i, const void* y, R_len_t j) {
  return cpl_equal_scalar_na_equal(((const Rcomplex*) x) + i, ((const Rcomplex*) y) + j);
}
static int cpl_p_equal_missing(const void* x, R_len_t i) {
  return cpl_equal_scalar_na_equal(((const Rcomplex*) x) + i, &vctrs_shared_na_cpl);
}

static int chr_p_equal(const void* x, R_len_t i, const void* y, R_len_t j) {
  return chr_equal_scalar_na_equal(((const SEXP*) x) + i, ((const SEXP*) y) + j);
}
static int chr_p_equal_missing(const void* x, R_len_t i) {
  return chr_equal_scalar_na_equal(((const SEXP*) x) + i, &NA_STRING);
}

static int raw_p_equal(const void* x, R_len_t i, const void* y, R_len_t j) {
  return raw_equal_scalar_na_equal(((const Rbyte*) x) + i, ((const Rbyte*) y) + j);
}
static int raw_p_equal_missing(const void* x, R_len_t i) {
  return false;
}

static int list_p_equal(const void* x, R_len_t i, const void* y, R_len_t j) {
  return list_equal_scalar_na_equal(((const SEXP) x), i, ((const SEXP) y), j);
}
static int list_p_equal_missing(const void* x, R_len_t i) {
  return list_equal_scalar_na_equal(((const SEXP) x), i, vctrs_shared_na_list, 0);
}


static void init_dictionary_nil(struct dictionary* d) {
  d->vec_p = NULL;
  d->equal = &nil_p_equal;
  d->equal_missing = &nil_p_equal_missing;
}
static void init_dictionary_lgl(struct dictionary* d) {
  d->vec_p = (const void*) LOGICAL_RO(d->vec);
  d->equal = &lgl_p_equal;
  d->equal_missing = &lgl_p_equal_missing;
}
static void init_dictionary_int(struct dictionary* d) {
  d->vec_p = (const void*) INTEGER_RO(d->vec);
  d->equal = &int_p_equal;
  d->equal_missing = &int_p_equal_missing;
}
static void init_dictionary_dbl(struct dictionary* d) {
  d->vec_p = (const void*) REAL_RO(d->vec);
  d->equal = dbl_p_equal;
  d->equal_missing = &dbl_p_equal_missing;
}
static void init_dictionary_cpl(struct dictionary* d) {
  d->vec_p = (const void*) COMPLEX_RO(d->vec);
  d->equal = &cpl_p_equal;
  d->equal_missing = &cpl_p_equal_missing;
}
static void init_dictionary_chr(struct dictionary* d) {
  d->vec_p = (const void*) STRING_PTR_RO(d->vec);
  d->equal = &chr_p_equal;
  d->equal_missing = &chr_p_equal_missing;
}
static void init_dictionary_raw(struct dictionary* d) {
  d->vec_p = (const void*) RAW_RO(d->vec);
  d->equal = &raw_p_equal;
  d->equal_missing = &raw_p_equal_missing;
}
static void init_dictionary_list(struct dictionary* d) {
  d->vec_p = (const void*) d->vec;
  d->equal = &list_p_equal;
  d->equal_missing = &list_p_equal_missing;
}

struct dictionary_df_data {
  enum vctrs_type* col_types;
  const void** col_ptrs;
  R_len_t n_col;
};

static int df_equal(const void* x, R_len_t i, const void* y, R_len_t j) {
  struct dictionary_df_data* x_data = (struct dictionary_df_data*) x;
  struct dictionary_df_data* y_data = (struct dictionary_df_data*) y;

  R_len_t n_col = x_data->n_col;
  if (n_col != y_data->n_col) {
    stop_internal("df_equal", "`x` and `y` must have the same number of columns.");
  }

  enum vctrs_type* types = x_data->col_types;
  const void** x_ptrs = x_data->col_ptrs;
  const void** y_ptrs = y_data->col_ptrs;

  // `vec_proxy_equal()` flattens data frames so we don't need to
  // worry about df-cols
  for (R_len_t col = 0; col < n_col; ++col) {
    if (!equal_scalar_na_equal_p(types[col],
                                 R_NilValue, x_ptrs[col], i,
                                 R_NilValue, y_ptrs[col], j)) {
      return false;
    }
  }

  return true;
}

static int df_equal_missing(const void* x, R_len_t i) {
  struct dictionary_df_data* x_data = (struct dictionary_df_data*) x;

  enum vctrs_type* types = x_data->col_types;
  const void** x_ptrs = x_data->col_ptrs;
  R_len_t n_col = x_data->n_col;

  for (R_len_t col = 0; col < n_col; ++col) {
    enum vctrs_type type = types[col];

    // Raw doesn't have missing values
    if (type == vctrs_type_raw) {
      continue;
    }

    if (equal_scalar_na_equal_p(type,
                                R_NilValue, x_ptrs[col], i,
                                R_NilValue, vec_type_missing_value(type), 0)) {
      return true;
    }
  }

  return false;
}

static void init_dictionary_df(struct dictionary* d) {
  SEXP df = d->vec;
  R_len_t n_col = Rf_length(df);

  SEXP data_handle = PROTECT(Rf_allocVector(RAWSXP, sizeof(struct dictionary_df_data)));
  SEXP col_types_handle = PROTECT(Rf_allocVector(RAWSXP, n_col * sizeof(enum vctrs_type)));
  SEXP col_ptrs_handle = PROTECT(Rf_allocVector(RAWSXP, n_col * sizeof(void*)));

  SEXP handle = PROTECT(Rf_allocVector(VECSXP, 4));
  SET_VECTOR_ELT(handle, 0, d->protect);
  SET_VECTOR_ELT(handle, 1, data_handle);
  SET_VECTOR_ELT(handle, 2, col_types_handle);
  SET_VECTOR_ELT(handle, 3, col_ptrs_handle);


  struct dictionary_df_data* data = (struct dictionary_df_data*) RAW(data_handle);
  enum vctrs_type* col_types = (enum vctrs_type*) RAW(col_types_handle);
  const void** col_ptrs = (const void**) RAW(col_ptrs_handle);

  data->col_types = col_types;
  data->col_ptrs = col_ptrs;
  data->n_col = n_col;

  for (R_len_t i = 0; i < n_col; ++i) {
    SEXP col = VECTOR_ELT(df, i);
    enum vctrs_type col_type = vec_proxy_typeof(col);

    col_types[i] = col_type;

    if (col_type == vctrs_type_list) {
      col_ptrs[i] = col;
    } else {
      col_ptrs[i] = r_vec_deref_const(col);
    }
  }

  d->protect = handle;
  d->vec_p = data;
  d->equal = df_equal;
  d->equal_missing = &df_equal_missing;

  UNPROTECT(4);
}


// Dictionaries must be protected in consistent stack order with
// `PROTECT_DICT()`
struct dictionary* new_dictionary(SEXP x) {
  struct dictionary_opts opts = {
    .partial = false,
    .na_equal = true
  };
  return new_dictionary_opts(x, &opts);
}
struct dictionary* new_dictionary_partial(SEXP x) {
  struct dictionary_opts opts = {
    .partial = true,
    .na_equal = true
  };
  return new_dictionary_opts(x, &opts);
}

static struct dictionary* new_dictionary_params(SEXP x, bool partial, bool na_equal) {
  struct dictionary_opts opts;
  opts.partial = partial;
  opts.na_equal = na_equal;
  return new_dictionary_opts(x, &opts);
}

static struct dictionary* new_dictionary_opts(SEXP x, struct dictionary_opts* opts) {
  SEXP out = PROTECT(Rf_allocVector(RAWSXP, sizeof(struct dictionary)));
  struct dictionary* d = (struct dictionary*) RAW(out);

  d->vec = x;
  d->type = vec_proxy_typeof(x);
  d->protect = out;

  switch (d->type) {
  case vctrs_type_null: init_dictionary_nil(d); break;
  case vctrs_type_logical: init_dictionary_lgl(d); break;
  case vctrs_type_integer: init_dictionary_int(d); break;
  case vctrs_type_double: init_dictionary_dbl(d); break;
  case vctrs_type_complex: init_dictionary_cpl(d); break;
  case vctrs_type_character: init_dictionary_chr(d); break;
  case vctrs_type_raw: init_dictionary_raw(d); break;
  case vctrs_type_list: init_dictionary_list(d); break;
  case vctrs_type_dataframe: init_dictionary_df(d); break;
  default: stop_unimplemented_vctrs_type("new_dictionary_opts", d->type);
  }

  // `init_dictionary_*()` functions may allocate
  PROTECT(d->protect);

  d->used = 0;

  if (opts->partial) {
    d->key = NULL;
    d->size = 0;
  } else {
    // assume worst case, that every value is distinct, aiming for a load factor
    // of at most 77%. We round up to power of 2 to ensure quadratic probing
    // strategy works.
    // Rprintf("size: %i\n", size);
    R_len_t size = ceil2(vec_size(x) / 0.77);
    size = (size < 16) ? 16 : size;

    d->key = (R_len_t*) R_alloc(size, sizeof(R_len_t));
    memset(d->key, DICT_EMPTY, size * sizeof(R_len_t));

    d->size = size;
  }

  R_len_t n = vec_size(x);
  if (n) {
    d->hash = (uint32_t*) R_alloc(n, sizeof(uint32_t));

    if (!(d->hash)) {
      Rf_errorcall(R_NilValue, "Can't allocate hash lookup table. Please free memory.");
    }

    memset(d->hash, 0, n * sizeof(R_len_t));
    hash_fill(d->hash, n, x, opts->na_equal);
  } else {
    d->hash = NULL;
  }

  UNPROTECT(2);
  return d;
}


// Use hash from `x` but value from `d`. `x` does not need a full
// initialisation of the key vector and can be created with
// `new_dictionary_partial()`.
uint32_t dict_hash_with(struct dictionary* d, struct dictionary* x, R_len_t i) {
  uint32_t hash = x->hash[i];

  // Quadratic probing: will try every slot if d->size is power of 2
  // http://research.cs.vt.edu/AVresearch/hashing/quadratic.php
  for (uint32_t k = 0; k < d->size; ++k) {
    uint32_t probe = (hash + k * (k + 1) / 2) & (d->size - 1);
    // Rprintf("Probe: %i\n", probe);

    // If we circled back to start, dictionary is full
    if (k > 1 && probe == hash) {
      break;
    }

    // Check for unused slot
    R_len_t idx = d->key[probe];
    if (idx == DICT_EMPTY) {
      return probe;
    }

    // Check for same value as there might be a collision
    if (d->equal(d->vec_p, idx, x->vec_p, i)) {
      return probe;
    }

    // Collision. next iteration will find another spot using
    // quadratic probing.
  }

  stop_internal("dict_hash_with", "Dictionary is full.");
}

uint32_t dict_hash_scalar(struct dictionary* d, R_len_t i) {
  return dict_hash_with(d, d, i);
}

bool dict_is_missing(struct dictionary* d, R_len_t i) {
  return d->hash[i] == HASH_MISSING && d->equal_missing(d->vec_p, i);
}


void dict_put(struct dictionary* d, uint32_t hash, R_len_t i) {
  d->key[hash] = i;
  d->used++;
}

// R interface -----------------------------------------------------------------
// TODO: rename to match R function names
// TODO: separate out into individual files

SEXP vctrs_unique_loc(SEXP x) {
  int nprot = 0;

  R_len_t n = vec_size(x);

  x = PROTECT_N(vec_proxy_equal(x), &nprot);
  x = PROTECT_N(obj_maybe_translate_encoding(x, n), &nprot);

  struct dictionary* d = new_dictionary(x);
  PROTECT_DICT(d, &nprot);

  struct growable g = new_growable(INTSXP, 256);
  PROTECT_GROWABLE(&g, &nprot);

  for (int i = 0; i < n; ++i) {
    uint32_t hash = dict_hash_scalar(d, i);

    if (d->key[hash] == DICT_EMPTY) {
      dict_put(d, hash, i);
      growable_push_int(&g, i + 1);
    }
  }

  SEXP out = growable_values(&g);

  UNPROTECT(nprot);
  return out;
}

// [[ include("vctrs.h") ]]
SEXP vec_unique(SEXP x) {
  SEXP index = PROTECT(vctrs_unique_loc(x));
  SEXP out = vec_slice(x, index);
  UNPROTECT(1);
  return out;
}

SEXP vctrs_duplicated_any(SEXP x) {
  bool out = duplicated_any(x);
  return Rf_ScalarLogical(out);
}

// [[ include("vctrs.h") ]]
bool duplicated_any(SEXP x) {
  int nprot = 0;

  R_len_t n = vec_size(x);

  x = PROTECT_N(vec_proxy_equal(x), &nprot);
  x = PROTECT_N(obj_maybe_translate_encoding(x, n), &nprot);

  struct dictionary* d = new_dictionary(x);
  PROTECT_DICT(d, &nprot);

  bool out = false;

  for (int i = 0; i < n; ++i) {
    uint32_t hash = dict_hash_scalar(d, i);

    if (d->key[hash] == DICT_EMPTY) {
      dict_put(d, hash, i);
    } else {
      out = true;
      break;
    }
  }

  UNPROTECT(nprot);
  return out;
}

// [[ export() ]]
SEXP vctrs_all_duplicate(SEXP x) {
  int nprot = 0;

  R_len_t n = vec_size(x);

  x = PROTECT_N(vec_proxy_equal(x), &nprot);
  x = PROTECT_N(obj_maybe_translate_encoding(x, n), &nprot);

  dictionary d;
  dict_init(&d, x);
  PROTECT_DICT(&d, &nprot);

  // Initialize to 0. Some hash locations may never be touched but we
  // iterate over them later so they need a value.
  SEXP counts = PROTECT_N(Rf_allocVector(INTSXP, d.size), &nprot);
  int* p_counts = INTEGER(counts);
  memset(p_counts, 0, d.size * sizeof(int));

  for (int i = 0; i < n; ++i) {
    int32_t hash = dict_hash_scalar(&d, i);

    if (d.key[hash] == DICT_EMPTY) {
      dict_put(&d, hash, i);
    }

    ++p_counts[hash];
  }

  int out = 1;

  for (int i = 0; i < d.size; ++i) {
    if (p_counts[i] == 1) {
      out = 0;
      break;
    }
  }

  UNPROTECT(nprot);
  return Rf_ScalarLogical(out);
}

SEXP vctrs_n_distinct(SEXP x) {
  int nprot = 0;

  R_len_t n = vec_size(x);

  x = PROTECT_N(vec_proxy_equal(x), &nprot);
  x = PROTECT_N(obj_maybe_translate_encoding(x, n), &nprot);

  struct dictionary* d = new_dictionary(x);
  PROTECT_DICT(d, &nprot);

  for (int i = 0; i < n; ++i) {
    uint32_t hash = dict_hash_scalar(d, i);

    if (d->key[hash] == DICT_EMPTY) {
      dict_put(d, hash, i);
    }
  }

  UNPROTECT(nprot);
  return Rf_ScalarInteger(d->used);
}

SEXP vctrs_id(SEXP x) {
  int nprot = 0;

  R_len_t n = vec_size(x);

  x = PROTECT_N(vec_proxy_equal(x), &nprot);
  x = PROTECT_N(obj_maybe_translate_encoding(x, n), &nprot);

  struct dictionary* d = new_dictionary(x);
  PROTECT_DICT(d, &nprot);

  SEXP out = PROTECT_N(Rf_allocVector(INTSXP, n), &nprot);
  int* p_out = INTEGER(out);

  for (int i = 0; i < n; ++i) {
    uint32_t hash = dict_hash_scalar(d, i);

    if (d->key[hash] == DICT_EMPTY) {
      dict_put(d, hash, i);
    }
    p_out[i] = d->key[hash] + 1;
  }

  UNPROTECT(nprot);
  return out;
}

// [[ register() ]]
SEXP vctrs_match(SEXP needles, SEXP haystack, SEXP na_equal,
                 SEXP needles_arg_, SEXP haystack_arg_) {
  struct vctrs_arg needles_arg = vec_as_arg(needles_arg_);
  struct vctrs_arg haystack_arg = vec_as_arg(haystack_arg_);

  return vec_match_params(needles,
                          haystack,
                          r_bool_as_int(na_equal),
                          &needles_arg,
                          &haystack_arg);
}

static inline void vec_match_loop(int* p_out,
                                  struct dictionary* d,
                                  struct dictionary* d_needles,
                                  R_len_t n_needle);
static inline void vec_match_loop_propagate(int* p_out,
                                            struct dictionary* d,
                                            struct dictionary* d_needles,
                                            R_len_t n_needle);

SEXP vec_match_params(SEXP needles,
                      SEXP haystack,
                      bool na_equal,
                      struct vctrs_arg* needles_arg,
                      struct vctrs_arg* haystack_arg) {
  int nprot = 0;
  int _;
  SEXP type = vec_ptype2_params(needles, haystack,
                                needles_arg, haystack_arg,
                                DF_FALLBACK_quiet,
                                &_);
  PROTECT_N(type, &nprot);

  needles = vec_cast_params(needles, type,
                            needles_arg, args_empty,
                            DF_FALLBACK_quiet,
                            S3_FALLBACK_false);
  PROTECT_N(needles, &nprot);

  haystack = vec_cast_params(haystack, type,
                             haystack_arg, args_empty,
                             DF_FALLBACK_quiet,
                             S3_FALLBACK_false);
  PROTECT_N(haystack, &nprot);

  needles = PROTECT_N(vec_proxy_equal(needles), &nprot);
  haystack = PROTECT_N(vec_proxy_equal(haystack), &nprot);

  R_len_t n_haystack = vec_size(haystack);
  R_len_t n_needle = vec_size(needles);

  SEXP translated = PROTECT_N(obj_maybe_translate_encoding2(needles, n_needle, haystack, n_haystack), &nprot);
  needles = VECTOR_ELT(translated, 0);
  haystack = VECTOR_ELT(translated, 1);

  struct dictionary* d = new_dictionary_params(haystack, false, na_equal);
  PROTECT_DICT(d, &nprot);

  // Load dictionary with haystack
  for (int i = 0; i < n_haystack; ++i) {
    uint32_t hash = dict_hash_scalar(d, i);

    if (d->key[hash] == DICT_EMPTY) {
      dict_put(d, hash, i);
    }
  }

  struct dictionary* d_needles = new_dictionary_params(needles, true, na_equal);
  PROTECT_DICT(d_needles, &nprot);

  // Locate needles
  SEXP out = PROTECT_N(Rf_allocVector(INTSXP, n_needle), &nprot);
  int* p_out = INTEGER(out);

  if (na_equal) {
    vec_match_loop(p_out, d, d_needles, n_needle);
  } else {
    vec_match_loop_propagate(p_out, d, d_needles, n_needle);
  }

  UNPROTECT(nprot);
  return out;
}

static inline void vec_match_loop(int* p_out,
                                  struct dictionary* d,
                                  struct dictionary* d_needles,
                                  R_len_t n_needle) {
  for (R_len_t i = 0; i < n_needle; ++i) {
    uint32_t hash = dict_hash_with(d, d_needles, i);

    if (d->key[hash] == DICT_EMPTY) {
      // TODO: Return `no_match` instead
      p_out[i] = NA_INTEGER;
    } else {
      p_out[i] = d->key[hash] + 1;
    }
  }
}
static inline void vec_match_loop_propagate(int* p_out,
                                            struct dictionary* d,
                                            struct dictionary* d_needles,
                                            R_len_t n_needle) {
  for (R_len_t i = 0; i < n_needle; ++i) {
    if (dict_is_missing(d_needles, i)) {
      p_out[i] = NA_INTEGER;
      continue;
    }

    uint32_t hash = dict_hash_with(d, d_needles, i);

    if (d->key[hash] == DICT_EMPTY) {
      // TODO: Return `no_match` instead
      p_out[i] = NA_INTEGER;
    } else {
      p_out[i] = d->key[hash] + 1;
    }
  }
}

// [[ register() ]]
SEXP vctrs_in(SEXP needles, SEXP haystack, SEXP na_equal_,
              SEXP needles_arg_, SEXP haystack_arg_) {
  int nprot = 0;
  bool na_equal = r_bool_as_int(na_equal_);

  int _;
  struct vctrs_arg needles_arg = vec_as_arg(needles_arg_);
  struct vctrs_arg haystack_arg = vec_as_arg(haystack_arg_);

  SEXP type = vec_ptype2_params(needles, haystack,
                                &needles_arg, &haystack_arg,
                                DF_FALLBACK_quiet,
                                &_);
  PROTECT_N(type, &nprot);

  needles = vec_cast_params(needles, type,
                            &needles_arg, args_empty,
                            DF_FALLBACK_quiet,
                            S3_FALLBACK_false);
  PROTECT_N(needles, &nprot);

  haystack = vec_cast_params(haystack, type,
                             &haystack_arg, args_empty,
                             DF_FALLBACK_quiet,
                             S3_FALLBACK_false);
  PROTECT_N(haystack, &nprot);

  needles = PROTECT_N(vec_proxy_equal(needles), &nprot);
  haystack = PROTECT_N(vec_proxy_equal(haystack), &nprot);

  R_len_t n_haystack = vec_size(haystack);
  R_len_t n_needle = vec_size(needles);

  SEXP translated = PROTECT_N(obj_maybe_translate_encoding2(needles, n_needle, haystack, n_haystack), &nprot);
  needles = VECTOR_ELT(translated, 0);
  haystack = VECTOR_ELT(translated, 1);

  struct dictionary* d = new_dictionary_params(haystack, false, na_equal);
  PROTECT_DICT(d, &nprot);

  // Load dictionary with haystack
  for (int i = 0; i < n_haystack; ++i) {
    uint32_t hash = dict_hash_scalar(d, i);

    if (d->key[hash] == DICT_EMPTY) {
      dict_put(d, hash, i);
    }
  }

  struct dictionary* d_needles = new_dictionary_params(needles, true, na_equal);
  PROTECT_DICT(d_needles, &nprot);

  // Locate needles
  SEXP out = PROTECT_N(Rf_allocVector(LGLSXP, n_needle), &nprot);
  int* p_out = LOGICAL(out);

  bool propagate = !na_equal;

  for (int i = 0; i < n_needle; ++i) {
    if (propagate && dict_is_missing(d_needles, i)) {
      p_out[i] = NA_LOGICAL;
    } else {
      uint32_t hash = dict_hash_with(d, d_needles, i);
      p_out[i] = (d->key[hash] != DICT_EMPTY);
    }
  }

  UNPROTECT(nprot);
  return out;
}

SEXP vctrs_count(SEXP x) {
  int nprot = 0;

  R_len_t n = vec_size(x);

  x = PROTECT_N(vec_proxy_equal(x), &nprot);
  x = PROTECT_N(obj_maybe_translate_encoding(x, n), &nprot);

  struct dictionary* d = new_dictionary(x);
  PROTECT_DICT(d, &nprot);

  SEXP val = PROTECT_N(Rf_allocVector(INTSXP, d->size), &nprot);
  int* p_val = INTEGER(val);

  for (int i = 0; i < n; ++i) {
    int32_t hash = dict_hash_scalar(d, i);

    if (d->key[hash] == DICT_EMPTY) {
      dict_put(d, hash, i);
      p_val[hash] = 0;
    }
    p_val[hash]++;
  }

  // Create output
  SEXP out_key = PROTECT_N(Rf_allocVector(INTSXP, d->used), &nprot);
  SEXP out_val = PROTECT_N(Rf_allocVector(INTSXP, d->used), &nprot);
  int* p_out_key = INTEGER(out_key);
  int* p_out_val = INTEGER(out_val);

  int i = 0;
  for (int hash = 0; hash < d->size; ++hash) {
    if (d->key[hash] == DICT_EMPTY)
      continue;

    p_out_key[i] = d->key[hash] + 1;
    p_out_val[i] = p_val[hash];
    i++;
  }

  SEXP out = PROTECT_N(Rf_allocVector(VECSXP, 2), &nprot);
  SET_VECTOR_ELT(out, 0, out_key);
  SET_VECTOR_ELT(out, 1, out_val);
  SEXP names = PROTECT_N(Rf_allocVector(STRSXP, 2), &nprot);
  SET_STRING_ELT(names, 0, Rf_mkChar("key"));
  SET_STRING_ELT(names, 1, Rf_mkChar("val"));
  Rf_setAttrib(out, R_NamesSymbol, names);

  UNPROTECT(nprot);
  return out;
}

SEXP vctrs_duplicated(SEXP x) {
  int nprot = 0;

  R_len_t n = vec_size(x);

  x = PROTECT_N(vec_proxy_equal(x), &nprot);
  x = PROTECT_N(obj_maybe_translate_encoding(x, n), &nprot);

  struct dictionary* d = new_dictionary(x);
  PROTECT_DICT(d, &nprot);

  SEXP val = PROTECT_N(Rf_allocVector(INTSXP, d->size), &nprot);
  int* p_val = INTEGER(val);

  for (int i = 0; i < n; ++i) {
    int32_t hash = dict_hash_scalar(d, i);

    if (d->key[hash] == DICT_EMPTY) {
      dict_put(d, hash, i);
      p_val[hash] = 0;
    }
    p_val[hash]++;
  }

  // Create output
  SEXP out = PROTECT_N(Rf_allocVector(LGLSXP, n), &nprot);
  int* p_out = LOGICAL(out);

  for (int i = 0; i < n; ++i) {
    int32_t hash = dict_hash_scalar(d, i);
    p_out[i] = p_val[hash] != 1;
  }

  UNPROTECT(nprot);
  return out;
}


void vctrs_init_dictionary(SEXP ns) {
  args_needles = new_wrapper_arg(NULL, "needles");
  args_haystack = new_wrapper_arg(NULL, "haystack");
}
