#include "vctrs.h"
#include "dictionary.h"
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

// Dictionary functions assume that `x` has been proxied recursively because:
// - `dict_init_impl()` uses `hash_fill()`
// - `dict_hash_with()` uses `equal_scalar()`

static void dict_init_impl(dictionary* d, SEXP x, bool partial);

// Dictionaries must be protected and unprotected in consistent stack
// order with `PROTECT_DICT()` and `UNPROTECT_DICT()`.
void dict_init(dictionary* d, SEXP x) {
  dict_init_impl(d, x, false);
}
void dict_init_partial(dictionary* d, SEXP x) {
  dict_init_impl(d, x, true);
}

static void dict_init_impl(dictionary* d, SEXP x, bool partial) {
  d->vec = x;
  d->used = 0;

  if (partial) {
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
  d->hash = (uint32_t*) R_alloc(n, sizeof(uint32_t));

  if (d->hash) {
    memset(d->hash, 0, n * sizeof(R_len_t));
    hash_fill(d->hash, n, x);
  }
}

uint32_t dict_hash_with(dictionary* d, dictionary* x, R_len_t i) {
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

    // Check for same value as there might be a collision. If there is
    // a collision, next iteration will find another spot using
    // quadratic probing.
    if (equal_scalar(d->vec, idx, x->vec, i, true)) {
      return probe;
    }
  }

  Rf_errorcall(R_NilValue, "Internal error: Dictionary is full!");
}

uint32_t dict_hash_scalar(dictionary* d, R_len_t i) {
  return dict_hash_with(d, d, i);
}


void dict_put(dictionary* d, uint32_t hash, R_len_t i) {
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

  dictionary d;
  dict_init(&d, x);
  PROTECT_DICT(&d, &nprot);

  struct growable g = new_growable(INTSXP, 256);
  PROTECT_GROWABLE(&g, &nprot);

  for (int i = 0; i < n; ++i) {
    uint32_t hash = dict_hash_scalar(&d, i);

    if (d.key[hash] == DICT_EMPTY) {
      dict_put(&d, hash, i);
      growable_push_int(&g, i + 1);
    }
  }

  SEXP out = growable_values(&g);

  UNPROTECT(nprot);
  return out;
}

SEXP vec_duplicate_loc_impl(SEXP x, R_len_t n);
SEXP vec_duplicate_loc_first_impl(SEXP x, R_len_t n);

SEXP vec_duplicate_loc(SEXP x, bool first) {
  R_len_t n = vec_size(x);

  x = PROTECT(vec_proxy_equal(x));
  x = PROTECT(obj_maybe_translate_encoding(x, n));

  SEXP out;

  if (first) {
    out = vec_duplicate_loc_first_impl(x, n);
  } else {
    out = vec_duplicate_loc_impl(x, n);
  }

  UNPROTECT(2);
  return out;
}

SEXP vctrs_duplicate_loc(SEXP x, SEXP first) {
  return vec_duplicate_loc(x, Rf_asLogical(first));
}

SEXP vec_duplicate_loc_impl(SEXP x, R_len_t n) {
  int nprot = 0;

  dictionary d;
  dict_init(&d, x);
  PROTECT_DICT(&d, &nprot);

  struct growable g = new_growable(INTSXP, 256);
  PROTECT_GROWABLE(&g, &nprot);

  for (int i = 0; i < n; ++i) {
    uint32_t hash = dict_hash_scalar(&d, i);

    if (d.key[hash] == DICT_EMPTY) {
      dict_put(&d, hash, i);
    } else {
      growable_push_int(&g, i + 1);
    }
  }

  SEXP out = growable_values(&g);

  UNPROTECT(nprot);
  return out;
}

SEXP vec_duplicate_loc_first_impl(SEXP x, R_len_t n) {
  int nprot = 0;

  dictionary d;
  dict_init(&d, x);
  PROTECT_DICT(&d, &nprot);

  SEXP val = PROTECT_N(Rf_allocVector(INTSXP, d.size), &nprot);
  int* p_val = INTEGER(val);

  // Store hash values so we don't have to look them up twice
  int32_t* hashes = (int32_t*) R_alloc(n, sizeof(int32_t));

  R_len_t n_out = 0;

  for (int i = 0; i < n; ++i) {
    int32_t hash = dict_hash_scalar(&d, i);
    hashes[i] = hash;

    if (d.key[hash] == DICT_EMPTY) {
      dict_put(&d, hash, i);
      p_val[hash] = 1;
      continue;
    }

    ++p_val[hash];

    if (p_val[hash] == 2) {
      n_out += 2;
    } else {
      n_out += 1;
    }
  }

  // Create output
  SEXP out = PROTECT_N(Rf_allocVector(INTSXP, n_out), &nprot);
  int* p_out = INTEGER(out);

  int j = 0;

  for (int i = 0; i < n; ++i) {
    int32_t hash = hashes[i];

    if (p_val[hash] != 1) {
      p_out[j] = i + 1;
      ++j;
    }
  }

  UNPROTECT(nprot);
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

  dictionary d;
  dict_init(&d, x);
  PROTECT_DICT(&d, &nprot);

  bool out = false;

  for (int i = 0; i < n; ++i) {
    uint32_t hash = dict_hash_scalar(&d, i);

    if (d.key[hash] == DICT_EMPTY) {
      dict_put(&d, hash, i);
    } else {
      out = true;
      break;
    }
  }

  UNPROTECT(nprot);
  return out;
}

SEXP vctrs_n_distinct(SEXP x) {
  int nprot = 0;

  R_len_t n = vec_size(x);

  x = PROTECT_N(vec_proxy_equal(x), &nprot);
  x = PROTECT_N(obj_maybe_translate_encoding(x, n), &nprot);

  dictionary d;
  dict_init(&d, x);
  PROTECT_DICT(&d, &nprot);

  for (int i = 0; i < n; ++i) {
    uint32_t hash = dict_hash_scalar(&d, i);

    if (d.key[hash] == DICT_EMPTY)
      dict_put(&d, hash, i);
  }

  UNPROTECT(nprot);
  return Rf_ScalarInteger(d.used);
}

SEXP vctrs_first_loc(SEXP x) {
  int nprot = 0;

  R_len_t n = vec_size(x);

  x = PROTECT_N(vec_proxy_equal(x), &nprot);
  x = PROTECT_N(obj_maybe_translate_encoding(x, n), &nprot);

  dictionary d;
  dict_init(&d, x);
  PROTECT_DICT(&d, &nprot);

  SEXP out = PROTECT_N(Rf_allocVector(INTSXP, n), &nprot);
  int* p_out = INTEGER(out);

  for (int i = 0; i < n; ++i) {
    uint32_t hash = dict_hash_scalar(&d, i);

    if (d.key[hash] == DICT_EMPTY) {
      dict_put(&d, hash, i);
    }
    p_out[i] = d.key[hash] + 1;
  }

  UNPROTECT(nprot);
  return out;
}

// [[ register() ]]
SEXP vctrs_match(SEXP needles, SEXP haystack) {
  int nprot = 0;
  int _;
  SEXP type = PROTECT_N(vec_type2(needles, haystack, &args_needles, &args_haystack, &_), &nprot);

  needles = PROTECT_N(vec_cast(needles, type, args_empty, args_empty), &nprot);
  haystack = PROTECT_N(vec_cast(haystack, type, args_empty, args_empty), &nprot);

  needles = PROTECT_N(vec_proxy_equal(needles), &nprot);
  haystack = PROTECT_N(vec_proxy_equal(haystack), &nprot);

  R_len_t n_haystack = vec_size(haystack);
  R_len_t n_needle = vec_size(needles);

  SEXP translated = PROTECT_N(obj_maybe_translate_encoding2(needles, n_needle, haystack, n_haystack), &nprot);
  needles = VECTOR_ELT(translated, 0);
  haystack = VECTOR_ELT(translated, 1);

  dictionary d;
  dict_init(&d, haystack);
  PROTECT_DICT(&d, &nprot);

  // Load dictionary with haystack
  for (int i = 0; i < n_haystack; ++i) {
    uint32_t hash = dict_hash_scalar(&d, i);

    if (d.key[hash] == DICT_EMPTY) {
      dict_put(&d, hash, i);
    }
  }

  dictionary d_needles;
  dict_init_partial(&d_needles, needles);

  // Locate needles
  SEXP out = PROTECT_N(Rf_allocVector(INTSXP, n_needle), &nprot);
  int* p_out = INTEGER(out);

  for (int i = 0; i < n_needle; ++i) {
    uint32_t hash = dict_hash_with(&d, &d_needles, i);
    if (d.key[hash] == DICT_EMPTY) {
      p_out[i] = NA_INTEGER;
    } else {
      p_out[i] = d.key[hash] + 1;
    }
  }

  UNPROTECT(nprot);
  return out;
}

// [[ register() ]]
SEXP vctrs_in(SEXP needles, SEXP haystack) {
  int nprot = 0;

  int _;
  SEXP type = PROTECT_N(vec_type2(needles, haystack, &args_needles, &args_haystack, &_), &nprot);

  needles = PROTECT_N(vec_cast(needles, type, args_empty, args_empty), &nprot);
  haystack = PROTECT_N(vec_cast(haystack, type, args_empty, args_empty), &nprot);

  needles = PROTECT_N(vec_proxy_equal(needles), &nprot);
  haystack = PROTECT_N(vec_proxy_equal(haystack), &nprot);

  R_len_t n_haystack = vec_size(haystack);
  R_len_t n_needle = vec_size(needles);

  SEXP translated = PROTECT_N(obj_maybe_translate_encoding2(needles, n_needle, haystack, n_haystack), &nprot);
  needles = VECTOR_ELT(translated, 0);
  haystack = VECTOR_ELT(translated, 1);

  dictionary d;
  dict_init(&d, haystack);
  PROTECT_DICT(&d, &nprot);

  // Load dictionary with haystack
  for (int i = 0; i < n_haystack; ++i) {
    uint32_t hash = dict_hash_scalar(&d, i);

    if (d.key[hash] == DICT_EMPTY) {
      dict_put(&d, hash, i);
    }
  }

  dictionary d_needles;
  dict_init_partial(&d_needles, needles);
  PROTECT_DICT(&d_needles, &nprot);

  // Locate needles
  SEXP out = PROTECT_N(Rf_allocVector(LGLSXP, n_needle), &nprot);
  int* p_out = LOGICAL(out);

  for (int i = 0; i < n_needle; ++i) {
    uint32_t hash = dict_hash_with(&d, &d_needles, i);
    p_out[i] = (d.key[hash] != DICT_EMPTY);
  }

  UNPROTECT(nprot);
  return out;
}

SEXP vctrs_count(SEXP x) {
  int nprot = 0;

  R_len_t n = vec_size(x);

  x = PROTECT_N(vec_proxy_equal(x), &nprot);
  x = PROTECT_N(obj_maybe_translate_encoding(x, n), &nprot);

  dictionary d;
  dict_init(&d, x);
  PROTECT_DICT(&d, &nprot);

  SEXP val = PROTECT_N(Rf_allocVector(INTSXP, d.size), &nprot);
  int* p_val = INTEGER(val);

  for (int i = 0; i < n; ++i) {
    int32_t hash = dict_hash_scalar(&d, i);

    if (d.key[hash] == DICT_EMPTY) {
      dict_put(&d, hash, i);
      p_val[hash] = 0;
    }
    p_val[hash]++;
  }

  // Create output
  SEXP out_key = PROTECT_N(Rf_allocVector(INTSXP, d.used), &nprot);
  SEXP out_val = PROTECT_N(Rf_allocVector(INTSXP, d.used), &nprot);
  int* p_out_key = INTEGER(out_key);
  int* p_out_val = INTEGER(out_val);

  int i = 0;
  for (int hash = 0; hash < d.size; ++hash) {
    if (d.key[hash] == DICT_EMPTY)
      continue;

    p_out_key[i] = d.key[hash] + 1;
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

SEXP vec_duplicate_flg_impl(SEXP x, R_len_t n);
SEXP vec_duplicate_flg_first_impl(SEXP x, R_len_t n);

SEXP vec_duplicate_flg(SEXP x, bool first) {
  int nprot = 0;

  R_len_t n = vec_size(x);

  x = PROTECT(vec_proxy_equal(x));
  x = PROTECT(obj_maybe_translate_encoding(x, n));

  SEXP out;

  if (first) {
    out = vec_duplicate_flg_first_impl(x, n);
  } else {
    out = vec_duplicate_flg_impl(x, n);
  }

  UNPROTECT(2);
  return out;
}

// [[ export() ]]
SEXP vctrs_duplicate_flg(SEXP x, SEXP first) {
  return vec_duplicate_flg(x, Rf_asLogical(first));
}

SEXP vec_duplicate_flg_impl(SEXP x, R_len_t n) {
  int nprot = 0;

  dictionary d;
  dict_init(&d, x);
  PROTECT_DICT(&d, &nprot);

  SEXP out = PROTECT_N(Rf_allocVector(LGLSXP, n), &nprot);
  int* p_out = LOGICAL(out);
  memset(p_out, 0, n * sizeof(int));

  for (int i = 0; i < n; ++i) {
    int32_t hash = dict_hash_scalar(&d, i);

    if (d.key[hash] == DICT_EMPTY) {
      dict_put(&d, hash, i);
    } else {
      p_out[i] = 1;
    }
  }

  UNPROTECT(nprot);
  return out;
}

SEXP vec_duplicate_flg_first_impl(SEXP x, R_len_t n) {
  int nprot = 0;

  dictionary d;
  dict_init(&d, x);
  PROTECT_DICT(&d, &nprot);

  SEXP val = PROTECT_N(Rf_allocVector(INTSXP, d.size), &nprot);
  int* p_val = INTEGER(val);
  memset(p_val, 0, d.size * sizeof(int));

  // Store hash values so we don't have to look them up twice
  int32_t* hashes = (int32_t*) R_alloc(n, sizeof(int32_t));

  for (int i = 0; i < n; ++i) {
    int32_t hash = dict_hash_scalar(&d, i);
    hashes[i] = hash;

    if (d.key[hash] == DICT_EMPTY) {
      dict_put(&d, hash, i);
    } else {
      p_val[hash] = 1;
    }
  }

  // Create output
  SEXP out = PROTECT_N(Rf_allocVector(LGLSXP, n), &nprot);
  int* p_out = LOGICAL(out);

  for (int i = 0; i < n; ++i) {
    p_out[i] = p_val[hashes[i]];
  }

  UNPROTECT(nprot);
  return out;
}


void vctrs_init_dictionary(SEXP ns) {
  args_needles = new_wrapper_arg(NULL, "needles");
  args_haystack = new_wrapper_arg(NULL, "haystack");
}
