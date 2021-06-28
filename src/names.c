#include <ctype.h>
#include <rlang.h>
#include "vctrs.h"
#include "type-data-frame.h"
#include "utils.h"
#include "dim.h"

static void describe_repair(SEXP old_names, SEXP new_names);

// 3 leading '.' + 1 trailing '\0' + 24 characters
#define MAX_IOTA_SIZE 28

// Initialised at load time
SEXP syms_as_universal_names = NULL;
SEXP syms_validate_unique_names = NULL;
SEXP fns_as_universal_names = NULL;
SEXP fns_validate_unique_names = NULL;

// Defined below
SEXP vctrs_as_minimal_names(SEXP names);
SEXP vec_as_universal_names(SEXP names, bool quiet);
SEXP vec_validate_unique_names(SEXP names, struct vctrs_arg* arg);
SEXP vec_as_custom_names(SEXP names, const struct name_repair_opts* opts);
static void vec_validate_minimal_names(SEXP names, R_len_t n);


// [[ include("names.h") ]]
SEXP vec_as_names(SEXP names, const struct name_repair_opts* opts) {
  if (!opts) {
    return names;
  }
  switch (opts->type) {
  case name_repair_none: return names;
  case name_repair_minimal: return vctrs_as_minimal_names(names);
  case name_repair_unique: return vec_as_unique_names(names, opts->quiet);
  case name_repair_universal: return vec_as_universal_names(names, opts->quiet);
  case name_repair_check_unique: return vec_validate_unique_names(names, opts->arg);
  case name_repair_custom: return vec_as_custom_names(names, opts);
  }
  never_reached("vec_as_names");
}

// [[ register() ]]
SEXP vctrs_as_names(SEXP names, SEXP repair, SEXP repair_arg, SEXP quiet) {
  if (!r_is_bool(quiet)) {
    Rf_errorcall(R_NilValue, "`quiet` must a boolean value.");
  }
  bool quiet_ = LOGICAL(quiet)[0];

  struct vctrs_arg arg_ = vec_as_arg(repair_arg);

  struct name_repair_opts repair_opts = new_name_repair_opts(repair, &arg_, quiet_);
  PROTECT_NAME_REPAIR_OPTS(&repair_opts);

  SEXP out = vec_as_names(names, &repair_opts);

  UNPROTECT(1);
  return out;
}

SEXP vec_as_universal_names(SEXP names, bool quiet) {
  SEXP quiet_obj = PROTECT(r_lgl(quiet));
  SEXP out = vctrs_dispatch2(syms_as_universal_names, fns_as_universal_names,
                             syms_names, names,
                             syms_quiet, quiet_obj);
  UNPROTECT(1);
  return out;
}
SEXP vec_validate_unique_names(SEXP names, struct vctrs_arg* arg) {
  SEXP arg_obj = PROTECT(vctrs_arg(arg));

  SEXP out = PROTECT(vctrs_dispatch2(syms_validate_unique_names, fns_validate_unique_names,
                                     syms_names, names,
                                     syms_arg, arg_obj));

  // Restore visibility
  Rf_eval(R_NilValue, R_EmptyEnv);

  UNPROTECT(2);
  return out;
}

SEXP vec_as_custom_names(SEXP names, const struct name_repair_opts* opts) {
  names = PROTECT(vctrs_as_minimal_names(names));

  // Don't use vctrs dispatch utils because we match argument positionally
  SEXP call = PROTECT(Rf_lang2(syms_repair, syms_names));
  SEXP mask = PROTECT(r_new_environment(R_GlobalEnv));
  Rf_defineVar(syms_repair, opts->fn, mask);
  Rf_defineVar(syms_names, names, mask);
  SEXP out = PROTECT(Rf_eval(call, mask));

  vec_validate_minimal_names(out, Rf_length(names));

  UNPROTECT(4);
  return out;
}

static
SEXP vec_names_impl(SEXP x, bool proxy) {
  bool has_class = OBJECT(x);

  if (has_class && Rf_inherits(x, "data.frame")) {
    // Only return row names if they are character. Data frames with
    // automatic row names are treated as unnamed.
    SEXP rn = df_rownames(x);
    if (rownames_type(rn) == ROWNAMES_IDENTIFIERS) {
      return rn;
    } else {
      return R_NilValue;
    }
  }

  if (vec_bare_dim(x) == R_NilValue) {
    if (!proxy && has_class) {
      return vctrs_dispatch1(syms_names, fns_names, syms_x, x);
    } else {
      return r_names(x);
    }
  }

  SEXP dimnames = PROTECT(r_attrib_get(x, R_DimNamesSymbol));
  if (dimnames == R_NilValue || Rf_length(dimnames) < 1) {
    UNPROTECT(1);
    return R_NilValue;
  }

  SEXP out = VECTOR_ELT(dimnames, 0);
  UNPROTECT(1);
  return out;
}

// [[ register(); include("vctrs.h") ]]
SEXP vec_names(SEXP x) {
  return vec_names_impl(x, false);
}
// [[ include("vctrs.h") ]]
SEXP vec_proxy_names(SEXP x) {
  return vec_names_impl(x, true);
}

// [[ register() ]]
SEXP vctrs_as_minimal_names(SEXP names) {
  if (TYPEOF(names) != STRSXP) {
    Rf_errorcall(R_NilValue, "`names` must be a character vector");
  }

  R_len_t i = 0;
  R_len_t n = Rf_length(names);
  const SEXP* ptr = STRING_PTR_RO(names);

  for (; i < n; ++i, ++ptr) {
    SEXP elt = *ptr;
    if (elt == NA_STRING) {
      break;
    }
  }
  if (i == n) {
    return names;
  }

  names = PROTECT(Rf_shallow_duplicate(names));

  for (; i < n; ++i, ++ptr) {
    SEXP elt = *ptr;
    if (elt == NA_STRING) {
      SET_STRING_ELT(names, i, strings_empty);
    }
  }

  UNPROTECT(1);
  return names;
}

// [[ register() ]]
SEXP vctrs_minimal_names(SEXP x) {
  SEXP names = PROTECT(vec_names(x));

  if (names == R_NilValue) {
    names = Rf_allocVector(STRSXP, vec_size(x));
  } else {
    names = vctrs_as_minimal_names(names);
  }

  UNPROTECT(1);
  return names;
}


// From dictionary.c
SEXP vctrs_duplicated(SEXP x);

static bool any_has_suffix(SEXP names);
static SEXP as_unique_names_impl(SEXP names, bool quiet);
static void stop_large_name();
static bool is_dotdotint(const char* name);
static ptrdiff_t suffix_pos(const char* name);
static bool needs_suffix(SEXP str);

// [[ include("vctrs.h") ]]
SEXP vec_as_unique_names(SEXP names, bool quiet) {
  if (is_unique_names(names) && !any_has_suffix(names)) {
    return names;
  } else {
    return(as_unique_names_impl(names, quiet));
  }
}

// [[ include("vctrs.h") ]]
bool is_unique_names(SEXP names) {
  if (TYPEOF(names) != STRSXP) {
    Rf_errorcall(R_NilValue, "`names` must be a character vector");
  }

  R_len_t n = Rf_length(names);
  const SEXP* names_ptr = STRING_PTR_RO(names);

  if (duplicated_any(names)) {
    return false;
  }

  for (R_len_t i = 0; i < n; ++i) {
    SEXP elt = names_ptr[i];

    if (needs_suffix(elt)) {
      return false;
    }
  }

  return true;
}

bool any_has_suffix(SEXP names) {
  R_len_t n = Rf_length(names);
  const SEXP* names_ptr = STRING_PTR_RO(names);

  for (R_len_t i = 0; i < n; ++i) {
    SEXP elt = names_ptr[i];

    if (suffix_pos(CHAR(elt)) >= 0) {
      return true;
    }
  }

  return false;
}

SEXP as_unique_names_impl(SEXP names, bool quiet) {
  R_len_t n = Rf_length(names);

  SEXP new_names = PROTECT(Rf_shallow_duplicate(names));
  const SEXP* new_names_ptr = STRING_PTR_RO(new_names);

  for (R_len_t i = 0; i < n; ++i) {
    SEXP elt = new_names_ptr[i];

    // Set `NA` and dots values to "" so they get replaced by `...n`
    // later on
    if (needs_suffix(elt)) {
      elt = strings_empty;
      SET_STRING_ELT(new_names, i, elt);
      continue;
    }

    // Strip `...n` suffixes
    const char* nm = CHAR(elt);
    int pos = suffix_pos(nm);
    if (pos >= 0) {
      elt = Rf_mkCharLenCE(nm, pos, Rf_getCharCE(elt));
      SET_STRING_ELT(new_names, i, elt);
      continue;
    }
  }

  // Append all duplicates with a suffix

  SEXP dups = PROTECT(vctrs_duplicated(new_names));
  const int* dups_ptr = LOGICAL_RO(dups);

  for (R_len_t i = 0; i < n; ++i) {
    SEXP elt = new_names_ptr[i];

    if (elt != strings_empty && !dups_ptr[i]) {
      continue;
    }

    const char* name = CHAR(elt);

    int size = strlen(name);
    int buf_size = size + MAX_IOTA_SIZE;

    R_CheckStack2(buf_size);
    char buf[buf_size];
    buf[0] = '\0';

    memcpy(buf, name, size);
    int remaining = buf_size - size;

    int needed = snprintf(buf + size, remaining, "...%d", i + 1);
    if (needed >= remaining) {
      stop_large_name();
    }

    SET_STRING_ELT(new_names, i, Rf_mkCharLenCE(buf, size + needed, Rf_getCharCE(elt)));
  }

  if (!quiet) {
    describe_repair(names, new_names);
  }

  UNPROTECT(2);
  return new_names;
}

SEXP vctrs_as_unique_names(SEXP names, SEXP quiet) {
  SEXP out = PROTECT(vec_as_unique_names(names, LOGICAL(quiet)[0]));
  UNPROTECT(1);
  return out;
}

SEXP vctrs_is_unique_names(SEXP names) {
  bool out = is_unique_names(names);
  return Rf_ScalarLogical(out);
}

static bool is_dotdotint(const char* name) {
  int n = strlen(name);

  if (n < 3) {
    return false;
  }
  if (name[0] != '.' || name[1] != '.') {
    return false;
  }

  if (name[2] == '.') {
    name += 3;
  } else {
    name += 2;
  }

  return (bool) strtol(name, NULL, 10);
}

static ptrdiff_t suffix_pos(const char* name) {
  int n = strlen(name);

  const char* suffix_end = NULL;
  int in_dots = 0;
  bool in_digits = false;

  for (const char* ptr = name + n - 1; ptr >= name; --ptr) {
    char c = *ptr;

    if (in_digits) {
      if (c == '.') {
        in_digits = false;
        in_dots = 1;
        continue;
      }

      if (isdigit(c)) {
        continue;
      }

      goto done;
    }

    switch (in_dots) {
    case 0:
      if (isdigit(c)) {
        in_digits = true;
        continue;
      }
      goto done;
    case 1:
    case 2:
      if (c == '.') {
        ++in_dots;
        continue;
      }
      goto done;
    case 3:
      suffix_end = ptr + 1;
      if (isdigit(c)) {
        in_dots = 0;
        in_digits = true;
        continue;
      }
      goto done;

    default:
      r_stop_internal("suffix_pos", "Unexpected state.");
    }}

 done:
  if (suffix_end) {
    return suffix_end - name;
  } else {
    return -1;
  }
}

static void stop_large_name() {
  Rf_errorcall(R_NilValue, "Can't tidy up name because it is too large");
}

static bool needs_suffix(SEXP str) {
  return
    str == NA_STRING ||
    str == strings_dots ||
    str == strings_empty ||
    is_dotdotint(CHAR(str));
}


static SEXP names_iota(R_len_t n);
static SEXP vec_unique_names_impl(SEXP names, R_len_t n, bool quiet);

// [[ register() ]]
SEXP vctrs_unique_names(SEXP x, SEXP quiet) {
  return vec_unique_names(x, LOGICAL(quiet)[0]);
}

// [[ include("utils.h") ]]
SEXP vec_unique_names(SEXP x, bool quiet) {
  SEXP names = PROTECT(vec_names(x));
  SEXP out = vec_unique_names_impl(names, vec_size(x), quiet);
  UNPROTECT(1);
  return out;
}
// [[ include("utils.h") ]]
SEXP vec_unique_colnames(SEXP x, bool quiet) {
  SEXP names = PROTECT(colnames(x));
  SEXP out = vec_unique_names_impl(names, Rf_ncols(x), quiet);
  UNPROTECT(1);
  return out;
}

static SEXP vec_unique_names_impl(SEXP names, R_len_t n, bool quiet) {
  SEXP out;
  if (names == R_NilValue) {
    out = PROTECT(names_iota(n));
    if (!quiet) {
      describe_repair(names, out);
    }
  } else {
    out = PROTECT(vec_as_unique_names(names, quiet));
  }

  UNPROTECT(1);
  return(out);
}

static SEXP names_iota(R_len_t n) {
  char buf[MAX_IOTA_SIZE];
  SEXP nms = r_chr_iota(n, buf, MAX_IOTA_SIZE, "...");

  if (nms == R_NilValue) {
    Rf_errorcall(R_NilValue, "Too many names to repair.");
  }

  return nms;
}



static void describe_repair(SEXP old_names, SEXP new_names) {
  SEXP call = PROTECT(Rf_lang3(Rf_install("describe_repair"),
    old_names, new_names));
  Rf_eval(call, vctrs_ns_env);

  // To reset visibility when called from a `.External2()`
  Rf_eval(R_NilValue, R_EmptyEnv);

  UNPROTECT(1);
}


// [[ register() ]]
SEXP vctrs_outer_names(SEXP names, SEXP outer, SEXP n) {
  if (names != R_NilValue && TYPEOF(names) != STRSXP) {
    r_stop_internal("vctrs_outer_names", "`names` must be `NULL` or a string.");
  }
  if (!r_is_number(n)) {
    r_stop_internal("vctrs_outer_names", "`n` must be a single integer.");
  }

  if (outer != R_NilValue) {
    outer = r_chr_get(outer, 0);
  }

  return outer_names(names, outer, r_int_get(n, 0));
}

// [[ include("utils.h") ]]
SEXP outer_names(SEXP names, SEXP outer, R_len_t n) {
  if (outer == R_NilValue) {
    return names;
  }
  if (TYPEOF(outer) != CHARSXP) {
    r_stop_internal("outer_names", "`outer` must be a scalar string.");
  }

  if (outer == strings_empty || outer == NA_STRING) {
    return names;
  }

  if (r_is_empty_names(names)) {
    if (n == 1) {
      return r_str_as_character(outer);
    } else {
      return r_seq_chr(CHAR(outer), n);
    }
  } else {
    return r_chr_paste_prefix(names, CHAR(outer), "..");
  }
}

// [[ register() ]]
SEXP vctrs_apply_name_spec(SEXP name_spec, SEXP outer, SEXP inner, SEXP n) {
  return apply_name_spec(name_spec, r_chr_get(outer, 0), inner, r_int_get(n, 0));
}

static SEXP glue_as_name_spec(SEXP spec);

// [[ include("utils.h") ]]
SEXP apply_name_spec(SEXP name_spec, SEXP outer, SEXP inner, R_len_t n) {
  if (Rf_inherits(name_spec, "rlang_zap")) {
    return R_NilValue;
  }

  if (outer == R_NilValue) {
    return inner;
  }
  if (TYPEOF(outer) != CHARSXP) {
    r_stop_internal("apply_name_spec", "`outer` must be a scalar string.");
  }

  if (outer == strings_empty || outer == NA_STRING) {
    if (inner == R_NilValue) {
      return chrs_empty;
    } else {
      return inner;
    }
  }

  if (r_is_empty_names(inner)) {
    if (n == 0) {
      return vctrs_shared_empty_chr;
    }
    if (n == 1) {
      return r_str_as_character(outer);
    }
    inner = PROTECT(r_seq(1, n + 1));
  } else {
    inner = PROTECT(inner);
  }

  switch (TYPEOF(name_spec)) {
  case CLOSXP:
    break;
  case STRSXP:
    name_spec = glue_as_name_spec(name_spec);
    break;
  default:
    name_spec = r_as_function(name_spec, ".name_spec");
    break;
  case NILSXP:
    Rf_errorcall(R_NilValue,
                 "Can't merge the outer name `%s` with a vector of length > 1.\n"
                 "Please supply a `.name_spec` specification.",
                 CHAR(outer));
  }
  PROTECT(name_spec);

  SEXP outer_chr = PROTECT(r_str_as_character(outer));

  SEXP out = PROTECT(vctrs_dispatch2(syms_dot_name_spec, name_spec,
                                     syms_outer, outer_chr,
                                     syms_inner, inner));
  out = vec_recycle(out, n, NULL);

  if (out != R_NilValue) {
    if (TYPEOF(out) != STRSXP) {
      Rf_errorcall(R_NilValue, "`.name_spec` must return a character vector.");
    }
    if (Rf_length(out) != n) {
      Rf_errorcall(R_NilValue, "`.name_spec` must return a character vector as long as `inner`.");
    }
  }

  UNPROTECT(4);
  return out;
}


static SEXP syms_glue_as_name_spec = NULL;
static SEXP fns_glue_as_name_spec = NULL;
static SEXP syms_internal_spec = NULL;

static SEXP glue_as_name_spec(SEXP spec) {
  if (!r_is_string(spec)) {
    Rf_errorcall(R_NilValue, "Glue specification in `.name_spec` must be a single string.");
  }
  return vctrs_dispatch1(syms_glue_as_name_spec, fns_glue_as_name_spec,
                         syms_internal_spec, spec);
}

#define VCTRS_PASTE_BUFFER_MAX_SIZE 4096
char vctrs_paste_buffer[VCTRS_PASTE_BUFFER_MAX_SIZE];

// [[ include("names.h") ]]
SEXP r_chr_paste_prefix(SEXP names, const char* prefix, const char* sep) {
  int n_protect = 0;

  names = PROTECT_N(Rf_shallow_duplicate(names), &n_protect);
  R_len_t n = Rf_length(names);

  int outer_len = strlen(prefix);
  int names_len = r_chr_max_len(names);

  int sep_len = strlen(sep);
  int total_len = outer_len + names_len + sep_len + 1;

  char* buf = vctrs_paste_buffer;
  if (total_len > VCTRS_PASTE_BUFFER_MAX_SIZE) {
    SEXP buf_box = PROTECT_N(
      Rf_allocVector(RAWSXP, total_len * sizeof(char)),
      &n_protect
    );
    buf = (char*) RAW(buf_box);
  }

  buf[total_len - 1] = '\0';
  char* bufp = buf;

  memcpy(bufp, prefix, outer_len); bufp += outer_len;

  for (int i = 0; i < sep_len; ++i) {
    *bufp++ = sep[i];
  }

  SEXP const* p_names = STRING_PTR_RO(names);

  for (R_len_t i = 0; i < n; ++i) {
    const char* inner = CHAR(p_names[i]);
    int inner_n = strlen(inner);

    memcpy(bufp, inner, inner_n);
    bufp[inner_n] = '\0';

    SET_STRING_ELT(names, i, r_str(buf));
  }

  UNPROTECT(n_protect);
  return names;
}

// [[ register() ]]
SEXP vctrs_chr_paste_prefix(SEXP names, SEXP prefix, SEXP sep) {
  return r_chr_paste_prefix(names,
                            r_chr_get_c_string(prefix, 0),
                            r_chr_get_c_string(sep, 0));
}

// [[ include("names.h") ]]
SEXP r_seq_chr(const char* prefix, R_len_t n) {
  int total_len = 24 + strlen(prefix) + 1;

  R_CheckStack2(total_len);
  char buf[total_len];

  return r_chr_iota(n, buf, total_len, prefix);
}


// Initialised at load time
SEXP syms_set_rownames_fallback = NULL;
SEXP fns_set_rownames_fallback = NULL;

static SEXP set_rownames_fallback(SEXP x, SEXP names) {
  return vctrs_dispatch2(syms_set_rownames_fallback, fns_set_rownames_fallback,
                         syms_x, x,
                         syms_names, names);
}

// Initialised at load time
SEXP syms_set_names_fallback = NULL;
SEXP fns_set_names_fallback = NULL;

static SEXP set_names_fallback(SEXP x, SEXP names) {
  return vctrs_dispatch2(syms_set_names_fallback, fns_set_names_fallback,
                         syms_x, x,
                         syms_names, names);
}

static void check_names(SEXP x, SEXP names) {
  if (names == R_NilValue) {
    return;
  }

  if (TYPEOF(names) != STRSXP) {
    Rf_errorcall(
      R_NilValue,
      "`names` must be a character vector, not a %s.",
      Rf_type2char(TYPEOF(names))
    );
  }

  R_len_t x_size = vec_size(x);
  R_len_t names_size = vec_size(names);

  if (x_size != names_size) {
    Rf_errorcall(
      R_NilValue,
      "The size of `names`, %i, must be the same as the size of `x`, %i.",
      names_size,
      x_size
    );
  }
}

SEXP vec_set_rownames(SEXP x, SEXP names, bool proxy, const enum vctrs_owned owned) {
  if (!proxy && OBJECT(x)) {
    return set_rownames_fallback(x, names);
  }

  int nprot = 0;

  SEXP dim_names = Rf_getAttrib(x, R_DimNamesSymbol);

  // Early exit when no new row names and no existing row names
  if (names == R_NilValue) {
    if (dim_names == R_NilValue || VECTOR_ELT(dim_names, 0) == R_NilValue) {
      return x;
    }
  }

  x = PROTECT_N(vec_clone_referenced(x, owned), &nprot);

  if (dim_names == R_NilValue) {
    dim_names = PROTECT_N(Rf_allocVector(VECSXP, vec_dim_n(x)), &nprot);
  } else {
    // Also clone attribute
    dim_names = PROTECT_N(Rf_shallow_duplicate(dim_names), &nprot);
  }

  SET_VECTOR_ELT(dim_names, 0, names);

  Rf_setAttrib(x, R_DimNamesSymbol, dim_names);

  UNPROTECT(nprot);
  return x;
}

SEXP vec_set_df_rownames(SEXP x, SEXP names, bool proxy, const enum vctrs_owned owned) {
  if (names == R_NilValue) {
    if (rownames_type(df_rownames(x)) != ROWNAMES_IDENTIFIERS) {
      return(x);
    }

    x = PROTECT(vec_clone_referenced(x, owned));
    init_compact_rownames(x, vec_size(x));

    UNPROTECT(1);
    return x;
  }

  // Repair row names silently
  if (!proxy) {
    names = vec_as_names(names, p_unique_repair_silent_opts);
  }
  PROTECT(names);

  x = PROTECT(vec_clone_referenced(x, owned));
  Rf_setAttrib(x, R_RowNamesSymbol, names);

  UNPROTECT(2);
  return x;
}

// FIXME: Do we need to get the vec_proxy() and only fall back if it doesn't
// exist? See #526 and #531 for discussion and the related issue.
SEXP vec_set_names_impl(SEXP x, SEXP names, bool proxy, const enum vctrs_owned owned) {
  check_names(x, names);

  if (is_data_frame(x)) {
    return vec_set_df_rownames(x, names, proxy, owned);
  }

  if (has_dim(x)) {
    return vec_set_rownames(x, names, proxy, owned);
  }

  if (!proxy && OBJECT(x)) {
    return set_names_fallback(x, names);
  }

  // Early exit if no new names and no existing names
  if (names == R_NilValue && Rf_getAttrib(x, R_NamesSymbol) == R_NilValue) {
    return x;
  }

  x = PROTECT(vec_clone_referenced(x, owned));
  Rf_setAttrib(x, R_NamesSymbol, names);

  UNPROTECT(1);
  return x;
}
// [[ include("utils.h"); register() ]]
SEXP vec_set_names(SEXP x, SEXP names) {
  return vec_set_names_impl(x, names, false, VCTRS_OWNED_false);
}
// [[ include("utils.h") ]]
SEXP vec_proxy_set_names(SEXP x, SEXP names, const enum vctrs_owned owned) {
  return vec_set_names_impl(x, names, true, owned);
}


SEXP vctrs_validate_name_repair_arg(SEXP arg) {
  struct name_repair_opts opts = new_name_repair_opts(arg, args_empty, true);
  if (opts.type == name_repair_custom) {
    return opts.fn;
  } else if (Rf_length(arg) != 1) {
    return r_str_as_character(r_str(name_repair_arg_as_c_string(opts.type)));
  } else {
    return arg;
  }
}

void stop_name_repair() {
  Rf_errorcall(R_NilValue, "`.name_repair` must be a string or a function. See `?vctrs::vec_as_names`.");
}

struct name_repair_opts new_name_repair_opts(SEXP name_repair, struct vctrs_arg* arg, bool quiet) {
  struct name_repair_opts opts = {
    .type = 0,
    .fn = R_NilValue,
    .arg = arg,
    .quiet = quiet
  };

  switch (TYPEOF(name_repair)) {
  case STRSXP: {
    if (!Rf_length(name_repair)) {
      stop_name_repair();
    }

    SEXP c = r_chr_get(name_repair, 0);

    if (c == strings_none) {
      opts.type = name_repair_none;
    } else if (c == strings_minimal) {
      opts.type = name_repair_minimal;
    } else if (c == strings_unique) {
      opts.type = name_repair_unique;
    } else if (c == strings_universal) {
      opts.type = name_repair_universal;
    } else if (c == strings_check_unique) {
      opts.type = name_repair_check_unique;
    } else {
      Rf_errorcall(R_NilValue, "`.name_repair` can't be \"%s\". See `?vctrs::vec_as_names`.", CHAR(c));
    }

    return opts;
  }

  case LANGSXP:
    opts.fn = r_as_function(name_repair, ".name_repair");
    opts.type = name_repair_custom;
    return opts;

  case CLOSXP:
    opts.fn = name_repair;
    opts.type = name_repair_custom;
    return opts;

  default:
    stop_name_repair();
  }

  never_reached("new_name_repair_opts");
}

// [[ include("vctrs.h") ]]
const char* name_repair_arg_as_c_string(enum name_repair_type type) {
  switch (type) {
  case name_repair_none: return "none";
  case name_repair_minimal: return "minimal";
  case name_repair_unique: return "unique";
  case name_repair_universal: return "universal";
  case name_repair_check_unique: return "check_unique";
  case name_repair_custom: return "custom";
  }
  never_reached("name_repair_arg_as_c_string");
}

static void vec_validate_minimal_names(SEXP names, R_len_t n) {
  if (names == R_NilValue) {
    Rf_errorcall(R_NilValue, "Names repair functions can't return `NULL`.");
  }

  if (TYPEOF(names) != STRSXP) {
    Rf_errorcall(R_NilValue, "Names repair functions must return a character vector.");
  }

  if (n >= 0 && Rf_length(names) != n) {
    Rf_errorcall(R_NilValue,
                 "Repaired names have length %d instead of length %d.",
                 Rf_length(names),
                 n);
  }

  if (r_chr_has_string(names, NA_STRING)) {
    Rf_errorcall(R_NilValue, "Names repair functions can't return `NA` values.");
  }
}
SEXP vctrs_validate_minimal_names(SEXP names, SEXP n_) {
  R_len_t n = -1;

  if (TYPEOF(n_) == INTSXP) {
    if (Rf_length(n_) != 1) {
      r_stop_internal("vctrs_validate_minimal_names", "`n` must be a single number.");
    }
    n = INTEGER(n_)[0];
  }

  vec_validate_minimal_names(names, n);
  return names;
}


struct name_repair_opts unique_repair_default_opts;
struct name_repair_opts unique_repair_silent_opts;
struct name_repair_opts no_repair_silent_ops;

void vctrs_init_names(SEXP ns) {
  syms_set_rownames_fallback = Rf_install("set_rownames_fallback");
  syms_set_names_fallback = Rf_install("set_names_fallback");
  syms_as_universal_names = Rf_install("as_universal_names");
  syms_validate_unique_names = Rf_install("validate_unique");

  fns_set_rownames_fallback = r_env_get(ns, syms_set_rownames_fallback);
  fns_set_names_fallback = r_env_get(ns, syms_set_names_fallback);
  fns_as_universal_names = r_env_get(ns, syms_as_universal_names);
  fns_validate_unique_names = r_env_get(ns, syms_validate_unique_names);

  syms_glue_as_name_spec = Rf_install("glue_as_name_spec");
  fns_glue_as_name_spec = r_env_get(ns, syms_glue_as_name_spec);
  syms_internal_spec = Rf_install("_spec");

  unique_repair_default_opts.type = name_repair_unique;
  unique_repair_default_opts.fn = R_NilValue;
  unique_repair_default_opts.quiet = false;

  unique_repair_silent_opts.type = name_repair_unique;
  unique_repair_silent_opts.fn = R_NilValue;
  unique_repair_silent_opts.quiet = true;

  no_repair_silent_ops.type = name_repair_none;
  unique_repair_silent_opts.fn = R_NilValue;
  unique_repair_silent_opts.quiet = true;
}
