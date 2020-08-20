#ifndef VCTRS_UTILS_H
#define VCTRS_UTILS_H

#include "arg-counter.h"
#include "utils-rlang.h"


#define SWAP(T, x, y) do {                      \
    T tmp = x;                                  \
    x = y;                                      \
    y = tmp;                                    \
  } while (0)

#define PROTECT_N(x, n) (++*n, PROTECT(x))
#define PROTECT2(x, y) (PROTECT(x), PROTECT(y))

enum vctrs_class_type {
  vctrs_class_list,
  vctrs_class_data_frame,
  vctrs_class_bare_data_frame,
  vctrs_class_bare_tibble,
  vctrs_class_bare_factor,
  vctrs_class_bare_ordered,
  vctrs_class_bare_date,
  vctrs_class_bare_posixct,
  vctrs_class_bare_posixlt,
  vctrs_class_unknown,
  vctrs_class_none
};

bool r_is_bool(SEXP x);
int r_bool_as_int(SEXP x);

SEXP vctrs_eval_mask_n(SEXP fn,
                       SEXP* syms, SEXP* args);
SEXP vctrs_eval_mask1(SEXP fn,
                      SEXP x_sym, SEXP x);
SEXP vctrs_eval_mask2(SEXP fn,
                      SEXP x_sym, SEXP x,
                      SEXP y_sym, SEXP y);
SEXP vctrs_eval_mask3(SEXP fn,
                      SEXP x_sym, SEXP x,
                      SEXP y_sym, SEXP y,
                      SEXP z_sym, SEXP z);
SEXP vctrs_eval_mask4(SEXP fn,
                      SEXP x1_sym, SEXP x1,
                      SEXP x2_sym, SEXP x2,
                      SEXP x3_sym, SEXP x3,
                      SEXP x4_sym, SEXP x4);
SEXP vctrs_eval_mask5(SEXP fn,
                      SEXP x1_sym, SEXP x1,
                      SEXP x2_sym, SEXP x2,
                      SEXP x3_sym, SEXP x3,
                      SEXP x4_sym, SEXP x4,
                      SEXP x5_sym, SEXP x5);
SEXP vctrs_eval_mask6(SEXP fn,
                      SEXP x1_sym, SEXP x1,
                      SEXP x2_sym, SEXP x2,
                      SEXP x3_sym, SEXP x3,
                      SEXP x4_sym, SEXP x4,
                      SEXP x5_sym, SEXP x5,
                      SEXP x6_sym, SEXP x6);
SEXP vctrs_eval_mask7(SEXP fn,
                      SEXP x1_sym, SEXP x1,
                      SEXP x2_sym, SEXP x2,
                      SEXP x3_sym, SEXP x3,
                      SEXP x4_sym, SEXP x4,
                      SEXP x5_sym, SEXP x5,
                      SEXP x6_sym, SEXP x6,
                      SEXP x7_sym, SEXP x7);

SEXP vctrs_dispatch_n(SEXP fn_sym, SEXP fn,
                      SEXP* syms, SEXP* args);
SEXP vctrs_dispatch1(SEXP fn_sym, SEXP fn,
                     SEXP x_sym, SEXP x);
SEXP vctrs_dispatch2(SEXP fn_sym, SEXP fn,
                     SEXP x_sym, SEXP x,
                     SEXP y_sym, SEXP y);
SEXP vctrs_dispatch3(SEXP fn_sym, SEXP fn,
                     SEXP x_sym, SEXP x,
                     SEXP y_sym, SEXP y,
                     SEXP z_sym, SEXP z);
SEXP vctrs_dispatch4(SEXP fn_sym, SEXP fn,
                     SEXP w_sym, SEXP w,
                     SEXP x_sym, SEXP x,
                     SEXP y_sym, SEXP y,
                     SEXP z_sym, SEXP z);
SEXP vctrs_dispatch6(SEXP fn_sym, SEXP fn,
                     SEXP x1_sym, SEXP x1,
                     SEXP x2_sym, SEXP x2,
                     SEXP x3_sym, SEXP x3,
                     SEXP x4_sym, SEXP x4,
                     SEXP x5_sym, SEXP x5,
                     SEXP x6_sym, SEXP x6);

__attribute__((noreturn)) void r_abort(const char* fmt, ...);
__attribute__((noreturn)) void stop_internal(const char* fn, const char* fmt, ...);
__attribute__((noreturn)) void stop_unimplemented_vctrs_type(const char* fn, enum vctrs_type);

static inline
__attribute__((noreturn))
void stop_unimplemented_type(const char* fn, SEXPTYPE type) {
  stop_internal(fn, "Unimplemented type `%s`.", Rf_type2char(type));
}

static inline
SEXP r_eval_force(SEXP expr, SEXP env) {
#if R_VERSION >= R_Version(3, 2, 3)
  return R_forceAndCall(expr, 1, env);
#else
  return Rf_eval(expr, env);
#endif
}

SEXP map(SEXP x, SEXP (*fn)(SEXP));
SEXP map_with_data(SEXP x, SEXP (*fn)(SEXP, void*), void* data);
SEXP df_map(SEXP df, SEXP (*fn)(SEXP));
SEXP bare_df_map(SEXP df, SEXP (*fn)(SEXP));

enum vctrs_class_type class_type(SEXP x);
bool is_data_frame(SEXP x);
bool is_bare_data_frame(SEXP x);
bool is_bare_tibble(SEXP x);

SEXP vec_unique_names(SEXP x, bool quiet);
SEXP vec_unique_colnames(SEXP x, bool quiet);

// Returns S3 / S4 method for `generic` suitable for the class of `x`. The
// inheritance hierarchy is explored except for the default method.
SEXP s3_get_method(const char* generic, const char* cls, SEXP table);
SEXP s3_sym_get_method(SEXP sym, SEXP table);
SEXP s3_find_method(const char* generic, SEXP x, SEXP table);
SEXP s3_class_find_method(const char* generic, SEXP class, SEXP table);
SEXP s3_get_class(SEXP x);
SEXP s3_find_method_xy(const char* generic,
                       SEXP x,
                       SEXP y,
                       SEXP table,
                       SEXP* method_sym_out);
SEXP s3_find_method2(const char* generic,
                     SEXP x,
                     SEXP table,
                     SEXP* method_sym_out);
SEXP s3_paste_method_sym(const char* generic, const char* cls);
SEXP s3_bare_class(SEXP x);
SEXP s4_find_method(SEXP x, SEXP table);
SEXP s4_class_find_method(SEXP class, SEXP table);
bool vec_implements_ptype2(SEXP x);

SEXP r_env_get(SEXP env, SEXP sym);

static inline
void r_env_poke(SEXP env, SEXP sym, SEXP value) {
  Rf_defineVar(sym, value, env);
}

extern SEXP syms_s3_methods_table;
static inline SEXP s3_get_table(SEXP env) {
  return r_env_get(env, syms_s3_methods_table);
}


SEXP list_first_non_null(SEXP xs, R_len_t* non_null_i);
bool list_is_homogeneously_classed(SEXP xs);

// Destructive compacting
SEXP node_compact_d(SEXP xs);

extern struct vctrs_arg args_empty_;
static struct vctrs_arg* const args_empty = &args_empty_;

extern struct vctrs_arg args_dot_ptype_;
static struct vctrs_arg* const args_dot_ptype = &args_dot_ptype_;

void never_reached(const char* fn) __attribute__((noreturn));

enum vctrs_type2 vec_typeof2_impl(enum vctrs_type type_x, enum vctrs_type type_y, int* left);
enum vctrs_type2_s3 vec_typeof2_s3_impl(SEXP x, SEXP y, enum vctrs_type type_x, enum vctrs_type type_y, int* left);

enum vctrs_class_type class_type(SEXP x);

SEXP new_empty_factor(SEXP levels);
SEXP new_empty_ordered(SEXP levels);

bool list_has_inner_vec_names(SEXP x, R_len_t size);
SEXP list_pluck(SEXP xs, R_len_t i);

void init_compact_seq(int* p, R_len_t start, R_len_t size, bool increasing);
SEXP compact_seq(R_len_t start, R_len_t size, bool increasing);
bool is_compact_seq(SEXP x);

void init_compact_rep(int* p, R_len_t i, R_len_t n);
SEXP compact_rep(R_len_t i, R_len_t n);
bool is_compact_rep(SEXP x);

bool is_compact(SEXP x);
SEXP compact_materialize(SEXP x);
R_len_t vec_subscript_size(SEXP x);

bool is_integer64(SEXP x);

SEXP apply_name_spec(SEXP name_spec, SEXP outer, SEXP inner, R_len_t n);
SEXP outer_names(SEXP names, SEXP outer, R_len_t n);
SEXP vec_set_names(SEXP x, SEXP names);
SEXP colnames(SEXP x);

R_len_t size_validate(SEXP size, const char* arg);

extern bool (*rlang_is_splice_box)(SEXP);
extern SEXP (*rlang_unbox)(SEXP);
extern SEXP (*rlang_env_dots_values)(SEXP);
extern SEXP (*rlang_env_dots_list)(SEXP);

void* r_vec_deref(SEXP x);
const void* r_vec_deref_const(SEXP x);
void* r_vec_deref_barrier(SEXP x);
const void* r_vec_deref_barrier_const(SEXP x);

void r_vec_fill(SEXPTYPE type,
                void* p_dest,
                r_ssize dest_i,
                const void* p_src,
                r_ssize src_i,
                r_ssize n);

R_len_t r_lgl_sum(SEXP lgl, bool na_true);
SEXP r_lgl_which(SEXP x, bool na_true);

void r_lgl_fill(SEXP x, int value, R_len_t n);
void r_int_fill(SEXP x, int value, R_len_t n);
void r_chr_fill(SEXP x, SEXP value, R_len_t n);

void r_int_fill_seq(SEXP x, int start, R_len_t n);
SEXP r_seq(R_len_t from, R_len_t to);
bool r_int_any_na(SEXP x);

R_len_t r_chr_find(SEXP x, SEXP value);

#define r_resize Rf_xlengthgets

int r_chr_max_len(SEXP x);
SEXP r_chr_iota(R_len_t n, char* buf, int len, const char* prefix);

#define R_LAZY_ALLOC(SYM, PI, R_TYPE, SIZE) do {        \
    if (SYM == R_NilValue) {                            \
      SYM = Rf_allocVector(R_TYPE, SIZE);               \
      REPROTECT(SYM, PI);                               \
    }                                                   \
  } while (0);

static inline
SEXP r_new_logical(R_len_t n) {
  return Rf_allocVector(LGLSXP, n);
}
static inline
SEXP r_new_integer(R_len_t n) {
  return Rf_allocVector(INTSXP, n);
}
static inline
SEXP r_new_character(R_len_t n) {
  return Rf_allocVector(STRSXP, n);
}
static inline
SEXP r_new_list(R_len_t n) {
  return Rf_allocVector(VECSXP, n);
}

static inline
SEXP r_new_environment(SEXP parent) {
  SEXP env = Rf_allocSExp(ENVSXP);
  SET_ENCLOS(env, parent);
  return env;
}

static inline
SEXP r_new_function(SEXP formals, SEXP body, SEXP env) {
  SEXP fn = Rf_allocSExp(CLOSXP);
  SET_FORMALS(fn, formals);
  SET_BODY(fn, body);
  SET_CLOENV(fn, env);
  return fn;
}

SEXP r_as_function(SEXP x, const char* arg);

SEXP r_protect(SEXP x);
bool r_is_true(SEXP x);
bool r_is_string(SEXP x);
bool r_is_number(SEXP x);
SEXP r_peek_option(const char* option);
SEXP r_peek_frame();
SEXP r_clone_referenced(SEXP x);
SEXP r_clone_shared(SEXP x);

SEXP r_parse(const char* str);
SEXP r_parse_eval(const char* str, SEXP env);

static inline
SEXP r_copy(SEXP x) {
  return Rf_duplicate(x);
}
static inline
SEXP r_clone(SEXP x) {
  return Rf_shallow_duplicate(x);
}

SEXP r_pairlist(SEXP* tags, SEXP* cars);
SEXP r_call(SEXP fn, SEXP* tags, SEXP* cars);

static inline SEXP r_poke_names(SEXP x, SEXP names) {
  return Rf_setAttrib(x, R_NamesSymbol, names);
}
static inline SEXP r_poke_class(SEXP x, SEXP names) {
  return Rf_setAttrib(x, R_ClassSymbol, names);
}
static inline SEXP r_dim(SEXP x) {
  return Rf_getAttrib(x, R_DimSymbol);
}
static inline SEXP r_poke_dim(SEXP x, SEXP dim) {
  return Rf_setAttrib(x, R_DimSymbol, dim);
}
static inline SEXP r_mark_s4(SEXP x) {
  SET_S4_OBJECT(x);
  return(x);
}
static inline SEXP r_unmark_s4(SEXP x) {
  UNSET_S4_OBJECT(x);
  return(x);
}

bool r_has_name_at(SEXP names, R_len_t i);
bool r_is_names(SEXP names);
bool r_is_minimal_names(SEXP x);
bool r_is_empty_names(SEXP x);
bool r_is_function(SEXP x);
bool r_chr_has_string(SEXP x, SEXP str);

static inline const char* r_chr_get_c_string(SEXP chr, R_len_t i) {
  return CHAR(STRING_ELT(chr, i));
}

static inline void r__vec_get_check(SEXP x, R_len_t i, const char* fn) {
  if ((Rf_length(x) - 1) < i) {
    stop_internal(fn, "Vector is too small.");
  }
}
static inline int r_lgl_get(SEXP x, R_len_t i) {
  r__vec_get_check(x, i, "r_lgl_get");
  return LOGICAL(x)[i];
}
static inline int r_int_get(SEXP x, R_len_t i) {
  r__vec_get_check(x, i, "r_int_get");
  return INTEGER(x)[i];
}
static inline double r_dbl_get(SEXP x, R_len_t i) {
  r__vec_get_check(x, i, "r_dbl_get");
  return REAL(x)[i];
}
#define r_chr_get STRING_ELT
#define r_list_get VECTOR_ELT
#define r_chr_poke SET_STRING_ELT
#define r_list_poke SET_VECTOR_ELT

static inline
void r_int_poke(SEXP x, R_len_t i, int value) {
  r__vec_get_check(x, i, "r_int_poke");
  INTEGER(x)[i] = value;
}

static inline void* r_vec_unwrap(SEXPTYPE type, SEXP x) {
  switch (type) {
  case INTSXP: return (void*) INTEGER(x);
  default: stop_unimplemented_type("r_vec_unwrap", type);
  }
}

#define r_lgl Rf_ScalarLogical
#define r_int Rf_ScalarInteger
#define r_str Rf_mkChar
#define r_chr Rf_mkString
#define r_sym Rf_install

static inline SEXP r_list(SEXP x) {
  SEXP out = Rf_allocVector(VECSXP, 1);
  SET_VECTOR_ELT(out, 0, x);
  return out;
}

#define r_str_as_character Rf_ScalarString

static inline SEXP r_sym_as_character(SEXP x) {
  return r_str_as_character(PRINTNAME(x));
}
// This unserialises ASCII Unicode tags of the form `<U+xxxx>`
extern SEXP (*rlang_sym_as_character)(SEXP x);

SEXP r_as_data_frame(SEXP x);

static inline void r_dbg_save(SEXP x, const char* name) {
  Rf_defineVar(Rf_install(name), x, R_GlobalEnv);
}

ERR r_try_catch(void (*fn)(void*),
                void* fn_data,
                SEXP cnd_sym,
                void (*hnd)(void*),
                void* hnd_data);

extern SEXP vctrs_ns_env;
extern SEXP syms_cnd_signal;
static inline void r_cnd_signal(SEXP cnd) {
  SEXP call = PROTECT(Rf_lang2(syms_cnd_signal, cnd));
  Rf_eval(call, vctrs_ns_env);
  UNPROTECT(1);
}

extern SEXP result_attrib;

static inline SEXP r_result(SEXP x, ERR err) {
  if (!err) {
    err = R_NilValue;
  }

  SEXP result = PROTECT(Rf_allocVector(VECSXP, 2));
  SET_VECTOR_ELT(result, 0, x);
  SET_VECTOR_ELT(result, 1, err);

  SET_ATTRIB(result, result_attrib);
  SET_OBJECT(result, 1);

  UNPROTECT(1);
  return result;
}

static inline SEXP r_result_get(SEXP x, ERR err) {
  if (err) {
    r_cnd_signal(err);
  }

  return x;
}

static inline struct vctrs_arg vec_as_arg(SEXP x) {
  if (x == R_NilValue) {
    return *args_empty;
  }

  if (!r_is_string(x)) {
    Rf_errorcall(R_NilValue, "Argument tag must be a string.");
  }
  return new_wrapper_arg(NULL, r_chr_get_c_string(x, 0));
}

extern SEXP fns_quote;
static inline SEXP expr_protect(SEXP x) {
  switch (TYPEOF(x)) {
  case SYMSXP:
  case LANGSXP:
    return Rf_lang2(fns_quote, x);
  default:
    return x;
  }
}

static inline const void* vec_type_missing_value(enum vctrs_type type) {
  switch (type) {
  case vctrs_type_logical: return &NA_LOGICAL;
  case vctrs_type_integer: return &NA_INTEGER;
  case vctrs_type_double: return &NA_REAL;
  case vctrs_type_complex: return &vctrs_shared_na_cpl;
  case vctrs_type_character: return &NA_STRING;
  case vctrs_type_list: return vctrs_shared_na_list;
  default: stop_unimplemented_vctrs_type("vec_type_missing_value", type);
  }
}

void c_print_backtrace();
void r_browse(SEXP x);


// Adapted from CERT C coding standards
static inline
intmax_t intmax_add(intmax_t x, intmax_t y) {
  if ((y > 0 && x > (INTMAX_MAX - y)) ||
      (y < 0 && x < (INTMAX_MIN - y))) {
    stop_internal("intmax_add", "Values too large to be added.");
  }

  return x + y;
}

static inline
r_ssize r_ssize_add(r_ssize x, r_ssize y) {
  intmax_t out = intmax_add(x, y);

  if (out > R_SSIZE_MAX) {
    stop_internal("r_ssize_safe_add", "Result too large for an `r_ssize`.");
  }

  return (r_ssize) out;
}

SEXP chr_c(SEXP x, SEXP y);

bool r_is_bare_list(SEXP x);


extern SEXP vctrs_ns_env;
extern SEXP vctrs_shared_empty_str;
extern SEXP vctrs_shared_zero_int;

extern SEXP classes_data_frame;
extern SEXP classes_factor;
extern SEXP classes_ordered;
extern SEXP classes_date;
extern SEXP classes_posixct;
extern SEXP classes_tibble;
extern SEXP classes_vctrs_group_rle;

extern SEXP strings_dots;
extern SEXP strings_empty;
extern SEXP strings_tbl;
extern SEXP strings_tbl_df;
extern SEXP strings_data_frame;
extern SEXP strings_date;
extern SEXP strings_posixct;
extern SEXP strings_posixlt;
extern SEXP strings_posixt;
extern SEXP strings_factor;
extern SEXP strings_ordered;
extern SEXP strings_list;
extern SEXP strings_none;
extern SEXP strings_minimal;
extern SEXP strings_unique;
extern SEXP strings_universal;
extern SEXP strings_check_unique;
extern SEXP strings_key;
extern SEXP strings_loc;
extern SEXP strings_val;
extern SEXP strings_group;
extern SEXP strings_length;
extern SEXP strings_vctrs_vctr;

extern SEXP chrs_subset;
extern SEXP chrs_extract;
extern SEXP chrs_assign;
extern SEXP chrs_rename;
extern SEXP chrs_remove;
extern SEXP chrs_negate;
extern SEXP chrs_null;
extern SEXP chrs_logical;
extern SEXP chrs_integer;
extern SEXP chrs_double;
extern SEXP chrs_complex;
extern SEXP chrs_character;
extern SEXP chrs_raw;
extern SEXP chrs_list;
extern SEXP chrs_expression;
extern SEXP chrs_numeric;
extern SEXP chrs_function;
extern SEXP chrs_empty;
extern SEXP chrs_cast;
extern SEXP chrs_error;
extern SEXP chrs_combine;
extern SEXP chrs_convert;

extern SEXP syms_i;
extern SEXP syms_n;
extern SEXP syms_x;
extern SEXP syms_y;
extern SEXP syms_x_size;
extern SEXP syms_y_size;
extern SEXP syms_to;
extern SEXP syms_dots;
extern SEXP syms_bracket;
extern SEXP syms_arg;
extern SEXP syms_x_arg;
extern SEXP syms_y_arg;
extern SEXP syms_to_arg;
extern SEXP syms_times_arg;
extern SEXP syms_subscript_arg;
extern SEXP syms_out;
extern SEXP syms_value;
extern SEXP syms_quiet;
extern SEXP syms_dot_name_spec;
extern SEXP syms_outer;
extern SEXP syms_inner;
extern SEXP syms_tilde;
extern SEXP syms_dot_environment;
extern SEXP syms_ptype;
extern SEXP syms_missing;
extern SEXP syms_size;
extern SEXP syms_subscript_action;
extern SEXP syms_subscript_type;
extern SEXP syms_repair;
extern SEXP syms_tzone;
extern SEXP syms_data;
extern SEXP syms_vctrs_error_incompatible_type;
extern SEXP syms_vctrs_error_cast_lossy;
extern SEXP syms_cnd_signal;
extern SEXP syms_logical;
extern SEXP syms_numeric;
extern SEXP syms_character;
extern SEXP syms_body;
extern SEXP syms_parent;
extern SEXP syms_from_dispatch;
extern SEXP syms_df_fallback;
extern SEXP syms_s3_fallback;
extern SEXP syms_stop_incompatible_type;
extern SEXP syms_stop_incompatible_size;
extern SEXP syms_action;
extern SEXP syms_vctrs_common_class_fallback;
extern SEXP syms_fallback_class;
extern SEXP syms_abort;
extern SEXP syms_message;
extern SEXP syms_dot_elt;

static const char * const c_strs_vctrs_common_class_fallback = "vctrs:::common_class_fallback";

#define syms_names R_NamesSymbol

extern SEXP fns_bracket;
extern SEXP fns_quote;
extern SEXP fns_names;


extern SEXP vctrs_method_table;
extern SEXP base_method_table;
extern SEXP s4_c_method_table;


#if defined(RLIB_DEBUG)
SEXP R_inspect(SEXP x);
SEXP R_inspect3(SEXP x, int deep, int pvec);
#endif


#endif
