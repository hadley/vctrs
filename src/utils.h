#ifndef VCTRS_UTILS_H
#define VCTRS_UTILS_H

#include "arg-counter.h"


#define SWAP(T, x, y) do {                      \
    T tmp = x;                                  \
    x = y;                                      \
    y = tmp;                                    \
  } while (0)

#define PROTECT_N(x, n) (++*n, PROTECT(x))

enum vctrs_class_type {
  vctrs_class_data_frame,
  vctrs_class_bare_data_frame,
  vctrs_class_bare_tibble,
  vctrs_class_rcrd,
  vctrs_class_posixlt,
  vctrs_class_unknown,
  vctrs_class_none
};


bool r_is_bool(SEXP x);

SEXP vctrs_eval_mask_n(SEXP fn,
                       SEXP* syms, SEXP* args,
                       SEXP env);
SEXP vctrs_eval_mask1(SEXP fn,
                      SEXP x_sym, SEXP x,
                      SEXP env);
SEXP vctrs_eval_mask2(SEXP fn,
                      SEXP x_sym, SEXP x,
                      SEXP y_sym, SEXP y,
                      SEXP env);

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


SEXP r_get(SEXP x, R_len_t i);

SEXP map(SEXP x, SEXP (*fn)(SEXP));
SEXP map2(SEXP x, SEXP y, SEXP (*fn)(SEXP, SEXP));
SEXP df_map(SEXP df, SEXP (*fn)(SEXP));

enum vctrs_class_type class_type(SEXP x);
bool is_data_frame(SEXP x);
bool is_bare_data_frame(SEXP x);
bool is_bare_tibble(SEXP x);
bool is_record(SEXP x);

SEXP vec_unique_names(SEXP x, bool quiet);
SEXP vec_unique_colnames(SEXP x, bool quiet);

// Returns S3 method for `generic` suitable for the class of `x`. The
// inheritance hierarchy is explored except for the default method.
SEXP s3_find_method(const char* generic, SEXP x);

extern struct vctrs_arg* args_empty;

void never_reached(const char* fn) __attribute__((noreturn));

enum vctrs_type2 vec_typeof2_impl(enum vctrs_type type_x, enum vctrs_type type_y, int* left);

SEXP new_list_of(SEXP x, SEXP ptype);
void init_list_of(SEXP x, SEXP ptype);

SEXP new_data_frame(SEXP x, R_len_t n);
void init_data_frame(SEXP x, R_len_t n);
void init_tibble(SEXP x, R_len_t n);
void init_compact_rownames(SEXP x, R_len_t n);
SEXP get_rownames(SEXP x);

bool is_native_df(SEXP x);
bool is_compact_rownames(SEXP x);
R_len_t compact_rownames_length(SEXP x);
SEXP df_poke(SEXP x, R_len_t i, SEXP value);
SEXP df_poke_at(SEXP x, SEXP name, SEXP value);

void init_compact_seq(int* p, R_len_t start, R_len_t size, bool increasing);
SEXP compact_seq(R_len_t start, R_len_t size, bool increasing);
bool is_compact_seq(SEXP x);

void init_compact_rep(int* p, R_len_t i, R_len_t n);
SEXP compact_rep(R_len_t i, R_len_t n);
bool is_compact_rep(SEXP x);

bool is_compact(SEXP x);
SEXP compact_materialize(SEXP x);
R_len_t vec_index_size(SEXP x);

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
const void* r_vec_const_deref(SEXP x);
void r_vec_ptr_inc(SEXPTYPE type, void** p, R_len_t i);
void r_vec_fill(SEXPTYPE type, void* p, const void* value_p, R_len_t value_i, R_len_t n);

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

SEXP r_new_environment(SEXP parent, R_len_t size);
SEXP r_new_function(SEXP formals, SEXP body, SEXP env);
SEXP r_as_function(SEXP x, const char* arg);

SEXP r_protect(SEXP x);
bool r_is_true(SEXP x);
bool r_is_string(SEXP x);
bool r_is_number(SEXP x);
SEXP r_peek_option(const char* option);
SEXP r_maybe_duplicate(SEXP x);

SEXP r_pairlist(SEXP* tags, SEXP* cars);
SEXP r_call(SEXP fn, SEXP* tags, SEXP* cars);

SEXP r_names(SEXP x);
SEXP r_poke_names(SEXP x, SEXP names);
bool r_has_name_at(SEXP names, R_len_t i);
bool r_is_names(SEXP names);
bool r_is_minimal_names(SEXP x);
bool r_is_empty_names(SEXP x);
SEXP r_env_get(SEXP env, SEXP sym);
bool r_is_function(SEXP x);
bool r_chr_has_string(SEXP x, SEXP str);

static inline const char* r_chr_get_c_string(SEXP chr, R_len_t i) {
  return CHAR(STRING_ELT(chr, i));
}

static inline void r__vec_get_check(SEXP x, R_len_t i, const char* fn) {
  if ((Rf_length(x) - 1) < i) {
    Rf_error("Internal error in `%s()`: Vector is too small", fn);
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

static inline void* r_vec_unwrap(SEXPTYPE type, SEXP x) {
  switch (type) {
  case INTSXP: return (void*) INTEGER(x);
  default: Rf_error("Internal error: Unimplemented type in `r_vec_unwrap()`.");
  }
}

#define r_lgl Rf_ScalarLogical
#define r_int Rf_ScalarInteger
#define r_str Rf_mkChar
#define r_sym Rf_install

static inline SEXP r_list(SEXP x) {
  SEXP out = Rf_allocVector(VECSXP, 1);
  SET_VECTOR_ELT(out, 0, x);
  return out;
}

#define r_str_as_character Rf_ScalarString

SEXP r_as_data_frame(SEXP x);

static inline void r_dbg_save(SEXP x, const char* name) {
  Rf_defineVar(Rf_install(name), x, R_GlobalEnv);
}
static inline void r_dbg_str(SEXP x) {
  SEXP call = PROTECT(Rf_lang2(Rf_install("str"), x));
  Rf_eval(call, R_GlobalEnv);
  UNPROTECT(1);
}

SEXP r_node_reverse(SEXP node);

static inline void r_node_push(SEXP* node_out, SEXP elt, PROTECT_INDEX i) {
  SEXP node = Rf_cons(elt, *node_out);
  REPROTECT(node, i);
  *node_out = node;
}
static inline SEXP r_node_pop(SEXP* node_out, PROTECT_INDEX i) {
  SEXP node = *node_out;

  SEXP out = CAR(node);
  node = CDR(node);
  REPROTECT(node, i);

  *node_out = node;
  return out;
}


extern SEXP vctrs_ns_env;
extern SEXP vctrs_shared_empty_str;
extern SEXP vctrs_shared_na_lgl;
extern SEXP vctrs_shared_zero_int;

extern SEXP classes_data_frame;
extern SEXP classes_tibble;
extern SEXP classes_list_of;
extern SEXP classes_vctrs_group_rle;

extern SEXP strings_dots;
extern SEXP strings_empty;
extern SEXP strings_tbl;
extern SEXP strings_tbl_df;
extern SEXP strings_data_frame;
extern SEXP strings_vctrs_rcrd;
extern SEXP strings_posixlt;
extern SEXP strings_posixt;
extern SEXP strings_vctrs_vctr;
extern SEXP strings_none;
extern SEXP strings_minimal;
extern SEXP strings_unique;
extern SEXP strings_universal;
extern SEXP strings_check_unique;
extern SEXP strings_key;
extern SEXP strings_pos;
extern SEXP strings_val;
extern SEXP strings_group;
extern SEXP strings_length;
extern SEXP strings_vcols;

extern SEXP syms_i;
extern SEXP syms_n;
extern SEXP syms_x;
extern SEXP syms_y;
extern SEXP syms_to;
extern SEXP syms_dots;
extern SEXP syms_bracket;
extern SEXP syms_x_arg;
extern SEXP syms_y_arg;
extern SEXP syms_to_arg;
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

#define syms_names R_NamesSymbol

extern SEXP fns_bracket;
extern SEXP fns_quote;
extern SEXP fns_names;


#endif
