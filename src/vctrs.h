#ifndef VCTRS_H
#define VCTRS_H


#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include <Rversion.h>

#include <stdbool.h>
#include <stdint.h>


extern bool vctrs_debug_verbose;

#define VCTRS_ASSERT(condition) ((void)sizeof(char[1 - 2*!(condition)]))


typedef R_xlen_t r_ssize;
#define R_SSIZE_MAX R_XLEN_T_MAX

#define r_length Rf_xlength
#define r_new_vector Rf_allocVector

// An ERR indicates either a C NULL in case of no error, or a
// condition object otherwise
#define ERR SEXP


// Vector types -------------------------------------------------

enum vctrs_type {
  vctrs_type_null = 0,
  vctrs_type_unspecified,
  vctrs_type_logical,
  vctrs_type_integer,
  vctrs_type_double,
  vctrs_type_complex,
  vctrs_type_character,
  vctrs_type_raw,
  vctrs_type_list,
  vctrs_type_dataframe,
  vctrs_type_scalar,
  vctrs_type_s3 = 255
};

/**
 * @member type The vector type of the original data.
 * @member proxy_method The function of the `vec_proxy()` method, if
 *   any. This method is looked up with [vec_proxy_method()].
 */
struct vctrs_type_info {
  enum vctrs_type type;
  SEXP proxy_method;
};
/**
 * @inheritMembers vctrs_type_info
 * @member type If `proxy_method` was found, the vector type of the
 *   proxy data. Otherwise, the vector type of the original data.
 *   This is never `vctrs_type_s3`.
 * @member proxy If `proxy_method` was found, the result of invoking
 *   the method. Otherwise, the original data.
 */
struct vctrs_proxy_info {
  enum vctrs_type type;
  SEXP proxy_method;
  SEXP proxy;
};

/**
 * Return the type information of a vector or its proxy
 *
 * `vec_type_info()` returns the vctrs type of `x`. `vec_proxy_info()`
 * returns the vctrs type of `x` or its proxy if it has one. The
 * former returns `vctrs_type_s3` with S3 objects (expect for native
 * types like bare data frames). The latter returns the bare type of
 * the proxy, if any. It never returns `vctrs_type_s3`.
 *
 * `vec_proxy_info()` returns both the proxy method and the proxy
 * data. `vec_type_info()` only returns the proxy method, which it
 * needs to determine whether S3 lists and non-vector base types are
 * scalars or proxied vectors.
 *
 * Use `PROTECT_PROXY_INFO()` and `PROTECT_TYPE_INFO()` to protect the
 * members of the return value. These helpers take a pointer to a
 * protection counter that can be passed to `UNPROTECT()`.
 */
struct vctrs_type_info vec_type_info(SEXP x);
struct vctrs_proxy_info vec_proxy_info(SEXP x);

#define PROTECT_PROXY_INFO(info, n) do {        \
    PROTECT((info)->proxy);                     \
    PROTECT((info)->proxy_method);              \
    *n += 2;                                    \
  } while (0)

#define PROTECT_TYPE_INFO(info, n) do {         \
    PROTECT((info)->proxy_method);              \
    *n += 1;                                    \
  } while (0)

enum vctrs_type vec_typeof(SEXP x);
enum vctrs_type vec_proxy_typeof(SEXP x);
const char* vec_type_as_str(enum vctrs_type type);
bool vec_is_list(SEXP x);
bool vec_is_vector(SEXP x);
bool vec_is_partial(SEXP x);

// After adding a new `vctrs_dispatch` type, add the missing entries
// in `vec_typeof2()`
enum vctrs_type2 {
  vctrs_type2_null_null,
  vctrs_type2_null_unspecified,
  vctrs_type2_null_logical,
  vctrs_type2_null_integer,
  vctrs_type2_null_double,
  vctrs_type2_null_complex,
  vctrs_type2_null_character,
  vctrs_type2_null_raw,
  vctrs_type2_null_list,
  vctrs_type2_null_dataframe,
  vctrs_type2_null_s3,
  vctrs_type2_null_scalar,

  vctrs_type2_unspecified_unspecified,
  vctrs_type2_unspecified_logical,
  vctrs_type2_unspecified_integer,
  vctrs_type2_unspecified_double,
  vctrs_type2_unspecified_complex,
  vctrs_type2_unspecified_character,
  vctrs_type2_unspecified_raw,
  vctrs_type2_unspecified_list,
  vctrs_type2_unspecified_dataframe,
  vctrs_type2_unspecified_s3,
  vctrs_type2_unspecified_scalar,

  vctrs_type2_logical_logical,
  vctrs_type2_logical_integer,
  vctrs_type2_logical_double,
  vctrs_type2_logical_complex,
  vctrs_type2_logical_character,
  vctrs_type2_logical_raw,
  vctrs_type2_logical_list,
  vctrs_type2_logical_dataframe,
  vctrs_type2_logical_s3,
  vctrs_type2_logical_scalar,

  vctrs_type2_integer_integer,
  vctrs_type2_integer_double,
  vctrs_type2_integer_complex,
  vctrs_type2_integer_character,
  vctrs_type2_integer_raw,
  vctrs_type2_integer_list,
  vctrs_type2_integer_dataframe,
  vctrs_type2_integer_s3,
  vctrs_type2_integer_scalar,

  vctrs_type2_double_double,
  vctrs_type2_double_complex,
  vctrs_type2_double_character,
  vctrs_type2_double_raw,
  vctrs_type2_double_list,
  vctrs_type2_double_dataframe,
  vctrs_type2_double_s3,
  vctrs_type2_double_scalar,

  vctrs_type2_complex_complex,
  vctrs_type2_complex_character,
  vctrs_type2_complex_raw,
  vctrs_type2_complex_list,
  vctrs_type2_complex_dataframe,
  vctrs_type2_complex_s3,
  vctrs_type2_complex_scalar,

  vctrs_type2_character_character,
  vctrs_type2_character_raw,
  vctrs_type2_character_list,
  vctrs_type2_character_dataframe,
  vctrs_type2_character_s3,
  vctrs_type2_character_scalar,

  vctrs_type2_raw_raw,
  vctrs_type2_raw_list,
  vctrs_type2_raw_dataframe,
  vctrs_type2_raw_s3,
  vctrs_type2_raw_scalar,

  vctrs_type2_list_list,
  vctrs_type2_list_dataframe,
  vctrs_type2_list_s3,
  vctrs_type2_list_scalar,

  vctrs_type2_dataframe_dataframe,
  vctrs_type2_dataframe_s3,
  vctrs_type2_dataframe_scalar,

  vctrs_type2_s3_s3,
  vctrs_type2_s3_scalar,

  vctrs_type2_scalar_scalar
};

enum vctrs_type2_s3 {
  vctrs_type2_s3_null_bare_factor,
  vctrs_type2_s3_null_bare_ordered,
  vctrs_type2_s3_null_bare_date,
  vctrs_type2_s3_null_bare_posixct,
  vctrs_type2_s3_null_bare_posixlt,
  vctrs_type2_s3_null_bare_tibble,
  vctrs_type2_s3_null_unknown,

  vctrs_type2_s3_unspecified_bare_factor,
  vctrs_type2_s3_unspecified_bare_ordered,
  vctrs_type2_s3_unspecified_bare_date,
  vctrs_type2_s3_unspecified_bare_posixct,
  vctrs_type2_s3_unspecified_bare_posixlt,
  vctrs_type2_s3_unspecified_bare_tibble,
  vctrs_type2_s3_unspecified_unknown,

  vctrs_type2_s3_logical_bare_factor,
  vctrs_type2_s3_logical_bare_ordered,
  vctrs_type2_s3_logical_bare_date,
  vctrs_type2_s3_logical_bare_posixct,
  vctrs_type2_s3_logical_bare_posixlt,
  vctrs_type2_s3_logical_bare_tibble,
  vctrs_type2_s3_logical_unknown,

  vctrs_type2_s3_integer_bare_factor,
  vctrs_type2_s3_integer_bare_ordered,
  vctrs_type2_s3_integer_bare_date,
  vctrs_type2_s3_integer_bare_posixct,
  vctrs_type2_s3_integer_bare_posixlt,
  vctrs_type2_s3_integer_bare_tibble,
  vctrs_type2_s3_integer_unknown,

  vctrs_type2_s3_double_bare_factor,
  vctrs_type2_s3_double_bare_ordered,
  vctrs_type2_s3_double_bare_date,
  vctrs_type2_s3_double_bare_posixct,
  vctrs_type2_s3_double_bare_posixlt,
  vctrs_type2_s3_double_bare_tibble,
  vctrs_type2_s3_double_unknown,

  vctrs_type2_s3_complex_bare_factor,
  vctrs_type2_s3_complex_bare_ordered,
  vctrs_type2_s3_complex_bare_date,
  vctrs_type2_s3_complex_bare_posixct,
  vctrs_type2_s3_complex_bare_posixlt,
  vctrs_type2_s3_complex_bare_tibble,
  vctrs_type2_s3_complex_unknown,

  vctrs_type2_s3_character_bare_factor,
  vctrs_type2_s3_character_bare_ordered,
  vctrs_type2_s3_character_bare_date,
  vctrs_type2_s3_character_bare_posixct,
  vctrs_type2_s3_character_bare_posixlt,
  vctrs_type2_s3_character_bare_tibble,
  vctrs_type2_s3_character_unknown,

  vctrs_type2_s3_raw_bare_factor,
  vctrs_type2_s3_raw_bare_ordered,
  vctrs_type2_s3_raw_bare_date,
  vctrs_type2_s3_raw_bare_posixct,
  vctrs_type2_s3_raw_bare_posixlt,
  vctrs_type2_s3_raw_bare_tibble,
  vctrs_type2_s3_raw_unknown,

  vctrs_type2_s3_list_bare_factor,
  vctrs_type2_s3_list_bare_ordered,
  vctrs_type2_s3_list_bare_date,
  vctrs_type2_s3_list_bare_posixct,
  vctrs_type2_s3_list_bare_posixlt,
  vctrs_type2_s3_list_bare_tibble,
  vctrs_type2_s3_list_unknown,

  vctrs_type2_s3_dataframe_bare_factor,
  vctrs_type2_s3_dataframe_bare_ordered,
  vctrs_type2_s3_dataframe_bare_date,
  vctrs_type2_s3_dataframe_bare_posixct,
  vctrs_type2_s3_dataframe_bare_posixlt,
  vctrs_type2_s3_dataframe_bare_tibble,
  vctrs_type2_s3_dataframe_unknown,

  vctrs_type2_s3_scalar_bare_factor,
  vctrs_type2_s3_scalar_bare_ordered,
  vctrs_type2_s3_scalar_bare_date,
  vctrs_type2_s3_scalar_bare_posixct,
  vctrs_type2_s3_scalar_bare_posixlt,
  vctrs_type2_s3_scalar_bare_tibble,
  vctrs_type2_s3_scalar_unknown,

  vctrs_type2_s3_bare_factor_bare_factor,
  vctrs_type2_s3_bare_factor_bare_ordered,
  vctrs_type2_s3_bare_factor_bare_date,
  vctrs_type2_s3_bare_factor_bare_posixct,
  vctrs_type2_s3_bare_factor_bare_posixlt,
  vctrs_type2_s3_bare_factor_bare_tibble,
  vctrs_type2_s3_bare_factor_unknown,

  vctrs_type2_s3_bare_ordered_bare_ordered,
  vctrs_type2_s3_bare_ordered_bare_date,
  vctrs_type2_s3_bare_ordered_bare_posixct,
  vctrs_type2_s3_bare_ordered_bare_posixlt,
  vctrs_type2_s3_bare_ordered_bare_tibble,
  vctrs_type2_s3_bare_ordered_unknown,

  vctrs_type2_s3_bare_date_bare_date,
  vctrs_type2_s3_bare_date_bare_posixct,
  vctrs_type2_s3_bare_date_bare_posixlt,
  vctrs_type2_s3_bare_date_bare_tibble,
  vctrs_type2_s3_bare_date_unknown,

  vctrs_type2_s3_bare_posixct_bare_posixct,
  vctrs_type2_s3_bare_posixct_bare_posixlt,
  vctrs_type2_s3_bare_posixct_bare_tibble,
  vctrs_type2_s3_bare_posixct_unknown,

  vctrs_type2_s3_bare_posixlt_bare_posixlt,
  vctrs_type2_s3_bare_posixlt_bare_tibble,
  vctrs_type2_s3_bare_posixlt_unknown,

  vctrs_type2_s3_bare_tibble_bare_tibble,
  vctrs_type2_s3_bare_tibble_unknown,

  vctrs_type2_s3_unknown_unknown
};

enum vctrs_type2 vec_typeof2(SEXP x, SEXP y);
const char* vctrs_type2_as_str(enum vctrs_type2 type);

extern SEXP vctrs_shared_empty_lgl;
extern SEXP vctrs_shared_empty_int;
extern SEXP vctrs_shared_empty_dbl;
extern SEXP vctrs_shared_empty_cpl;
extern SEXP vctrs_shared_empty_chr;
extern SEXP vctrs_shared_empty_raw;
extern SEXP vctrs_shared_empty_list;
extern SEXP vctrs_shared_empty_date;
extern SEXP vctrs_shared_empty_uns;

extern SEXP vctrs_shared_true;
extern SEXP vctrs_shared_false;

extern Rcomplex vctrs_shared_na_cpl;
extern SEXP vctrs_shared_na_lgl;
extern SEXP vctrs_shared_na_list;

SEXP vec_unspecified(R_len_t n);
bool vec_is_unspecified(SEXP x);


// Vector methods ------------------------------------------------

#include "arg.h"
#include "names.h"
#include "owned.h"

enum vctrs_proxy_kind {
  VCTRS_PROXY_KIND_default,
  VCTRS_PROXY_KIND_equal,
  VCTRS_PROXY_KIND_compare,
  VCTRS_PROXY_KIND_order
};

SEXP vec_proxy(SEXP x);
SEXP vec_proxy_equal(SEXP x);
SEXP vec_proxy_compare(SEXP x);
SEXP vec_proxy_order(SEXP x);
SEXP vec_restore(SEXP x, SEXP to, SEXP n, const enum vctrs_owned owned);
SEXP vec_restore_default(SEXP x, SEXP to, const enum vctrs_owned owned);
R_len_t vec_size(SEXP x);
R_len_t vec_size_common(SEXP xs, R_len_t absent);
SEXP vec_cast_common(SEXP xs, SEXP to);
SEXP vec_slice(SEXP x, SEXP subscript);
SEXP vec_slice_impl(SEXP x, SEXP subscript);
SEXP vec_chop(SEXP x, SEXP indices);
SEXP vec_slice_shaped(enum vctrs_type type, SEXP x, SEXP index);
SEXP vec_proxy_assign(SEXP proxy, SEXP index, SEXP value);
bool vec_requires_fallback(SEXP x, struct vctrs_proxy_info info);
SEXP vec_init(SEXP x, R_len_t n);
SEXP vec_ptype(SEXP x, struct vctrs_arg* x_arg);
SEXP vec_ptype_finalise(SEXP x);
bool vec_is_unspecified(SEXP x);
SEXP vec_recycle(SEXP x, R_len_t size, struct vctrs_arg* x_arg);
SEXP vec_recycle_fallback(SEXP x, R_len_t size, struct vctrs_arg* x_arg);
SEXP vec_recycle_common(SEXP xs, R_len_t size);
SEXP vec_names(SEXP x);
SEXP vec_proxy_names(SEXP x);
SEXP vec_group_loc(SEXP x);
SEXP vec_match_params(SEXP needles, SEXP haystack, bool na_equal,
                      struct vctrs_arg* needles_arg, struct vctrs_arg* haystack_arg);

#include "cast.h"
static inline SEXP vec_cast(SEXP x, SEXP to, struct vctrs_arg* x_arg, struct vctrs_arg* to_arg) {
  struct cast_opts opts = {
    .x = x,
    .to = to,
    .x_arg = x_arg,
    .to_arg = to_arg
  };
  return vec_cast_opts(&opts);
}

static inline SEXP vec_match(SEXP needles, SEXP haystack) {
  return vec_match_params(needles, haystack, true, NULL, NULL);
}


SEXP vec_c(SEXP xs,
           SEXP ptype,
           SEXP name_spec,
           const struct name_repair_opts* name_repair);

bool is_data_frame(SEXP x);

R_len_t df_size(SEXP x);
R_len_t df_rownames_size(SEXP x);
R_len_t df_raw_size(SEXP x);
R_len_t df_raw_size_from_list(SEXP x);
SEXP vec_bare_df_restore(SEXP x, SEXP to, SEXP n, const enum vctrs_owned owned);
SEXP vec_df_restore(SEXP x, SEXP to, SEXP n, const enum vctrs_owned owned);

// equal_object() never propagates missingness, so
// it can return a `bool`
bool equal_object(SEXP x, SEXP y);
bool equal_names(SEXP x, SEXP y);

/**
 * These functions are meant to be used in loops so it is the callers
 * responsibility to ensure that:
 *
 * - `x` and `y` have identical SEXTYPEs
 * - `i` is a valid index into `x`, and `j` is a valid index into `y`.
 *
 * The behaviour is undefined if these conditions are not true.
 */
int equal_scalar(SEXP x, R_len_t i, SEXP y, R_len_t j, bool na_equal);
int equal_scalar_na_equal_p(enum vctrs_type proxy_type,
                            SEXP x, const void* x_p, R_len_t i,
                            SEXP y, const void* y_p, R_len_t j);
int equal_scalar_na_propagate_p(enum vctrs_type proxy_type,
                                SEXP x, const void* x_p, R_len_t i,
                                SEXP y, const void* y_p, R_len_t j);
int compare_scalar(SEXP x, R_len_t i, SEXP y, R_len_t j, bool na_equal);

uint32_t hash_object(SEXP x);
void hash_fill(uint32_t* p, R_len_t n, SEXP x, bool na_equal);

SEXP vec_unique(SEXP x);
bool duplicated_any(SEXP names);

// Data frame column iteration ----------------------------------

// Used in functions that treat data frames as vectors of rows, but
// iterate over columns. Examples are `vec_equal()` and
// `vec_compare()`.

/**
 * @member row_known A boolean array of size `n_row`. Allocated on the R heap.
 *   Initially, all values are initialized to `false`. As we iterate along the
 *   columns, we flip the corresponding row's `row_known` value to `true` if we
 *   can determine the `out` value for that row from the current columns.
 *   Once a row's `row_known` value is `true`, we never check that row again
 *   as we continue through the columns.
 * @member p_row_known A pointer to the boolean array stored in `row_known`.
 *   Initialized with `(bool*) RAW(info.row_known)`.
 * @member remaining The number of `row_known` values that are still `false`.
 *   If this hits `0` before we traverse the entire data frame, we can exit
 *   immediately because all `out` values are already known.
 * @member size The number of rows in the data frame.
 */
struct df_short_circuit_info {
  SEXP row_known;
  bool* p_row_known;
  R_len_t remaining;
  R_len_t size;
};

#define PROTECT_DF_SHORT_CIRCUIT_INFO(info, n) do {  \
  PROTECT((info)->row_known);                        \
  *n += 1;                                           \
} while (0)

static inline struct df_short_circuit_info new_df_short_circuit_info(R_len_t size) {
  SEXP row_known = PROTECT(Rf_allocVector(RAWSXP, size * sizeof(bool)));
  bool* p_row_known = (bool*) RAW(row_known);

  // To begin with, no rows have a known comparison value
  memset(p_row_known, false, size * sizeof(bool));

  struct df_short_circuit_info info = {
    .row_known = row_known,
    .p_row_known = p_row_known,
    .remaining = size,
    .size = size
  };

  UNPROTECT(1);
  return info;
}

// Missing values -----------------------------------------------

// Annex F of C99 specifies that `double` should conform to the IEEE 754
// type `binary64`, which is defined as:
// * 1  bit : sign
// * 11 bits: exponent
// * 52 bits: significand
//
// R stores the value "1954" in the last 32 bits: this payload marks
// the value as a NA, not a regular NaN.
//
// On big endian systems, this corresponds to the second element of an
// integer array of size 2. On little endian systems, this is flipped
// and the NA marker is in the first element.
//
// The type assumptions made here are asserted in `vctrs_init_utils()`

#ifdef WORDS_BIGENDIAN
static const int vctrs_indicator_pos = 1;
#else
static const int vctrs_indicator_pos = 0;
#endif

union vctrs_dbl_indicator {
  double value;        // 8 bytes
  unsigned int key[2]; // 4 * 2 bytes
};

enum vctrs_dbl_class {
  vctrs_dbl_number,
  vctrs_dbl_missing,
  vctrs_dbl_nan
};

enum vctrs_dbl_class dbl_classify(double x);

// Factor methods -----------------------------------------------

SEXP chr_as_factor(SEXP x, SEXP to, bool* lossy, struct vctrs_arg* to_arg);
SEXP chr_as_ordered(SEXP x, SEXP to, bool* lossy, struct vctrs_arg* to_arg);

SEXP fct_as_character(SEXP x, struct vctrs_arg* x_arg);
SEXP fct_as_factor(SEXP x, SEXP to, bool* lossy, struct vctrs_arg* x_arg, struct vctrs_arg* to_arg);

SEXP ord_as_character(SEXP x, struct vctrs_arg* x_arg);

// Datetime methods ---------------------------------------------

SEXP date_as_date(SEXP x);
SEXP date_as_posixct(SEXP x, SEXP to);
SEXP date_as_posixlt(SEXP x, SEXP to);
SEXP posixct_as_date(SEXP x, bool* lossy);
SEXP posixlt_as_date(SEXP x, bool* lossy);
SEXP posixct_as_posixct(SEXP x, SEXP to);
SEXP posixlt_as_posixct(SEXP x, SEXP to);
SEXP posixct_as_posixlt(SEXP x, SEXP to);
SEXP posixlt_as_posixlt(SEXP x, SEXP to);

SEXP vec_date_restore(SEXP x, SEXP to, const enum vctrs_owned owned);
SEXP vec_posixct_restore(SEXP x, SEXP to, const enum vctrs_owned owned);
SEXP vec_posixlt_restore(SEXP x, SEXP to, const enum vctrs_owned owned);

SEXP date_datetime_ptype2(SEXP x, SEXP y);
SEXP datetime_datetime_ptype2(SEXP x, SEXP y);

// Character translation ---------------------------------------------

SEXP obj_maybe_translate_encoding(SEXP x, R_len_t size);
SEXP obj_maybe_translate_encoding2(SEXP x, R_len_t x_size, SEXP y, R_len_t y_size);

// Growable vector ----------------------------------------------

struct growable {
  SEXP x;
  SEXPTYPE type;
  void* array;
  PROTECT_INDEX idx;
  int n;
  int capacity;
};

struct growable new_growable(SEXPTYPE type, int capacity);
SEXP growable_values(struct growable* g);

static inline void growable_push_int(struct growable* g, int i) {
  if (g->n == g->capacity) {
    g->capacity *= 2;
    g->x = Rf_lengthgets(g->x, g->capacity);
    REPROTECT(g->x, g->idx);
    g->array = INTEGER(g->x);
  }

  int* p = (int*) g->array;
  p[g->n] = i;
  ++(g->n);
}

#define PROTECT_GROWABLE(g, n) do {             \
    PROTECT_WITH_INDEX((g)->x, &((g)->idx));    \
    *n += 1;                                    \
  } while(0)

#define UNPROTECT_GROWABLE(g) do { UNPROTECT(1);} while(0)

// Conditions ---------------------------------------------------

void stop_scalar_type(SEXP x, struct vctrs_arg* arg) __attribute__((noreturn));
void vec_assert(SEXP x, struct vctrs_arg* arg);
__attribute__((noreturn))
void stop_incompatible_size(SEXP x, SEXP y,
                            R_len_t x_size, R_len_t y_size,
                            struct vctrs_arg* x_arg,
                            struct vctrs_arg* y_arg);
__attribute__((noreturn))
void stop_incompatible_type(SEXP x,
                            SEXP y,
                            struct vctrs_arg* x_arg,
                            struct vctrs_arg* y_arg,
                            bool cast);
__attribute__((noreturn))
void stop_recycle_incompatible_size(R_len_t x_size, R_len_t size,
                                    struct vctrs_arg* x_arg);
__attribute__((noreturn))
void stop_incompatible_shape(SEXP x, SEXP y,
                             R_len_t x_size, R_len_t y_size, int axis,
                             struct vctrs_arg* p_x_arg, struct vctrs_arg* p_y_arg);
void stop_corrupt_factor_levels(SEXP x, struct vctrs_arg* arg) __attribute__((noreturn));
void stop_corrupt_ordered_levels(SEXP x, struct vctrs_arg* arg) __attribute__((noreturn));

// Compatibility ------------------------------------------------

#if (R_VERSION < R_Version(3, 5, 0))
# define LOGICAL_RO(x) ((const int*) LOGICAL(x))
# define INTEGER_RO(x) ((const int*) INTEGER(x))
# define REAL_RO(x) ((const double*) REAL(x))
# define COMPLEX_RO(x) ((const Rcomplex*) COMPLEX(x))
# define STRING_PTR_RO(x) ((const SEXP*) STRING_PTR(x))
# define RAW_RO(x) ((const Rbyte*) RAW(x))
# define DATAPTR_RO(x) ((const void*) DATAPTR(x))
#endif

#define VECTOR_PTR_RO(x) ((const SEXP*) DATAPTR_RO(x))


#endif
