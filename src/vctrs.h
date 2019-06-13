#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include <Rversion.h>

#include <stdbool.h>
#include <stdint.h>


typedef R_xlen_t r_ssize_t;


// Vector types -------------------------------------------------

enum vctrs_type {
  vctrs_type_null = 0,
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

static inline struct vctrs_proxy_info PROTECT_PROXY_INFO(struct vctrs_proxy_info info, int* n) {
  *n += 2; PROTECT(info.proxy); PROTECT(info.proxy_method);
  return info;
}
static inline struct vctrs_type_info PROTECT_TYPE_INFO(struct vctrs_type_info info, int* n) {
  ++(*n); PROTECT(info.proxy_method);
  return info;
}

enum vctrs_type vec_typeof(SEXP x);
enum vctrs_type vec_proxy_typeof(SEXP x);
const char* vec_type_as_str(enum vctrs_type type);
bool vec_is_vector(SEXP x);
bool vec_is_partial(SEXP x);

// After adding a new `vctrs_dispatch` type, add the missing entries
// in `vec_typeof2()`
enum vctrs_type2 {
  vctrs_type2_null_null,
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

enum vctrs_type2 vec_typeof2(SEXP x, SEXP y);
const char* vctrs_type2_as_str(enum vctrs_type2 type);

extern SEXP vctrs_shared_empty_lgl;
extern SEXP vctrs_shared_empty_int;
extern SEXP vctrs_shared_empty_dbl;
extern SEXP vctrs_shared_empty_cpl;
extern SEXP vctrs_shared_empty_chr;
extern SEXP vctrs_shared_empty_raw;
extern SEXP vctrs_shared_empty_list;
extern SEXP vctrs_shared_empty_uns;

extern SEXP vctrs_shared_true;
extern SEXP vctrs_shared_false;

extern Rcomplex vctrs_shared_na_cpl;

SEXP vec_unspecified(R_len_t n);
bool vec_is_unspecified(SEXP x);


// Vector methods ------------------------------------------------

#include "arg.h"

SEXP vec_proxy(SEXP x);
SEXP vec_proxy_equal(SEXP x);
SEXP vec_restore(SEXP x, SEXP to, SEXP i);
R_len_t vec_size(SEXP x);
R_len_t vec_size_common(SEXP xs, R_len_t absent);
SEXP vec_dim(SEXP x);
R_len_t vec_dim_n(SEXP x);
SEXP vec_cast(SEXP x, SEXP to);
SEXP vec_cast_common(SEXP xs, SEXP to);
SEXP vec_coercible_cast(SEXP x, SEXP to, struct vctrs_arg* x_arg, struct vctrs_arg* to_arg);
SEXP vec_slice(SEXP x, SEXP index);
SEXP vec_assign(SEXP x, SEXP index, SEXP value);
SEXP vec_na(SEXP x, R_len_t n);
SEXP vec_type(SEXP x);
SEXP vec_type_finalise(SEXP x);
bool vec_is_unspecified(SEXP x);
SEXP vec_recycle(SEXP x, R_len_t size);
SEXP vec_names(SEXP x);

SEXP vec_type2(SEXP x,
               SEXP y,
               struct vctrs_arg* x_arg,
               struct vctrs_arg* y_arg,
               int* left);

bool is_data_frame(SEXP x);
bool is_record(SEXP x);

R_len_t df_size(SEXP x);
R_len_t df_rownames_size(SEXP x);
R_len_t df_raw_size(SEXP x);
SEXP df_restore(SEXP x, SEXP to, SEXP i);
SEXP df_restore_impl(SEXP x, SEXP to, R_len_t size);

SEXP chr_assign(SEXP out, SEXP index, SEXP value, bool clone);
SEXP list_assign(SEXP out, SEXP index, SEXP value, bool clone);
SEXP df_assign(SEXP out, SEXP index, SEXP value, bool clone);

// Most vector predicates return `int` because missing values are
// propagated as `NA_LOGICAL`
int equal_object(SEXP x, SEXP y, bool na_equal);
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
int compare_scalar(SEXP x, R_len_t i, SEXP y, R_len_t j, bool na_equal);

uint32_t hash_object(SEXP x);
uint32_t hash_scalar(SEXP x, R_len_t i);
void hash_fill(uint32_t* p, R_len_t n, SEXP x);

SEXP as_unique_names(SEXP names, bool quiet);

// Experimental:
SEXP vec_restore_container(SEXP x, SEXP to, R_len_t n);


// Growable vector -----------------------------------------------

struct growable {
  SEXP x;
  int32_t idx;
  int n;
  int capacity;
};
typedef struct growable growable;

void growable_init(growable* g, SEXPTYPE type, int capacity);
void growable_free(growable* g);
void growable_push_int(growable* g, int i);
SEXP growable_values(growable* g);

// Shape --------------------------------------------------------

bool has_dim(SEXP x);


// Conditions ---------------------------------------------------

void vctrs_stop_unsupported_type(enum vctrs_type, const char* fn) __attribute__((noreturn));
void stop_scalar_type(SEXP x, struct vctrs_arg* arg) __attribute__((noreturn));
void vec_assert(SEXP x, struct vctrs_arg* arg);
void stop_incompatible_size(SEXP x, SEXP y,
                            R_len_t x_size, R_len_t y_size,
                            struct vctrs_arg* x_arg,
                            struct vctrs_arg* y_arg)
  __attribute__((noreturn));


// Compatibility ------------------------------------------------

#if (R_VERSION < R_Version(3, 5, 0))
# define LOGICAL_RO(x) ((const int*) LOGICAL(x))
# define INTEGER_RO(x) ((const int*) INTEGER(x))
# define REAL_RO(x) ((const double*) REAL(x))
# define COMPLEX_RO(x) ((const Rcomplex*) COMPLEX(x))
# define STRING_PTR_RO(x) ((const SEXP*) STRING_PTR(x))
# define RAW_RO(x) ((const Rbyte*) RAW(x))
#endif
