#include <rlang.h>
#include "poly-op.h"
#include "vctrs.h"
#include "equal.h"
#include "utils.h"

// -----------------------------------------------------------------------------

static int p_df_equal_na_equal(const void* x, r_ssize i, const void* y, r_ssize j);

// [[ include("poly-op.h") ]]
poly_binary_int_fn_ptr new_poly_p_equal_na_equal(enum vctrs_type type) {
  switch (type) {
  case vctrs_type_null: return p_nil_equal_na_equal;
  case vctrs_type_logical: return p_lgl_equal_na_equal;
  case vctrs_type_integer: return p_int_equal_na_equal;
  case vctrs_type_double: return p_dbl_equal_na_equal;
  case vctrs_type_complex: return p_cpl_equal_na_equal;
  case vctrs_type_character: return p_chr_equal_na_equal;
  case vctrs_type_raw: return p_raw_equal_na_equal;
  case vctrs_type_list: return p_list_equal_na_equal;
  case vctrs_type_dataframe: return p_df_equal_na_equal;
  default: stop_unimplemented_vctrs_type("new_poly_p_equal_na_equal", type);
  }
}

static
int p_df_equal_na_equal(const void* x, r_ssize i, const void* y, r_ssize j) {
  struct poly_df_data* x_data = (struct poly_df_data*) x;
  struct poly_df_data* y_data = (struct poly_df_data*) y;

  r_ssize n_col = x_data->n_col;
  if (n_col != y_data->n_col) {
    r_stop_internal("p_df_equal_na_equal", "`x` and `y` must have the same number of columns.");
  }

  enum vctrs_type* types = x_data->col_types;
  const void** x_ptrs = x_data->col_ptrs;
  const void** y_ptrs = y_data->col_ptrs;

  // df-cols should already be flattened
  for (r_ssize col = 0; col < n_col; ++col) {
    if (!p_equal_na_equal(x_ptrs[col], i, y_ptrs[col], j, types[col])) {
      return false;
    }
  }

  return true;
}

// -----------------------------------------------------------------------------

static bool p_df_is_missing(const void* x, r_ssize i);

// [[ include("poly-op.h") ]]
poly_unary_bool_fn_ptr new_poly_p_is_missing(enum vctrs_type type) {
  switch (type) {
  case vctrs_type_null: return p_nil_is_missing;
  case vctrs_type_logical: return p_lgl_is_missing;
  case vctrs_type_integer: return p_int_is_missing;
  case vctrs_type_double: return p_dbl_is_missing;
  case vctrs_type_complex: return p_cpl_is_missing;
  case vctrs_type_character: return p_chr_is_missing;
  case vctrs_type_raw: return p_raw_is_missing;
  case vctrs_type_list: return p_list_is_missing;
  case vctrs_type_dataframe: return p_df_is_missing;
  default: stop_unimplemented_vctrs_type("new_poly_p_is_missing", type);
  }
}

static
bool p_df_is_missing(const void* x, r_ssize i) {
  struct poly_df_data* x_data = (struct poly_df_data*) x;

  enum vctrs_type* types = x_data->col_types;
  const void** x_ptrs = x_data->col_ptrs;
  r_ssize n_col = x_data->n_col;

  for (r_ssize col = 0; col < n_col; ++col) {
    if (p_is_missing(x_ptrs[col], i, types[col])) {
      return true;
    }
  }

  return false;
}

// -----------------------------------------------------------------------------

static void init_nil_poly_vec(struct poly_vec* p_poly_vec);
static void init_lgl_poly_vec(struct poly_vec* p_poly_vec);
static void init_int_poly_vec(struct poly_vec* p_poly_vec);
static void init_dbl_poly_vec(struct poly_vec* p_poly_vec);
static void init_cpl_poly_vec(struct poly_vec* p_poly_vec);
static void init_chr_poly_vec(struct poly_vec* p_poly_vec);
static void init_raw_poly_vec(struct poly_vec* p_poly_vec);
static void init_list_poly_vec(struct poly_vec* p_poly_vec);
static void init_df_poly_vec(struct poly_vec* p_poly_vec);

// [[ include("poly-op.h") ]]
struct poly_vec* new_poly_vec(SEXP proxy, enum vctrs_type type) {
  SEXP self = PROTECT(Rf_allocVector(RAWSXP, sizeof(struct poly_vec)));
  struct poly_vec* p_poly_vec = (struct poly_vec*) RAW(self);

  p_poly_vec->self = self;
  p_poly_vec->vec = proxy;

  switch (type) {
  case vctrs_type_null: init_nil_poly_vec(p_poly_vec); break;
  case vctrs_type_logical: init_lgl_poly_vec(p_poly_vec); break;
  case vctrs_type_integer: init_int_poly_vec(p_poly_vec); break;
  case vctrs_type_double: init_dbl_poly_vec(p_poly_vec); break;
  case vctrs_type_complex: init_cpl_poly_vec(p_poly_vec); break;
  case vctrs_type_character: init_chr_poly_vec(p_poly_vec); break;
  case vctrs_type_raw: init_raw_poly_vec(p_poly_vec); break;
  case vctrs_type_list: init_list_poly_vec(p_poly_vec); break;
  case vctrs_type_dataframe: init_df_poly_vec(p_poly_vec); break;
  default: stop_unimplemented_vctrs_type("new_poly_vec", type);
  }

  // `init_*_poly_vec()` functions may allocate
  PROTECT(p_poly_vec->self);

  UNPROTECT(2);
  return p_poly_vec;
}

static
void init_nil_poly_vec(struct poly_vec* p_poly_vec) {
  p_poly_vec->p_vec = NULL;
}
static
void init_lgl_poly_vec(struct poly_vec* p_poly_vec) {
  p_poly_vec->p_vec = (const void*) LOGICAL_RO(p_poly_vec->vec);
}
static
void init_int_poly_vec(struct poly_vec* p_poly_vec) {
  p_poly_vec->p_vec = (const void*) INTEGER_RO(p_poly_vec->vec);
}
static
void init_dbl_poly_vec(struct poly_vec* p_poly_vec) {
  p_poly_vec->p_vec = (const void*) REAL_RO(p_poly_vec->vec);
}
static
void init_cpl_poly_vec(struct poly_vec* p_poly_vec) {
  p_poly_vec->p_vec = (const void*) COMPLEX_RO(p_poly_vec->vec);
}
static
void init_chr_poly_vec(struct poly_vec* p_poly_vec) {
  p_poly_vec->p_vec = (const void*) STRING_PTR_RO(p_poly_vec->vec);
}
static
void init_raw_poly_vec(struct poly_vec* p_poly_vec) {
  p_poly_vec->p_vec = (const void*) RAW_RO(p_poly_vec->vec);
}
static
void init_list_poly_vec(struct poly_vec* p_poly_vec) {
  p_poly_vec->p_vec = (const void*) VECTOR_PTR_RO(p_poly_vec->vec);
}
static
void init_df_poly_vec(struct poly_vec* p_poly_vec) {
  SEXP df = p_poly_vec->vec;
  r_ssize n_col = Rf_xlength(df);

  SEXP self = PROTECT(Rf_allocVector(VECSXP, 4));

  SET_VECTOR_ELT(self, 0, p_poly_vec->self);
  p_poly_vec->self = self;

  SEXP data_handle = PROTECT(Rf_allocVector(RAWSXP, sizeof(struct poly_df_data)));
  struct poly_df_data* data = (struct poly_df_data*) RAW(data_handle);
  SET_VECTOR_ELT(self, 1, data_handle);

  SEXP col_types_handle = PROTECT(Rf_allocVector(RAWSXP, n_col * sizeof(enum vctrs_type)));
  enum vctrs_type* col_types = (enum vctrs_type*) RAW(col_types_handle);
  SET_VECTOR_ELT(self, 2, col_types_handle);

  SEXP col_ptrs_handle = PROTECT(Rf_allocVector(RAWSXP, n_col * sizeof(void*)));
  const void** col_ptrs = (const void**) RAW(col_ptrs_handle);
  SET_VECTOR_ELT(self, 3, col_ptrs_handle);

  for (r_ssize i = 0; i < n_col; ++i) {
    SEXP col = VECTOR_ELT(df, i);
    col_types[i] = vec_proxy_typeof(col);
    col_ptrs[i] = r_vec_cbegin(col);
  }

  data->col_types = col_types;
  data->col_ptrs = col_ptrs;
  data->n_col = n_col;

  p_poly_vec->p_vec = (void*) data;

  UNPROTECT(4);
}
