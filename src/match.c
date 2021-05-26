#include <rlang.h>
#include "vctrs.h"
#include "utils.h"
#include "poly-op.h"
#include "compare.h"

#include "decl/matches-decl.h"

// -----------------------------------------------------------------------------

// Currently assumes `proxy` is in completely increasing order
r_obj* nested_containment_order(r_obj* proxy,
                                r_obj* order,
                                r_obj* group_sizes) {
  if (!is_data_frame(proxy)) {
    r_stop_internal(
      "compute_nested_containment_order",
      "`proxy` must be a data frame."
    );
  }

  int n_prot = 0;

  r_ssize n_cols = r_length(proxy);
  r_ssize size = r_length(order);

  r_obj* out = KEEP_N(r_alloc_integer(size), &n_prot);
  int* v_out = r_int_begin(out);

  for (r_ssize i = 0; i < size; ++i) {
    v_out[i] = 1;
  }

  if (size == 0) {
    // Algorithm requires at least 1 row
    FREE(n_prot);
    return out;
  }
  if (n_cols == 1) {
    // No-op if 1 already sorted column
    FREE(n_prot);
    return out;
  }

  const int* v_order = r_int_cbegin(order);
  const int* v_group_sizes = r_int_cbegin(group_sizes);

  r_ssize n_groups = r_length(group_sizes);

  struct growable prev_rows = new_growable(INTSXP, 10000);
  PROTECT_GROWABLE(&prev_rows, &n_prot);

  struct poly_vec* p_poly_vec = new_poly_vec(proxy, vctrs_type_dataframe);
  PROTECT_POLY_VEC(p_poly_vec, &n_prot);
  const void* v_proxy = p_poly_vec->p_vec;

  int i_order = v_group_sizes[0];
  growable_push_int(&prev_rows, v_order[0] - 1);

  for (r_ssize i_group = 1; i_group < n_groups; ++i_group) {
    int cur_row = v_order[i_order] - 1;

    bool new_id = true;
    int prev_row_id = 0;
    int max_prev_row_id = prev_rows.n;

    for (; prev_row_id < max_prev_row_id; ++prev_row_id) {
      int prev_row = growable_get_int(&prev_rows, prev_row_id);

      if (p_df_nested_containment_compare_ge_na_equal(v_proxy, cur_row, v_proxy, prev_row)) {
        new_id = false;
        break;
      }
    }

    if (new_id) {
      // Completely new id, which we add to the end
      growable_push_int(&prev_rows, cur_row);
    } else {
      // Update existing row location to the current row, since it is larger
      growable_set_int(&prev_rows, prev_row_id, cur_row);
    }

    int elt_out = new_id ? max_prev_row_id : prev_row_id;
    int group_size = v_group_sizes[i_group];

    for (int i = 0; i < group_size; ++i) {
      v_out[v_order[i_order + i] - 1] = elt_out;
    }

    i_order += group_size;
  }

  FREE(n_prot);
  return out;
}

static inline
int p_df_nested_containment_compare_ge_na_equal(const void* x,
                                                r_ssize i,
                                                const void* y,
                                                r_ssize j) {
  // Checks if each column of `x` is `>=` `y`.
  // Assumes inputs are ordered, so first column can be ignored.
  // Iterates backwards to ideally maximize chance of hitting the fastest
  // varying column.

  struct poly_df_data* x_data = (struct poly_df_data*) x;
  struct poly_df_data* y_data = (struct poly_df_data*) y;

  r_ssize n_col = x_data->n_col;
  enum vctrs_type* types = x_data->col_types;

  const void** x_ptrs = x_data->col_ptrs;
  const void** y_ptrs = y_data->col_ptrs;

  // df-cols should already be flattened
  for (r_ssize col = n_col - 1; col > 0; --col) {
    if (!p_compare_ge_na_equal(x_ptrs[col], i, y_ptrs[col], j, types[col])) {
      return false;
    }
  }

  return true;
}
