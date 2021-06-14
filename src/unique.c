#include <rlang.h>
#include "vctrs.h"
#include "order-radix.h"

// -----------------------------------------------------------------------------

static
r_obj* vec_unique_loc2(r_obj* x) {
  const bool nan_distinct = true;
  const bool chr_ordered = false;
  r_obj* chr_transform = r_null;

  r_obj* info = KEEP(vec_order_info(
    x,
    chrs_asc,
    chrs_largest,
    nan_distinct,
    chr_transform,
    chr_ordered
  ));

  r_obj* order = r_list_get(info, 0);
  const int* v_order = r_int_cbegin(order);

  r_obj* group_sizes = r_list_get(info, 1);
  int* v_group_sizes = r_int_begin(group_sizes);

  r_ssize n_groups = r_length(group_sizes);

  r_ssize start = 0;

  // Reuse `group_sizes` memory to hold the ordering of unique values
  r_obj* loc_ordered_uniques = group_sizes;
  int* v_loc_ordered_uniques = v_group_sizes;

  // Extract ordering of unique keys
  for (r_ssize i = 0; i < n_groups; ++i) {
    r_ssize group_size = v_group_sizes[i];
    v_loc_ordered_uniques[i] = v_order[start];
    start += group_size;
  }

  // Compute ordering of the unique order values
  // to be able to report uniques by first appearance
  r_obj* key_first_appearance = KEEP(vec_order(
    loc_ordered_uniques,
    chrs_asc,
    chrs_largest,
    nan_distinct,
    chr_transform
  ));
  int* v_key_first_appearance = r_int_begin(key_first_appearance);

  // Reuse `key_first_appearance` memory for output
  r_obj* loc_uniques = key_first_appearance;
  int* v_loc_uniques = v_key_first_appearance;

  // Compute unique locations by ordering
  // unique order values by first appearance
  for (r_ssize i = 0; i < n_groups; ++i) {
    v_loc_uniques[i] = v_loc_ordered_uniques[v_key_first_appearance[i] - 1];
  }

  FREE(2);
  return loc_uniques;
}

// [[ register() ]]
r_obj* vctrs_unique_loc2(r_obj* x) {
  return vec_unique_loc2(x);
}

// -----------------------------------------------------------------------------

// [[ include("vctrs.h") ]]
r_obj* vec_unique2(r_obj* x) {
  r_obj* index = KEEP(vec_unique_loc2(x));
  r_obj* out = vec_slice_impl(x, index);
  FREE(1);
  return out;
}

// [[ register() ]]
r_obj* vctrs_unique2(r_obj* x) {
  return vec_unique2(x);
}

// -----------------------------------------------------------------------------

static
r_obj* vec_unique_count2(r_obj* x) {
  const bool nan_distinct = true;
  const bool chr_ordered = false;
  r_obj* chr_transform = r_null;

  r_obj* info = KEEP(vec_order_info(
    x,
    chrs_asc,
    chrs_largest,
    nan_distinct,
    chr_transform,
    chr_ordered
  ));

  r_obj* group_sizes = r_list_get(info, 1);
  int n_groups = (int) r_length(group_sizes);

  r_obj* out = r_int(n_groups);

  FREE(1);
  return out;
}

// [[ register() ]]
r_obj* vctrs_unique_count2(r_obj* x) {
  return vec_unique_count2(x);
}
