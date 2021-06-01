#include <rlang.h>
#include "vctrs.h"
#include "utils.h"
#include "poly-op.h"
#include "compare.h"
#include "ptype2.h"
#include "translate.h"
#include "order-radix.h"

// -----------------------------------------------------------------------------

enum vctrs_multiple {
  VCTRS_MULTIPLE_all = 0,
  VCTRS_MULTIPLE_warning = 1, // Warning + all
  VCTRS_MULTIPLE_first = 2,
  VCTRS_MULTIPLE_last = 3,
  VCTRS_MULTIPLE_error = 4
};

enum vctrs_ops {
  VCTRS_OPS_eq = 0,
  VCTRS_OPS_neq = 1,
  VCTRS_OPS_gt = 2,
  VCTRS_OPS_gte = 3,
  VCTRS_OPS_lt = 4,
  VCTRS_OPS_lte = 5
};

// -----------------------------------------------------------------------------

#include "decl/matches-decl.h"

// -----------------------------------------------------------------------------

static inline
r_ssize midpoint(r_ssize lhs, r_ssize rhs) {
  return lhs + (rhs - lhs) / 2;
}

// -----------------------------------------------------------------------------

// [[ register() ]]
r_obj* vctrs_matches(r_obj* needles,
                     r_obj* haystack,
                     r_obj* condition,
                     r_obj* na_equal,
                     r_obj* no_match,
                     r_obj* multiple,
                     r_obj* needles_arg,
                     r_obj* haystack_arg) {
  bool c_na_equal = r_as_bool(na_equal);

  if (r_typeof(no_match) != R_TYPE_integer || r_length(no_match) != 1) {
    r_abort("`no_match` must be an integer of length 1.");
  }
  int c_no_match = r_int_get(no_match, 0);

  struct vctrs_arg c_needles_arg = vec_as_arg(needles_arg);
  struct vctrs_arg c_haystack_arg = vec_as_arg(haystack_arg);

  enum vctrs_multiple c_multiple = parse_multiple(multiple);

  return vec_matches(
    needles,
    haystack,
    condition,
    c_na_equal,
    c_no_match,
    c_multiple,
    &c_needles_arg,
    &c_haystack_arg
  );
}

static
r_obj* vec_matches(r_obj* needles,
                   r_obj* haystack,
                   r_obj* condition,
                   bool na_equal,
                   int no_match,
                   enum vctrs_multiple multiple,
                   struct vctrs_arg* needles_arg,
                   struct vctrs_arg* haystack_arg) {
  int n_prot = 0;

  int _;
  r_obj* common = KEEP_N(vec_ptype2_params(
    needles,
    haystack,
    needles_arg,
    haystack_arg,
    DF_FALLBACK_quiet,
    &_
  ), &n_prot);

  needles = KEEP_N(vec_cast_params(
    needles,
    common,
    needles_arg,
    args_empty,
    DF_FALLBACK_quiet,
    S3_FALLBACK_false
  ), &n_prot);

  haystack = KEEP_N(vec_cast_params(
    haystack,
    common,
    haystack_arg,
    args_empty,
    DF_FALLBACK_quiet,
    S3_FALLBACK_false
  ), &n_prot);

  // TODO: Generalize argument expansion code of `vec_order()` and use to
  // expand `condition` in the presence of data frame needles/haystack.
  // TODO: This should also validate the size of `condition`.

  needles = KEEP_N(vec_proxy_compare(needles), &n_prot);
  needles = KEEP_N(vec_normalize_encoding(needles), &n_prot);

  haystack = KEEP_N(vec_proxy_compare(haystack), &n_prot);
  haystack = KEEP_N(vec_normalize_encoding(haystack), &n_prot);

  r_ssize size_needles = vec_size(needles);
  r_ssize size_haystack = vec_size(haystack);

  // Support non-data frame types by wrapping them in a data frame.
  // This also re-wraps 1 column data frames that `vec_proxy_compare()` unwrapped.
  if (!is_data_frame(needles)) {
    needles = KEEP_N(r_list(needles), &n_prot);
    r_poke_names(needles, r_chr("x"));
    r_init_data_frame(needles, size_needles);

    haystack = KEEP_N(r_list(haystack), &n_prot);
    r_poke_names(haystack, r_chr("x"));
    r_init_data_frame(haystack, size_haystack);
  }

  r_ssize size_condition = vec_size(condition);
  enum vctrs_ops* v_ops = (enum vctrs_ops*) R_alloc(size_condition, sizeof(enum vctrs_ops));
  parse_condition(condition, size_condition, v_ops);

  r_obj* out = df_matches(
    needles,
    haystack,
    size_needles,
    size_haystack,
    na_equal,
    no_match,
    multiple,
    v_ops
  );

  FREE(n_prot);
  return out;
}

// -----------------------------------------------------------------------------

static
r_obj* df_matches(r_obj* needles,
                  r_obj* haystack,
                  r_ssize size_needles,
                  r_ssize size_haystack,
                  bool na_equal,
                  int no_match,
                  enum vctrs_multiple multiple,
                  enum vctrs_ops* v_ops) {
  int n_prot = 0;

  r_ssize n_cols = r_length(needles);

  // `vec_order()` setup
  r_obj* const direction = KEEP_N(r_chr("asc"), &n_prot);
  r_obj* const na_value = KEEP_N(r_chr("largest"), &n_prot);
  const bool nan_distinct = true;
  r_obj* const chr_transform = r_null;
  const bool chr_ordered = true;

  r_obj* o_needles = KEEP_N(vec_order(needles, direction, na_value, nan_distinct, chr_transform), &n_prot);
  const int* v_o_needles = r_int_cbegin(o_needles);

  // Are there any directional ops (>, >=, <, <=)?
  bool any_directional = false;
  for (r_ssize i = 0; i < n_cols; ++i) {
    enum vctrs_ops op = v_ops[i];
    if (op == VCTRS_OPS_eq || op == VCTRS_OPS_neq) {
      continue;
    }
    any_directional = true;
    break;
  }

  r_obj* o_haystack = vctrs_shared_empty_int;
  r_obj* nested_groups = vctrs_shared_empty_int;
  int n_nested_groups = 1;
  bool possibly_nested = any_directional && n_cols > 1;

  if (possibly_nested) {
    r_obj* info = KEEP_N(vec_order_info(haystack, direction, na_value, nan_distinct, chr_transform, chr_ordered), &n_prot);
    o_haystack = r_list_get(info, 0);
    r_obj* group_sizes_haystack = r_list_get(info, 1);
    r_obj* nested_info = KEEP_N(nested_containment_order(haystack, o_haystack, group_sizes_haystack), &n_prot);
    n_nested_groups = r_as_int(r_list_get(nested_info, 1));

    if (n_nested_groups != 1) {
      // If only a single nested group exists, we hit the somewhat rare case of
      // having a >1 col data frame that is already in nested containment order.
      // In that case, original haystack ordering is sufficient. Otherwise,
      // recompute haystack ordering with `nested_groups` as the first column.
      nested_groups = r_list_get(nested_info, 0);

      r_obj* tweaked_haystack = KEEP_N(r_alloc_list(n_cols + 1), &n_prot);
      r_obj* tweaked_names = r_alloc_character(n_cols + 1);
      r_poke_names(tweaked_haystack, tweaked_names);

      r_list_poke(tweaked_haystack, 0, nested_groups);
      r_chr_poke(tweaked_names, 0, r_str("..nested_groups.."));

      r_obj* const* v_haystack = r_list_cbegin(haystack);
      r_obj* const* v_haystack_names = r_chr_cbegin(r_names(haystack));

      for (r_ssize i = 0; i < n_cols; ++i) {
        r_list_poke(tweaked_haystack, i + 1, v_haystack[i]);
        r_chr_poke(tweaked_names, i + 1, v_haystack_names[i]);
      }

      r_init_data_frame(tweaked_haystack, size_haystack);

      o_haystack = KEEP_N(vec_order(tweaked_haystack, direction, na_value, nan_distinct, chr_transform), &n_prot);
    }
  } else {
    // Nested group info isn't required for only `==` and `!=` ops, or when
    // there is only 1 column in the data frame
    o_haystack = KEEP_N(vec_order(haystack, direction, na_value, nan_distinct, chr_transform), &n_prot);
  }

  const int* v_o_haystack = r_int_cbegin(o_haystack);
  const int* v_nested_groups = r_int_cbegin(nested_groups);

  r_ssize initial_capacity;
  if (n_nested_groups == 1) {
    initial_capacity = size_needles;
  } else {
    // In the case of possible multiple matches that fall in separate
    // nested containers, allocate ~20% extra room
    initial_capacity = size_needles * 1.2;
  }

  struct r_dyn_array* p_o_haystack_starts = r_new_dyn_vector(R_TYPE_integer, initial_capacity);
  KEEP_N(p_o_haystack_starts->shelter, &n_prot);

  // TODO: `match_sizes` and `needles_locs` aren't needed if `multiple %in% c("last", "first")`
  // Size is always 1, and location is increasing sequence of locations.
  struct r_dyn_array* p_match_sizes = r_new_dyn_vector(R_TYPE_integer, initial_capacity);
  KEEP_N(p_match_sizes->shelter, &n_prot);

  struct r_dyn_array* p_needles_locs = r_new_dyn_vector(R_TYPE_integer, initial_capacity);
  KEEP_N(p_needles_locs->shelter, &n_prot);

  {
    // Temporary unstable pointers
    int* v_o_haystack_starts = (int*) r_arr_begin(p_o_haystack_starts);
    int* v_match_sizes = (int*) r_arr_begin(p_match_sizes);
    int* v_needles_locs = (int*) r_arr_begin(p_needles_locs);

    for (r_ssize i = 0; i < size_needles; ++i) {
      // Initialize to no match everywhere, no need to initialize extra buffer
      v_o_haystack_starts[i] = r_globals.na_int;
      v_match_sizes[i] = 1;
      v_needles_locs[i] = i + 1;
    }

    p_o_haystack_starts->count = size_needles;
    p_match_sizes->count = size_needles;
    p_needles_locs->count = size_needles;
  }

  struct poly_vec* p_poly_needles = new_poly_vec(needles, vctrs_type_dataframe);
  PROTECT_POLY_VEC(p_poly_needles, &n_prot);
  const struct poly_df_data* p_needles = (const struct poly_df_data*) p_poly_needles->p_vec;

  struct poly_vec* p_poly_haystack = new_poly_vec(haystack, vctrs_type_dataframe);
  PROTECT_POLY_VEC(p_poly_haystack, &n_prot);
  const struct poly_df_data* p_haystack = (const struct poly_df_data*) p_poly_haystack->p_vec;

  r_ssize n_extra = 0;
  bool any_multiple = false;

  const r_ssize col = 0;
  const r_ssize lower_o_needles = 0;
  const r_ssize upper_o_needles = size_needles - 1;

  if (n_nested_groups == 1) {
    const r_ssize lower_o_haystack = 0;
    const r_ssize upper_o_haystack = size_haystack - 1;

    df_matches_recurse(
      col,
      lower_o_needles,
      upper_o_needles,
      lower_o_haystack,
      upper_o_haystack,
      p_needles,
      p_haystack,
      v_o_needles,
      v_o_haystack,
      na_equal,
      multiple,
      v_ops,
      p_o_haystack_starts,
      p_match_sizes,
      p_needles_locs,
      &n_extra,
      &any_multiple
    );
  } else {
    df_matches_with_nested_groups(
      size_haystack,
      n_nested_groups,
      v_nested_groups,
      col,
      lower_o_needles,
      upper_o_needles,
      p_needles,
      p_haystack,
      v_o_needles,
      v_o_haystack,
      na_equal,
      multiple,
      v_ops,
      p_o_haystack_starts,
      p_match_sizes,
      p_needles_locs,
      &n_extra,
      &any_multiple
    );
  }

  if (any_multiple) {
    if (multiple == VCTRS_MULTIPLE_error) {
      r_abort("Oh no, multiple matches!");
    } else if (multiple == VCTRS_MULTIPLE_warning) {
      r_warn("Oh no, multiple matches! (but we are ok with that)");
    }
  }

  r_ssize n_used = size_needles + n_extra;

  r_obj* out = KEEP_N(expand_compact_indices(
    v_o_haystack,
    p_o_haystack_starts,
    p_match_sizes,
    p_needles_locs,
    n_used,
    no_match
  ), &n_prot);

  FREE(n_prot);
  return out;
}

// -----------------------------------------------------------------------------

static
void df_matches_recurse(r_ssize col,
                        r_ssize lower_o_needles,
                        r_ssize upper_o_needles,
                        r_ssize lower_o_haystack,
                        r_ssize upper_o_haystack,
                        const struct poly_df_data* p_needles,
                        const struct poly_df_data* p_haystack,
                        const int* v_o_needles,
                        const int* v_o_haystack,
                        bool na_equal,
                        enum vctrs_multiple multiple,
                        enum vctrs_ops* v_ops,
                        struct r_dyn_array* p_o_haystack_starts,
                        struct r_dyn_array* p_match_sizes,
                        struct r_dyn_array* p_needles_locs,
                        r_ssize* p_n_extra,
                        bool* p_any_multiple) {
  const enum vctrs_ops op = v_ops[col];

  r_ssize grp_lower_o_needles = lower_o_needles;
  r_ssize grp_upper_o_needles = upper_o_needles;
  r_ssize grp_lower_o_haystack = lower_o_haystack;
  r_ssize grp_upper_o_haystack = upper_o_haystack;

  const r_ssize n_col = p_needles->n_col;

  // TODO: Generalize with this
  // const enum vctrs_type type = p_needles->col_types[col];

  const int* v_needles_col = (const int*) p_needles->col_ptrs[col];
  const int* v_haystack_col = (const int*) p_haystack->col_ptrs[col];

  const r_ssize mid_o_needles = midpoint(lower_o_needles, upper_o_needles);
  const r_ssize mid_needles = v_o_needles[mid_o_needles] - 1;
  const int val_needle = v_needles_col[mid_needles];

  while (grp_lower_o_haystack <= grp_upper_o_haystack) {
    const r_ssize grp_mid_o_haystack = midpoint(grp_lower_o_haystack, grp_upper_o_haystack);
    const r_ssize grp_mid_haystack = v_o_haystack[grp_mid_o_haystack] - 1;
    const int val_haystack = v_haystack_col[grp_mid_haystack];

    const int cmp = int_compare_na_equal(val_needle, val_haystack);

    if (cmp == 1) {
      grp_lower_o_haystack = grp_mid_o_haystack + 1;
    } else if (cmp == -1) {
      grp_upper_o_haystack = grp_mid_o_haystack - 1;
    } else {
      // Hit!
      // Find lower and upper group bounds for the haystack value
      grp_lower_o_haystack = int_lower_duplicate(
        val_haystack,
        v_haystack_col,
        v_o_haystack,
        grp_lower_o_haystack,
        grp_mid_o_haystack
      );
      grp_upper_o_haystack = int_upper_duplicate(
        val_haystack,
        v_haystack_col,
        v_o_haystack,
        grp_mid_o_haystack,
        grp_upper_o_haystack
      );
      break;
    }
  }

  // Adjust bounds based on condition
  // TODO: Handle NA
  // TODO: Handle !=
  switch (op) {
  case VCTRS_OPS_lt: {
    grp_lower_o_haystack = grp_upper_o_haystack + 1;
    grp_upper_o_haystack = upper_o_haystack;
    break;
  }
  case VCTRS_OPS_lte: {
    grp_upper_o_haystack = upper_o_haystack;
    break;
  }
  case VCTRS_OPS_gt: {
    grp_upper_o_haystack = grp_lower_o_haystack - 1;
    grp_lower_o_haystack = lower_o_haystack;
    break;
  }
  case VCTRS_OPS_gte: {
    grp_lower_o_haystack = lower_o_haystack;
    break;
  }
  case VCTRS_OPS_eq: break;
  case VCTRS_OPS_neq: r_abort("not yet implemented");
  }

  // TODO: Handle <=, < case to ensure NA haystack values aren't captured
  // unless the needle value is also NA

  // Find lower and upper group bounds for the needle value
  grp_lower_o_needles = int_lower_duplicate(
    val_needle,
    v_needles_col,
    v_o_needles,
    grp_lower_o_needles,
    mid_o_needles
  );
  grp_upper_o_needles = int_upper_duplicate(
    val_needle,
    v_needles_col,
    v_o_needles,
    mid_o_needles,
    grp_upper_o_needles
  );

  if (grp_lower_o_haystack <= grp_upper_o_haystack) {
    // Hit!
    if (col < n_col - 1) {
      // Recurse into next column on this subgroup
      df_matches_recurse(
        col + 1,
        grp_lower_o_needles,
        grp_upper_o_needles,
        grp_lower_o_haystack,
        grp_upper_o_haystack,
        p_needles,
        p_haystack,
        v_o_needles,
        v_o_haystack,
        na_equal,
        multiple,
        v_ops,
        p_o_haystack_starts,
        p_match_sizes,
        p_needles_locs,
        p_n_extra,
        p_any_multiple
      );
    } else {
      for (r_ssize i = grp_lower_o_needles; i <= grp_upper_o_needles; ++i) {
        const int loc = v_o_needles[i] - 1;
        const int o_haystack_start = R_ARR_GET(int, p_o_haystack_starts, loc);
        const bool first_touch = o_haystack_start == r_globals.na_int;

        switch (multiple) {
        case VCTRS_MULTIPLE_first: {
          const int elt_o_haystack_starts = grp_lower_o_haystack + 1;

          if (first_touch) {
            R_ARR_POKE(int, p_o_haystack_starts, loc, elt_o_haystack_starts);
            continue;
          }

          const int o_haystack = v_o_haystack[o_haystack_start - 1];
          const int o_haystack_lower = v_o_haystack[grp_lower_o_haystack];

          if (o_haystack_lower < o_haystack) {
            // New start is before current one
            R_ARR_POKE(int, p_o_haystack_starts, loc, elt_o_haystack_starts);
          }

          break;
        }
        case VCTRS_MULTIPLE_last: {
          const int elt_o_haystack_starts = grp_upper_o_haystack + 1;

          if (first_touch) {
            R_ARR_POKE(int, p_o_haystack_starts, loc, elt_o_haystack_starts);
            continue;
          }

          const int o_haystack = v_o_haystack[o_haystack_start - 1];
          const int o_haystack_upper = v_o_haystack[grp_upper_o_haystack];

          if (o_haystack_upper > o_haystack) {
            // New start is after current one
            R_ARR_POKE(int, p_o_haystack_starts, loc, elt_o_haystack_starts);
          }

          break;
        }
        case VCTRS_MULTIPLE_all:
        case VCTRS_MULTIPLE_error:
        case VCTRS_MULTIPLE_warning: {
          const int elt_o_haystack_starts = grp_lower_o_haystack + 1;
          const int elt_match_sizes = grp_upper_o_haystack - grp_lower_o_haystack + 1;
          const int elt_needles_locs = loc + 1;

          if (elt_match_sizes > 1) {
            *p_any_multiple = true;
          }

          if (first_touch) {
            R_ARR_POKE(int, p_o_haystack_starts, loc, elt_o_haystack_starts);
            R_ARR_POKE(int, p_match_sizes, loc, elt_match_sizes);
            continue;
          }

          r_arr_push_back(p_o_haystack_starts, &elt_o_haystack_starts);
          r_arr_push_back(p_match_sizes, &elt_match_sizes);
          r_arr_push_back(p_needles_locs, &elt_needles_locs);
          ++(*p_n_extra);
          break;
        }
        }
      }
    }
  }

  bool do_lhs;
  bool do_rhs;

  // Default to current bounds
  r_ssize lhs_lower_o_needles = lower_o_needles;
  r_ssize lhs_upper_o_needles = upper_o_needles;
  r_ssize lhs_lower_o_haystack = lower_o_haystack;
  r_ssize lhs_upper_o_haystack = upper_o_haystack;

  r_ssize rhs_lower_o_needles = lower_o_needles;
  r_ssize rhs_upper_o_needles = upper_o_needles;
  r_ssize rhs_lower_o_haystack = lower_o_haystack;
  r_ssize rhs_upper_o_haystack = upper_o_haystack;

  switch (op) {
  case VCTRS_OPS_eq: {
    do_lhs = grp_lower_o_needles > lower_o_needles && grp_lower_o_haystack > lower_o_haystack;
    do_rhs = grp_upper_o_needles < upper_o_needles && grp_upper_o_haystack < upper_o_haystack;

    // Limit bounds of both needle and haystack using existing info
    if (do_lhs) {
      lhs_upper_o_needles = grp_lower_o_needles - 1;
      lhs_upper_o_haystack = grp_lower_o_haystack - 1;
    }
    if (do_rhs) {
      rhs_lower_o_needles = grp_upper_o_needles + 1;
      rhs_lower_o_haystack = grp_upper_o_haystack + 1;
    }

    break;
  }
  case VCTRS_OPS_neq: {
    r_abort("not yet implemented");
  }
  case VCTRS_OPS_lt:
  case VCTRS_OPS_lte:
  case VCTRS_OPS_gt:
  case VCTRS_OPS_gte: {
    // Can't update haystack here, as nested containment groups make this difficult
    do_lhs = grp_lower_o_needles > lower_o_needles;
    do_rhs = grp_upper_o_needles < upper_o_needles;

    if (do_lhs) {
      lhs_upper_o_needles = grp_lower_o_needles - 1;
    }
    if (do_rhs) {
      rhs_lower_o_needles = grp_upper_o_needles + 1;
    }

    break;
  }
  }

  if (do_lhs) {
    df_matches_recurse(
      col,
      lhs_lower_o_needles,
      lhs_upper_o_needles,
      lhs_lower_o_haystack,
      lhs_upper_o_haystack,
      p_needles,
      p_haystack,
      v_o_needles,
      v_o_haystack,
      na_equal,
      multiple,
      v_ops,
      p_o_haystack_starts,
      p_match_sizes,
      p_needles_locs,
      p_n_extra,
      p_any_multiple
    );
  }
  if (do_rhs) {
    df_matches_recurse(
      col,
      rhs_lower_o_needles,
      rhs_upper_o_needles,
      rhs_lower_o_haystack,
      rhs_upper_o_haystack,
      p_needles,
      p_haystack,
      v_o_needles,
      v_o_haystack,
      na_equal,
      multiple,
      v_ops,
      p_o_haystack_starts,
      p_match_sizes,
      p_needles_locs,
      p_n_extra,
      p_any_multiple
    );
  }
}

// -----------------------------------------------------------------------------

static
void df_matches_with_nested_groups(r_ssize size_haystack,
                                   int n_nested_groups,
                                   const int* v_nested_groups,
                                   r_ssize col,
                                   r_ssize lower_o_needles,
                                   r_ssize upper_o_needles,
                                   const struct poly_df_data* p_needles,
                                   const struct poly_df_data* p_haystack,
                                   const int* v_o_needles,
                                   const int* v_o_haystack,
                                   bool na_equal,
                                   enum vctrs_multiple multiple,
                                   enum vctrs_ops* v_ops,
                                   struct r_dyn_array* p_o_haystack_starts,
                                   struct r_dyn_array* p_match_sizes,
                                   struct r_dyn_array* p_needles_locs,
                                   r_ssize* p_n_extra,
                                   bool* p_any_multiple) {
  const int* v_haystack_col = v_nested_groups;

  r_ssize grp_lower_o_haystack = 0;
  r_ssize grp_upper_o_haystack = size_haystack - 1;

  for (int i = 0; i < n_nested_groups; ++i) {
    const int val_needle = i;

    while (grp_lower_o_haystack <= grp_upper_o_haystack) {
      const r_ssize grp_mid_o_haystack = midpoint(grp_lower_o_haystack, grp_upper_o_haystack);
      const r_ssize grp_mid_haystack = v_o_haystack[grp_mid_o_haystack] - 1;
      const int val_haystack = v_haystack_col[grp_mid_haystack];

      const int cmp = int_compare_na_equal(val_needle, val_haystack);

      if (cmp == 1) {
        grp_lower_o_haystack = grp_mid_o_haystack + 1;
      } else if (cmp == -1) {
        grp_upper_o_haystack = grp_mid_o_haystack - 1;
      } else {
        // Hit!
        // Find lower and upper group bounds
        grp_lower_o_haystack = int_lower_duplicate(
          val_haystack,
          v_haystack_col,
          v_o_haystack,
          grp_lower_o_haystack,
          grp_mid_o_haystack
        );
        grp_upper_o_haystack = int_upper_duplicate(
          val_haystack,
          v_haystack_col,
          v_o_haystack,
          grp_mid_o_haystack,
          grp_upper_o_haystack
        );
        break;
      }
     }

    df_matches_recurse(
      col,
      lower_o_needles,
      upper_o_needles,
      grp_lower_o_haystack,
      grp_upper_o_haystack,
      p_needles,
      p_haystack,
      v_o_needles,
      v_o_haystack,
      na_equal,
      multiple,
      v_ops,
      p_o_haystack_starts,
      p_match_sizes,
      p_needles_locs,
      p_n_extra,
      p_any_multiple
    );

    // Update bounds for next group
    grp_lower_o_haystack = grp_upper_o_haystack + 1;
    grp_upper_o_haystack = size_haystack - 1;
  }
}

// -----------------------------------------------------------------------------

// Find the smallest contiguous location containing `needle`
static inline
r_ssize int_lower_duplicate(int needle,
                            const int* v_haystack,
                            const int* v_o_haystack,
                            r_ssize lower_o_haystack,
                            r_ssize upper_o_haystack) {
  while (lower_o_haystack <= upper_o_haystack) {
    const r_ssize mid_o_haystack = midpoint(lower_o_haystack, upper_o_haystack);
    const r_ssize mid_haystack = v_o_haystack[mid_o_haystack] - 1;
    const int elt_haystack = v_haystack[mid_haystack];

    if (int_equal_na_equal(needle, elt_haystack)) {
      upper_o_haystack = mid_o_haystack - 1;
    } else {
      lower_o_haystack = mid_o_haystack + 1;
    }
  }

  return lower_o_haystack;
}

// Find the largest contiguous location containing `needle`
static inline
r_ssize int_upper_duplicate(int needle,
                            const int* v_haystack,
                            const int* v_o_haystack,
                            r_ssize lower_o_haystack,
                            r_ssize upper_o_haystack) {
  while (lower_o_haystack <= upper_o_haystack) {
    const r_ssize mid_o_haystack = midpoint(lower_o_haystack, upper_o_haystack);
    const r_ssize mid_haystack = v_o_haystack[mid_o_haystack] - 1;
    const int elt_haystack = v_haystack[mid_haystack];

    if (int_equal_na_equal(needle, elt_haystack)) {
      lower_o_haystack = mid_o_haystack + 1;
    } else {
      upper_o_haystack = mid_o_haystack - 1;
    }
  }

  return upper_o_haystack;
}

// -----------------------------------------------------------------------------

static inline
void parse_condition(r_obj* condition, r_ssize size, enum vctrs_ops* v_ops) {
  if (r_typeof(condition) != R_TYPE_character) {
    r_abort("`condition` must be a character vector.");
  }

  r_obj* const* v_condition = r_chr_cbegin(condition);

  for (r_ssize i = 0; i < size; ++i) {
    r_obj* elt = v_condition[i];
    const char* c_elt = CHAR(elt);

    if (!strcmp(c_elt, "==")) { v_ops[i] = VCTRS_OPS_eq;  continue; }
    if (!strcmp(c_elt, "!=")) { v_ops[i] = VCTRS_OPS_neq; continue; }
    if (!strcmp(c_elt, ">"))  { v_ops[i] = VCTRS_OPS_gt;  continue; }
    if (!strcmp(c_elt, ">=")) { v_ops[i] = VCTRS_OPS_gte; continue; }
    if (!strcmp(c_elt, "<"))  { v_ops[i] = VCTRS_OPS_lt;  continue; }
    if (!strcmp(c_elt, "<=")) { v_ops[i] = VCTRS_OPS_lte; continue; }

    r_abort("`condition` must only contain \"==\", \"!=\", \">\", \">=\", \"<\", or \"<=\".");
  }
}

// -----------------------------------------------------------------------------

static inline
enum vctrs_multiple parse_multiple(r_obj* multiple) {
  if (!r_is_string(multiple)) {
    r_abort("`multiple` must be a string.");
  }

  const char* c_multiple = r_chr_get_c_string(multiple, 0);

  if (!strcmp(c_multiple, "all")) return VCTRS_MULTIPLE_all;
  if (!strcmp(c_multiple, "warning")) return VCTRS_MULTIPLE_warning;
  if (!strcmp(c_multiple, "first")) return VCTRS_MULTIPLE_first;
  if (!strcmp(c_multiple, "last")) return VCTRS_MULTIPLE_last;
  if (!strcmp(c_multiple, "error")) return VCTRS_MULTIPLE_error;

  r_abort("`multiple` must be one of \"all\", \"first\", \"last\", \"warning\", or \"error\".");
}

// -----------------------------------------------------------------------------

static
const char* v_matches_df_names_c_strings[] = {
  "needles",
  "haystack"
};
static
const enum r_type v_matches_df_types[] = {
  R_TYPE_integer,
  R_TYPE_integer
};
enum matches_df_locs {
  MATCHES_DF_LOCS_needles,
  MATCHES_DF_LOCS_haystack
};
#define MATCHES_DF_SIZE R_ARR_SIZEOF(v_matches_df_types)


static
r_obj* expand_compact_indices(const int* v_o_haystack,
                              struct r_dyn_array* p_o_haystack_starts,
                              struct r_dyn_array* p_match_sizes,
                              struct r_dyn_array* p_needles_locs,
                              r_ssize n_used,
                              int no_match) {
  const int* v_o_haystack_starts = (const int*) r_arr_cbegin(p_o_haystack_starts);
  const int* v_match_sizes = (const int*) r_arr_cbegin(p_match_sizes);
  const int* v_needles_locs = (const int*) r_arr_cbegin(p_needles_locs);

  r_ssize out_size = 0;
  for (r_ssize i = 0; i < n_used; ++i) {
    // TODO: Check for overflow?
    // This could get extremely large with improperly specified non-equi joins.
    out_size += (r_ssize) v_match_sizes[i];
  }

  r_obj* names = KEEP(r_chr_n(v_matches_df_names_c_strings, MATCHES_DF_SIZE));

  r_obj* out = KEEP(r_alloc_df_list(
    out_size,
    names,
    v_matches_df_types,
    MATCHES_DF_SIZE
  ));
  r_init_data_frame(out, out_size);

  int* v_out_needles = r_int_begin(r_list_get(out, MATCHES_DF_LOCS_needles));
  int* v_out_haystack = r_int_begin(r_list_get(out, MATCHES_DF_LOCS_haystack));

  // `vec_order()` setup
  r_obj* const direction = KEEP(r_chr("asc"));
  r_obj* const na_value = KEEP(r_chr("largest"));
  const bool nan_distinct = true;
  r_obj* const chr_transform = r_null;

  r_obj* needles_locs = KEEP(r_arr_unwrap(p_needles_locs));
  r_obj* o_needles_locs = KEEP(vec_order(needles_locs, direction, na_value, nan_distinct, chr_transform));
  const int* v_o_needles_locs = r_int_cbegin(o_needles_locs);

  r_ssize out_loc = 0;

  for (r_ssize i = 0; i < n_used; ++i) {
    const int loc = v_o_needles_locs[i] - 1;

    int o_haystack_loc = v_o_haystack_starts[loc];
    const int match_size = v_match_sizes[loc];
    const int needles_loc = v_needles_locs[loc];

    if (int_is_missing(o_haystack_loc)) {
      if (match_size != 1) {
        r_stop_internal(
          "expand_compact_indices",
          "`match_size` should always be 1 in the case of no matches."
        );
      }

      v_out_needles[out_loc] = needles_loc;
      v_out_haystack[out_loc] = no_match;
      ++out_loc;

      continue;
    }

    for (r_ssize j = 0; j < match_size; ++j) {
      const int haystack_loc = v_o_haystack[o_haystack_loc - 1];

      v_out_needles[out_loc] = needles_loc;
      v_out_haystack[out_loc] = haystack_loc;

      ++out_loc;
      ++o_haystack_loc;
    }
  }

  FREE(6);
  return out;
}

// -----------------------------------------------------------------------------

// Currently assumes `proxy` is in completely increasing order
r_obj* nested_containment_order(r_obj* proxy,
                                r_obj* order,
                                r_obj* group_sizes) {
  if (!is_data_frame(proxy)) {
    r_stop_internal("nested_containment_order", "`proxy` must be a data frame.");
  }

  int n_prot = 0;

  r_ssize n_cols = r_length(proxy);
  r_ssize size = r_length(order);

  r_obj* out = KEEP_N(r_alloc_list(2), &n_prot);

  r_obj* ids = r_alloc_integer(size);
  r_list_poke(out, 0, ids);
  int* v_ids = r_int_begin(ids);

  r_obj* n_ids = r_alloc_integer(1);
  r_list_poke(out, 1, n_ids);
  int* v_n_ids = r_int_begin(n_ids);
  *v_n_ids = 1;

  for (r_ssize i = 0; i < size; ++i) {
    v_ids[i] = 0;
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

    int id = new_id ? max_prev_row_id : prev_row_id;
    int group_size = v_group_sizes[i_group];

    for (int i = 0; i < group_size; ++i) {
      v_ids[v_order[i_order + i] - 1] = id;
    }

    i_order += group_size;
  }

  *v_n_ids = prev_rows.n;

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
