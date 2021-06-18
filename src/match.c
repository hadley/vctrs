#include <rlang.h>
#include "vctrs.h"
#include "utils.h"
#include "rank.h"
#include "complete.h"
#include "poly-op.h"
#include "compare.h"
#include "ptype2.h"
#include "order-radix.h"

// -----------------------------------------------------------------------------

enum vctrs_multiple {
  VCTRS_MULTIPLE_all = 0,
  VCTRS_MULTIPLE_warning = 1, // Collects multiple matches like all, warns if there are any
  VCTRS_MULTIPLE_error = 2, // Collects multiple matches like all, errors if there are any
  VCTRS_MULTIPLE_first = 3,
  VCTRS_MULTIPLE_last = 4
};

enum vctrs_filter {
  VCTRS_FILTER_none = 0,
  VCTRS_FILTER_min = 1,
  VCTRS_FILTER_max = 2
};

enum vctrs_ops {
  VCTRS_OPS_eq = 0,
  VCTRS_OPS_gt = 1,
  VCTRS_OPS_gte = 2,
  VCTRS_OPS_lt = 3,
  VCTRS_OPS_lte = 4
};

enum vctrs_missing_needle {
  VCTRS_MISSING_NEEDLE_match = 0,
  VCTRS_MISSING_NEEDLE_propagate = 1,
  VCTRS_MISSING_NEEDLE_drop = 2,
  VCTRS_MISSING_NEEDLE_error = 3
};

enum vctrs_check_duplicates {
  VCTRS_CHECK_DUPLICATES_neither = 0,
  VCTRS_CHECK_DUPLICATES_needles = 1,
  VCTRS_CHECK_DUPLICATES_haystack = 2,
  VCTRS_CHECK_DUPLICATES_both = 3
};

struct vctrs_no_match {
  bool error;
  bool drop;
  int value;
};

#define SIGNAL_NO_MATCH r_globals.na_int
#define SIGNAL_NA_PROPAGATE -1

// -----------------------------------------------------------------------------

#include "decl/matches-decl.h"

// -----------------------------------------------------------------------------

// [[ register() ]]
r_obj* vctrs_matches(r_obj* needles,
                     r_obj* haystack,
                     r_obj* condition,
                     r_obj* filter,
                     r_obj* missing,
                     r_obj* no_match,
                     r_obj* remaining,
                     r_obj* multiple,
                     r_obj* check_duplicates,
                     r_obj* nan_distinct,
                     r_obj* chr_transform,
                     r_obj* needles_arg,
                     r_obj* haystack_arg) {
  enum vctrs_missing_needle c_missing = parse_missing(missing);
  const struct vctrs_no_match c_no_match = parse_no_match(no_match, "no_match");
  const struct vctrs_no_match c_remaining = parse_no_match(remaining, "remaining");
  enum vctrs_multiple c_multiple = parse_multiple(multiple);
  enum vctrs_check_duplicates c_check_duplicates = parse_check_duplicates(check_duplicates);

  if (!r_is_bool(nan_distinct)) {
    r_abort("`nan_distinct` must be a single `TRUE` or `FALSE`.");
  }
  bool c_nan_distinct = r_lgl_get(nan_distinct, 0);

  struct vctrs_arg c_needles_arg = vec_as_arg(needles_arg);
  struct vctrs_arg c_haystack_arg = vec_as_arg(haystack_arg);

  return vec_matches(
    needles,
    haystack,
    condition,
    filter,
    c_missing,
    &c_no_match,
    &c_remaining,
    c_multiple,
    c_check_duplicates,
    c_nan_distinct,
    chr_transform,
    &c_needles_arg,
    &c_haystack_arg
  );
}

static
r_obj* vec_matches(r_obj* needles,
                   r_obj* haystack,
                   r_obj* condition,
                   r_obj* filter,
                   enum vctrs_missing_needle missing,
                   const struct vctrs_no_match* no_match,
                   const struct vctrs_no_match* remaining,
                   enum vctrs_multiple multiple,
                   enum vctrs_check_duplicates check_duplicates,
                   bool nan_distinct,
                   r_obj* chr_transform,
                   struct vctrs_arg* needles_arg,
                   struct vctrs_arg* haystack_arg) {
  int n_prot = 0;

  int _;
  r_obj* ptype = KEEP_N(vec_ptype2_params(
    needles,
    haystack,
    needles_arg,
    haystack_arg,
    DF_FALLBACK_quiet,
    &_
  ), &n_prot);

  needles = KEEP_N(vec_cast_params(
    needles,
    ptype,
    needles_arg,
    args_empty,
    DF_FALLBACK_quiet,
    S3_FALLBACK_false
  ), &n_prot);

  haystack = KEEP_N(vec_cast_params(
    haystack,
    ptype,
    haystack_arg,
    args_empty,
    DF_FALLBACK_quiet,
    S3_FALLBACK_false
  ), &n_prot);

  r_ssize size_needles = vec_size(needles);
  r_ssize size_haystack = vec_size(haystack);

  // Support non-data frame types by wrapping them in a 1-col data frame
  if (!is_data_frame(needles)) {
    needles = KEEP_N(r_list(needles), &n_prot);
    r_poke_names(needles, r_chr("x"));
    r_init_data_frame(needles, size_needles);

    haystack = KEEP_N(r_list(haystack), &n_prot);
    r_poke_names(haystack, r_chr("x"));
    r_init_data_frame(haystack, size_haystack);

    ptype = KEEP_N(r_list(ptype), &n_prot);
    r_poke_names(ptype, r_chr("x"));
    r_init_data_frame(ptype, 0);
  }

  if (condition == r_null) {
    // Special case of "match on nothing" to mimic `LEFT JOIN needles, haystack`
    // with no ON condition
    r_obj* out = expand_match_on_nothing(
      size_needles,
      size_haystack,
      multiple,
      no_match,
      remaining,
      needles_arg,
      haystack_arg
    );
    FREE(n_prot);
    return out;
  }

  r_ssize n_cols = r_length(needles);

  enum vctrs_ops* v_ops = (enum vctrs_ops*) R_alloc(n_cols, sizeof(enum vctrs_ops));
  parse_condition(condition, v_ops, n_cols);

  bool any_filters = false;
  enum vctrs_filter* v_filters = (enum vctrs_filter*) R_alloc(n_cols, sizeof(enum vctrs_filter));
  parse_filter(filter, n_cols, v_filters, &any_filters);

  if (n_cols == 0) {
    // If we have a match `condition`, but there are no columns, this operation
    // isn't well defined
    r_abort("Must have at least 1 column to match on unless `condition = NULL`.");
  }

  const bool missing_propagate =
    missing == VCTRS_MISSING_NEEDLE_propagate ||
    missing == VCTRS_MISSING_NEEDLE_drop ||
    missing == VCTRS_MISSING_NEEDLE_error;

  // Compute the locations of missing values for each column if computing ranks
  // later on is going to replace the missing values with integer ranks
  r_obj* needles_missings = missing_propagate ? r_null : df_missings_by_col(needles, size_needles, n_cols);
  KEEP_N(needles_missings, &n_prot);

  r_obj* haystack_missings = missing_propagate ? r_null : df_missings_by_col(haystack, size_haystack, n_cols);
  KEEP_N(haystack_missings, &n_prot);

  // Compute joint ranks to simplify each column down to an integer vector
  r_obj* args = KEEP_N(df_joint_ranks(
    needles,
    haystack,
    size_needles,
    size_haystack,
    n_cols,
    ptype,
    missing_propagate,
    nan_distinct,
    chr_transform
  ), &n_prot);
  needles = r_list_get(args, 0);
  haystack = r_list_get(args, 1);

  r_obj* out = df_matches(
    needles,
    haystack,
    needles_missings,
    haystack_missings,
    size_needles,
    size_haystack,
    missing,
    missing_propagate,
    no_match,
    remaining,
    multiple,
    check_duplicates,
    any_filters,
    v_filters,
    v_ops,
    needles_arg,
    haystack_arg
  );

  FREE(n_prot);
  return out;
}

// -----------------------------------------------------------------------------

static
r_obj* df_matches(r_obj* needles,
                  r_obj* haystack,
                  r_obj* needles_missings,
                  r_obj* haystack_missings,
                  r_ssize size_needles,
                  r_ssize size_haystack,
                  enum vctrs_missing_needle missing,
                  bool missing_propagate,
                  const struct vctrs_no_match* no_match,
                  const struct vctrs_no_match* remaining,
                  enum vctrs_multiple multiple,
                  enum vctrs_check_duplicates check_duplicates,
                  bool any_filters,
                  const enum vctrs_filter* v_filters,
                  const enum vctrs_ops* v_ops,
                  struct vctrs_arg* needles_arg,
                  struct vctrs_arg* haystack_arg) {
  int n_prot = 0;

  r_obj* o_needles;
  if (check_duplicates == VCTRS_CHECK_DUPLICATES_both || check_duplicates == VCTRS_CHECK_DUPLICATES_needles) {
    r_obj* info = KEEP(vec_order_info(needles, chrs_asc, chrs_smallest, true, r_null, true));
    check_for_duplicates(info, needles_arg, true);
    o_needles = r_list_get(info, 0);
    FREE(1);
  } else {
    o_needles = vec_order(needles, chrs_asc, chrs_smallest, true, r_null);
  }
  KEEP_N(o_needles, &n_prot);
  const int* v_o_needles = r_int_cbegin(o_needles);

  r_obj* info = KEEP_N(compute_nested_containment_info(
    haystack,
    multiple,
    check_duplicates,
    v_ops,
    haystack_arg
  ), &n_prot);

  r_obj* o_haystack = r_list_get(info, 0);
  const int* v_o_haystack = r_int_cbegin(o_haystack);

  // Will be `integer()` if no nesting info is required.
  // In that case, `n_nested_groups == 1`.
  r_obj* nested_groups = r_list_get(info, 1);
  const int* v_nested_groups = r_int_cbegin(nested_groups);

  int n_nested_groups = r_as_int(r_list_get(info, 2));
  bool any_directional = r_as_bool(r_list_get(info, 3));

  // In the case of possible multiple matches that fall in separate
  // nested containers, allocate ~20% extra room
  r_ssize initial_capacity = (n_nested_groups == 1) ? size_needles : size_needles * 1.2;

  struct r_dyn_array* p_locs_start_o_haystack = r_new_dyn_vector(R_TYPE_integer, initial_capacity);
  KEEP_N(p_locs_start_o_haystack->shelter, &n_prot);

  {
    // Temporary unstable pointer
    int* v_locs_start_o_haystack = (int*) r_arr_begin(p_locs_start_o_haystack);
    for (r_ssize i = 0; i < size_needles; ++i) {
      // Initialize to no match everywhere, no need to initialize extra buffer
      v_locs_start_o_haystack[i] = SIGNAL_NO_MATCH;
    }
    p_locs_start_o_haystack->count = size_needles;
  }

  // If we can skip, `match_sizes` will always be `1`
  const bool skip_match_sizes = (multiple == VCTRS_MULTIPLE_first || multiple == VCTRS_MULTIPLE_last);

  struct r_dyn_array* p_match_sizes = NULL;
  if (!skip_match_sizes) {
    p_match_sizes = r_new_dyn_vector(R_TYPE_integer, initial_capacity);
    KEEP_N(p_match_sizes->shelter, &n_prot);

    int* v_match_sizes = (int*) r_arr_begin(p_match_sizes);
    for (r_ssize i = 0; i < size_needles; ++i) {
      // No need to initialize extra buffer
      v_match_sizes[i] = 1;
    }
    p_match_sizes->count = size_needles;
  }

  // If we can skip, `locs_needles` will always be an increasing sequence of values
  const bool skip_locs_needles = (multiple == VCTRS_MULTIPLE_first || multiple == VCTRS_MULTIPLE_last);

  struct r_dyn_array* p_locs_needles = NULL;
  if (!skip_locs_needles) {
    p_locs_needles = r_new_dyn_vector(R_TYPE_integer, initial_capacity);
    KEEP_N(p_locs_needles->shelter, &n_prot);

    int* v_locs_needles = (int*) r_arr_begin(p_locs_needles);
    for (r_ssize i = 0; i < size_needles; ++i) {
      // No need to initialize extra buffer
      v_locs_needles[i] = i + 1;
    }
    p_locs_needles->count = size_needles;
  }

  // When filtering, we find the filtered match for a particular needle in each
  // nested containment group of the haystack. `locs_filter_match_haystack`
  // keeps track of the overall filtered match loc for a needle across all
  // nested groups in the haystack.
  bool has_locs_filter_match_haystack =
    any_filters &&
    (multiple == VCTRS_MULTIPLE_all ||
     multiple == VCTRS_MULTIPLE_warning ||
     multiple == VCTRS_MULTIPLE_error);

  int* v_locs_filter_match_haystack = NULL;
  if (has_locs_filter_match_haystack) {
    r_obj* locs_filter_match_haystack = KEEP_N(r_alloc_integer(size_needles), &n_prot);
    v_locs_filter_match_haystack = r_int_begin(locs_filter_match_haystack);
  }

  struct poly_vec* p_poly_needles = new_poly_vec(needles, vctrs_type_dataframe);
  PROTECT_POLY_VEC(p_poly_needles, &n_prot);
  const struct poly_df_data* p_needles = (const struct poly_df_data*) p_poly_needles->p_vec;

  struct poly_vec* p_poly_haystack = new_poly_vec(haystack, vctrs_type_dataframe);
  PROTECT_POLY_VEC(p_poly_haystack, &n_prot);
  const struct poly_df_data* p_haystack = (const struct poly_df_data*) p_poly_haystack->p_vec;

  const struct poly_df_data* p_needles_missings;
  const struct poly_df_data* p_haystack_missings;

  if (missing_propagate) {
    // NAs propagated through the ranks, so we can use them directly
    p_needles_missings = p_needles;
    p_haystack_missings = p_haystack;
  } else {
    // NAs were removed from the ranks, so grab them from the "missing" data frames
    struct poly_vec* p_poly_needles_missings = new_poly_vec(needles_missings, vctrs_type_dataframe);
    PROTECT_POLY_VEC(p_poly_needles_missings, &n_prot);
    p_needles_missings = (const struct poly_df_data*) p_poly_needles_missings->p_vec;

    struct poly_vec* p_poly_haystack_missings = new_poly_vec(haystack_missings, vctrs_type_dataframe);
    PROTECT_POLY_VEC(p_poly_haystack_missings, &n_prot);
    p_haystack_missings = (const struct poly_df_data*) p_poly_haystack_missings->p_vec;
  }

  r_ssize n_extra = 0;

  if (size_needles > 0) {
    // Recursion requires at least 1 row in needles.
    // In the case of size 0 needles, there is nothing to do, but this avoids
    // a segfault.

    const r_ssize col = 0;
    const r_ssize loc_lower_o_needles = 0;
    const r_ssize loc_upper_o_needles = size_needles - 1;

    if (n_nested_groups == 1) {
      const r_ssize loc_lower_o_haystack = 0;
      const r_ssize loc_upper_o_haystack = size_haystack - 1;

      df_matches_recurse(
        col,
        loc_lower_o_needles,
        loc_upper_o_needles,
        loc_lower_o_haystack,
        loc_upper_o_haystack,
        p_needles,
        p_haystack,
        p_needles_missings,
        p_haystack_missings,
        v_o_needles,
        v_o_haystack,
        missing_propagate,
        multiple,
        any_filters,
        v_filters,
        v_ops,
        p_locs_start_o_haystack,
        p_match_sizes,
        p_locs_needles,
        v_locs_filter_match_haystack,
        &n_extra
      );
    } else {
      df_matches_with_nested_groups(
        size_haystack,
        n_nested_groups,
        v_nested_groups,
        col,
        loc_lower_o_needles,
        loc_upper_o_needles,
        p_needles,
        p_haystack,
        p_needles_missings,
        p_haystack_missings,
        v_o_needles,
        v_o_haystack,
        missing_propagate,
        multiple,
        any_filters,
        v_filters,
        v_ops,
        p_locs_start_o_haystack,
        p_match_sizes,
        p_locs_needles,
        v_locs_filter_match_haystack,
        &n_extra
      );
    }
  }

  r_obj* out = KEEP_N(expand_compact_indices(
    v_o_haystack,
    p_locs_start_o_haystack,
    p_match_sizes,
    p_locs_needles,
    skip_match_sizes,
    skip_locs_needles,
    missing,
    missing_propagate,
    no_match,
    remaining,
    multiple,
    size_needles,
    size_haystack,
    any_directional,
    has_locs_filter_match_haystack,
    v_filters,
    v_locs_filter_match_haystack,
    p_haystack,
    needles_arg,
    haystack_arg
  ), &n_prot);

  FREE(n_prot);
  return out;
}

// -----------------------------------------------------------------------------

static
void df_matches_recurse(r_ssize col,
                        r_ssize loc_lower_o_needles,
                        r_ssize loc_upper_o_needles,
                        r_ssize loc_lower_o_haystack,
                        r_ssize loc_upper_o_haystack,
                        const struct poly_df_data* p_needles,
                        const struct poly_df_data* p_haystack,
                        const struct poly_df_data* p_needles_missings,
                        const struct poly_df_data* p_haystack_missings,
                        const int* v_o_needles,
                        const int* v_o_haystack,
                        bool missing_propagate,
                        enum vctrs_multiple multiple,
                        bool any_filters,
                        const enum vctrs_filter* v_filters,
                        const enum vctrs_ops* v_ops,
                        struct r_dyn_array* p_locs_start_o_haystack,
                        struct r_dyn_array* p_match_sizes,
                        struct r_dyn_array* p_locs_needles,
                        int* v_locs_filter_match_haystack,
                        r_ssize* p_n_extra) {
  const enum vctrs_ops op = v_ops[col];
  const enum vctrs_filter filter = v_filters[col];
  const r_ssize n_col = p_needles->n_col;

  const int* v_needles = (const int*) p_needles->col_ptrs[col];
  const int* v_needles_missings = (const int*) p_needles_missings->col_ptrs[col];

  const int* v_haystack = (const int*) p_haystack->col_ptrs[col];
  const int* v_haystack_missings = (const int*) p_haystack_missings->col_ptrs[col];

  r_ssize loc_group_lower_o_needles = loc_lower_o_needles;
  r_ssize loc_group_upper_o_needles = loc_upper_o_needles;

  const r_ssize loc_group_mid_o_needles = midpoint(loc_group_lower_o_needles, loc_group_upper_o_needles);
  const r_ssize loc_group_mid_needles = v_o_needles[loc_group_mid_o_needles] - 1;

  const int val_needle = v_needles[loc_group_mid_needles];
  const bool needle_is_missing = int_is_missing(v_needles_missings[loc_group_mid_needles]);

  // Find lower and upper group bounds for the needle value
  loc_group_lower_o_needles = int_lower_duplicate(
    val_needle,
    v_needles,
    v_o_needles,
    loc_group_lower_o_needles,
    loc_group_mid_o_needles
  );
  loc_group_upper_o_needles = int_upper_duplicate(
    val_needle,
    v_needles,
    v_o_needles,
    loc_group_mid_o_needles,
    loc_group_upper_o_needles
  );

  if (missing_propagate && needle_is_missing) {
    // Propagate NA, don't recursive into further columns.
    for (r_ssize i = loc_group_lower_o_needles; i <= loc_group_upper_o_needles; ++i) {
      // Will always be the first and only time the output is touched for this
      // needle, so we can poke directly into it
      const int loc_needles = v_o_needles[i] - 1;
      R_ARR_POKE(int, p_locs_start_o_haystack, loc_needles, SIGNAL_NA_PROPAGATE);
    }

    // Learned nothing about haystack!
    bool do_lhs = loc_group_lower_o_needles > loc_lower_o_needles;
    bool do_rhs = loc_group_upper_o_needles < loc_upper_o_needles;

    if (do_lhs) {
      loc_upper_o_needles = loc_group_lower_o_needles - 1;

      df_matches_recurse(
        col,
        loc_lower_o_needles,
        loc_upper_o_needles,
        loc_lower_o_haystack,
        loc_upper_o_haystack,
        p_needles,
        p_haystack,
        p_needles_missings,
        p_haystack_missings,
        v_o_needles,
        v_o_haystack,
        missing_propagate,
        multiple,
        any_filters,
        v_filters,
        v_ops,
        p_locs_start_o_haystack,
        p_match_sizes,
        p_locs_needles,
        v_locs_filter_match_haystack,
        p_n_extra
      );
    }
    if (do_rhs) {
      loc_lower_o_needles = loc_group_upper_o_needles + 1;

      df_matches_recurse(
        col,
        loc_lower_o_needles,
        loc_upper_o_needles,
        loc_lower_o_haystack,
        loc_upper_o_haystack,
        p_needles,
        p_haystack,
        p_needles_missings,
        p_haystack_missings,
        v_o_needles,
        v_o_haystack,
        missing_propagate,
        multiple,
        any_filters,
        v_filters,
        v_ops,
        p_locs_start_o_haystack,
        p_match_sizes,
        p_locs_needles,
        v_locs_filter_match_haystack,
        p_n_extra
      );
    }

    return;
  }

  r_ssize loc_group_lower_o_haystack = loc_lower_o_haystack;
  r_ssize loc_group_upper_o_haystack = loc_upper_o_haystack;

  while (loc_group_lower_o_haystack <= loc_group_upper_o_haystack) {
    const r_ssize loc_group_mid_o_haystack = midpoint(loc_group_lower_o_haystack, loc_group_upper_o_haystack);
    const r_ssize loc_group_mid_haystack = v_o_haystack[loc_group_mid_o_haystack] - 1;
    const int val_haystack = v_haystack[loc_group_mid_haystack];

    const int cmp = int_compare_na_equal(val_needle, val_haystack);

    if (cmp == 1) {
      loc_group_lower_o_haystack = loc_group_mid_o_haystack + 1;
    } else if (cmp == -1) {
      loc_group_upper_o_haystack = loc_group_mid_o_haystack - 1;
    } else {
      // Hit!
      // Find lower and upper group bounds for the haystack value
      loc_group_lower_o_haystack = int_lower_duplicate(
        val_haystack,
        v_haystack,
        v_o_haystack,
        loc_group_lower_o_haystack,
        loc_group_mid_o_haystack
      );
      loc_group_upper_o_haystack = int_upper_duplicate(
        val_haystack,
        v_haystack,
        v_o_haystack,
        loc_group_mid_o_haystack,
        loc_group_upper_o_haystack
      );
      break;
    }
  }

  // Adjust bounds based on non-equi condition.
  // If needle is NA, never extend the bounds to capture values past it.
  switch (op) {
  case VCTRS_OPS_lt: {
    // Exclude found needle
    loc_group_lower_o_haystack = loc_group_upper_o_haystack + 1;
    if (!needle_is_missing) {
      loc_group_upper_o_haystack = loc_upper_o_haystack;
    }
    break;
  }
  case VCTRS_OPS_lte: {
    if (!needle_is_missing) {
      loc_group_upper_o_haystack = loc_upper_o_haystack;
    }
    break;
  }
  case VCTRS_OPS_gt: {
    // Exclude found needle
    loc_group_upper_o_haystack = loc_group_lower_o_haystack - 1;
    if (!needle_is_missing) {
      loc_group_lower_o_haystack = loc_lower_o_haystack;
    }
    break;
  }
  case VCTRS_OPS_gte: {
    if (!needle_is_missing) {
      loc_group_lower_o_haystack = loc_lower_o_haystack;
    }
    break;
  }
  case VCTRS_OPS_eq: {
    break;
  }
  }

  if (!needle_is_missing &&
      (op == VCTRS_OPS_gt || op == VCTRS_OPS_gte) &&
      (loc_group_lower_o_haystack <= loc_group_upper_o_haystack)) {
    // In this specific case, a non-NA needle may match an NA in the haystack
    // from the condition adjustments made above. If there was an NA in the
    // haystack, we avoid including it by shifting the lower bound to 1 past
    // the final NA.
    const r_ssize loc_group_lower_haystack = v_o_haystack[loc_group_lower_o_haystack] - 1;
    const bool group_lower_haystack_is_missing = int_is_missing(v_haystack_missings[loc_group_lower_haystack]);

    if (group_lower_haystack_is_missing) {
      /* If there was an NA in the haystack, find the last NA */
      loc_group_lower_o_haystack = int_locate_upper_missing(
        v_haystack_missings,
        v_o_haystack,
        loc_group_lower_o_haystack,
        loc_group_upper_o_haystack
      );

      /* Exclude it and all before it */
      ++loc_group_lower_o_haystack;
    }
  }

  if (loc_group_lower_o_haystack <= loc_group_upper_o_haystack) {
    // Hit!

    switch (filter) {
    case VCTRS_FILTER_max: {
      if (needle_is_missing || op == VCTRS_OPS_eq) {
        // Lower bound value will already equal upper bound value
        break;
      }
      if (multiple == VCTRS_MULTIPLE_last) {
        // "last" only requires the upper bound, which already points to "max"
        break;
      }

      // We want the max values of this group. That's the upper value of the
      // haystack and its corresponding lower duplicate.
      const int loc_group_lower_haystack = v_o_haystack[loc_group_lower_o_haystack] - 1;
      const int loc_group_upper_haystack = v_o_haystack[loc_group_upper_o_haystack] - 1;
      const int val_group_lower_haystack = v_haystack[loc_group_lower_haystack];
      const int val_group_upper_haystack = v_haystack[loc_group_upper_haystack];

      if (val_group_lower_haystack != val_group_upper_haystack) {
        loc_group_lower_o_haystack = int_lower_duplicate(
          val_group_upper_haystack,
          v_haystack,
          v_o_haystack,
          loc_group_lower_o_haystack,
          loc_group_upper_o_haystack
        );
      }

      break;
    }
    case VCTRS_FILTER_min: {
      if (needle_is_missing || op == VCTRS_OPS_eq) {
        // Lower bound value will already equal upper bound value
        break;
      }
      if (multiple == VCTRS_MULTIPLE_first) {
        // "first" only requires the lower bound, which already points to "min"
        break;
      }

      // We want the min values of this group. That's the lower value of the
      // haystack and its corresponding upper duplicate.
      const int loc_group_lower_haystack = v_o_haystack[loc_group_lower_o_haystack] - 1;
      const int loc_group_upper_haystack = v_o_haystack[loc_group_upper_o_haystack] - 1;
      const int val_group_lower_haystack = v_haystack[loc_group_lower_haystack];
      const int val_group_upper_haystack = v_haystack[loc_group_upper_haystack];

      if (val_group_lower_haystack != val_group_upper_haystack) {
        loc_group_upper_o_haystack = int_upper_duplicate(
          val_group_lower_haystack,
          v_haystack,
          v_o_haystack,
          loc_group_lower_o_haystack,
          loc_group_upper_o_haystack
        );
      }

      break;
    }
    case VCTRS_FILTER_none: {
      break;
    }
    }

    if (col < n_col - 1) {
      // Recurse into next column on this subgroup
      df_matches_recurse(
        col + 1,
        loc_group_lower_o_needles,
        loc_group_upper_o_needles,
        loc_group_lower_o_haystack,
        loc_group_upper_o_haystack,
        p_needles,
        p_haystack,
        p_needles_missings,
        p_haystack_missings,
        v_o_needles,
        v_o_haystack,
        missing_propagate,
        multiple,
        any_filters,
        v_filters,
        v_ops,
        p_locs_start_o_haystack,
        p_match_sizes,
        p_locs_needles,
        v_locs_filter_match_haystack,
        p_n_extra
      );
    } else {
      for (r_ssize i = loc_group_lower_o_needles; i <= loc_group_upper_o_needles; ++i) {
        const int loc_needles = v_o_needles[i] - 1;
        int loc_start_o_haystack = R_ARR_GET(int, p_locs_start_o_haystack, loc_needles);
        const bool first_touch = loc_start_o_haystack == r_globals.na_int;

        switch (multiple) {
        case VCTRS_MULTIPLE_first: {
          const int new_loc_start_o_haystack = loc_group_lower_o_haystack + 1;

          if (first_touch) {
            R_ARR_POKE(int, p_locs_start_o_haystack, loc_needles, new_loc_start_o_haystack);
            break;
          }

          const int loc_start_haystack = v_o_haystack[loc_start_o_haystack - 1] - 1;
          const int loc_group_lower_haystack = v_o_haystack[loc_group_lower_o_haystack] - 1;

          if (any_filters) {
            int cmp = p_matches_df_compare_na_equal(
              p_haystack,
              loc_group_lower_haystack,
              p_haystack,
              loc_start_haystack,
              v_filters
            );

            if (cmp == -1) {
              // New haystack value loses vs previous one, do nothing
              break;
            } else if (cmp == 1) {
              // New haystack value wins vs previous one, automatically update like first_touch
              R_ARR_POKE(int, p_locs_start_o_haystack, loc_needles, new_loc_start_o_haystack);
              break;
            }
          }

          if (loc_group_lower_haystack < loc_start_haystack) {
            // New start is before current one
            R_ARR_POKE(int, p_locs_start_o_haystack, loc_needles, new_loc_start_o_haystack);
          }

          break;
        }
        case VCTRS_MULTIPLE_last: {
          const int new_loc_start_o_haystack = loc_group_upper_o_haystack + 1;

          if (first_touch) {
            R_ARR_POKE(int, p_locs_start_o_haystack, loc_needles, new_loc_start_o_haystack);
            break;
          }

          const int loc_start_haystack = v_o_haystack[loc_start_o_haystack - 1] - 1;
          const int loc_group_upper_haystack = v_o_haystack[loc_group_upper_o_haystack] - 1;

          if (any_filters) {
            int cmp = p_matches_df_compare_na_equal(
              p_haystack,
              loc_group_upper_haystack,
              p_haystack,
              loc_start_haystack,
              v_filters
            );

            if (cmp == -1) {
              // New haystack value loses vs previous one, do nothing
              break;
            } else if (cmp == 1) {
              // New haystack value wins vs previous one, automatically update like first_touch
              R_ARR_POKE(int, p_locs_start_o_haystack, loc_needles, new_loc_start_o_haystack);
              break;
            }
          }

          if (loc_group_upper_haystack > loc_start_haystack) {
            // New start is after current one
            R_ARR_POKE(int, p_locs_start_o_haystack, loc_needles, new_loc_start_o_haystack);
          }

          break;
        }
        case VCTRS_MULTIPLE_all:
        case VCTRS_MULTIPLE_error:
        case VCTRS_MULTIPLE_warning: {
          const int new_loc_start_o_haystack = loc_group_lower_o_haystack + 1;
          const int new_match_size = loc_group_upper_o_haystack - loc_group_lower_o_haystack + 1;
          const int new_loc_needles = loc_needles + 1;

          if (first_touch) {
            R_ARR_POKE(int, p_locs_start_o_haystack, loc_needles, new_loc_start_o_haystack);
            R_ARR_POKE(int, p_match_sizes, loc_needles, new_match_size);

            if (any_filters) {
              const int loc_group_upper_haystack = v_o_haystack[loc_group_upper_o_haystack] - 1;
              v_locs_filter_match_haystack[loc_needles] = loc_group_upper_haystack;
            }

            break;
          }

          if (any_filters) {
            const int loc_filter_match_haystack = v_locs_filter_match_haystack[loc_needles];
            const int loc_group_upper_haystack = v_o_haystack[loc_group_upper_o_haystack] - 1;

            int cmp = p_matches_df_compare_na_equal(
              p_haystack,
              loc_group_upper_haystack,
              p_haystack,
              loc_filter_match_haystack,
              v_filters
            );

            if (cmp == 1) {
              // Update filter match location for later use
              v_locs_filter_match_haystack[loc_needles] = loc_group_upper_haystack;
            } else if (cmp == -1) {
              // Not a valid match, as another previous match wins over this one
              break;
            }
          }

          r_arr_push_back(p_locs_start_o_haystack, &new_loc_start_o_haystack);
          r_arr_push_back(p_match_sizes, &new_match_size);
          r_arr_push_back(p_locs_needles, &new_loc_needles);
          ++(*p_n_extra);
          break;
        }
        }
      }
    }
  } else if (missing_propagate && col < n_col - 1) {
    // Miss! This `needles` column group has no matches in the corresponding
    // `haystack` column. However, we still need to propagate any potential
    // NAs that might occur in future columns of this `needles` group. If
    // `val_needles` was an NA, it would have been caught above, so we only
    // need to look at future columns.
    for (r_ssize i = loc_group_lower_o_needles; i <= loc_group_upper_o_needles; ++i) {
      const r_ssize loc_needles = v_o_needles[i] - 1;

      for (r_ssize j = col + 1; j < n_col; ++j) {
        const int* v_future_needles_missings = (const int*) p_needles_missings->col_ptrs[j];
        const bool future_needle_is_missing = int_is_missing(v_future_needles_missings[loc_needles]);

        if (future_needle_is_missing) {
          R_ARR_POKE(int, p_locs_start_o_haystack, loc_needles, SIGNAL_NA_PROPAGATE);
          break;
        }
      }
    }
  }

  bool do_lhs;
  bool do_rhs;

  // Default to current bounds
  r_ssize lhs_loc_lower_o_needles = loc_lower_o_needles;
  r_ssize lhs_loc_upper_o_needles = loc_upper_o_needles;
  r_ssize lhs_loc_lower_o_haystack = loc_lower_o_haystack;
  r_ssize lhs_loc_upper_o_haystack = loc_upper_o_haystack;

  r_ssize rhs_loc_lower_o_needles = loc_lower_o_needles;
  r_ssize rhs_loc_upper_o_needles = loc_upper_o_needles;
  r_ssize rhs_loc_lower_o_haystack = loc_lower_o_haystack;
  r_ssize rhs_loc_upper_o_haystack = loc_upper_o_haystack;

  switch (op) {
  case VCTRS_OPS_eq: {
    do_lhs =
      (loc_group_lower_o_needles > loc_lower_o_needles) &&
      (loc_group_lower_o_haystack > loc_lower_o_haystack);
    do_rhs =
      (loc_group_upper_o_needles < loc_upper_o_needles) &&
      (loc_group_upper_o_haystack < loc_upper_o_haystack);

    // Limit bounds of both needle and haystack using existing info
    if (do_lhs) {
      lhs_loc_upper_o_needles = loc_group_lower_o_needles - 1;
      lhs_loc_upper_o_haystack = loc_group_lower_o_haystack - 1;
    }
    if (do_rhs) {
      rhs_loc_lower_o_needles = loc_group_upper_o_needles + 1;
      rhs_loc_lower_o_haystack = loc_group_upper_o_haystack + 1;
    }

    break;
  }
  case VCTRS_OPS_lt:
  case VCTRS_OPS_lte:
  case VCTRS_OPS_gt:
  case VCTRS_OPS_gte: {
    // Can't update haystack here, as nested containment groups make this impossible
    do_lhs = loc_group_lower_o_needles > loc_lower_o_needles;
    do_rhs = loc_group_upper_o_needles < loc_upper_o_needles;

    if (do_lhs) {
      lhs_loc_upper_o_needles = loc_group_lower_o_needles - 1;
    }
    if (do_rhs) {
      rhs_loc_lower_o_needles = loc_group_upper_o_needles + 1;
    }

    break;
  }
  }

  if (do_lhs) {
    df_matches_recurse(
      col,
      lhs_loc_lower_o_needles,
      lhs_loc_upper_o_needles,
      lhs_loc_lower_o_haystack,
      lhs_loc_upper_o_haystack,
      p_needles,
      p_haystack,
      p_needles_missings,
      p_haystack_missings,
      v_o_needles,
      v_o_haystack,
      missing_propagate,
      multiple,
      any_filters,
      v_filters,
      v_ops,
      p_locs_start_o_haystack,
      p_match_sizes,
      p_locs_needles,
      v_locs_filter_match_haystack,
      p_n_extra
    );
  }
  if (do_rhs) {
    df_matches_recurse(
      col,
      rhs_loc_lower_o_needles,
      rhs_loc_upper_o_needles,
      rhs_loc_lower_o_haystack,
      rhs_loc_upper_o_haystack,
      p_needles,
      p_haystack,
      p_needles_missings,
      p_haystack_missings,
      v_o_needles,
      v_o_haystack,
      missing_propagate,
      multiple,
      any_filters,
      v_filters,
      v_ops,
      p_locs_start_o_haystack,
      p_match_sizes,
      p_locs_needles,
      v_locs_filter_match_haystack,
      p_n_extra
    );
  }
}

// -----------------------------------------------------------------------------

static
void df_matches_with_nested_groups(r_ssize size_haystack,
                                   int n_nested_groups,
                                   const int* v_nested_groups,
                                   r_ssize col,
                                   r_ssize loc_lower_o_needles,
                                   r_ssize loc_upper_o_needles,
                                   const struct poly_df_data* p_needles,
                                   const struct poly_df_data* p_haystack,
                                   const struct poly_df_data* p_needles_missings,
                                   const struct poly_df_data* p_haystack_missings,
                                   const int* v_o_needles,
                                   const int* v_o_haystack,
                                   bool missing_propagate,
                                   enum vctrs_multiple multiple,
                                   bool any_filters,
                                   const enum vctrs_filter* v_filters,
                                   const enum vctrs_ops* v_ops,
                                   struct r_dyn_array* p_locs_start_o_haystack,
                                   struct r_dyn_array* p_match_sizes,
                                   struct r_dyn_array* p_locs_needles,
                                   int* v_locs_filter_match_haystack,
                                   r_ssize* p_n_extra) {
  const int* v_haystack = v_nested_groups;

  r_ssize loc_group_lower_o_haystack = 0;
  r_ssize loc_group_upper_o_haystack = size_haystack - 1;

  for (int i = 0; i < n_nested_groups; ++i) {
    const int val_needle = i;

    while (loc_group_lower_o_haystack <= loc_group_upper_o_haystack) {
      const r_ssize loc_group_mid_o_haystack = midpoint(loc_group_lower_o_haystack, loc_group_upper_o_haystack);
      const r_ssize loc_group_mid_haystack = v_o_haystack[loc_group_mid_o_haystack] - 1;
      const int val_haystack = v_haystack[loc_group_mid_haystack];

      const int cmp = int_compare_na_equal(val_needle, val_haystack);

      if (cmp == 1) {
        loc_group_lower_o_haystack = loc_group_mid_o_haystack + 1;
      } else if (cmp == -1) {
        loc_group_upper_o_haystack = loc_group_mid_o_haystack - 1;
      } else {
        // Hit!
        // Find lower and upper group bounds
        loc_group_lower_o_haystack = int_lower_duplicate(
          val_haystack,
          v_haystack,
          v_o_haystack,
          loc_group_lower_o_haystack,
          loc_group_mid_o_haystack
        );
        loc_group_upper_o_haystack = int_upper_duplicate(
          val_haystack,
          v_haystack,
          v_o_haystack,
          loc_group_mid_o_haystack,
          loc_group_upper_o_haystack
        );
        break;
      }
     }

    df_matches_recurse(
      col,
      loc_lower_o_needles,
      loc_upper_o_needles,
      loc_group_lower_o_haystack,
      loc_group_upper_o_haystack,
      p_needles,
      p_haystack,
      p_needles_missings,
      p_haystack_missings,
      v_o_needles,
      v_o_haystack,
      missing_propagate,
      multiple,
      any_filters,
      v_filters,
      v_ops,
      p_locs_start_o_haystack,
      p_match_sizes,
      p_locs_needles,
      v_locs_filter_match_haystack,
      p_n_extra
    );

    // Update bounds for next group
    loc_group_lower_o_haystack = loc_group_upper_o_haystack + 1;
    loc_group_upper_o_haystack = size_haystack - 1;
  }
}

// -----------------------------------------------------------------------------

// Find the largest contiguous location containing a missing value
static inline
r_ssize int_locate_upper_missing(const int* v_haystack_missings,
                                 const int* v_o_haystack,
                                 r_ssize loc_lower_o_haystack,
                                 r_ssize loc_upper_o_haystack) {
  while (loc_lower_o_haystack <= loc_upper_o_haystack) {
    const r_ssize loc_mid_o_haystack = midpoint(loc_lower_o_haystack, loc_upper_o_haystack);
    const r_ssize loc_mid_haystack = v_o_haystack[loc_mid_o_haystack] - 1;
    const bool haystack_is_missing = int_is_missing(v_haystack_missings[loc_mid_haystack]);

    if (haystack_is_missing) {
      loc_lower_o_haystack = loc_mid_o_haystack + 1;
    } else {
      loc_upper_o_haystack = loc_mid_o_haystack - 1;
    }
  }

  return loc_upper_o_haystack;
}

// -----------------------------------------------------------------------------

// Find the smallest contiguous location containing `needle`
static inline
r_ssize int_lower_duplicate(int needle,
                            const int* v_haystack,
                            const int* v_o_haystack,
                            r_ssize loc_lower_o_haystack,
                            r_ssize loc_upper_o_haystack) {
  while (loc_lower_o_haystack <= loc_upper_o_haystack) {
    const r_ssize loc_mid_o_haystack = midpoint(loc_lower_o_haystack, loc_upper_o_haystack);
    const r_ssize loc_mid_haystack = v_o_haystack[loc_mid_o_haystack] - 1;
    const int val_haystack = v_haystack[loc_mid_haystack];

    if (int_equal_na_equal(needle, val_haystack)) {
      loc_upper_o_haystack = loc_mid_o_haystack - 1;
    } else {
      loc_lower_o_haystack = loc_mid_o_haystack + 1;
    }
  }

  return loc_lower_o_haystack;
}

// -----------------------------------------------------------------------------

// Find the largest contiguous location containing `needle`
static inline
r_ssize int_upper_duplicate(int needle,
                            const int* v_haystack,
                            const int* v_o_haystack,
                            r_ssize loc_lower_o_haystack,
                            r_ssize loc_upper_o_haystack) {
  while (loc_lower_o_haystack <= loc_upper_o_haystack) {
    const r_ssize loc_mid_o_haystack = midpoint(loc_lower_o_haystack, loc_upper_o_haystack);
    const r_ssize loc_mid_haystack = v_o_haystack[loc_mid_o_haystack] - 1;
    const int elt_haystack = v_haystack[loc_mid_haystack];

    if (int_equal_na_equal(needle, elt_haystack)) {
      loc_lower_o_haystack = loc_mid_o_haystack + 1;
    } else {
      loc_upper_o_haystack = loc_mid_o_haystack - 1;
    }
  }

  return loc_upper_o_haystack;
}

// -----------------------------------------------------------------------------

static
r_obj* df_joint_ranks(r_obj* x,
                      r_obj* y,
                      r_ssize x_size,
                      r_ssize y_size,
                      r_ssize n_cols,
                      r_obj* ptype,
                      bool na_propagate,
                      bool nan_distinct,
                      r_obj* chr_transform) {
  r_obj* out = KEEP(r_alloc_list(2));

  x = r_clone(x);
  r_list_poke(out, 0, x);

  y = r_clone(y);
  r_list_poke(out, 1, y);

  r_obj* x_slicer = KEEP(compact_seq(0, x_size, true));
  r_obj* y_slicer = KEEP(compact_seq(x_size, y_size, true));

  r_obj* const* v_x = r_list_cbegin(x);
  r_obj* const* v_y = r_list_cbegin(y);
  r_obj* const* v_ptype = r_list_cbegin(ptype);

  r_obj* zap = KEEP(r_alloc_list(0));
  r_poke_class(zap, r_chr("rlang_zap"));

  r_obj* c_args = KEEP(r_alloc_list(2));

  for (r_ssize i = 0; i < n_cols; ++i) {
    r_obj* x_elt = v_x[i];
    r_obj* y_elt = v_y[i];
    r_obj* ptype_elt = v_ptype[i];

    r_list_poke(c_args, 0, x_elt);
    r_list_poke(c_args, 1, y_elt);

    // Combine columns
    r_obj* both = KEEP(vec_c(c_args, ptype_elt, zap, p_no_repair_silent_ops));

    // Compute joint rank
    r_obj* rank = KEEP(vec_rank(
      both,
      TIES_dense,
      na_propagate,
      chrs_asc,
      chrs_smallest,
      nan_distinct,
      chr_transform
    ));

    // Separate and store back in x and y
    r_list_poke(x, i, vec_slice_impl(rank, x_slicer));
    r_list_poke(y, i, vec_slice_impl(rank, y_slicer));

    FREE(2);
  }

  FREE(5);
  return out;
}

// -----------------------------------------------------------------------------

static
r_obj* df_missings_by_col(r_obj* x, r_ssize x_size, r_ssize n_cols) {
  r_obj* out = KEEP(r_alloc_list(n_cols));
  r_poke_names(out, r_names(x));
  r_init_data_frame(out, x_size);

  r_obj* const* v_x = r_list_cbegin(x);

  for (r_ssize i = 0; i < n_cols; ++i) {
    r_obj* col = v_x[i];

    // Use completeness to match `vec_rank()` and `vec_match()`
    r_obj* complete = vec_detect_complete(col);
    r_list_poke(out, i, complete);
    int* v_complete = r_lgl_begin(complete);

    // Flip any `FALSE` to `NA_integer_` to align with propagated integer NAs in
    // `x` ranks when `missing_propagate = true`.
    // Relies on the fact that logical and integer are the same type internally.
    for (r_ssize j = 0; j < x_size; ++j) {
      if (!v_complete[j]) {
        v_complete[j] = r_globals.na_int;
      }
    }
  }

  FREE(1);
  return out;
}

// -----------------------------------------------------------------------------

static inline
enum vctrs_ops parse_condition_one(const char* condition) {
  if (!strcmp(condition, "==")) { return VCTRS_OPS_eq; }
  if (!strcmp(condition, ">"))  { return VCTRS_OPS_gt; }
  if (!strcmp(condition, ">=")) { return VCTRS_OPS_gte; }
  if (!strcmp(condition, "<"))  { return VCTRS_OPS_lt; }
  if (!strcmp(condition, "<=")) { return VCTRS_OPS_lte; }

  r_abort("`condition` must only contain \"==\", \">\", \">=\", \"<\", or \"<=\".");
}

static inline
void parse_condition(r_obj* condition, enum vctrs_ops* v_ops, r_ssize n_cols) {
  if (r_typeof(condition) != R_TYPE_character) {
    r_abort("`condition` must be a character vector, or `NULL`.");
  }

  r_obj* const* v_condition = r_chr_cbegin(condition);
  r_ssize size_condition = vec_size(condition);

  if (size_condition == 1) {
    const char* elt = CHAR(v_condition[0]);
    enum vctrs_ops op = parse_condition_one(elt);
    for (r_ssize i = 0; i < n_cols; ++i) {
      v_ops[i] = op;
    }
  } else if (size_condition == n_cols) {
    for (r_ssize i = 0; i < n_cols; ++i) {
      const char* elt = CHAR(v_condition[i]);
      v_ops[i] = parse_condition_one(elt);
    }
  } else {
    r_abort(
      "If `condition` is a character vector, it must be length 1, or the same "
      "length as the number of columns of the input."
    );
  }
}

// -----------------------------------------------------------------------------

static inline
enum vctrs_missing_needle parse_missing(r_obj* missing) {
  if (!r_is_string(missing)) {
    r_abort("`missing` must be a string.");
  }

  const char* c_missing = r_chr_get_c_string(missing, 0);

  if (!strcmp(c_missing, "match")) return VCTRS_MISSING_NEEDLE_match;
  if (!strcmp(c_missing, "propagate")) return VCTRS_MISSING_NEEDLE_propagate;
  if (!strcmp(c_missing, "drop")) return VCTRS_MISSING_NEEDLE_drop;
  if (!strcmp(c_missing, "error")) return VCTRS_MISSING_NEEDLE_error;

  r_abort("`missing` must be one of \"match\", \"propagate\", \"drop\", or \"error\".");
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

static inline
enum vctrs_check_duplicates parse_check_duplicates(r_obj* check_duplicates) {
  if (!r_is_string(check_duplicates)) {
    r_abort("`check_duplicates` must be a string.");
  }

  const char* c_check_duplicates = r_chr_get_c_string(check_duplicates, 0);

  if (!strcmp(c_check_duplicates, "neither")) return VCTRS_CHECK_DUPLICATES_neither;
  if (!strcmp(c_check_duplicates, "needles")) return VCTRS_CHECK_DUPLICATES_needles;
  if (!strcmp(c_check_duplicates, "haystack")) return VCTRS_CHECK_DUPLICATES_haystack;
  if (!strcmp(c_check_duplicates, "both")) return VCTRS_CHECK_DUPLICATES_both;

  r_abort("`check_duplicates` must be one of \"neither\", \"needles\", \"haystack\", or \"both\".");
}

static inline
r_ssize find_first_duplicate(const int* v_group_sizes, r_ssize size) {
  r_ssize group_start = 0;

  for (r_ssize i = 0; i < size; ++i) {
    int group_size = v_group_sizes[i];

    if (group_size > 1) {
      // The group start isn't the duplicate, 1 location past it is
      return group_start + 1;
    }

    group_start += group_size;
  }

  r_stop_internal("find_first_duplicate", "Duplicate value should have been found.");
}

static inline
void check_for_duplicates(r_obj* info, struct vctrs_arg* arg, bool needles) {
  r_ssize max_group_size = r_as_int(r_list_get(info, 2));

  if (max_group_size == 0) {
    r_stop_internal("check_unique", "Maximum group size should never be 0.");
  }

  if (max_group_size == 1) {
    // No duplicates
    return;
  }

  r_obj* o = r_list_get(info, 0);
  const int* v_o = r_int_cbegin(o);

  r_obj* group_sizes = r_list_get(info, 1);
  const int* v_group_sizes = r_int_cbegin(group_sizes);

  r_ssize size = r_length(o);

  r_ssize loc_duplicate_o = find_first_duplicate(v_group_sizes, size);
  r_ssize loc_duplicate = v_o[loc_duplicate_o];

  stop_matches_duplicates(loc_duplicate - 1, arg, needles);
}

// -----------------------------------------------------------------------------

static inline
enum vctrs_filter parse_filter_one(const char* filter, bool* p_any_filters) {
  if (!strcmp(filter, "none")) {
    return VCTRS_FILTER_none;
  }
  if (!strcmp(filter, "min")) {
    *p_any_filters = true;
    return VCTRS_FILTER_min;
  }
  if (!strcmp(filter, "max")) {
    *p_any_filters = true;
    return VCTRS_FILTER_max;
  }

  r_abort("`filter` must be one of \"none\", \"min\", or \"max\".");
}

static inline
void parse_filter(r_obj* filter,
                  r_ssize n_cols,
                  enum vctrs_filter* v_filters,
                  bool* p_any_filters) {
  if (r_typeof(filter) != R_TYPE_character) {
    r_abort("`filter` must be a character vector.");
  }

  r_obj* const* v_filter = r_chr_cbegin(filter);
  r_ssize size_filter = vec_size(filter);

  if (size_filter == 1) {
    const char* elt = CHAR(v_filter[0]);
    enum vctrs_filter elt_filter = parse_filter_one(elt, p_any_filters);

    for (r_ssize i = 0; i < n_cols; ++i) {
      v_filters[i] = elt_filter;
    }

    return;
  }

  if (size_filter == n_cols) {
    for (r_ssize i = 0; i < n_cols; ++i) {
      const char* elt = CHAR(v_filter[i]);
      v_filters[i] = parse_filter_one(elt, p_any_filters);
    }

    return;
  }

  r_abort(
    "`filter` must be length 1, or the same "
    "length as the number of columns of the input."
  );
}

// -----------------------------------------------------------------------------

static inline
struct vctrs_no_match parse_no_match(r_obj* no_match, const char* arg) {
  if (r_is_string(no_match)) {
    const char* c_no_match = r_chr_get_c_string(no_match, 0);

    if (!strcmp(c_no_match, "error")) {
      return (struct vctrs_no_match) {
        .error = true,
        .drop = false,
        .value = SIGNAL_NO_MATCH
      };
    }

    if (!strcmp(c_no_match, "drop")) {
      return (struct vctrs_no_match) {
        .error = false,
        .drop = true,
        .value = SIGNAL_NO_MATCH
      };
    }
  }

  if (r_typeof(no_match) == R_TYPE_integer && r_length(no_match) == 1) {
    int c_no_match = r_int_get(no_match, 0);

    return (struct vctrs_no_match) {
      .error = false,
      .drop = false,
      .value = c_no_match
    };
  }

  r_abort("`%s` must be a length 1 integer, \"drop\", or \"error\".", arg);
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

static inline
r_obj* new_vec_matches_result(r_ssize size) {
  r_obj* names = KEEP(r_chr_n(v_matches_df_names_c_strings, MATCHES_DF_SIZE));

  r_obj* out = KEEP(r_alloc_df_list(
    size,
    names,
    v_matches_df_types,
    MATCHES_DF_SIZE
  ));
  r_init_data_frame(out, size);

  FREE(2);
  return out;
}

// -----------------------------------------------------------------------------

static
r_obj* expand_match_on_nothing(r_ssize size_needles,
                               r_ssize size_haystack,
                               enum vctrs_multiple multiple,
                               const struct vctrs_no_match* no_match,
                               const struct vctrs_no_match* remaining,
                               struct vctrs_arg* needles_arg,
                               struct vctrs_arg* haystack_arg) {
  if (size_haystack == 0) {
    // Handle empty `haystack` up front
    // `no_match` everywhere, retaining size of `needles`

    if (no_match->error && size_needles > 0) {
      stop_matches_nothing(0, needles_arg, haystack_arg);
    }

    // If `no_match = "drop"`, since everything is a no-match there are
    // no results
    const r_ssize size_out = no_match->drop ? 0 : size_needles;

    r_obj* out = KEEP(new_vec_matches_result(size_out));
    int* v_out_needles = r_int_begin(r_list_get(out, MATCHES_DF_LOCS_needles));
    int* v_out_haystack = r_int_begin(r_list_get(out, MATCHES_DF_LOCS_haystack));
    r_ssize loc_out = 0;

    const int loc_haystack = no_match->value;

    for (r_ssize i = 0; i < size_out; ++i) {
      v_out_needles[loc_out] = i + 1;
      v_out_haystack[loc_out] = loc_haystack;
      ++loc_out;
    }

    FREE(1);
    return out;
  }

  if (size_needles == 0 && !remaining->drop) {
    // Handle empty `needles` up front
    // All elements of `haystack` are "remaining"

    if (remaining->error) {
      stop_matches_remaining(0, needles_arg, haystack_arg);
    }

    const r_ssize size_out = size_haystack;

    r_obj* out = KEEP(new_vec_matches_result(size_out));
    int* v_out_needles = r_int_begin(r_list_get(out, MATCHES_DF_LOCS_needles));
    int* v_out_haystack = r_int_begin(r_list_get(out, MATCHES_DF_LOCS_haystack));
    r_ssize loc_out = 0;

    const int loc_needles = remaining->value;

    for (r_ssize i = 0; i < size_out; ++i) {
      v_out_needles[loc_out] = loc_needles;
      v_out_haystack[loc_out] = i + 1;
      ++loc_out;
    }

    FREE(1);
    return out;
  }

  if (multiple == VCTRS_MULTIPLE_first || multiple == VCTRS_MULTIPLE_last) {
    // Handle first/last cases next
    r_obj* out = KEEP(new_vec_matches_result(size_needles));
    int* v_out_needles = r_int_begin(r_list_get(out, MATCHES_DF_LOCS_needles));
    int* v_out_haystack = r_int_begin(r_list_get(out, MATCHES_DF_LOCS_haystack));
    r_ssize loc_out = 0;

    const int loc_haystack = (multiple == VCTRS_MULTIPLE_first) ? 1 : size_haystack;

    for (r_ssize i = 0; i < size_needles; ++i) {
      v_out_needles[loc_out] = i + 1;
      v_out_haystack[loc_out] = loc_haystack;
      ++loc_out;
    }

    FREE(1);
    return out;
  }

  if (size_haystack > 1 && size_needles > 0) {
    if (multiple == VCTRS_MULTIPLE_error) {
      stop_matches_multiple(0, needles_arg, haystack_arg);
    } else if (multiple == VCTRS_MULTIPLE_warning) {
      warn_matches_multiple(0, needles_arg, haystack_arg);
    }
  }

  r_ssize size_out = r_ssize_mult(size_needles, size_haystack);

  if (size_out > R_LEN_T_MAX) {
    stop_matches_overflow((double) size_out);
  }

  r_obj* out = KEEP(new_vec_matches_result(size_out));
  int* v_out_needles = r_int_begin(r_list_get(out, MATCHES_DF_LOCS_needles));
  int* v_out_haystack = r_int_begin(r_list_get(out, MATCHES_DF_LOCS_haystack));
  r_ssize loc_out = 0;

  for (r_ssize i = 0; i < size_needles; ++i) {
    for (r_ssize j = 0; j < size_haystack; ++j) {
      v_out_needles[loc_out] = i + 1;
      v_out_haystack[loc_out] = j + 1;
      ++loc_out;
    }
  }

  FREE(1);
  return out;
}

// -----------------------------------------------------------------------------

static
r_obj* expand_compact_indices(const int* v_o_haystack,
                              struct r_dyn_array* p_locs_start_o_haystack,
                              struct r_dyn_array* p_match_sizes,
                              struct r_dyn_array* p_locs_needles,
                              bool skip_match_sizes,
                              bool skip_locs_needles,
                              enum vctrs_missing_needle missing,
                              bool missing_propagate,
                              const struct vctrs_no_match* no_match,
                              const struct vctrs_no_match* remaining,
                              enum vctrs_multiple multiple,
                              r_ssize size_needles,
                              r_ssize size_haystack,
                              bool any_directional,
                              bool has_locs_filter_match_haystack,
                              const enum vctrs_filter* v_filters,
                              const int* v_locs_filter_match_haystack,
                              const struct poly_df_data* p_haystack,
                              struct vctrs_arg* needles_arg,
                              struct vctrs_arg* haystack_arg) {
  int n_prot = 0;

  const r_ssize n_used = p_locs_start_o_haystack->count;

  const int* v_locs_start_o_haystack = (const int*) r_arr_cbegin(p_locs_start_o_haystack);
  const int* v_match_sizes = skip_match_sizes ? NULL : (const int*) r_arr_cbegin(p_match_sizes);
  const int* v_locs_needles = skip_locs_needles ? NULL : (const int*) r_arr_cbegin(p_locs_needles);

  r_ssize size_out = 0;
  if (skip_match_sizes) {
    size_out = n_used;
  } else {
    double dbl_size_out = 0;

    for (r_ssize i = 0; i < n_used; ++i) {
      // This could get extremely large with improperly specified non-equi joins.
      // May over-allocate in the case of `filters` with `multiple = "all"`,
      // or when `no_match = "drop"` or `missing = "drop"`.
      dbl_size_out += (double) v_match_sizes[i];
    }

    if (dbl_size_out > R_LEN_T_MAX) {
      // TODO: Update this after a switch to long vector support
      r_abort(
        "Match procedure results in an allocation larger than 2^31-1 elements. "
        "Attempted allocation size was %.0lf. "
        "Please report this to the vctrs maintainers at "
        "<https://github.com/r-lib/vctrs/issues>.",
        dbl_size_out
      );
    }

    size_out = (r_ssize) dbl_size_out;
  }

  r_obj* out = KEEP_N(new_vec_matches_result(size_out), &n_prot);

  r_obj* out_needles = r_list_get(out, MATCHES_DF_LOCS_needles);
  r_obj* out_haystack = r_list_get(out, MATCHES_DF_LOCS_haystack);

  int* v_out_needles = r_int_begin(out_needles);
  int* v_out_haystack = r_int_begin(out_haystack);

  const int* v_o_locs_needles = NULL;
  if (!skip_locs_needles) {
    r_obj* locs_needles = KEEP_N(r_arr_unwrap(p_locs_needles), &n_prot);
    r_obj* o_locs_needles = KEEP_N(vec_order(locs_needles, chrs_asc, chrs_smallest, true, r_null), &n_prot);
    v_o_locs_needles = r_int_cbegin(o_locs_needles);
  }

  const bool has_no_match_haystack = !remaining->drop;
  int* v_detect_no_match_haystack = NULL;
  if (has_no_match_haystack) {
    r_obj* detect_no_match_haystack = KEEP_N(r_alloc_integer(size_haystack), &n_prot);
    v_detect_no_match_haystack = r_int_begin(detect_no_match_haystack);

    for (r_ssize i = 0; i < size_haystack; ++i) {
      // Initialize to no-match
      v_detect_no_match_haystack[i] = 1;
    }
  }

  r_ssize loc_out = 0;

  bool any_multiple = false;
  bool maybe_multiple =
    multiple == VCTRS_MULTIPLE_all ||
    multiple == VCTRS_MULTIPLE_error ||
    multiple == VCTRS_MULTIPLE_warning;

  for (r_ssize i = 0; i < n_used; ++i) {
    const int loc = skip_locs_needles ? i : v_o_locs_needles[i] - 1;

    int loc_start_o_haystack = v_locs_start_o_haystack[loc];
    const int match_size = skip_match_sizes ? 1 : v_match_sizes[loc];
    const int loc_needles = skip_locs_needles ? loc + 1 : v_locs_needles[loc];

    if (missing_propagate && loc_start_o_haystack == SIGNAL_NA_PROPAGATE) {
      if (match_size != 1) {
        r_stop_internal(
          "expand_compact_indices",
          "`match_size` should always be 1 in the case of NA propagation."
        );
      }

      switch (missing) {
      case VCTRS_MISSING_NEEDLE_propagate: {
        v_out_needles[loc_out] = loc_needles;
        v_out_haystack[loc_out] = r_globals.na_int;
        ++loc_out;
        break;
      }
      case VCTRS_MISSING_NEEDLE_drop: {
        // Do not increment `loc_out`, do not store locations
        break;
      }
      case VCTRS_MISSING_NEEDLE_error: {
        stop_matches_missing(loc_needles - 1, needles_arg);
      }
      case VCTRS_MISSING_NEEDLE_match: {
        r_stop_internal(
          "expand_compact_indices",
          "Needles should never be marked as `SIGNAL_NA_PROPAGATE` when `missing = 'match'."
        );
      }
      }

      continue;
    }

    if (loc_start_o_haystack == SIGNAL_NO_MATCH) {
      if (match_size != 1) {
        r_stop_internal(
          "expand_compact_indices",
          "`match_size` should always be 1 in the case of no matches."
        );
      }

      if (no_match->error) {
        stop_matches_nothing(i, needles_arg, haystack_arg);
      } else if (no_match->drop) {
        continue;
      }

      v_out_needles[loc_out] = loc_needles;
      v_out_haystack[loc_out] = no_match->value;
      ++loc_out;
      continue;
    }

    if (has_locs_filter_match_haystack) {
      const int loc_start_haystack = v_o_haystack[loc_start_o_haystack - 1] - 1;
      const int loc_filter_match_haystack = v_locs_filter_match_haystack[loc_needles - 1];

      const bool equal = p_matches_df_equal_na_equal(
        p_haystack,
        loc_start_haystack,
        p_haystack,
        loc_filter_match_haystack,
        v_filters
      );

      if (!equal) {
        // Potential haystack match doesn't aligned with known filtered match.
        // This can happen if the potential match came from a different nested group.
        continue;
      }
    }

    if (maybe_multiple && !any_multiple) {
      if (i < size_needles - 1) {
        any_multiple = match_size > 1;
      } else {
        // Guaranteed second match if in the "extra" matches section
        any_multiple = true;
      }

      if (any_multiple) {
        if (multiple == VCTRS_MULTIPLE_error) {
          stop_matches_multiple(loc_needles - 1, needles_arg, haystack_arg);
        } else if (multiple == VCTRS_MULTIPLE_warning) {
          warn_matches_multiple(loc_needles - 1, needles_arg, haystack_arg);
        }
      }
    }

    for (r_ssize j = 0; j < match_size; ++j) {
      const int loc_haystack = v_o_haystack[loc_start_o_haystack - 1];

      v_out_needles[loc_out] = loc_needles;
      v_out_haystack[loc_out] = loc_haystack;

      if (has_no_match_haystack) {
        v_detect_no_match_haystack[loc_haystack - 1] = 0;
      }

      ++loc_out;
      ++loc_start_o_haystack;
    }
  }

  if (loc_out < size_out) {
    // Can happen with a `filter` and `multiple = "all"`, where it is possible
    // for potential matches coming from a different nested containment group
    // to be filtered out in the above loop.
    // Can also happen with `no_match = "drop"` or `missing = "drop"`.
    size_out = loc_out;
    r_init_data_frame(out, size_out);
    out_needles = r_int_resize(out_needles, size_out);
    r_list_poke(out, MATCHES_DF_LOCS_needles, out_needles);
    out_haystack = r_int_resize(out_haystack, size_out);
    r_list_poke(out, MATCHES_DF_LOCS_haystack, out_haystack);
    v_out_needles = r_int_begin(out_needles);
    v_out_haystack = r_int_begin(out_haystack);
  }

  if (any_multiple && any_directional) {
    // If we had multiple matches and we were doing a non-equi join, then
    // the needles column will be correct, but any group of multiple matches in
    // the haystack column will be ordered incorrectly within the needle group.
    // They will be ordered using the order of the original haystack values,
    // rather than by first appearance. Reordering the entire output data frame
    // orders them correctly, as within each needle group it will put the
    // haystack locations in ascending order (i.e. by first appearance).
    // This is expensive! `out` could have a huge number of matches.
    r_obj* o_haystack_appearance = KEEP(vec_order(out, chrs_asc, chrs_smallest, true, r_null));
    const int* v_o_haystack_appearance = r_int_cbegin(o_haystack_appearance);

    r_obj* out_haystack2 = KEEP(r_alloc_integer(size_out));
    int* v_out_haystack2 = r_int_begin(out_haystack2);

    for (r_ssize i = 0; i < size_out; ++i) {
      v_out_haystack2[i] = v_out_haystack[v_o_haystack_appearance[i] - 1];
    }

    r_list_poke(out, 1, out_haystack2);
    FREE(2);
  }

  if (has_no_match_haystack) {
    r_ssize n_no_match_haystack = 0;

    for (r_ssize i = 0; i < size_haystack; ++i) {
      if (v_detect_no_match_haystack[i]) {
        if (remaining->error) {
          stop_matches_remaining(i, needles_arg, haystack_arg);
        }

        // Overwrite with location, this moves all no-match locs up to the front
        v_detect_no_match_haystack[n_no_match_haystack] = i + 1;
        ++n_no_match_haystack;
      }
    }

    if (n_no_match_haystack > 0) {
      // Resize to have enough room for haystack no-matches at the end
      r_ssize new_size_out = r_ssize_add(size_out, n_no_match_haystack);
      r_init_data_frame(out, new_size_out);
      out_needles = r_int_resize(out_needles, new_size_out);
      r_list_poke(out, MATCHES_DF_LOCS_needles, out_needles);
      out_haystack = r_int_resize(out_haystack, new_size_out);
      r_list_poke(out, MATCHES_DF_LOCS_haystack, out_haystack);
      v_out_needles = r_int_begin(out_needles);
      v_out_haystack = r_int_begin(out_haystack);

      for (r_ssize i = size_out, j = 0; i < new_size_out; ++i, ++j) {
        v_out_needles[i] = remaining->value;
        v_out_haystack[i] = v_detect_no_match_haystack[j];
      }

      size_out = new_size_out;
    }
  }

  FREE(n_prot);
  return out;
}

// -----------------------------------------------------------------------------

// [[ register() ]]
r_obj* vctrs_test_compute_nested_containment_info(r_obj* haystack,
                                                  r_obj* condition,
                                                  r_obj* multiple,
                                                  r_obj* check_duplicates) {
  r_ssize n_cols = r_length(haystack);
  enum vctrs_ops* v_ops = (enum vctrs_ops*) R_alloc(n_cols, sizeof(enum vctrs_ops));
  parse_condition(condition, v_ops, n_cols);
  enum vctrs_multiple c_multiple = parse_multiple(multiple);
  enum vctrs_check_duplicates c_check_duplicates = parse_check_duplicates(check_duplicates);
  struct vctrs_arg haystack_arg = new_wrapper_arg(NULL, "haystack");
  return compute_nested_containment_info(haystack, c_multiple, c_check_duplicates, v_ops, &haystack_arg);
}

static
r_obj* compute_nested_containment_info(r_obj* haystack,
                                       enum vctrs_multiple multiple,
                                       enum vctrs_check_duplicates check_duplicates,
                                       const enum vctrs_ops* v_ops,
                                       struct vctrs_arg* haystack_arg) {
  r_ssize n_prot = 0;

  r_ssize n_cols = r_length(haystack);
  r_ssize size_haystack = vec_size(haystack);

  // Haystack order, nested groups, number of nested groups, and any directional
  r_obj* out = KEEP_N(r_alloc_list(4), &n_prot);

  // Are there any directional ops (>, >=, <, <=)? And where is the first?
  bool any_directional = false;
  int first_directional = 0;
  for (r_ssize i = 0; i < n_cols; ++i) {
    enum vctrs_ops op = v_ops[i];
    if (op == VCTRS_OPS_eq) {
      continue;
    }
    any_directional = true;
    first_directional = i;
    break;
  }

  if (!any_directional) {
    // Nested group info isn't required for only `==`, but we still have to
    // check for potential duplicates in `haystack`.
    r_obj* o_haystack;
    if (check_duplicates == VCTRS_CHECK_DUPLICATES_both || check_duplicates == VCTRS_CHECK_DUPLICATES_haystack) {
      r_obj* info = KEEP_N(vec_order_info(haystack, chrs_asc, chrs_smallest, true, r_null, true), &n_prot);
      check_for_duplicates(info, haystack_arg, false);
      o_haystack = r_list_get(info, 0);
    } else {
      o_haystack = KEEP_N(vec_order(haystack, chrs_asc, chrs_smallest, true, r_null), &n_prot);
    }

    r_list_poke(out, 0, o_haystack);
    r_list_poke(out, 1, vctrs_shared_empty_int);
    r_list_poke(out, 2, r_int(1));
    r_list_poke(out, 3, r_lgl(any_directional));
    FREE(n_prot);
    return out;
  }

  r_obj* info = KEEP_N(vec_order_info(haystack, chrs_asc, chrs_smallest, true, r_null, true), &n_prot);

  if (check_duplicates == VCTRS_CHECK_DUPLICATES_both || check_duplicates == VCTRS_CHECK_DUPLICATES_haystack) {
    check_for_duplicates(info, haystack_arg, false);
  }

  r_obj* o_haystack = r_list_get(info, 0);
  r_obj* group_sizes_haystack = r_list_get(info, 1);

  r_keep_t haystack_inner_pi;
  r_obj* haystack_inner = haystack;
  KEEP_HERE(haystack_inner, &haystack_inner_pi);
  ++n_prot;

  r_keep_t outer_run_sizes_pi;
  r_obj* outer_run_sizes = vctrs_shared_empty_int;
  KEEP_HERE(outer_run_sizes, &outer_run_sizes_pi);
  ++n_prot;

  if (first_directional != 0) {
    // We have equality comparisons before the first directional comparison.
    // In this case, we can skip nested containment ordering for the equality
    // comparisons before the first directional comparison if we pass on the
    // group sizes of the ordered equality columns.
    r_obj* const* v_haystack = r_list_cbegin(haystack);
    r_obj* const* v_haystack_names = r_chr_cbegin(r_names(haystack));

    // "Outer" data frame columns
    r_obj* haystack_outer = KEEP_N(r_alloc_list(first_directional), &n_prot);
    r_obj* haystack_outer_names = r_alloc_character(first_directional);
    r_poke_names(haystack_outer, haystack_outer_names);
    r_init_data_frame(haystack_outer, size_haystack);
    for (r_ssize i = 0; i < first_directional; ++i) {
      r_list_poke(haystack_outer, i, v_haystack[i]);
      r_chr_poke(haystack_outer_names, i, v_haystack_names[i]);
    }

    // "Inner" data frame columns
    haystack_inner = r_alloc_list(n_cols - first_directional);
    KEEP_AT(haystack_inner, haystack_inner_pi);
    r_obj* haystack_inner_names = r_alloc_character(n_cols - first_directional);
    r_poke_names(haystack_inner, haystack_inner_names);
    r_init_data_frame(haystack_inner, size_haystack);
    for (r_ssize i = first_directional, j = 0; i < n_cols; ++i, ++j) {
      r_list_poke(haystack_inner, j, v_haystack[i]);
      r_chr_poke(haystack_inner_names, j, v_haystack_names[i]);
    }

    r_obj* info = vec_order_info(
      haystack_outer,
      chrs_asc,
      chrs_smallest,
      true,
      r_null,
      true
    );
    outer_run_sizes = r_list_get(info, 1);
    KEEP_AT(outer_run_sizes, outer_run_sizes_pi);
  }

  r_obj* nested_info = KEEP_N(nested_containment_order(
    haystack_inner,
    o_haystack,
    group_sizes_haystack,
    outer_run_sizes,
    multiple
  ), &n_prot);

  int n_nested_groups = r_as_int(r_list_get(nested_info, 1));

  if (n_nested_groups == 1) {
    // If only a single nested group exists, we hit the somewhat rare case of
    // having a >1 col data frame that is already in nested containment order.
    // In that case, original haystack ordering is sufficient.
    r_list_poke(out, 0, o_haystack);
    r_list_poke(out, 1, vctrs_shared_empty_int);
    r_list_poke(out, 2, r_int(1));
    r_list_poke(out, 3, r_lgl(any_directional));
    FREE(n_prot);
    return out;
  }

  // Otherwise, we have to recompute haystack ordering with `nested_groups` as
  // the first column.
  r_obj* nested_groups = r_list_get(nested_info, 0);

  r_obj* const* v_haystack = r_list_cbegin(haystack);
  r_obj* const* v_haystack_names = r_chr_cbegin(r_names(haystack));

  r_obj* haystack_with_nesting = KEEP_N(r_alloc_list(n_cols + 1), &n_prot);
  r_obj* haystack_with_nesting_names = r_alloc_character(n_cols + 1);
  r_poke_names(haystack_with_nesting, haystack_with_nesting_names);
  r_init_data_frame(haystack_with_nesting, size_haystack);

  r_list_poke(haystack_with_nesting, 0, nested_groups);
  r_chr_poke(haystack_with_nesting_names, 0, r_str("..nested_groups.."));

  for (r_ssize i = 0; i < n_cols; ++i) {
    r_list_poke(haystack_with_nesting, i + 1, v_haystack[i]);
    r_chr_poke(haystack_with_nesting_names, i + 1, v_haystack_names[i]);
  }

  o_haystack = KEEP_N(vec_order(
    haystack_with_nesting,
    chrs_asc,
    chrs_smallest,
    true,
    r_null
  ), &n_prot);

  r_list_poke(out, 0, o_haystack);
  r_list_poke(out, 1, nested_groups);
  r_list_poke(out, 2, r_int(n_nested_groups));
  r_list_poke(out, 3, r_lgl(any_directional));

  FREE(n_prot);
  return out;
}


// -----------------------------------------------------------------------------

static
r_obj* nested_containment_order(r_obj* x,
                                r_obj* order,
                                r_obj* group_sizes,
                                r_obj* outer_run_sizes,
                                enum vctrs_multiple multiple) {
  if (!is_data_frame(x)) {
    r_stop_internal("nested_containment_order", "`x` must be a data frame.");
  }

  int n_prot = 0;

  r_ssize n_cols = r_length(x);
  r_ssize size = r_length(order);

  // For first/last, we require not only increasing order for each
  // column, but also increasing row order as well. This can generate more
  // groups, but ensures that the assignment loop of `df_matches_recurse()`
  // works correctly for these cases.
  const bool enforce_row_order =
    multiple == VCTRS_MULTIPLE_first ||
    multiple == VCTRS_MULTIPLE_last;

  r_obj* out = KEEP_N(r_alloc_list(2), &n_prot);

  r_obj* ids = r_alloc_integer(size);
  r_list_poke(out, 0, ids);
  int* v_ids = r_int_begin(ids);

  r_obj* n_ids = r_alloc_integer(1);
  r_list_poke(out, 1, n_ids);
  int* p_n_ids = r_int_begin(n_ids);
  *p_n_ids = 1;

  for (r_ssize i = 0; i < size; ++i) {
    v_ids[i] = 0;
  }

  if (size == 0) {
    // Algorithm requires at least 1 row
    FREE(n_prot);
    return out;
  }
  if (n_cols == 1 && !enforce_row_order) {
    // If there is only 1 column, `x` is in increasing order already when
    // ordered by `order`. If we don't require that the actual row numbers also
    // be in order, then we are done.
    // If `outer_run_sizes` were supplied, each individual run group will
    // be in increasing order (since the single `x` column is the one that
    // broke any ties), and that is all that is required.
    FREE(n_prot);
    return out;
  }

  const int* v_order = r_int_cbegin(order);
  const int* v_group_sizes = r_int_cbegin(group_sizes);
  const int* v_outer_run_sizes = r_int_cbegin(outer_run_sizes);

  r_ssize n_groups = r_length(group_sizes);

  struct r_dyn_array* p_prev_rows = r_new_dyn_vector(R_TYPE_integer, 10000);
  KEEP_N(p_prev_rows->shelter, &n_prot);

  struct poly_vec* p_poly_vec = new_poly_vec(x, vctrs_type_dataframe);
  PROTECT_POLY_VEC(p_poly_vec, &n_prot);
  const void* v_x = p_poly_vec->p_vec;

  int i_order_group_start = 0;
  int group_size = v_group_sizes[0];
  int i_order = i_order_group_start + ((multiple == VCTRS_MULTIPLE_last) ? group_size - 1 : 0);

  const int first_row = v_order[i_order] - 1;
  p_prev_rows->count = 1;
  R_ARR_POKE(int, p_prev_rows, 0, first_row);

  r_ssize next_outer_run_start = 0;
  r_ssize outer_run_sizes_loc = 0;
  if (r_length(outer_run_sizes) > 0) {
    // If no outer runs exist, `integer(0)` is used and `next_outer_run_start = 0`
    // will never match `i_order_group_start` since the first group size is at least 1.
    next_outer_run_start = v_outer_run_sizes[outer_run_sizes_loc];
    ++outer_run_sizes_loc;
  }

  for (r_ssize i_group = 1; i_group < n_groups; ++i_group) {
    // Catch group start index up to current group using previous group size
    i_order_group_start += group_size;
    // Update to size of current group
    group_size = v_group_sizes[i_group];
    // Update index into order to point to either start / end of this group
    i_order = i_order_group_start + ((multiple == VCTRS_MULTIPLE_last) ? group_size - 1 : 0);

    int cur_row = v_order[i_order] - 1;

    bool new_id = true;
    int prev_row_id = 0;
    int max_prev_row_id = p_prev_rows->count;

    for (; prev_row_id < max_prev_row_id; ++prev_row_id) {
      int prev_row = R_ARR_GET(int, p_prev_rows, prev_row_id);

      if (enforce_row_order && cur_row < prev_row) {
        // Can't add to current group, `multiple = "first"/"last"`
        // require increasing row indices per group
        continue;
      }

      if (p_df_nested_containment_compare_ge_na_equal(v_x, cur_row, v_x, prev_row)) {
        new_id = false;
        break;
      }
    }

    int id;
    if (next_outer_run_start == i_order_group_start) {
      // Start of a new outer run
      id = 0;
      next_outer_run_start += v_outer_run_sizes[outer_run_sizes_loc];
      ++outer_run_sizes_loc;
      p_prev_rows->count = 1;
      R_ARR_POKE(int, p_prev_rows, 0, cur_row);
    } else if (new_id) {
      // Completely new id for this outer run, which we add to the end
      id = max_prev_row_id;
      r_arr_push_back(p_prev_rows, &cur_row);

      if (p_prev_rows->count > *p_n_ids) {
        *p_n_ids = p_prev_rows->count;
      }
    } else {
      // Update existing row location to the current row, since it is larger
      id = prev_row_id;
      R_ARR_POKE(int, p_prev_rows, prev_row_id, cur_row);
    }

    for (int i = 0; i < group_size; ++i) {
      v_ids[v_order[i_order_group_start + i] - 1] = id;
    }
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
  // All columns are integer vectors (ranks).

  struct poly_df_data* x_data = (struct poly_df_data*) x;
  struct poly_df_data* y_data = (struct poly_df_data*) y;

  r_ssize n_col = x_data->n_col;

  const void** x_ptrs = x_data->col_ptrs;
  const void** y_ptrs = y_data->col_ptrs;

  for (r_ssize col = n_col - 1; col > 0; --col) {
    if (p_int_compare_na_equal(x_ptrs[col], i, y_ptrs[col], j) < 0) {
      return false;
    }
  }

  return true;
}

// -----------------------------------------------------------------------------

static inline
int p_matches_df_compare_na_equal(const void* x,
                                  r_ssize i,
                                  const void* y,
                                  r_ssize j,
                                  const enum vctrs_filter* v_filters) {
  // First broken tie wins.
  // All columns are integer vectors (ranks).

  struct poly_df_data* x_data = (struct poly_df_data*) x;
  struct poly_df_data* y_data = (struct poly_df_data*) y;

  r_ssize n_col = x_data->n_col;

  const void** x_ptrs = x_data->col_ptrs;
  const void** y_ptrs = y_data->col_ptrs;

  for (r_ssize col = 0; col < n_col; ++col) {
    const enum vctrs_filter filter = v_filters[col];

    switch (filter) {
    case VCTRS_FILTER_none: {
      break;
    }
    case VCTRS_FILTER_max: {
      int cmp = p_int_compare_na_equal(x_ptrs[col], i, y_ptrs[col], j);
      if (cmp == 1) {
        // Signal replace
        return 1;
      } else if (cmp == -1) {
        return -1;
      }
      break;
    }
    case VCTRS_FILTER_min: {
      int cmp = p_int_compare_na_equal(x_ptrs[col], i, y_ptrs[col], j);
      if (cmp == -1) {
        // Signal replace
        return 1;
      } else if (cmp == 1) {
        return -1;
      }
      break;
    }
    }
  }

  // All columns are equal, or no columns.
  // In the all equal case, we don't need to update.
  return 0;
}

static inline
bool p_matches_df_equal_na_equal(const void* x,
                                 r_ssize i,
                                 const void* y,
                                 r_ssize j,
                                 const enum vctrs_filter* v_filters) {
  // All columns are integer vectors (ranks).

  struct poly_df_data* x_data = (struct poly_df_data*) x;
  struct poly_df_data* y_data = (struct poly_df_data*) y;

  r_ssize n_col = x_data->n_col;

  const void** x_ptrs = x_data->col_ptrs;
  const void** y_ptrs = y_data->col_ptrs;

  for (r_ssize col = 0; col < n_col; ++col) {
    const enum vctrs_filter filter = v_filters[col];

    if (filter == VCTRS_FILTER_none) {
      continue;
    }

    if (!p_int_equal_na_equal(x_ptrs[col], i, y_ptrs[col], j)) {
      return false;
    }
  }

  // All columns are equal, or no columns.
  return true;
}

// -----------------------------------------------------------------------------

static inline
r_ssize midpoint(r_ssize lhs, r_ssize rhs) {
  return lhs + (rhs - lhs) / 2;
}

// -----------------------------------------------------------------------------

static inline
void stop_matches_overflow(double size) {
  r_abort(
    "Match procedure results in an allocation larger than 2^31-1 elements. "
    "Attempted allocation size was %.0lf. "
    "Please report this to the vctrs maintainers at "
    "<https://github.com/r-lib/vctrs/issues>.",
    size
  );
}

static inline
void stop_matches_nothing(r_ssize i,
                          struct vctrs_arg* needles_arg,
                          struct vctrs_arg* haystack_arg) {
  r_obj* syms[4] = {
    syms_i,
    syms_needles_arg,
    syms_haystack_arg,
    NULL
  };
  r_obj* args[4] = {
    KEEP(r_int((int)i + 1)),
    KEEP(vctrs_arg(needles_arg)),
    KEEP(vctrs_arg(haystack_arg)),
    NULL
  };

  r_obj* call = KEEP(r_call_n(syms_stop_matches_nothing, syms, args));
  Rf_eval(call, vctrs_ns_env);

  never_reached("stop_matches_nothing");
}

static inline
void stop_matches_remaining(r_ssize i,
                            struct vctrs_arg* needles_arg,
                            struct vctrs_arg* haystack_arg) {
  r_obj* syms[4] = {
    syms_i,
    syms_needles_arg,
    syms_haystack_arg,
    NULL
  };
  r_obj* args[4] = {
    KEEP(r_int((int)i + 1)),
    KEEP(vctrs_arg(needles_arg)),
    KEEP(vctrs_arg(haystack_arg)),
    NULL
  };

  r_obj* call = KEEP(r_call_n(syms_stop_matches_remaining, syms, args));
  Rf_eval(call, vctrs_ns_env);

  never_reached("stop_matches_remaining");
}

static inline
void stop_matches_missing(r_ssize i, struct vctrs_arg* needles_arg) {
  r_obj* syms[3] = {
    syms_i,
    syms_needles_arg,
    NULL
  };
  r_obj* args[3] = {
    KEEP(r_int((int)i + 1)),
    KEEP(vctrs_arg(needles_arg)),
    NULL
  };

  r_obj* call = KEEP(r_call_n(syms_stop_matches_missing, syms, args));
  Rf_eval(call, vctrs_ns_env);

  never_reached("stop_matches_missing");
}

static inline
void stop_matches_multiple(r_ssize i,
                           struct vctrs_arg* needles_arg,
                           struct vctrs_arg* haystack_arg) {
  r_obj* syms[4] = {
    syms_i,
    syms_needles_arg,
    syms_haystack_arg,
    NULL
  };
  r_obj* args[4] = {
    KEEP(r_int((int)i + 1)),
    KEEP(vctrs_arg(needles_arg)),
    KEEP(vctrs_arg(haystack_arg)),
    NULL
  };

  r_obj* call = KEEP(r_call_n(syms_stop_matches_multiple, syms, args));
  Rf_eval(call, vctrs_ns_env);

  never_reached("stop_matches_multiple");
}

static inline
void stop_matches_duplicates(r_ssize i,
                             struct vctrs_arg* arg,
                             bool needles) {
  r_obj* syms[4] = {
   syms_i,
   syms_arg,
   syms_needles,
   NULL
  };
  r_obj* args[4] = {
    KEEP(r_int((int)i + 1)),
    KEEP(vctrs_arg(arg)),
    KEEP(r_lgl(needles)),
    NULL
  };

  r_obj* call = KEEP(r_call_n(syms_stop_matches_duplicates, syms, args));
  Rf_eval(call, vctrs_ns_env);

  never_reached("stop_matches_duplicates");
}

static inline
void warn_matches_multiple(r_ssize i,
                           struct vctrs_arg* needles_arg,
                           struct vctrs_arg* haystack_arg) {
  r_obj* syms[4] = {
    syms_i,
    syms_needles_arg,
    syms_haystack_arg,
    NULL
  };
  r_obj* args[4] = {
    KEEP(r_int((int)i + 1)),
    KEEP(vctrs_arg(needles_arg)),
    KEEP(vctrs_arg(haystack_arg)),
    NULL
  };

  r_obj* call = KEEP(r_call_n(syms_warn_matches_multiple, syms, args));
  Rf_eval(call, vctrs_ns_env);
  FREE(4);
}

// -----------------------------------------------------------------------------

#undef SIGNAL_NO_MATCH
#undef SIGNAL_NA_PROPAGATE
