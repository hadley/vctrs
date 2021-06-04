#include <rlang.h>
#include "vctrs.h"
#include "utils.h"
#include "rank.h"
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
  VCTRS_OPS_gt = 1,
  VCTRS_OPS_gte = 2,
  VCTRS_OPS_lt = 3,
  VCTRS_OPS_lte = 4
};

struct vctrs_no_match {
  bool error;
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
                     r_obj* na_equal,
                     r_obj* no_match,
                     r_obj* multiple,
                     r_obj* nan_distinct,
                     r_obj* chr_transform,
                     r_obj* needles_arg,
                     r_obj* haystack_arg) {
  bool c_na_equal = r_as_bool(na_equal);

  const struct vctrs_no_match c_no_match = parse_no_match(no_match);

  if (!r_is_bool(nan_distinct)) {
    r_abort("`nan_distinct` must be a single `TRUE` or `FALSE`.");
  }
  bool c_nan_distinct = r_lgl_get(nan_distinct, 0);

  struct vctrs_arg c_needles_arg = vec_as_arg(needles_arg);
  struct vctrs_arg c_haystack_arg = vec_as_arg(haystack_arg);

  enum vctrs_multiple c_multiple = parse_multiple(multiple);

  return vec_matches(
    needles,
    haystack,
    condition,
    c_na_equal,
    &c_no_match,
    c_multiple,
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
                   bool na_equal,
                   const struct vctrs_no_match* no_match,
                   enum vctrs_multiple multiple,
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

  r_ssize n_cols = r_length(needles);

  bool na_propagate = !na_equal;

  // Compute the locations of missing values for each column if computing ranks
  // later on is going to replace the missing values with integer ranks
  r_obj* needles_missings = na_propagate ? r_null : df_missings_by_col(needles, size_needles, n_cols);
  KEEP_N(needles_missings, &n_prot);

  r_obj* haystack_missings = na_propagate ? r_null : df_missings_by_col(haystack, size_haystack, n_cols);
  KEEP_N(haystack_missings, &n_prot);

  // Compute joint ranks to simplify each column down to an integer vector
  r_obj* args = KEEP_N(df_joint_ranks(
    needles,
    haystack,
    size_needles,
    size_haystack,
    n_cols,
    ptype,
    na_propagate,
    nan_distinct,
    chr_transform
  ), &n_prot);
  needles = r_list_get(args, 0);
  haystack = r_list_get(args, 1);

  enum vctrs_ops* v_ops = (enum vctrs_ops*) R_alloc(n_cols, sizeof(enum vctrs_ops));
  parse_condition(condition, v_ops, n_cols);

  r_obj* out = df_matches(
    needles,
    haystack,
    needles_missings,
    haystack_missings,
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
                  r_obj* needles_missings,
                  r_obj* haystack_missings,
                  r_ssize size_needles,
                  r_ssize size_haystack,
                  bool na_equal,
                  const struct vctrs_no_match* no_match,
                  enum vctrs_multiple multiple,
                  enum vctrs_ops* v_ops) {
  int n_prot = 0;

  r_obj* o_needles = KEEP_N(vec_order(needles, chrs_asc, chrs_smallest, true, r_null), &n_prot);
  const int* v_o_needles = r_int_cbegin(o_needles);

  r_obj* info = KEEP_N(compute_nested_containment_info(haystack, v_ops), &n_prot);

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

  struct r_dyn_array* p_o_haystack_starts = r_new_dyn_vector(R_TYPE_integer, initial_capacity);
  KEEP_N(p_o_haystack_starts->shelter, &n_prot);

  {
    // Temporary unstable pointer
    int* v_o_haystack_starts = (int*) r_arr_begin(p_o_haystack_starts);
    for (r_ssize i = 0; i < size_needles; ++i) {
      // Initialize to no match everywhere, no need to initialize extra buffer
      v_o_haystack_starts[i] = SIGNAL_NO_MATCH;
    }
    p_o_haystack_starts->count = size_needles;
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

  // If we can skip, `needles_locs` will always be an increasing sequence of values
  const bool skip_needles_locs = (multiple == VCTRS_MULTIPLE_first || multiple == VCTRS_MULTIPLE_last);

  // TODO: Also don't need `needles_locs` when `n_nested_groups == 1` (even if multiple="all"),
  // as this implies that there is no nested and the locations are again an ordered sequence.
  struct r_dyn_array* p_needles_locs;
  if (!skip_needles_locs) {
    p_needles_locs = r_new_dyn_vector(R_TYPE_integer, initial_capacity);
    KEEP_N(p_needles_locs->shelter, &n_prot);

    int* v_needles_locs = (int*) r_arr_begin(p_needles_locs);
    for (r_ssize i = 0; i < size_needles; ++i) {
      // No need to initialize extra buffer
      v_needles_locs[i] = i + 1;
    }
    p_needles_locs->count = size_needles;
  }

  struct poly_vec* p_poly_needles = new_poly_vec(needles, vctrs_type_dataframe);
  PROTECT_POLY_VEC(p_poly_needles, &n_prot);
  const struct poly_df_data* p_needles = (const struct poly_df_data*) p_poly_needles->p_vec;

  struct poly_vec* p_poly_haystack = new_poly_vec(haystack, vctrs_type_dataframe);
  PROTECT_POLY_VEC(p_poly_haystack, &n_prot);
  const struct poly_df_data* p_haystack = (const struct poly_df_data*) p_poly_haystack->p_vec;

  const struct poly_df_data* p_needles_missings;
  const struct poly_df_data* p_haystack_missings;

  if (na_equal) {
    // NAs were removed from the ranks, so grab them from the "missing" data frames
    struct poly_vec* p_poly_needles_missings = new_poly_vec(needles_missings, vctrs_type_dataframe);
    PROTECT_POLY_VEC(p_poly_needles_missings, &n_prot);
    p_needles_missings = (const struct poly_df_data*) p_poly_needles_missings->p_vec;

    struct poly_vec* p_poly_haystack_missings = new_poly_vec(haystack_missings, vctrs_type_dataframe);
    PROTECT_POLY_VEC(p_poly_haystack_missings, &n_prot);
    p_haystack_missings = (const struct poly_df_data*) p_poly_haystack_missings->p_vec;
  } else {
    // NAs propagated through the ranks, so we can use them directly
    p_needles_missings = p_needles;
    p_haystack_missings = p_haystack;
  }

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
      p_needles_missings,
      p_haystack_missings,
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
      p_needles_missings,
      p_haystack_missings,
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

  r_obj* out = KEEP_N(expand_compact_indices(
    v_o_haystack,
    p_o_haystack_starts,
    p_match_sizes,
    p_needles_locs,
    skip_match_sizes,
    skip_needles_locs,
    na_equal,
    no_match,
    any_multiple,
    any_directional
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
                        const struct poly_df_data* p_needles_missings,
                        const struct poly_df_data* p_haystack_missings,
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
  const r_ssize n_col = p_needles->n_col;

  const int* v_needles = (const int*) p_needles->col_ptrs[col];
  const int* v_needles_missings = (const int*) p_needles_missings->col_ptrs[col];

  const int* v_haystack = (const int*) p_haystack->col_ptrs[col];
  const int* v_haystack_missings = (const int*) p_haystack_missings->col_ptrs[col];

  const r_ssize grp_mid_o_needles = midpoint(lower_o_needles, upper_o_needles);
  const r_ssize grp_mid_needles = v_o_needles[grp_mid_o_needles] - 1;

  const int val_needle = v_needles[grp_mid_needles];
  const bool needle_is_missing = int_is_missing(v_needles_missings[grp_mid_needles]);

  // Find lower and upper group bounds for the needle value
  const r_ssize grp_lower_o_needles = int_lower_duplicate(
    val_needle,
    v_needles,
    v_o_needles,
    lower_o_needles,
    grp_mid_o_needles
  );
  const r_ssize grp_upper_o_needles = int_upper_duplicate(
    val_needle,
    v_needles,
    v_o_needles,
    grp_mid_o_needles,
    upper_o_needles
  );

  if (!na_equal && needle_is_missing) {
    // Propagate NA, don't recursive into further columns.
    for (r_ssize i = grp_lower_o_needles; i <= grp_upper_o_needles; ++i) {
      // Will always be the first and only time the output is touched for this
      // needle, so we can poke directly into it
      const int loc = v_o_needles[i] - 1;
      R_ARR_POKE(int, p_o_haystack_starts, loc, SIGNAL_NA_PROPAGATE);
    }

    // Learned nothing about haystack!
    bool do_lhs = grp_lower_o_needles > lower_o_needles;
    bool do_rhs = grp_upper_o_needles < upper_o_needles;

    if (do_lhs) {
      upper_o_needles = grp_lower_o_needles - 1;

      df_matches_recurse(
        col,
        lower_o_needles,
        upper_o_needles,
        lower_o_haystack,
        upper_o_haystack,
        p_needles,
        p_haystack,
        p_needles_missings,
        p_haystack_missings,
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
      lower_o_needles = grp_upper_o_needles + 1;

      df_matches_recurse(
        col,
        lower_o_needles,
        upper_o_needles,
        lower_o_haystack,
        upper_o_haystack,
        p_needles,
        p_haystack,
        p_needles_missings,
        p_haystack_missings,
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

    return;
  }

  r_ssize grp_lower_o_haystack = lower_o_haystack;
  r_ssize grp_upper_o_haystack = upper_o_haystack;

  while (grp_lower_o_haystack <= grp_upper_o_haystack) {
    const r_ssize grp_mid_o_haystack = midpoint(grp_lower_o_haystack, grp_upper_o_haystack);
    const r_ssize grp_mid_haystack = v_o_haystack[grp_mid_o_haystack] - 1;
    const int val_haystack = v_haystack[grp_mid_haystack];

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
        v_haystack,
        v_o_haystack,
        grp_lower_o_haystack,
        grp_mid_o_haystack
      );
      grp_upper_o_haystack = int_upper_duplicate(
        val_haystack,
        v_haystack,
        v_o_haystack,
        grp_mid_o_haystack,
        grp_upper_o_haystack
      );
      break;
    }
  }

  // Adjust bounds based on non-equi condition.
  // If needle is NA, never extend the bounds to capture values past it.
  switch (op) {
  case VCTRS_OPS_lt: {
    // Exclude found needle
    grp_lower_o_haystack = grp_upper_o_haystack + 1;
    if (!needle_is_missing) {
      grp_upper_o_haystack = upper_o_haystack;
    }
    break;
  }
  case VCTRS_OPS_lte: {
    if (!needle_is_missing) {
      grp_upper_o_haystack = upper_o_haystack;
    }
    break;
  }
  case VCTRS_OPS_gt: {
    // Exclude found needle
    grp_upper_o_haystack = grp_lower_o_haystack - 1;
    if (!needle_is_missing) {
      grp_lower_o_haystack = lower_o_haystack;
    }
    break;
  }
  case VCTRS_OPS_gte: {
    if (!needle_is_missing) {
      grp_lower_o_haystack = lower_o_haystack;
    }
    break;
  }
  case VCTRS_OPS_eq: {
    break;
  }
  }

  if (!needle_is_missing && (op == VCTRS_OPS_gt || op == VCTRS_OPS_gte)) {
    // In this specific case, a non-NA needle may match an NA in the haystack
    // from the condition adjustments made above. If there was an NA in the
    // haystack, we avoid including it by shifting the lower bound to 1 past
    // the final NA.
    const r_ssize o_haystack_lower = v_o_haystack[grp_lower_o_haystack] - 1;
    const int val_haystack_missings_lower = v_haystack_missings[o_haystack_lower];

    if (int_is_missing(val_haystack_missings_lower)) {
      /* If there was an NA in the haystack, find the last NA */
      grp_lower_o_haystack = int_locate_upper_missing(
        v_haystack_missings,
        v_o_haystack,
        grp_lower_o_haystack,
        grp_upper_o_haystack
      );

      /* Exclude it and all before it */
      ++grp_lower_o_haystack;
    }
  }

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
        p_needles_missings,
        p_haystack_missings,
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

          if (first_touch) {
            if (elt_match_sizes > 1) {
              *p_any_multiple = true;
            }

            R_ARR_POKE(int, p_o_haystack_starts, loc, elt_o_haystack_starts);
            R_ARR_POKE(int, p_match_sizes, loc, elt_match_sizes);
            continue;
          }

          // At a minimum, this is the second match
          *p_any_multiple = true;

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
      p_needles_missings,
      p_haystack_missings,
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
      p_needles_missings,
      p_haystack_missings,
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
                                   const struct poly_df_data* p_needles_missings,
                                   const struct poly_df_data* p_haystack_missings,
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
  const int* v_haystack = v_nested_groups;

  r_ssize grp_lower_o_haystack = 0;
  r_ssize grp_upper_o_haystack = size_haystack - 1;

  for (int i = 0; i < n_nested_groups; ++i) {
    const int val_needle = i;

    while (grp_lower_o_haystack <= grp_upper_o_haystack) {
      const r_ssize grp_mid_o_haystack = midpoint(grp_lower_o_haystack, grp_upper_o_haystack);
      const r_ssize grp_mid_haystack = v_o_haystack[grp_mid_o_haystack] - 1;
      const int val_haystack = v_haystack[grp_mid_haystack];

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
          v_haystack,
          v_o_haystack,
          grp_lower_o_haystack,
          grp_mid_o_haystack
        );
        grp_upper_o_haystack = int_upper_duplicate(
          val_haystack,
          v_haystack,
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
      p_needles_missings,
      p_haystack_missings,
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

// Find the largest contiguous location containing a missing value
static inline
r_ssize int_locate_upper_missing(const int* v_haystack_missings,
                                 const int* v_o_haystack,
                                 r_ssize lower_o_haystack,
                                 r_ssize upper_o_haystack) {
  while (lower_o_haystack <= upper_o_haystack) {
    const r_ssize mid_o_haystack = midpoint(lower_o_haystack, upper_o_haystack);
    const r_ssize mid_haystack = v_o_haystack[mid_o_haystack] - 1;
    const int elt_haystack_missings = v_haystack_missings[mid_haystack];

    if (int_is_missing(elt_haystack_missings)) {
      lower_o_haystack = mid_o_haystack + 1;
    } else {
      upper_o_haystack = mid_o_haystack - 1;
    }
  }

  return upper_o_haystack;
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

// -----------------------------------------------------------------------------

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

    r_obj* missing = vec_equal_na(col);
    r_list_poke(out, i, missing);
    int* v_missing = r_lgl_begin(missing);

    // Flip any TRUE to NA_integer_ to align with propagated integer NAs in
    // `x` ranks when `na_equal = FALSE`.
    // Relies on the fact that logical and integer are the same type internally.
    for (r_ssize j = 0; j < x_size; ++j) {
      if (v_missing[j]) {
        v_missing[j] = r_globals.na_int;
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
    r_abort("`condition` must be a character vector.");
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
      "`condition` must be length 1, or the same length "
      "as the number of columns of the input."
    );
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

static inline
struct vctrs_no_match parse_no_match(r_obj* no_match) {
  if (r_is_string(no_match)) {
    const char* c_no_match = r_chr_get_c_string(no_match, 0);

    if (!strcmp(c_no_match, "error")) {
      return (struct vctrs_no_match) {true, SIGNAL_NO_MATCH};
    }
  }

  if (r_typeof(no_match) == R_TYPE_integer && r_length(no_match) == 1) {
    int c_no_match = r_int_get(no_match, 0);
    return (struct vctrs_no_match) {false, c_no_match};
  }

  r_abort("`no_match` must be a length 1 integer, or \"error\".");
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
                              bool skip_match_sizes,
                              bool skip_needles_locs,
                              bool na_equal,
                              const struct vctrs_no_match* no_match,
                              bool any_multiple,
                              bool any_directional) {
  const r_ssize n_used = p_o_haystack_starts->count;

  const int* v_o_haystack_starts = (const int*) r_arr_cbegin(p_o_haystack_starts);
  const int* v_match_sizes = skip_match_sizes ? NULL : (const int*) r_arr_cbegin(p_match_sizes);
  const int* v_needles_locs = skip_needles_locs ? NULL : (const int*) r_arr_cbegin(p_needles_locs);

  r_ssize out_size = 0;
  if (skip_match_sizes) {
    out_size = n_used;
  } else {
    for (r_ssize i = 0; i < n_used; ++i) {
      // TODO: Check for overflow?
      // This could get extremely large with improperly specified non-equi joins.
      out_size += (r_ssize) v_match_sizes[i];
    }
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

  r_obj* o_needles_locs = vctrs_shared_empty_int;
  if (!skip_needles_locs) {
    r_obj* needles_locs = KEEP(r_arr_unwrap(p_needles_locs));
    o_needles_locs = vec_order(needles_locs, chrs_asc, chrs_smallest, true, r_null);
    FREE(1);
  }
  KEEP(o_needles_locs);
  const int* v_o_needles_locs = r_int_cbegin(o_needles_locs);

  r_ssize out_loc = 0;

  for (r_ssize i = 0; i < n_used; ++i) {
    const int loc = skip_needles_locs ? i : v_o_needles_locs[i] - 1;

    int o_haystack_loc = v_o_haystack_starts[loc];
    const int match_size = skip_match_sizes ? 1 : v_match_sizes[loc];
    const int needles_loc = skip_needles_locs ? loc + 1 : v_needles_locs[loc];

    if (!na_equal && o_haystack_loc == SIGNAL_NA_PROPAGATE) {
      if (match_size != 1) {
        r_stop_internal(
          "expand_compact_indices",
          "`match_size` should always be 1 in the case of NA propagation."
        );
      }

      v_out_needles[out_loc] = needles_loc;
      v_out_haystack[out_loc] = r_globals.na_int;
      ++out_loc;
      continue;
    }

    if (o_haystack_loc == SIGNAL_NO_MATCH) {
      if (match_size != 1) {
        r_stop_internal(
          "expand_compact_indices",
          "`match_size` should always be 1 in the case of no matches."
        );
      }

      if (no_match->error) {
        r_abort(
          "Oh no! There were no matches for the `needle` at location %i.",
          (int) i + 1
        );
      }

      v_out_needles[out_loc] = needles_loc;
      v_out_haystack[out_loc] = no_match->value;
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

    r_obj* out_haystack2 = KEEP(r_alloc_integer(out_size));
    int* v_out_haystack2 = r_int_begin(out_haystack2);

    for (r_ssize i = 0; i < out_size; ++i) {
      v_out_haystack2[i] = v_out_haystack[v_o_haystack_appearance[i] - 1];
    }

    r_list_poke(out, 1, out_haystack2);
    FREE(2);
  }

  FREE(3);
  return out;
}

// -----------------------------------------------------------------------------

// [[ register() ]]
r_obj* vctrs_test_compute_nested_containment_info(r_obj* haystack, r_obj* condition) {
  r_ssize n_cols = r_length(haystack);
  enum vctrs_ops* v_ops = (enum vctrs_ops*) R_alloc(n_cols, sizeof(enum vctrs_ops));
  parse_condition(condition, v_ops, n_cols);
  return compute_nested_containment_info(haystack, v_ops);
}

static
r_obj* compute_nested_containment_info(r_obj* haystack, const enum vctrs_ops* v_ops) {
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

  bool any_nesting = any_directional && (n_cols - first_directional > 1);

  if (!any_nesting) {
    // Nested group info isn't required for only `==`, or when
    // there is only 1 column after the last non-directional column
    r_list_poke(out, 0, vec_order(haystack, chrs_asc, chrs_smallest, true, r_null));
    r_list_poke(out, 1, vctrs_shared_empty_int);
    r_list_poke(out, 2, r_int(1));
    r_list_poke(out, 3, r_lgl(any_directional));
    FREE(n_prot);
    return out;
  }

  r_obj* info = KEEP_N(vec_order_info(
    haystack,
    chrs_asc,
    chrs_smallest,
    true,
    r_null,
    true
  ), &n_prot);

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
    outer_run_sizes
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
                                r_obj* outer_run_sizes) {
  if (!is_data_frame(x)) {
    r_stop_internal("nested_containment_order", "`x` must be a data frame.");
  }

  int n_prot = 0;

  r_ssize n_cols = r_length(x);
  r_ssize size = r_length(order);

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
  if (n_cols == 1) {
    // If there is only 1 column, x is in increasing order already.
    // If `outer_run_sizes` were supplied, each individual run group will
    // be in increasing order (since the single x column is the one that
    // broke any ties), and that is all that is required.
    FREE(n_prot);
    return out;
  }

  const int* v_order = r_int_cbegin(order);
  const int* v_group_sizes = r_int_cbegin(group_sizes);
  const int* v_outer_run_sizes = r_int_cbegin(outer_run_sizes);

  r_ssize n_groups = r_length(group_sizes);

  struct growable prev_rows = new_growable(INTSXP, 10000);
  PROTECT_GROWABLE(&prev_rows, &n_prot);

  struct poly_vec* p_poly_vec = new_poly_vec(x, vctrs_type_dataframe);
  PROTECT_POLY_VEC(p_poly_vec, &n_prot);
  const void* v_x = p_poly_vec->p_vec;

  int i_order = v_group_sizes[0];
  growable_push_int(&prev_rows, v_order[0] - 1);

  r_ssize outer_run_stop = 0;
  r_ssize outer_run_sizes_loc = 0;
  if (r_length(outer_run_sizes) > 0) {
    // If no outer runs exist, `integer(0)` is used and `outer_run_stop = 0`
    // will never match `i_order` since the first group size is at least 1.
    outer_run_stop = v_outer_run_sizes[outer_run_sizes_loc];
    ++outer_run_sizes_loc;
  }

  for (r_ssize i_group = 1; i_group < n_groups; ++i_group) {
    int cur_row = v_order[i_order] - 1;

    bool new_id = true;
    int prev_row_id = 0;
    int max_prev_row_id = prev_rows.n;

    for (; prev_row_id < max_prev_row_id; ++prev_row_id) {
      int prev_row = growable_get_int(&prev_rows, prev_row_id);

      if (p_df_nested_containment_compare_ge_na_equal(v_x, cur_row, v_x, prev_row)) {
        new_id = false;
        break;
      }
    }

    int id;
    if (outer_run_stop == i_order) {
      // Start of a new outer run
      id = 0;
      outer_run_stop += v_outer_run_sizes[outer_run_sizes_loc];
      ++outer_run_sizes_loc;
      prev_rows.n = 1;
      growable_set_int(&prev_rows, 0, cur_row);
    } else if (new_id) {
      // Completely new id for this outer run, which we add to the end
      id = max_prev_row_id;
      growable_push_int(&prev_rows, cur_row);

      if (prev_rows.n > *p_n_ids) {
        *p_n_ids = prev_rows.n;
      }
    } else {
      // Update existing row location to the current row, since it is larger
      id = prev_row_id;
      growable_set_int(&prev_rows, prev_row_id, cur_row);
    }

    int group_size = v_group_sizes[i_group];

    for (int i = 0; i < group_size; ++i) {
      v_ids[v_order[i_order + i] - 1] = id;
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
r_ssize midpoint(r_ssize lhs, r_ssize rhs) {
  return lhs + (rhs - lhs) / 2;
}

// -----------------------------------------------------------------------------

#undef SIGNAL_NO_MATCH
#undef SIGNAL_NA_PROPAGATE
