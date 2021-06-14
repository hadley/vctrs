#include <rlang.h>
#include "vctrs.h"
#include "equal.h"
#include "order.h"

enum ties {
  TIES_min,
  TIES_max,
  TIES_sequential,
  TIES_dense
};

#include "decl/rank-decl.h"

// [[ register() ]]
r_obj* vctrs_rank(r_obj* x,
                  r_obj* ties,
                  r_obj* na_propagate,
                  r_obj* direction,
                  r_obj* na_value,
                  r_obj* nan_distinct,
                  r_obj* chr_transform) {
  const enum ties c_ties = parse_ties(ties);
  const bool c_na_propagate = r_as_bool(na_propagate);
  const bool c_nan_distinct = r_as_bool(nan_distinct);

  return vec_rank(
    x,
    c_ties,
    c_na_propagate,
    direction,
    na_value,
    c_nan_distinct,
    chr_transform
  );
}

static
r_obj* vec_rank(r_obj* x,
                enum ties ties_type,
                bool na_propagate,
                r_obj* direction,
                r_obj* na_value,
                bool nan_distinct,
                r_obj* chr_transform) {
  r_ssize size = vec_size(x);

  r_keep_t pi_x;
  KEEP_HERE(x, &pi_x);

  r_obj* missing = r_null;
  r_keep_t pi_missing;
  KEEP_HERE(missing, &pi_missing);
  int* v_missing = NULL;

  r_obj* not_missing = r_null;
  r_keep_t pi_not_missing;
  KEEP_HERE(not_missing, &pi_not_missing);
  int* v_not_missing = NULL;

  r_ssize rank_size = size;

  if (na_propagate) {
    // Slice out non-missing values of `x` to rank.
    // Retain `non_missing` logical vector for constructing `out`.
    missing = vec_equal_na(x);
    KEEP_AT(missing, pi_missing);
    v_missing = r_lgl_begin(missing);

    bool any_missing = r_lgl_any(missing);

    if (any_missing) {
      for (r_ssize i = 0; i < size; ++i) {
        v_missing[i] = !v_missing[i];
      }

      not_missing = missing;
      KEEP_AT(not_missing, pi_not_missing);
      v_not_missing = v_missing;
      missing = NULL;
      v_missing = NULL;

      x = vec_slice(x, not_missing);
      KEEP_AT(x, pi_x);

      rank_size = vec_size(x);
    } else {
      na_propagate = false;
    }
  }

  r_obj* rank = KEEP(r_alloc_integer(rank_size));
  int* v_rank = r_int_begin(rank);

  const bool chr_ordered = true;

  r_obj* info = KEEP(vec_order_info(x, direction, na_value, nan_distinct, chr_transform, chr_ordered));

  r_obj* order = r_list_get(info, 0);
  const int* v_order = r_int_cbegin(order);

  r_obj* group_sizes = r_list_get(info, 1);
  const int* v_group_sizes = r_int_cbegin(group_sizes);
  r_ssize n_groups = r_length(group_sizes);

  switch (ties_type) {
  case TIES_min: vec_rank_min(v_order, v_group_sizes, n_groups, v_rank); break;
  case TIES_max: vec_rank_max(v_order, v_group_sizes, n_groups, v_rank); break;
  case TIES_sequential: vec_rank_sequential(v_order, v_group_sizes, n_groups, v_rank); break;
  case TIES_dense: vec_rank_dense(v_order, v_group_sizes, n_groups, v_rank); break;
  }

  r_obj* out = r_null;

  if (na_propagate) {
    out = KEEP(r_alloc_integer(size));
    int* v_out = r_int_begin(out);
    r_ssize j = 0;

    for (r_ssize i = 0; i < size; ++i) {
      v_out[i] = v_not_missing[i] ? v_rank[j++] : r_globals.na_int;
    }

    FREE(1);
  } else {
    out = rank;
  }

  FREE(5);
  return out;
}

// -----------------------------------------------------------------------------

static
void vec_rank_min(const int* v_order,
                  const int* v_group_sizes,
                  r_ssize n_groups,
                  int* v_rank) {
  r_ssize k = 0;
  r_ssize rank = 1;

  for (r_ssize i = 0; i < n_groups; ++i) {
    const r_ssize group_size = v_group_sizes[i];

    for (r_ssize j = 0; j < group_size; ++j) {
      r_ssize loc = v_order[k] - 1;
      v_rank[loc] = rank;
      ++k;
    }

    rank += group_size;
  }
}

static
void vec_rank_max(const int* v_order,
                  const int* v_group_sizes,
                  r_ssize n_groups,
                  int* v_rank) {
  r_ssize k = 0;
  r_ssize rank = 0;

  for (r_ssize i = 0; i < n_groups; ++i) {
    const r_ssize group_size = v_group_sizes[i];
    rank += group_size;

    for (r_ssize j = 0; j < group_size; ++j) {
      r_ssize loc = v_order[k] - 1;
      v_rank[loc] = rank;
      ++k;
    }
  }
}

static
void vec_rank_sequential(const int* v_order,
                         const int* v_group_sizes,
                         r_ssize n_groups,
                         int* v_rank) {
  r_ssize k = 0;
  r_ssize rank = 1;

  for (r_ssize i = 0; i < n_groups; ++i) {
    const r_ssize group_size = v_group_sizes[i];

    for (r_ssize j = 0; j < group_size; ++j) {
      r_ssize loc = v_order[k] - 1;
      v_rank[loc] = rank;
      ++k;
      ++rank;
    }
  }
}

static
void vec_rank_dense(const int* v_order,
                    const int* v_group_sizes,
                    r_ssize n_groups,
                    int* v_rank) {
  r_ssize k = 0;
  r_ssize rank = 1;

  for (r_ssize i = 0; i < n_groups; ++i) {
    const r_ssize group_size = v_group_sizes[i];

    for (r_ssize j = 0; j < group_size; ++j) {
      r_ssize loc = v_order[k] - 1;
      v_rank[loc] = rank;
      ++k;
    }

    ++rank;
  }
}

// -----------------------------------------------------------------------------

static inline
enum ties parse_ties(r_obj* ties) {
  if (!r_is_string(ties)) {
    r_stop_internal("parse_ties", "`ties` must be a string.");
  }

  const char* c_ties = r_chr_get_c_string(ties, 0);

  if (!strcmp(c_ties, "min")) return TIES_min;
  if (!strcmp(c_ties, "max")) return TIES_max;
  if (!strcmp(c_ties, "sequential")) return TIES_sequential;
  if (!strcmp(c_ties, "dense")) return TIES_dense;

  r_stop_internal(
    "parse_ties",
    "`ties` must be one of: \"min\", \"max\", \"sequential\", or \"dense\"."
  );
}

// -----------------------------------------------------------------------------

// Treats missing values as `true`
static inline
bool r_lgl_any(r_obj* x) {
  if (r_typeof(x) != R_TYPE_logical) {
    r_stop_internal("r_lgl_any", "`x` must be a logical vector.");
  }

  const int* v_x = r_lgl_cbegin(x);
  r_ssize size = r_length(x);

  for (r_ssize i = 0; i < size; ++i) {
    if (v_x[i]) {
      return true;
    }
  }

  return false;
}
