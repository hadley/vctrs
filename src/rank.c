#include <rlang.h>
#include "vctrs.h"
#include "equal.h"
#include "order-radix.h"
#include "decl/rank-decl.h"

// [[ register() ]]
sexp* vctrs_rank(sexp* x,
                 sexp* ties,
                 sexp* na_propagate,
                 sexp* na_value,
                 sexp* nan_distinct,
                 sexp* chr_transform) {
  const enum ties c_ties = parse_ties(ties);
  const bool c_na_propagate = r_bool_as_int(na_propagate);
  const bool c_nan_distinct = r_bool_as_int(nan_distinct);

  return vec_rank(
    x,
    c_ties,
    c_na_propagate,
    na_value,
    c_nan_distinct,
    chr_transform
  );
}

static
sexp* vec_rank(sexp* x,
               enum ties ties_type,
               bool na_propagate,
               sexp* na_value,
               bool nan_distinct,
               sexp* chr_transform) {
  r_ssize size = vec_size(x);

  r_keep_t pi_x;
  KEEP_HERE(x, &pi_x);

  sexp* missing = r_null;
  r_keep_t pi_missing;
  KEEP_HERE(missing, &pi_missing);
  int* v_missing = NULL;

  sexp* not_missing = r_null;
  r_keep_t pi_not_missing;
  KEEP_HERE(not_missing, &pi_not_missing);
  int* v_not_missing = NULL;

  r_ssize rank_size = -1;

  if (na_propagate) {
    // Slice out non-missing values of `x` to rank.
    // Retain `non_missing` logical vector for constructing `out`.
    missing = vec_equal_na(x);
    KEEP_AT(missing, pi_missing);
    v_missing = r_lgl_deref(missing);

    bool any_missing = r_lgl_any(missing);
    if (!any_missing) {
      na_propagate = false;
      goto skip_propagate;
    }

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
    skip_propagate:;
    rank_size = size;
  }

  sexp* rank = KEEP(r_alloc_integer(rank_size));
  int* v_rank = r_int_deref(rank);

  sexp* direction = KEEP(r_chr("asc"));
  sexp* info = KEEP(vec_order_info(x, direction, na_value, nan_distinct, chr_transform));

  sexp* order = r_list_get(info, 0);
  const int* v_order = r_int_deref_const(order);

  sexp* group_sizes = r_list_get(info, 1);
  const int* v_group_sizes = r_int_deref_const(group_sizes);
  r_ssize n_groups = r_length(group_sizes);

  switch (ties_type) {
  case TIES_min: vec_rank_min(v_order, v_group_sizes, n_groups, v_rank); break;
  case TIES_max: vec_rank_max(v_order, v_group_sizes, n_groups, v_rank); break;
  case TIES_sequential: vec_rank_sequential(v_order, v_group_sizes, n_groups, v_rank); break;
  case TIES_dense: vec_rank_dense(v_order, v_group_sizes, n_groups, v_rank); break;
  }

  sexp* out = r_null;
  r_keep_t pi_out;
  KEEP_HERE(out, &pi_out);

  if (na_propagate) {
    out = r_alloc_integer(size);
    KEEP_AT(out, pi_out);
    int* v_out = r_int_deref(out);
    r_ssize j = 0;

    for (r_ssize i = 0; i < size; ++i) {
      v_out[i] = v_not_missing[i] ? v_rank[j++] : r_globals.na_int;
    }
  } else {
    out = rank;
  }

  FREE(7);
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
enum ties parse_ties(sexp* ties) {
  if (r_typeof(ties) != R_TYPE_character) {
    r_abort("`ties` must be a character vector.");
  }
  if (r_length(ties) < 1) {
    r_abort("`ties` must be at least length 1.");
  }

  const char* c_ties = r_chr_get_c_string(ties, 0);

  if (!strcmp(c_ties, "min")) return TIES_min;
  if (!strcmp(c_ties, "max")) return TIES_max;
  if (!strcmp(c_ties, "sequential")) return TIES_sequential;
  if (!strcmp(c_ties, "dense")) return TIES_dense;

  r_abort("`ties` must be one of: 'min', 'max', 'sequential', or 'dense'.");
}

// -----------------------------------------------------------------------------

// Treats missing values as `true`
static inline
bool r_lgl_any(sexp* x) {
  if (r_typeof(x) != R_TYPE_logical) {
    r_abort("Internal error: Expected logical vector in `r_lgl_any()`.");
  }

  const int* v_x = r_lgl_deref_const(x);
  r_ssize size = r_length(x);

  for (r_ssize i = 0; i < size; ++i) {
    if (v_x[i]) {
      return true;
    }
  }

  return false;
}
