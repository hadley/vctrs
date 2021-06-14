/*
 * The implementation of vec_order() is based on data.table’s forder() and their
 * earlier contribution to R’s order(). See LICENSE.note for more information.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this file,
 * You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2020, RStudio
 * Copyright (c) 2020, Data table team
 */

#include <rlang.h>
#include "vctrs.h"
#include "utils.h"
#include "lazy.h"
#include "type-data-frame.h"
#include "translate.h"
#include "order-radix.h"
#include "order-groups.h"
#include "order-truelength.h"
#include "order-sortedness.h"
#include "order-transform.h"

// -----------------------------------------------------------------------------

/*
 * High level description of `vec_order()`
 *
 * Heavily inspired by `radixsort.c` in base R and `forder()` from data.table
 * https://github.com/wch/r-source/blob/trunk/src/main/radixsort.c
 * https://github.com/Rdatatable/data.table/blob/master/src/forder.c
 *
 * Additional resources about radix sorting:
 * http://codercorner.com/RadixSortRevisited.htm
 * http://stereopsis.com/radix.html
 *
 * The very end of this has a MSB radix sort implementation
 * https://eternallyconfuzzled.com/sorting-c-introduction-to-the-automatic-ordering-of-data
 *
 * -----------------------------------------------------------------------------
 * Integers
 *
 * This uses a combination of 3 ordering algorithms.
 *
 * - `int_order_insertion()` - An insertion sort is used when `x` is very
 *   small. This has less overhead than the counting or radix sort and is
 *   faster for small input.
 *
 * - `int_order_counting()` - A counting sort is used when `x` has a range
 *   of less than `INT_ORDER_COUNTING_RANGE_BOUNDARY`. For integers with a
 *   small range like this, the bucketing in the counting sort can be very
 *   fast when compared with the recursive multipass approach of the radix sort.
 *
 * - `int_order_radix()` - A radix sort is used for everything else.
 *   This is a MSB radix sort. It orders the vector 1 byte (8 bits) at a time,
 *   so for a 4 byte int this makes a maximum of 4 passes over each integer.
 *   It orders from most significant byte to least significant. After each
 *   pass, there are 256 buckets (1 for each possible byte value). Each bucket
 *   is then ordered separately on the next byte. This happens recursively for
 *   the 4 passes. When the buckets get small enough in size, the insertion
 *   sort is used to finish them off.
 *
 * For radix sorting, we have to use unsigned types for bit shifting to
 * work reliably. We map `int` to `uint32_t` in a way that preserves order,
 * and also handle `na_last` and `decreasing` in this mapping. This all happens
 * in `int_adjust()`. It is assumed and checked at load time that
 * `sizeof(int) == 4`.
 *
 * -----------------------------------------------------------------------------
 * Doubles
 *
 * This uses a combination of 2 ordering algorithms:
 *
 * - `dbl_order_insertion()` - An insertion sort is used when `x` is very small.
 *
 * - `dbl_order_radix()` - This is similar to `int_order_radix()`, see above,
 *   but makes a max of 8 passes over the data.
 *
 * For doubles, we assume `sizeof(double) == 8`, which should pretty much be
 * ensured by IEEE 754 specifications.
 *
 * For the mapping here, it is possible to map `double -> uint64_t` in an
 * order preserving way. This is very cool, and involves always flipping the
 * sign bit of the value, and flipping all other bits if the value was negative.
 * This is described more in: http://stereopsis.com/radix.html.
 * This is implemented in `dbl_adjust()` which also handles `na_last` and
 * `decreasing`. For `na_last`, we treat `NA_real_` and `NaN` equivalently.
 * Base R does as well, but data.table does not.
 *
 * -----------------------------------------------------------------------------
 * Characters
 *
 * Character vector ordering is a bit trickier than integers or doubles. It
 * uses two internal details in R:
 *
 * - CHARSXP values in R are in a global string pool, and
 *   repeated strings like `c("hi", "hi")` both point to the same CHARSXP.
 *
 * - There is a property that all vectors have called the TRUELENGTH. This is
 *   used to overallocate in lists, but is otherwise unused
 *   in CHARSXPs. The truelength is stored as a `r_ssize`.
 *
 * Character ordering is achieved by first iterating through `x` and extracting
 * the unique values by using the TRUELENGTH. If the truelength is positive
 * or zero, we save it as an unseen string and set the truelength to `-1`.
 * Otherwise if it is negative we assume we have seen it already. At the
 * end of this we have a vector of unique strings. We order this with
 * `chr_order_radix()`. Then we iterate over the now sorted unique strings
 * and set their truelength to `-i - 1` (where `i` is the index while
 * iterating). This marks the unique strings with their ordering (as a negative
 * value to be different from R) and also updates the original character vector.
 * We then iterate through the original vector again, plucking off the now
 * updated truelength integer values. This gives us an integer proxy for the
 * ordering, which we can run through `int_order_chunk()` to get the final
 * ordering.
 *
 * The ordering of unique characters uses a combination of 2 ordering
 * algorithms:
 *
 * - `chr_order_insertion()` - Used when `x` is small.
 *
 * - `chr_order_radix()` - Same principle as integer/double ordering, but
 *   we iterate 1 character at a time. We assume a C locale here. Any non-ASCII
 *   and non-UTF-8 strings are translated up front by
 *   `vec_normalize_encoding()`.
 *
 * -----------------------------------------------------------------------------
 * Logicals
 *
 * Uses the same infrastructure as integers. Because the number of possible
 * unique values is low, this will always use either an insertion sort for
 * small vectors, or a counting sort for large ones.
 *
 * -----------------------------------------------------------------------------
 * Complex
 *
 * We treat complex as a data frame of two double columns. We order the
 * real part first using `dbl_order_chunk()`, then order the imaginary part also
 * using `dbl_order_chunk()`.
 *
 * -----------------------------------------------------------------------------
 * Data frames
 *
 * Multi-column data frame ordering uses the same principle as MSB ordering.
 * It starts with the first column (the most "significant" one) and orders it.
 * While ordering the column, group sizes are tracked ("groups" are duplicate
 * values in the column). The next column is broken into chunks corresponding
 * to these group sizes from the first column, and the chunks are ordered
 * individually. While ordering the chunks of the 2nd column, group sizes are
 * again tracked to use in subsequent columns.
 */

// -----------------------------------------------------------------------------

#define UINT8_MAX_SIZE (UINT8_MAX + 1)

/*
 * Maximum number of passes required to completely sort ints and doubles
 */
#define INT_MAX_RADIX_PASS 4
#define DBL_MAX_RADIX_PASS 8

/*
 * Maximum range allowed when deciding whether or not to use a counting sort
 * vs a radix sort. Counting sort is somewhat faster when less than this
 * boundary value.
 */
#define INT_ORDER_COUNTING_RANGE_BOUNDARY 100000

/*
 * Size of `x` that determines when an insertion sort should be used. Seems
 * to work better than 256 (from limited testing), base R uses 200.
 * Somewhat based on this post:
 * https://probablydance.com/2016/12/27/i-wrote-a-faster-sorting-algorithm/
 */
#define ORDER_INSERTION_BOUNDARY 128

/*
 * Adjustments for translating current `pass` into the current `radix` byte
 * that we need to shift to.
 */
#define PASS_TO_RADIX(X, MAX) (MAX - 1 - X)
#define SHIFT_ADJUSTMENT -CHAR_BIT

// -----------------------------------------------------------------------------

static inline bool parse_nan_distinct(SEXP nan_distinct);

// [[ register() ]]
SEXP vctrs_order(SEXP x,
                 SEXP direction,
                 SEXP na_value,
                 SEXP nan_distinct,
                 SEXP chr_transform) {
  bool c_nan_distinct = parse_nan_distinct(nan_distinct);
  return vec_order(x, direction, na_value, c_nan_distinct, chr_transform);
}

static SEXP vec_order_info_impl(SEXP x,
                                SEXP direction,
                                SEXP na_value,
                                bool nan_distinct,
                                SEXP chr_transform,
                                bool chr_ordered,
                                bool group_sizes);

// [[ include("order-radix.h") ]]
SEXP vec_order(SEXP x, SEXP direction, SEXP na_value, bool nan_distinct, SEXP chr_transform) {
  const bool chr_ordered = true;
  const bool group_sizes = false;
  SEXP info = vec_order_info_impl(x, direction, na_value, nan_distinct, chr_transform, chr_ordered, group_sizes);
  return r_list_get(info, 0);
}

// -----------------------------------------------------------------------------

static SEXP vec_order_locs(SEXP x,
                           SEXP direction,
                           SEXP na_value,
                           bool nan_distinct,
                           SEXP chr_transform);

// [[ register() ]]
SEXP vctrs_order_locs(SEXP x,
                      SEXP direction,
                      SEXP na_value,
                      SEXP nan_distinct,
                      SEXP chr_transform) {
  bool c_nan_distinct = parse_nan_distinct(nan_distinct);
  return vec_order_locs(x, direction, na_value, c_nan_distinct, chr_transform);
}


static
SEXP vec_order_locs(SEXP x, SEXP direction, SEXP na_value, bool nan_distinct, SEXP chr_transform) {
  const bool chr_ordered = true;

  SEXP info = KEEP(vec_order_info(x, direction, na_value, nan_distinct, chr_transform, chr_ordered));

  SEXP o = r_list_get(info, 0);
  const int* p_o = r_int_cbegin(o);

  SEXP sizes = r_list_get(info, 1);
  const int* p_sizes = r_int_cbegin(sizes);

  r_ssize n_groups = r_length(sizes);

  SEXP loc = KEEP(r_alloc_list(n_groups));

  SEXP key_loc = KEEP(r_alloc_integer(n_groups));
  int* p_key_loc = r_int_begin(key_loc);

  int start = 0;

  for (r_ssize i = 0; i < n_groups; ++i) {
    p_key_loc[i] = p_o[start];

    const int size = p_sizes[i];

    SEXP elt = r_alloc_integer(size);
    r_list_poke(loc, i, elt);
    int* p_elt = r_int_begin(elt);

    R_len_t k = 0;

    for (int j = 0; j < size; ++j) {
      p_elt[k] = p_o[start];
      ++start;
      ++k;
    }
  }

  SEXP key = KEEP(vec_slice(x, key_loc));

  // Construct output data frame
  SEXP out = KEEP(r_alloc_list(2));
  r_list_poke(out, 0, key);
  r_list_poke(out, 1, loc);

  SEXP names = KEEP(r_alloc_character(2));
  r_chr_poke(names, 0, strings_key);
  r_chr_poke(names, 1, strings_loc);

  r_attrib_poke(out, r_syms.names, names);

  out = new_data_frame(out, n_groups);

  FREE(6);
  return out;
}

// -----------------------------------------------------------------------------

/*
 * Returns a list of size two.
 * - The first element of the list contains the ordering as an integer vector.
 * - The second element of the list contains the group sizes as an integer
 *   vector.
 */
// [[ include("order-radix.h") ]]
SEXP vec_order_info(SEXP x,
                    SEXP direction,
                    SEXP na_value,
                    bool nan_distinct,
                    SEXP chr_transform,
                    bool chr_ordered) {
  const bool group_sizes = true;
  return vec_order_info_impl(x, direction, na_value, nan_distinct, chr_transform, chr_ordered, group_sizes);
}

// [[ register() ]]
SEXP vctrs_order_info(SEXP x,
                      SEXP direction,
                      SEXP na_value,
                      SEXP nan_distinct,
                      SEXP chr_transform,
                      SEXP chr_ordered) {
  bool c_nan_distinct = parse_nan_distinct(nan_distinct);
  bool c_chr_ordered = r_bool_as_int(chr_ordered);
  return vec_order_info(x, direction, na_value, c_nan_distinct, chr_transform, c_chr_ordered);
}

static inline size_t vec_compute_n_bytes_lazy_raw(SEXP x, const enum vctrs_type type);
static inline size_t vec_compute_n_bytes_lazy_counts(SEXP x, const enum vctrs_type type);

static SEXP parse_na_value(SEXP na_value);
static SEXP parse_direction(SEXP direction);
static SEXP vec_order_expand_args(SEXP x, SEXP decreasing, SEXP na_largest);
static SEXP vec_order_compute_na_last(SEXP na_largest, SEXP decreasing);

static void vec_order_switch(SEXP x,
                             SEXP decreasing,
                             SEXP na_last,
                             bool nan_distinct,
                             bool chr_ordered,
                             r_ssize size,
                             const enum vctrs_type type,
                             struct order* p_order,
                             struct lazy_raw* p_lazy_x_chunk,
                             struct lazy_raw* p_lazy_x_aux,
                             struct lazy_raw* p_lazy_o_aux,
                             struct lazy_raw* p_lazy_bytes,
                             struct lazy_raw* p_lazy_counts,
                             struct group_infos* p_group_infos,
                             struct truelength_info* p_truelength_info);

static
SEXP vec_order_info_impl(SEXP x,
                         SEXP direction,
                         SEXP na_value,
                         bool nan_distinct,
                         SEXP chr_transform,
                         bool chr_ordered,
                         bool group_sizes) {
  int n_prot = 0;

  SEXP decreasing = PROTECT_N(parse_direction(direction), &n_prot);
  SEXP na_largest = PROTECT_N(parse_na_value(na_value), &n_prot);

  // Call on `x` before potentially flattening cols with `vec_proxy_order()`
  SEXP args = PROTECT_N(vec_order_expand_args(x, decreasing, na_largest), &n_prot);
  R_len_t arg_size = vec_size_common(args, 0);
  args = PROTECT_N(vec_recycle_common(args, arg_size), &n_prot);

  decreasing = VECTOR_ELT(args, 0);
  na_largest = VECTOR_ELT(args, 1);

  SEXP na_last = PROTECT_N(vec_order_compute_na_last(na_largest, decreasing), &n_prot);

  SEXP proxy = PROTECT_N(vec_proxy_order(x), &n_prot);
  proxy = PROTECT_N(vec_normalize_encoding(proxy), &n_prot);
  proxy = PROTECT_N(proxy_chr_transform(proxy, chr_transform), &n_prot);

  r_ssize size = vec_size(proxy);
  const enum vctrs_type type = vec_proxy_typeof(proxy);

  // Compute the maximum size required for auxiliary working memory
  const size_t n_bytes_lazy_raw = vec_compute_n_bytes_lazy_raw(proxy, type);

  // Auxiliary vectors to hold intermediate results while ordering.
  // If `x` is a data frame we allocate enough room for the largest column type.
  struct lazy_raw* p_lazy_x_chunk = new_lazy_raw(size, n_bytes_lazy_raw);
  PROTECT_LAZY_VEC(p_lazy_x_chunk, &n_prot);

  struct lazy_raw* p_lazy_x_aux = new_lazy_raw(size, n_bytes_lazy_raw);
  PROTECT_LAZY_VEC(p_lazy_x_aux, &n_prot);

  struct lazy_raw* p_lazy_o_aux = new_lazy_raw(size, sizeof(int));
  PROTECT_LAZY_VEC(p_lazy_o_aux, &n_prot);

  struct lazy_raw* p_lazy_bytes = new_lazy_raw(size, sizeof(uint8_t));
  PROTECT_LAZY_VEC(p_lazy_bytes, &n_prot);

  // Compute the maximum size of the `counts` vector needed during radix
  // ordering. 4 * 256 for integers, 8 * 256 for doubles.
  size_t n_bytes_lazy_counts = vec_compute_n_bytes_lazy_counts(proxy, type);
  r_ssize size_lazy_counts = UINT8_MAX_SIZE * n_bytes_lazy_counts;

  struct lazy_raw* p_lazy_counts = new_lazy_raw(size_lazy_counts, sizeof(r_ssize));
  PROTECT_LAZY_VEC(p_lazy_counts, &n_prot);

  // Determine if group tracking can be turned off.
  // We turn if off if ordering non-data frame input as long as
  // locations haven't been requested by the user.
  // It is more efficient to ignore it when possible.
  bool force_groups = group_sizes;
  bool ignore_groups = force_groups ? false : (is_data_frame(proxy) ? false : true);

  // Construct the two sets of group info needed for tracking groups.
  // We switch between them after each data frame column is processed.
  struct group_info* p_group_info0 = new_group_info();
  PROTECT_GROUP_INFO(p_group_info0, &n_prot);

  struct group_info* p_group_info1 = new_group_info();
  PROTECT_GROUP_INFO(p_group_info1, &n_prot);

  struct group_infos* p_group_infos = new_group_infos(
    p_group_info0,
    p_group_info1,
    size,
    force_groups,
    ignore_groups
  );
  PROTECT_GROUP_INFOS(p_group_infos, &n_prot);

  // Used for character ordering - lazily generated to be fast
  // when not ordering character vectors
  struct truelength_info* p_truelength_info = new_truelength_info(size);
  PROTECT_TRUELENGTH_INFO(p_truelength_info, &n_prot);

  struct order* p_order = new_order(size);
  PROTECT_ORDER(p_order, &n_prot);

  vec_order_switch(
    proxy,
    decreasing,
    na_last,
    nan_distinct,
    chr_ordered,
    size,
    type,
    p_order,
    p_lazy_x_chunk,
    p_lazy_x_aux,
    p_lazy_o_aux,
    p_lazy_bytes,
    p_lazy_counts,
    p_group_infos,
    p_truelength_info
  );

  SEXP out = PROTECT_N(r_alloc_list(2), &n_prot);
  r_list_poke(out, 0, p_order->data);

  if (group_sizes) {
    struct group_info* p_group_info = groups_current(p_group_infos);
    SEXP sizes = p_group_info->data;
    sizes = r_int_resize(sizes, p_group_info->n_groups);
    r_list_poke(out, 1, sizes);
  }

  UNPROTECT(n_prot);
  return out;
}

// -----------------------------------------------------------------------------

static void df_order(SEXP x,
                     SEXP decreasing,
                     SEXP na_last,
                     bool nan_distinct,
                     bool chr_ordered,
                     r_ssize size,
                     struct order* p_order,
                     struct lazy_raw* p_lazy_x_chunk,
                     struct lazy_raw* p_lazy_x_aux,
                     struct lazy_raw* p_lazy_o_aux,
                     struct lazy_raw* p_lazy_bytes,
                     struct lazy_raw* p_lazy_counts,
                     struct group_infos* p_group_infos,
                     struct truelength_info* p_truelength_info);

static void vec_order_base_switch(SEXP x,
                                  bool decreasing,
                                  bool na_last,
                                  bool nan_distinct,
                                  bool chr_ordered,
                                  r_ssize size,
                                  const enum vctrs_type type,
                                  struct order* p_order,
                                  struct lazy_raw* p_lazy_x_chunk,
                                  struct lazy_raw* p_lazy_x_aux,
                                  struct lazy_raw* p_lazy_o_aux,
                                  struct lazy_raw* p_lazy_bytes,
                                  struct lazy_raw* p_lazy_counts,
                                  struct group_infos* p_group_infos,
                                  struct truelength_info* p_truelength_info);

static
void vec_order_switch(SEXP x,
                      SEXP decreasing,
                      SEXP na_last,
                      bool nan_distinct,
                      bool chr_ordered,
                      r_ssize size,
                      const enum vctrs_type type,
                      struct order* p_order,
                      struct lazy_raw* p_lazy_x_chunk,
                      struct lazy_raw* p_lazy_x_aux,
                      struct lazy_raw* p_lazy_o_aux,
                      struct lazy_raw* p_lazy_bytes,
                      struct lazy_raw* p_lazy_counts,
                      struct group_infos* p_group_infos,
                      struct truelength_info* p_truelength_info) {
  if (type == vctrs_type_dataframe) {
    df_order(
      x,
      decreasing,
      na_last,
      nan_distinct,
      chr_ordered,
      size,
      p_order,
      p_lazy_x_chunk,
      p_lazy_x_aux,
      p_lazy_o_aux,
      p_lazy_bytes,
      p_lazy_counts,
      p_group_infos,
      p_truelength_info
    );

    return;
  }

  if (r_length(decreasing) != 1) {
    Rf_errorcall(
      R_NilValue,
      "Internal error: Size of decreasing != 1, but "
      "`vec_order_expand_args()` didn't catch it."
    );
  }

  if (r_length(na_last) != 1) {
    Rf_errorcall(
      R_NilValue,
      "Internal error: Size of na_last != 1, but "
      "`vec_order_expand_args()` didn't catch it."
    );
  }

  bool c_decreasing = LOGICAL(decreasing)[0];
  bool c_na_last = LOGICAL(na_last)[0];

  vec_order_base_switch(
    x,
    c_decreasing,
    c_na_last,
    nan_distinct,
    chr_ordered,
    size,
    type,
    p_order,
    p_lazy_x_chunk,
    p_lazy_x_aux,
    p_lazy_o_aux,
    p_lazy_bytes,
    p_lazy_counts,
    p_group_infos,
    p_truelength_info
  );
}

// -----------------------------------------------------------------------------

static void int_order(SEXP x,
                      bool decreasing,
                      bool na_last,
                      r_ssize size,
                      struct order* p_order,
                      struct lazy_raw* p_lazy_x_chunk,
                      struct lazy_raw* p_lazy_x_aux,
                      struct lazy_raw* p_lazy_o_aux,
                      struct lazy_raw* p_lazy_bytes,
                      struct lazy_raw* p_lazy_counts,
                      struct group_infos* p_group_infos);

static void lgl_order(SEXP x,
                      bool decreasing,
                      bool na_last,
                      r_ssize size,
                      struct order* p_order,
                      struct lazy_raw* p_lazy_x_chunk,
                      struct lazy_raw* p_lazy_x_aux,
                      struct lazy_raw* p_lazy_o_aux,
                      struct lazy_raw* p_lazy_bytes,
                      struct lazy_raw* p_lazy_counts,
                      struct group_infos* p_group_infos);

static void dbl_order(SEXP x,
                      bool decreasing,
                      bool na_last,
                      bool nan_distinct,
                      r_ssize size,
                      struct order* p_order,
                      struct lazy_raw* p_lazy_x_chunk,
                      struct lazy_raw* p_lazy_x_aux,
                      struct lazy_raw* p_lazy_o_aux,
                      struct lazy_raw* p_lazy_bytes,
                      struct lazy_raw* p_lazy_counts,
                      struct group_infos* p_group_infos);

static void cpl_order(SEXP x,
                      bool decreasing,
                      bool na_last,
                      bool nan_distinct,
                      r_ssize size,
                      struct order* p_order,
                      struct lazy_raw* p_lazy_x_chunk,
                      struct lazy_raw* p_lazy_x_aux,
                      struct lazy_raw* p_lazy_o_aux,
                      struct lazy_raw* p_lazy_bytes,
                      struct lazy_raw* p_lazy_counts,
                      struct group_infos* p_group_infos);

static void chr_order(SEXP x,
                      bool decreasing,
                      bool na_last,
                      r_ssize size,
                      struct order* p_order,
                      struct lazy_raw* p_lazy_x_chunk,
                      struct lazy_raw* p_lazy_x_aux,
                      struct lazy_raw* p_lazy_o_aux,
                      struct lazy_raw* p_lazy_bytes,
                      struct lazy_raw* p_lazy_counts,
                      struct group_infos* p_group_infos,
                      struct truelength_info* p_truelength_info);

static void chr_appearance(SEXP x,
                           bool decreasing,
                           bool na_last,
                           r_ssize size,
                           struct order* p_order,
                           struct lazy_raw* p_lazy_x_chunk,
                           struct lazy_raw* p_lazy_x_aux,
                           struct lazy_raw* p_lazy_o_aux,
                           struct lazy_raw* p_lazy_bytes,
                           struct lazy_raw* p_lazy_counts,
                           struct group_infos* p_group_infos,
                           struct truelength_info* p_truelength_info);

// Used on bare vectors and the first column of data frame `x`s
static
void vec_order_base_switch(SEXP x,
                           bool decreasing,
                           bool na_last,
                           bool nan_distinct,
                           bool chr_ordered,
                           r_ssize size,
                           const enum vctrs_type type,
                           struct order* p_order,
                           struct lazy_raw* p_lazy_x_chunk,
                           struct lazy_raw* p_lazy_x_aux,
                           struct lazy_raw* p_lazy_o_aux,
                           struct lazy_raw* p_lazy_bytes,
                           struct lazy_raw* p_lazy_counts,
                           struct group_infos* p_group_infos,
                           struct truelength_info* p_truelength_info) {
  switch (type) {
  case vctrs_type_integer: {
    int_order(
      x,
      decreasing,
      na_last,
      size,
      p_order,
      p_lazy_x_chunk,
      p_lazy_x_aux,
      p_lazy_o_aux,
      p_lazy_bytes,
      p_lazy_counts,
      p_group_infos
    );

    break;
  }
  case vctrs_type_logical: {
    lgl_order(
      x,
      decreasing,
      na_last,
      size,
      p_order,
      p_lazy_x_chunk,
      p_lazy_x_aux,
      p_lazy_o_aux,
      p_lazy_bytes,
      p_lazy_counts,
      p_group_infos
    );

    break;
  }
  case vctrs_type_double: {
    dbl_order(
      x,
      decreasing,
      na_last,
      nan_distinct,
      size,
      p_order,
      p_lazy_x_chunk,
      p_lazy_x_aux,
      p_lazy_o_aux,
      p_lazy_bytes,
      p_lazy_counts,
      p_group_infos
    );

    break;
  }
  case vctrs_type_complex: {
    cpl_order(
      x,
      decreasing,
      na_last,
      nan_distinct,
      size,
      p_order,
      p_lazy_x_chunk,
      p_lazy_x_aux,
      p_lazy_o_aux,
      p_lazy_bytes,
      p_lazy_counts,
      p_group_infos
    );

    break;
  }
  case vctrs_type_character: {
    if (chr_ordered) {
      chr_order(
        x,
        decreasing,
        na_last,
        size,
        p_order,
        p_lazy_x_chunk,
        p_lazy_x_aux,
        p_lazy_o_aux,
        p_lazy_bytes,
        p_lazy_counts,
        p_group_infos,
        p_truelength_info
      );
    } else {
      chr_appearance(
        x,
        decreasing,
        na_last,
        size,
        p_order,
        p_lazy_x_chunk,
        p_lazy_x_aux,
        p_lazy_o_aux,
        p_lazy_bytes,
        p_lazy_counts,
        p_group_infos,
        p_truelength_info
      );
    }

    break;
  }
  case vctrs_type_dataframe: {
    Rf_errorcall(R_NilValue, "Internal error: Data frames should have been handled by now");
  }
  default: {
    Rf_errorcall(R_NilValue, "This type is not supported by `vec_order()`.");
  }
  }
}

// -----------------------------------------------------------------------------

static void int_order_chunk_impl(bool decreasing,
                                 bool na_last,
                                 r_ssize size,
                                 void* p_x,
                                 int* p_o,
                                 struct lazy_raw* p_lazy_x_aux,
                                 struct lazy_raw* p_lazy_o_aux,
                                 struct lazy_raw* p_lazy_bytes,
                                 struct lazy_raw* p_lazy_counts,
                                 struct group_infos* p_group_infos);

/*
 * These are the main entry points for integer ordering. They are nearly
 * identical except `int_order()` assumes that `p_x` cannot be
 * modified directly and is user input.
 *
 * `int_order_chunk()` assumes `p_x` is modifiable by reference. It is called
 * when iterating over data frame columns and `p_x` is the 2nd or greater
 * column, in which case `p_x` is really a chunk of that column that has been
 * copied into `x_chunk`.
 *
 * `int_order()` assumes `p_x` is user input which cannot be modified.
 * It copies `x` into another SEXP that can be modified directly unless a
 * counting sort is going to be used, in which case `p_x` can be used directly.
 */
static
void int_order_chunk(bool decreasing,
                     bool na_last,
                     r_ssize size,
                     int* p_o,
                     struct lazy_raw* p_lazy_x_chunk,
                     struct lazy_raw* p_lazy_x_aux,
                     struct lazy_raw* p_lazy_o_aux,
                     struct lazy_raw* p_lazy_bytes,
                     struct lazy_raw* p_lazy_counts,
                     struct group_infos* p_group_infos) {
  void* p_x_chunk = p_lazy_x_chunk->p_data;

  const enum vctrs_sortedness sortedness = int_sortedness(
    p_x_chunk,
    size,
    decreasing,
    na_last,
    p_group_infos
  );

  if (sortedness != VCTRS_SORTEDNESS_unsorted) {
    ord_resolve_sortedness_chunk(sortedness, size, p_o);
    return;
  }

  int_order_chunk_impl(
    decreasing,
    na_last,
    size,
    p_x_chunk,
    p_o,
    p_lazy_x_aux,
    p_lazy_o_aux,
    p_lazy_bytes,
    p_lazy_counts,
    p_group_infos
  );
}

static void int_order_impl(const int* p_x,
                           bool decreasing,
                           bool na_last,
                           r_ssize size,
                           bool copy,
                           struct order* p_order,
                           struct lazy_raw* p_lazy_x_chunk,
                           struct lazy_raw* p_lazy_x_aux,
                           struct lazy_raw* p_lazy_o_aux,
                           struct lazy_raw* p_lazy_bytes,
                           struct lazy_raw* p_lazy_counts,
                           struct group_infos* p_group_infos);

static
void int_order(SEXP x,
               bool decreasing,
               bool na_last,
               r_ssize size,
               struct order* p_order,
               struct lazy_raw* p_lazy_x_chunk,
               struct lazy_raw* p_lazy_x_aux,
               struct lazy_raw* p_lazy_o_aux,
               struct lazy_raw* p_lazy_bytes,
               struct lazy_raw* p_lazy_counts,
               struct group_infos* p_group_infos) {
  const int* p_x = INTEGER_RO(x);

  const enum vctrs_sortedness sortedness = int_sortedness(
    p_x,
    size,
    decreasing,
    na_last,
    p_group_infos
  );

  // Handle sorted cases and set ordering to initialized
  if (sortedness != VCTRS_SORTEDNESS_unsorted) {
    int* p_o = p_order->p_data;
    ord_resolve_sortedness(sortedness, size, p_o);
    p_order->initialized = true;
    return;
  }

  int_order_impl(
    p_x,
    decreasing,
    na_last,
    size,
    true,
    p_order,
    p_lazy_x_chunk,
    p_lazy_x_aux,
    p_lazy_o_aux,
    p_lazy_bytes,
    p_lazy_counts,
    p_group_infos
  );
}

static void int_adjust(const bool decreasing,
                       const bool na_last,
                       const r_ssize size,
                       void* p_x);

static void int_compute_range(const int* p_x,
                              r_ssize size,
                              int* p_x_min,
                              uint32_t* p_range);

static void int_order_counting(const int* p_x,
                               r_ssize size,
                               int x_min,
                               uint32_t range,
                               bool initialized,
                               bool decreasing,
                               bool na_last,
                               int* p_o,
                               int* p_o_aux,
                               struct group_infos* p_group_infos);

static void int_order_insertion(const r_ssize size,
                                uint32_t* p_x,
                                int* p_o,
                                struct group_infos* p_group_infos);

static void int_order_radix(const r_ssize size,
                            uint32_t* p_x,
                            int* p_o,
                            uint32_t* p_x_aux,
                            int* p_o_aux,
                            uint8_t* p_bytes,
                            r_ssize* p_counts,
                            struct group_infos* p_group_infos);

/*
 * `int_order_chunk_impl()` is used by both `int_order_chunk()` and by
 * `chr_order_chunk()`
 */
static
void int_order_chunk_impl(bool decreasing,
                          bool na_last,
                          r_ssize size,
                          void* p_x,
                          int* p_o,
                          struct lazy_raw* p_lazy_x_aux,
                          struct lazy_raw* p_lazy_o_aux,
                          struct lazy_raw* p_lazy_bytes,
                          struct lazy_raw* p_lazy_counts,
                          struct group_infos* p_group_infos) {
  if (size <= ORDER_INSERTION_BOUNDARY) {
    int_adjust(decreasing, na_last, size, p_x);
    int_order_insertion(size, p_x, p_o, p_group_infos);
    return;
  }

  int* p_o_aux = (int*) init_lazy_raw(p_lazy_o_aux);

  uint32_t range;
  int x_min;

  int_compute_range(p_x, size, &x_min, &range);

  /*
   * If in counting order range and on the second or higher column, we will
   * need `p_o_aux` as working memory. At this point, `p_o` will have been
   * initialized from ordering the first column.
   */
  if (range < INT_ORDER_COUNTING_RANGE_BOUNDARY) {
    const bool initialized = true;

    int_order_counting(
      p_x,
      size,
      x_min,
      range,
      initialized,
      decreasing,
      na_last,
      p_o,
      p_o_aux,
      p_group_infos
    );

    return;
  }

  uint32_t* p_x_aux = (uint32_t*) init_lazy_raw(p_lazy_x_aux);

  uint8_t* p_bytes = (uint8_t*) init_lazy_raw(p_lazy_bytes);

  r_ssize* p_counts = (r_ssize*) init_lazy_raw(p_lazy_counts);
  memset(p_counts, 0, p_lazy_counts->size);

  int_adjust(decreasing, na_last, size, p_x);

  int_order_radix(
    size,
    p_x,
    p_o,
    p_x_aux,
    p_o_aux,
    p_bytes,
    p_counts,
    p_group_infos
  );
}

/*
 * `int_order_impl()` is used by both `int_order()` and by
 * `chr_order()`.
 *
 * For `chr_order()`, the TRUELENGTH values will already be extracted into
 * `p_lazy_x_chunk`. In this case, we set `copy = false` to tell
 * `int_order_impl()` to use `p_lazy_x_chunk` directly rather than copying `p_x`
 * over to `p_x_chunk`.
 */
static
void int_order_impl(const int* p_x,
                    bool decreasing,
                    bool na_last,
                    r_ssize size,
                    bool copy,
                    struct order* p_order,
                    struct lazy_raw* p_lazy_x_chunk,
                    struct lazy_raw* p_lazy_x_aux,
                    struct lazy_raw* p_lazy_o_aux,
                    struct lazy_raw* p_lazy_bytes,
                    struct lazy_raw* p_lazy_counts,
                    struct group_infos* p_group_infos) {
  if (size <= ORDER_INSERTION_BOUNDARY) {
    int* p_o = init_order(p_order);

    void* p_x_chunk;
    if (copy) {
      p_x_chunk = init_lazy_raw(p_lazy_x_chunk);
      memcpy(p_x_chunk, p_x, size * sizeof(*p_x));
    } else {
      p_x_chunk = p_lazy_x_chunk->p_data;
    }

    int_adjust(decreasing, na_last, size, p_x_chunk);

    int_order_insertion(size, p_x_chunk, p_o, p_group_infos);

    return;
  }

  uint32_t range;
  int x_min;

  int_compute_range(p_x, size, &x_min, &range);

  /*
   * If in counting order range and on the first column / single vector,
   * `p_o_aux` won't be used, so no need to initialize it.
   *
   * Also, `p_o` will be filled directly, so for performance we don't
   * initialize its order.
   */
  if (range < INT_ORDER_COUNTING_RANGE_BOUNDARY) {
    const bool initialized = false;

    int* p_o = p_order->p_data;
    int* p_o_aux = (int*) p_lazy_o_aux->p_data;

    int_order_counting(
      p_x,
      size,
      x_min,
      range,
      initialized,
      decreasing,
      na_last,
      p_o,
      p_o_aux,
      p_group_infos
    );

    p_order->initialized = true;
    return;
  }

  int* p_o = init_order(p_order);

  int* p_o_aux = (int*) init_lazy_raw(p_lazy_o_aux);

  uint32_t* p_x_aux = (uint32_t*) init_lazy_raw(p_lazy_x_aux);

  uint8_t* p_bytes = (uint8_t*) init_lazy_raw(p_lazy_bytes);

  r_ssize* p_counts = (r_ssize*) init_lazy_raw(p_lazy_counts);
  memset(p_counts, 0, p_lazy_counts->size);

  void* p_x_chunk;
  if (copy) {
    p_x_chunk = init_lazy_raw(p_lazy_x_chunk);
    memcpy(p_x_chunk, p_x, size * sizeof(*p_x));
  } else {
    p_x_chunk = p_lazy_x_chunk->p_data;
  }

  int_adjust(decreasing, na_last, size, p_x_chunk);

  int_order_radix(
    size,
    p_x_chunk,
    p_o,
    p_x_aux,
    p_o_aux,
    p_bytes,
    p_counts,
    p_group_infos
  );
}

// -----------------------------------------------------------------------------

static inline uint32_t int_map_to_uint32(int x);

/*
 * - Shifts the integer elements of `p_x` in a way that correctly maintains
 *   ordering for `na_last` and `decreasing`
 *
 * - After shifting, also maps the elements from `int32_t` to `uint32_t` and
 *   stores them back in `p_x`.
 *
 * - Used before both the integer insertion sort and radix sort, which both
 *   expect their input to already have been "adjusted" for `na_last` and
 *   `decreasing` and expect a `uint32_t` pointer input.
 *
 * - If `na_last = true`, `NA` is always the maximum element, so we set it to
 *   `UINT32_MAX`. In that case, we also shift all non-NA values down by 1 to
 *   make room for it (defined by `na_shift`).
 *
 * - If `na_last = false`, we set `NA` to the minimum element of 0.
 *
 * - The multiplication by `direction` applies to non-NA values and correctly
 *   orders inputs based on whether we are in a decreasing order or not.
 */
static
void int_adjust(const bool decreasing,
                const bool na_last,
                const r_ssize size,
                void* p_x) {
  const int direction = decreasing ? -1 : 1;
  const uint32_t na_u32 = na_last ? UINT32_MAX : 0;
  const int na_shift = na_last ? -1 : 0;

  const int* p_x_int = (const int*) p_x;
  uint32_t* p_x_u32 = (uint32_t*) p_x;

  for (r_ssize i = 0; i < size; ++i) {
    int elt = p_x_int[i];

    if (elt == NA_INTEGER) {
      p_x_u32[i] = na_u32;
      continue;
    }

    elt = elt * direction + na_shift;

    p_x_u32[i] = int_map_to_uint32(elt);
  }
}


#define HEX_UINT32_SIGN_BIT 0x80000000u

// Flipping the sign bit is all we need to do to map in an order preserving way.
// [INT32_MIN, INT32_MAX] => [0, UINT32_MAX]
static inline uint32_t int_map_to_uint32(int x) {
  return ((uint32_t) x) ^ HEX_UINT32_SIGN_BIT;
}

#undef HEX_UINT32_SIGN_BIT

// -----------------------------------------------------------------------------

/*
 * `int_compute_range()` computes the range of all values in `p_x`.
 * It is used by counting sort to computes buckets with `p_x[i] - x_min`.
 *
 * - `p_range` and `p_x_min` are updated on the way out to retain both the
 *   range and the minimum value.
 *
 * - `NA` values are skipped over. If all values are `NA`, we defer to radix
 *   sort (which definitely can handle that case) by returning a `range` of the
 *   maximum uint32 value (which will be greater than
 *   INT_ORDER_COUNTING_RANGE_BOUNDARY).
 */
static
void int_compute_range(const int* p_x,
                       r_ssize size,
                       int* p_x_min,
                       uint32_t* p_range) {
  uint32_t range = UINT32_MAX;

  int x_min = NA_INTEGER;
  int x_max = NA_INTEGER;

  r_ssize i = 0;

  // Find first non-NA value
  while (i < size) {
    const int elt = p_x[i];

    if (elt == NA_INTEGER) {
      ++i;
      continue;
    }

    x_min = elt;
    x_max = elt;
    range = 0;

    // Bump to next `i` since we know this one's value
    ++i;

    break;
  }

  // All NAs - Return max range to signal to use radix sort
  if (x_min == NA_INTEGER) {
    *p_x_min = x_min;
    *p_range = range;
    return;
  }

  // Now that we have initial values, iterate through the rest
  // to compute the final min/max.
  for (r_ssize j = i; j < size; ++j) {
    const int elt = p_x[j];

    if (elt == NA_INTEGER) {
      continue;
    }

    if (elt > x_max) {
      x_max = elt;
    } else if (elt < x_min) {
      x_min = elt;
    }
  }

  /*
   * - Max possible range is from
   *   `c(.Machine$integer.max, -.Machine$integer.max)` which is exactly the
   *   max of a `uint32_t`.
   * - We need to go up to `intmax_t` to avoid intermediate overflow.
   * - `+ 1` to get an inclusive range on both ends.
   */
  range = (uint32_t) intmax_add(intmax_subtract(x_max, x_min), 1);

  *p_x_min = x_min;
  *p_range = range;
}

// -----------------------------------------------------------------------------

/*
 * The counting sort expects `p_x` to be unadjusted (i.e. `int_adjust()` has
 * not been used). It handles `decreasing` and `na_last` internally.
 *
 * Counting sort is used when `p_x` has a range less than
 * `INT_ORDER_COUNTING_RANGE_BOUNDARY`. In these cases radix sort
 * doesn't spread out values as much when looking at individual radixes.
 *
 * Counting sort does not modify `p_x` in any way.
 */
static
void int_order_counting(const int* p_x,
                        r_ssize size,
                        int x_min,
                        uint32_t range,
                        bool initialized,
                        bool decreasing,
                        bool na_last,
                        int* p_o,
                        int* p_o_aux,
                        struct group_infos* p_group_infos) {
  // - Only allocate this once (counts are reset to 0 at end)
  // - Allocating as static allows us to allocate an array this large
  // - `+ 1` to ensure there is room for the extra `NA` bucket
  static r_ssize p_counts[INT_ORDER_COUNTING_RANGE_BOUNDARY + 1] = { 0 };

  // `NA` values get counted in the last used bucket
  uint32_t na_bucket = range;
  r_ssize na_count = 0;

  // Sanity check
  if (range > INT_ORDER_COUNTING_RANGE_BOUNDARY) {
    Rf_errorcall(R_NilValue, "Internal error: `range > INT_ORDER_COUNTING_RANGE_BOUNDARY`.");
  }

  // Histogram pass
  for (r_ssize i = 0; i < size; ++i) {
    const int elt = p_x[i];

    if (elt == NA_INTEGER) {
      ++na_count;
    } else {
      const uint32_t bucket = elt - x_min;
      ++p_counts[bucket];
    }
  }

  // Add `NA` counts once at the end
  p_counts[na_bucket] = na_count;

  r_ssize cumulative = 0;

  // Handle decreasing/increasing by altering the order in which
  // counts are accumulated
  const int direction = decreasing ? -1 : 1;
  r_ssize j = decreasing ? range - 1 : 0;

  // `na_last = false` pushes NA counts to the front
  if (!na_last && na_count != 0) {
    p_counts[na_bucket] = cumulative;
    cumulative += na_count;
    groups_size_maybe_push(na_count, p_group_infos);
  }

  // Accumulate counts, skip zeros
  for (uint32_t i = 0; i < range; ++i) {
    r_ssize count = p_counts[j];

    if (count == 0) {
      j += direction;
      continue;
    }

    // Insert current cumulative value, then increment
    p_counts[j] = cumulative;
    cumulative += count;

    // At this point we will handle this group completely
    groups_size_maybe_push(count, p_group_infos);

    j += direction;
  }

  // `na_last = true` pushes NA counts to the back
  if (na_last && na_count != 0) {
    p_counts[na_bucket] = cumulative;
    groups_size_maybe_push(na_count, p_group_infos);
  }

  // If order is not initialized, we are on the first column / atomic vector
  // and can place the order directly into the result. Much faster than
  // initializing, placing in `p_o_aux`, and copying back over.
  if (initialized) {
    for (r_ssize i = 0; i < size; ++i) {
      const int elt = p_x[i];
      uint32_t bucket = (elt == NA_INTEGER) ? na_bucket : elt - x_min;
      const r_ssize loc = p_counts[bucket]++;
      p_o_aux[loc] = p_o[i];
    }

    memcpy(p_o, p_o_aux, size * sizeof(*p_o_aux));
  } else {
    for (r_ssize i = 0; i < size; ++i) {
      const int elt = p_x[i];
      uint32_t bucket = (elt == NA_INTEGER) ? na_bucket : elt - x_min;
      const r_ssize loc = p_counts[bucket]++;
      p_o[loc] = i + 1;
    }
  }

  // Reset counts for next column.
  // Only reset what we might have touched.
  // `+ 1` to reset the NA bucket too.
  memset(p_counts, 0, (range + 1) * sizeof(r_ssize));
}

// -----------------------------------------------------------------------------

/*
 * `int_order_insertion()` is used in two ways:
 * - It is how we "finish off" radix sorts rather than deep recursion.
 * - If we have an original `x` input that is small enough, we just immediately
 *   insertion sort it.
 *
 * For small inputs, it is much faster than deeply recursing with
 * radix ordering.
 *
 * Insertion ordering expects that `p_x` has been adjusted with `int_adjust()`
 * which takes care of `na_last` and `decreasing` and also maps `int32_t` to
 * `uint32_t` ahead of time.
 */
static
void int_order_insertion(const r_ssize size,
                         uint32_t* p_x,
                         int* p_o,
                         struct group_infos* p_group_infos) {
  // Don't think this can occur, but safer this way
  if (size == 0) {
    return;
  }

  for (r_ssize i = 1; i < size; ++i) {
    const uint32_t x_elt = p_x[i];
    const int o_elt = p_o[i];

    r_ssize j = i - 1;

    while (j >= 0) {
      const uint32_t x_cmp_elt = p_x[j];

      if (x_elt >= x_cmp_elt) {
        break;
      }

      int o_cmp_elt = p_o[j];

      // Swap
      p_x[j + 1] = x_cmp_elt;
      p_o[j + 1] = o_cmp_elt;

      // Next
      --j;
    }

    // Place original elements in new location
    // closer to start of the vector
    p_x[j + 1] = x_elt;
    p_o[j + 1] = o_elt;
  }

  // We've ordered a small chunk, we need to push at least one group size.
  // Depends on the post-ordered results so we have to do this
  // in a separate loop.
  r_ssize group_size = 1;
  uint32_t previous = p_x[0];

  for (r_ssize i = 1; i < size; ++i) {
    const uint32_t current = p_x[i];

    // Continue the current group run
    if (current == previous) {
      ++group_size;
      continue;
    }

    // Push current run size and reset size tracker
    groups_size_maybe_push(group_size, p_group_infos);
    group_size = 1;

    previous = current;
  }

  // Push final group run
  groups_size_maybe_push(group_size, p_group_infos);
}

// -----------------------------------------------------------------------------

static uint8_t int_compute_skips(const uint32_t* p_x, r_ssize size, bool* p_skips);

static void int_order_radix_recurse(const r_ssize size,
                                    const uint8_t pass,
                                    uint32_t* p_x,
                                    int* p_o,
                                    uint32_t* p_x_aux,
                                    int* p_o_aux,
                                    uint8_t* p_bytes,
                                    r_ssize* p_counts,
                                    bool* p_skips,
                                    struct group_infos* p_group_infos);

/*
 * Integer radix ordering entry point
 *
 * Expects that `int_adjust()` has been called on `p_x`, which takes care
 * of `na_last` and `decreasing` and also maps `int32_t` to `uint32_t` once
 * up front so we don't have to do it for each radix pass.
 *
 * Sorts `p_x` and `p_o` in place
 */
static
void int_order_radix(const r_ssize size,
                     uint32_t* p_x,
                     int* p_o,
                     uint32_t* p_x_aux,
                     int* p_o_aux,
                     uint8_t* p_bytes,
                     r_ssize* p_counts,
                     struct group_infos* p_group_infos) {
  bool p_skips[INT_MAX_RADIX_PASS];

  uint8_t pass = int_compute_skips(p_x, size, p_skips);

  // Skipped all passes - Happens when `x` is 1 value repeated
  if (pass == INT_MAX_RADIX_PASS) {
    groups_size_maybe_push(size, p_group_infos);
    return;
  }

  int_order_radix_recurse(
    size,
    pass,
    p_x,
    p_o,
    p_x_aux,
    p_o_aux,
    p_bytes,
    p_counts,
    p_skips,
    p_group_infos
  );
}

// -----------------------------------------------------------------------------

static inline uint8_t int_extract_uint32_byte(uint32_t x, uint8_t shift);

/*
 * Recursive function for radix ordering. Orders the current byte, then iterates
 * over the sub groups and recursively calls itself on each subgroup to order
 * the next byte.
 */
static
void int_order_radix_recurse(const r_ssize size,
                             const uint8_t pass,
                             uint32_t* p_x,
                             int* p_o,
                             uint32_t* p_x_aux,
                             int* p_o_aux,
                             uint8_t* p_bytes,
                             r_ssize* p_counts,
                             bool* p_skips,
                             struct group_infos* p_group_infos) {
  // Exit as fast as possible if we are below the insertion order boundary
  if (size <= ORDER_INSERTION_BOUNDARY) {
    int_order_insertion(size, p_x, p_o, p_group_infos);
    return;
  }

  // Skip passes where our up front check told us that all bytes were the same
  uint8_t next_pass = pass + 1;
  r_ssize* p_counts_next_pass = p_counts + UINT8_MAX_SIZE;

  while (next_pass < INT_MAX_RADIX_PASS && p_skips[next_pass]) {
    ++next_pass;
    p_counts_next_pass += UINT8_MAX_SIZE;
  }

  const uint8_t radix = PASS_TO_RADIX(pass, INT_MAX_RADIX_PASS);
  const uint8_t shift = radix * 8;

  uint8_t byte = 0;

  // Histogram for this pass
  for (r_ssize i = 0; i < size; ++i) {
    const uint32_t x_elt = p_x[i];

    byte = int_extract_uint32_byte(x_elt, shift);

    p_bytes[i] = byte;
    ++p_counts[byte];
  }

  // Fast check to see if all bytes were the same.
  // If so, skip this `pass` since we learned nothing.
  // No need to accumulate counts and iterate over chunks,
  // we know all others are zero.
  if (p_counts[byte] == size) {
    // Reset count for other group chunks
    p_counts[byte] = 0;

    if (next_pass == INT_MAX_RADIX_PASS) {
      // If we are already at the last pass, we are done
      groups_size_maybe_push(size, p_group_infos);
    } else {
      // Otherwise, recurse on next byte using the same `size` since
      // the group size hasn't changed
      int_order_radix_recurse(
        size,
        next_pass,
        p_x,
        p_o,
        p_x_aux,
        p_o_aux,
        p_bytes,
        p_counts_next_pass,
        p_skips,
        p_group_infos
      );
    }

    return;
  }

  r_ssize cumulative = 0;

  // Accumulate counts, skip zeros
  for (uint16_t i = 0; i < UINT8_MAX_SIZE; ++i) {
    r_ssize count = p_counts[i];

    if (count == 0) {
      continue;
    }

    // Replace with `cumulative` first, then bump `cumulative`.
    // `p_counts` now represents starting locations for each radix group.
    p_counts[i] = cumulative;
    cumulative += count;
  }

  // Place into auxiliary arrays in the correct order, then copy back over
  for (r_ssize i = 0; i < size; ++i) {
    const uint8_t byte = p_bytes[i];
    const r_ssize loc = p_counts[byte]++;
    p_o_aux[loc] = p_o[i];
    p_x_aux[loc] = p_x[i];
  }

  // Copy back over
  memcpy(p_o, p_o_aux, size * sizeof(*p_o_aux));
  memcpy(p_x, p_x_aux, size * sizeof(*p_x_aux));

  r_ssize last_cumulative_count = 0;

  // Recurse on subgroups as required
  for (uint16_t i = 0; last_cumulative_count < size && i < UINT8_MAX_SIZE; ++i) {
    const r_ssize cumulative_count = p_counts[i];

    if (!cumulative_count) {
      continue;
    }

    // Set to zero to clear for subsequent groups
    p_counts[i] = 0;

    // Diff the accumulated counts to get the radix group size
    const r_ssize group_size = cumulative_count - last_cumulative_count;
    last_cumulative_count = cumulative_count;

    if (group_size == 1) {
      groups_size_maybe_push(1, p_group_infos);
      ++p_x;
      ++p_o;
      continue;
    }

    // Can get here in the case of ties, like c(1L, 1L), which have a
    // `group_size` of 2 in the last radix, but there is nothing left to
    // compare so we are done.
    if (next_pass == INT_MAX_RADIX_PASS) {
      groups_size_maybe_push(group_size, p_group_infos);
      p_x += group_size;
      p_o += group_size;
      continue;
    }

    // Order next byte of this subgroup
    int_order_radix_recurse(
      group_size,
      next_pass,
      p_x,
      p_o,
      p_x_aux,
      p_o_aux,
      p_bytes,
      p_counts_next_pass,
      p_skips,
      p_group_infos
    );

    p_x += group_size;
    p_o += group_size;
  }
}

// -----------------------------------------------------------------------------

/*
 * Do a parallel histogram run over all 4 passes to determine if any passes
 * can be skipped (because all bytes were the same)
 */
static
uint8_t int_compute_skips(const uint32_t* p_x, r_ssize size, bool* p_skips) {
  uint8_t radix_start = PASS_TO_RADIX(0, INT_MAX_RADIX_PASS);
  uint8_t shift_start = radix_start * 8;

  for (uint8_t i = 0; i < INT_MAX_RADIX_PASS; ++i) {
    p_skips[i] = true;
  }

  uint8_t p_bytes[INT_MAX_RADIX_PASS];
  const uint32_t elt0 = p_x[0];

  // Get bytes of first element in MSD->LSD order.
  // Placed in `p_bytes` in a way that aligns with the `pass` variable
  for (uint8_t pass = 0, shift = shift_start;
       pass < INT_MAX_RADIX_PASS;
       ++pass, shift += SHIFT_ADJUSTMENT) {
    p_bytes[pass] = int_extract_uint32_byte(elt0, shift);
  }

  // Check to see which passes are skippable
  for (r_ssize i = 1; i < size; ++i) {
    uint8_t n_skips = INT_MAX_RADIX_PASS;
    const uint32_t elt = p_x[i];

    for (uint8_t pass = 0, shift = shift_start;
         pass < INT_MAX_RADIX_PASS;
         ++pass, shift += SHIFT_ADJUSTMENT) {
      bool skip = p_skips[pass];

      if (skip) {
        p_skips[pass] = (p_bytes[pass] == int_extract_uint32_byte(elt, shift));
      } else {
        --n_skips;
      }
    }

    // No passes are skippable
    if (n_skips == 0) {
      break;
    }
  }

  uint8_t pass = 0;

  // Shift forward to the first pass with varying bytes
  while (pass < INT_MAX_RADIX_PASS && p_skips[pass]) {
    ++pass;
  }

  return pass;
}

// -----------------------------------------------------------------------------

// Bytes will be extracted 8 bits at a time.
// This is a MSB radix sort, so they are extracted MSB->LSB.
static inline
uint8_t int_extract_uint32_byte(uint32_t x, uint8_t shift) {
  return (x >> shift) & UINT8_MAX;
}

// -----------------------------------------------------------------------------

/*
 * Entry points for logical ordering. These just use integer infrastructure.
 */
static
void lgl_order_chunk(bool decreasing,
                     bool na_last,
                     r_ssize size,
                     int* p_o,
                     struct lazy_raw* p_lazy_x_chunk,
                     struct lazy_raw* p_lazy_x_aux,
                     struct lazy_raw* p_lazy_o_aux,
                     struct lazy_raw* p_lazy_bytes,
                     struct lazy_raw* p_lazy_counts,
                     struct group_infos* p_group_infos) {
  int_order_chunk(
    decreasing,
    na_last,
    size,
    p_o,
    p_lazy_x_chunk,
    p_lazy_x_aux,
    p_lazy_o_aux,
    p_lazy_bytes,
    p_lazy_counts,
    p_group_infos
  );
}

static
void lgl_order(SEXP x,
               bool decreasing,
               bool na_last,
               r_ssize size,
               struct order* p_order,
               struct lazy_raw* p_lazy_x_chunk,
               struct lazy_raw* p_lazy_x_aux,
               struct lazy_raw* p_lazy_o_aux,
               struct lazy_raw* p_lazy_bytes,
               struct lazy_raw* p_lazy_counts,
               struct group_infos* p_group_infos) {
  int_order(
    x,
    decreasing,
    na_last,
    size,
    p_order,
    p_lazy_x_chunk,
    p_lazy_x_aux,
    p_lazy_o_aux,
    p_lazy_bytes,
    p_lazy_counts,
    p_group_infos
  );
}

// -----------------------------------------------------------------------------

static void dbl_order_chunk_impl(bool decreasing,
                                 bool na_last,
                                 bool nan_distinct,
                                 r_ssize size,
                                 void* p_x,
                                 int* p_o,
                                 struct lazy_raw* p_lazy_x_aux,
                                 struct lazy_raw* p_lazy_o_aux,
                                 struct lazy_raw* p_lazy_bytes,
                                 struct lazy_raw* p_lazy_counts,
                                 struct group_infos* p_group_infos);

/*
 * These are the main entry points for double ordering. They are nearly
 * identical except `dbl_order()` assumes that `p_x` cannot be
 * modified directly and is user input.
 *
 * `dbl_order_chunk()` assumes `p_x` is modifiable by reference. It is called
 * when iterating over data frame columns and `p_x` is the 2nd or greater
 * column, in which case `p_x` is really a chunk of that column that has been
 * copied into `x_chunk`.
 *
 * `dbl_order()` assumes `p_x` is user input which cannot be modified.
 * It copies `x` into another SEXP that can be modified directly.
 *
 * Unlike `int_order_chunk()`, there is no intermediate counting sort, as it is
 * sort of unclear how to compute the range of a double vector in the same
 * way, and even after adjusting to a `uint64_t`, it is unlikely that they
 * have a very small range of values.
 */
static
void dbl_order_chunk(bool decreasing,
                     bool na_last,
                     bool nan_distinct,
                     r_ssize size,
                     int* p_o,
                     struct lazy_raw* p_lazy_x_chunk,
                     struct lazy_raw* p_lazy_x_aux,
                     struct lazy_raw* p_lazy_o_aux,
                     struct lazy_raw* p_lazy_bytes,
                     struct lazy_raw* p_lazy_counts,
                     struct group_infos* p_group_infos) {
  void* p_x_chunk = p_lazy_x_chunk->p_data;

  dbl_order_chunk_impl(
    decreasing,
    na_last,
    nan_distinct,
    size,
    p_x_chunk,
    p_o,
    p_lazy_x_aux,
    p_lazy_o_aux,
    p_lazy_bytes,
    p_lazy_counts,
    p_group_infos
  );
}


static void dbl_order_impl(const double* p_x,
                           bool decreasing,
                           bool na_last,
                           bool nan_distinct,
                           r_ssize size,
                           bool copy,
                           struct order* p_order,
                           struct lazy_raw* p_lazy_x_chunk,
                           struct lazy_raw* p_lazy_x_aux,
                           struct lazy_raw* p_lazy_o_aux,
                           struct lazy_raw* p_lazy_bytes,
                           struct lazy_raw* p_lazy_counts,
                           struct group_infos* p_group_infos);

static
void dbl_order(SEXP x,
               bool decreasing,
               bool na_last,
               bool nan_distinct,
               r_ssize size,
               struct order* p_order,
               struct lazy_raw* p_lazy_x_chunk,
               struct lazy_raw* p_lazy_x_aux,
               struct lazy_raw* p_lazy_o_aux,
               struct lazy_raw* p_lazy_bytes,
               struct lazy_raw* p_lazy_counts,
               struct group_infos* p_group_infos) {
  const double* p_x = REAL_RO(x);

  dbl_order_impl(
    p_x,
    decreasing,
    na_last,
    nan_distinct,
    size,
    true,
    p_order,
    p_lazy_x_chunk,
    p_lazy_x_aux,
    p_lazy_o_aux,
    p_lazy_bytes,
    p_lazy_counts,
    p_group_infos
  );
}


static void dbl_adjust(const bool decreasing,
                       const bool na_last,
                       const bool nan_distinct,
                       const r_ssize size,
                       void* p_x);

static void dbl_order_insertion(const r_ssize size,
                                uint64_t* p_x,
                                int* p_o,
                                struct group_infos* p_group_infos);

static void dbl_order_radix(const r_ssize size,
                            uint64_t* p_x,
                            int* p_o,
                            uint64_t* p_x_aux,
                            int* p_o_aux,
                            uint8_t* p_bytes,
                            r_ssize* p_counts,
                            struct group_infos* p_group_infos);

/*
 * Used by `dbl_order_chunk()` and by `cpl_order()`
 *
 * Unlike `int_order_chunk_impl()`, `dbl_order_chunk_impl()` also deals with
 * sortedness since we don't have an up front sortedness check on complex
 * vectors.
 */
static
void dbl_order_chunk_impl(bool decreasing,
                          bool na_last,
                          bool nan_distinct,
                          r_ssize size,
                          void* p_x,
                          int* p_o,
                          struct lazy_raw* p_lazy_x_aux,
                          struct lazy_raw* p_lazy_o_aux,
                          struct lazy_raw* p_lazy_bytes,
                          struct lazy_raw* p_lazy_counts,
                          struct group_infos* p_group_infos) {
  const enum vctrs_sortedness sortedness = dbl_sortedness(
    p_x,
    size,
    decreasing,
    na_last,
    nan_distinct,
    p_group_infos
  );

  if (sortedness != VCTRS_SORTEDNESS_unsorted) {
    ord_resolve_sortedness_chunk(sortedness, size, p_o);
    return;
  }

  dbl_adjust(decreasing, na_last, nan_distinct, size, p_x);

  if (size <= ORDER_INSERTION_BOUNDARY) {
    dbl_order_insertion(size, p_x, p_o, p_group_infos);
    return;
  }

  uint64_t* p_x_aux = (uint64_t*) init_lazy_raw(p_lazy_x_aux);

  int* p_o_aux = (int*) init_lazy_raw(p_lazy_o_aux);

  uint8_t* p_bytes = (uint8_t*) init_lazy_raw(p_lazy_bytes);

  r_ssize* p_counts = (r_ssize*) init_lazy_raw(p_lazy_counts);
  memset(p_counts, 0, p_lazy_counts->size);

  dbl_order_radix(
    size,
    p_x,
    p_o,
    p_x_aux,
    p_o_aux,
    p_bytes,
    p_counts,
    p_group_infos
  );
}

/*
 * Used by `dbl_order()` and by `cpl_order()`
 *
 * Unlike `int_order_impl()`, `dbl_order_impl()` also deals with sortedness
 * since we don't have an up front sortedness check on complex vectors.
 *
 * When dealing with complex vectors, `p_x` and `p_lazy_x_chunk->p_data` will
 * already point to the same memory. In this case, we don't need to copy `p_x`
 * into `p_lazy_x_chunk`, so we set `copy = false` which tells
 * `dbl_order_impl()` to just use `p_lazy_x_chunk` directly.
 */
static
void dbl_order_impl(const double* p_x,
                    bool decreasing,
                    bool na_last,
                    bool nan_distinct,
                    r_ssize size,
                    bool copy,
                    struct order* p_order,
                    struct lazy_raw* p_lazy_x_chunk,
                    struct lazy_raw* p_lazy_x_aux,
                    struct lazy_raw* p_lazy_o_aux,
                    struct lazy_raw* p_lazy_bytes,
                    struct lazy_raw* p_lazy_counts,
                    struct group_infos* p_group_infos) {
  const enum vctrs_sortedness sortedness = dbl_sortedness(
    p_x,
    size,
    decreasing,
    na_last,
    nan_distinct,
    p_group_infos
  );

  // Handle sorted cases and set ordering to initialized
  if (sortedness != VCTRS_SORTEDNESS_unsorted) {
    int* p_o = p_order->p_data;
    ord_resolve_sortedness(sortedness, size, p_o);
    p_order->initialized = true;
    return;
  }

  int* p_o = init_order(p_order);

  void* p_x_chunk;
  if (copy) {
    p_x_chunk = init_lazy_raw(p_lazy_x_chunk);
    memcpy(p_x_chunk, p_x, size * sizeof(*p_x));
  } else {
    p_x_chunk = p_lazy_x_chunk->p_data;
  }

  dbl_adjust(decreasing, na_last, nan_distinct, size, p_x_chunk);

  if (size <= ORDER_INSERTION_BOUNDARY) {
    dbl_order_insertion(size, p_x_chunk, p_o, p_group_infos);
    return;
  }

  uint64_t* p_x_aux = (uint64_t*) init_lazy_raw(p_lazy_x_aux);

  int* p_o_aux = (int*) init_lazy_raw(p_lazy_o_aux);

  uint8_t* p_bytes = (uint8_t*) init_lazy_raw(p_lazy_bytes);

  r_ssize* p_counts = (r_ssize*) init_lazy_raw(p_lazy_counts);
  memset(p_counts, 0, p_lazy_counts->size);

  dbl_order_radix(
    size,
    p_x_chunk,
    p_o,
    p_x_aux,
    p_o_aux,
    p_bytes,
    p_counts,
    p_group_infos
  );
}

// -----------------------------------------------------------------------------

static inline void dbl_adjust_nan_identical(const bool decreasing,
                                            const bool na_last,
                                            const r_ssize size,
                                            double* p_x_dbl,
                                            uint64_t* p_x_u64);

static inline void dbl_adjust_nan_distinct(const bool decreasing,
                                           const bool na_last,
                                           const r_ssize size,
                                           double* p_x_dbl,
                                           uint64_t* p_x_u64);

/*
 * When mapping double -> uint64_t:
 *
 * Smallest possible value comes from:
 * dbl_map_to_uint64(-Inf) -> 4503599627370495
 *
 * One larger is:
 * dbl_map_to_uint64(-.Machine$double.xmax) -> 4503599627370496
 *
 * Largest possible value comes from:
 * dbl_map_to_uint64(Inf) -> 18442240474082181120
 *
 * One smaller is:
 * dbl_map_to_uint64(.Machine$double.xmax) -> 18442240474082181119
 *
 * This gives us room to manually map:
 * If (!nan_distinct):
 *   dbl_map_to_uint64(NA_real_) -> UINT64_MAX (or 0 if `na_last = false`)
 *   dbl_map_to_uint64(NaN) -> UINT64_MAX (or 0 if `na_last = false`)
 * If (nan_distinct):
 *   dbl_map_to_uint64(NA_real_) -> UINT64_MAX (or 0 if `na_last = false`)
 *   dbl_map_to_uint64(NaN) -> UINT64_MAX - 1 (or 1 if `na_last = false`)
 * When using `nan_distinct`, NaN is always ordered between NA_real_ and
 * non-missing numbers, regardless of `decreasing`.
 */
static
void dbl_adjust(const bool decreasing,
                const bool na_last,
                const bool nan_distinct,
                const r_ssize size,
                void* p_x) {
  double* p_x_dbl = (double*) p_x;
  uint64_t* p_x_u64 = (uint64_t*) p_x;

  if (nan_distinct) {
    dbl_adjust_nan_distinct(decreasing, na_last, size, p_x_dbl, p_x_u64);
  } else {
    dbl_adjust_nan_identical(decreasing, na_last, size, p_x_dbl, p_x_u64);
  }
}

static inline uint64_t dbl_map_to_uint64(double x);

static inline
void dbl_adjust_nan_identical(const bool decreasing,
                              const bool na_last,
                              const r_ssize size,
                              double* p_x_dbl,
                              uint64_t* p_x_u64) {
  const int direction = decreasing ? -1 : 1;
  const uint64_t na_u64 = na_last ? UINT64_MAX : 0;

  for (r_ssize i = 0; i < size; ++i) {
    double elt = p_x_dbl[i];

    if (isnan(elt)) {
      p_x_u64[i] = na_u64;
      continue;
    }

    elt = elt * direction;
    p_x_u64[i] = dbl_map_to_uint64(elt);
  }
}

static inline
void dbl_adjust_nan_distinct(const bool decreasing,
                             const bool na_last,
                             const r_ssize size,
                             double* p_x_dbl,
                             uint64_t* p_x_u64) {
  const int direction = decreasing ? -1 : 1;
  const uint64_t na_u64 = na_last ? UINT64_MAX : 0;
  const uint64_t nan_u64 = na_last ? UINT64_MAX - 1 : 1;

  for (r_ssize i = 0; i < size; ++i) {
    double elt = p_x_dbl[i];
    const enum vctrs_dbl_class type = dbl_classify(elt);

    switch (type) {
    case vctrs_dbl_number: {
      elt = elt * direction;
      p_x_u64[i] = dbl_map_to_uint64(elt);
      break;
    }
    case vctrs_dbl_missing: {
      p_x_u64[i] = na_u64;
      break;
    }
    case vctrs_dbl_nan: {
      p_x_u64[i] = nan_u64;
      break;
    }
    }
  }
}

static inline uint64_t dbl_flip_uint64(uint64_t x);

static union {
  double d;
  uint64_t u64;
} d_u64;

/*
 * Map `double -> `uint64_t` retaining ordering.
 *
 * Assumes `x` is not a `NA_real_` or `NaN` value.
 * Correctly handles `Inf` and `-Inf`.
 */
static inline
uint64_t dbl_map_to_uint64(double x) {
  // Catch `-0` vs `0`
  if (x == 0) {
    x = 0;
  }

  // Reinterpret as uint64_t without changing bytes
  d_u64.d = x;

  d_u64.u64 = dbl_flip_uint64(d_u64.u64);

  return d_u64.u64;
}


#define HEX_UINT64_SIGN 0x8000000000000000u
#define HEX_UINT64_ONES 0xffffffffffffffffu

// To retain ordering in mapping from double -> uint64_t we always have to
// flip the sign bit, and for negative numbers we also flip all of the other
// bits. Described more here: http://stereopsis.com/radix.html
static inline
uint64_t dbl_flip_uint64(uint64_t x) {
  const uint64_t mask = (x & HEX_UINT64_SIGN) ? HEX_UINT64_ONES : HEX_UINT64_SIGN;
  return x ^ mask;
}

#undef HEX_UINT64_SIGN
#undef HEX_UINT64_ONES

// -----------------------------------------------------------------------------

/*
 * `dbl_order_insertion()` is used in two ways:
 * - It is how we "finish off" radix sorts rather than deep recursion.
 * - If we have an original `x` input that is small enough, we just immediately
 *   insertion sort it.
 *
 * For small inputs, it is much faster than deeply recursing with
 * radix ordering.
 *
 * Insertion ordering expects that `p_x` has been adjusted with `dbl_adjust()`
 * which takes care of `na_last` and `decreasing` and also maps `double` to
 * `uint64_t` ahead of time.
 *
 * It is essentially the same as `int_order_insertion()` with different types.
 */
static
void dbl_order_insertion(const r_ssize size,
                         uint64_t* p_x,
                         int* p_o,
                         struct group_infos* p_group_infos) {
  // Don't think this can occur, but safer this way
  if (size == 0) {
    return;
  }

  for (r_ssize i = 1; i < size; ++i) {
    const uint64_t x_elt = p_x[i];
    const int o_elt = p_o[i];

    r_ssize j = i - 1;

    while (j >= 0) {
      const uint64_t x_cmp_elt = p_x[j];

      if (x_elt >= x_cmp_elt) {
        break;
      }

      int o_cmp_elt = p_o[j];

      // Swap
      p_x[j + 1] = x_cmp_elt;
      p_o[j + 1] = o_cmp_elt;

      // Next
      --j;
    }

    // Place original elements in new location
    // closer to start of the vector
    p_x[j + 1] = x_elt;
    p_o[j + 1] = o_elt;
  }

  // We've ordered a small chunk, we need to push at least one group size.
  // Depends on the post-ordered results so we have to do this
  // in a separate loop.
  r_ssize group_size = 1;
  uint64_t previous = p_x[0];

  for (r_ssize i = 1; i < size; ++i) {
    const uint64_t current = p_x[i];

    // Continue the current group run
    if (current == previous) {
      ++group_size;
      continue;
    }

    // Push current run size and reset size tracker
    groups_size_maybe_push(group_size, p_group_infos);
    group_size = 1;

    previous = current;
  }

  // Push final group run
  groups_size_maybe_push(group_size, p_group_infos);
}

// -----------------------------------------------------------------------------

static uint8_t dbl_compute_skips(const uint64_t* p_x, r_ssize size, bool* p_skips);

static void dbl_order_radix_recurse(const r_ssize size,
                                    const uint8_t pass,
                                    uint64_t* p_x,
                                    int* p_o,
                                    uint64_t* p_x_aux,
                                    int* p_o_aux,
                                    uint8_t* p_bytes,
                                    r_ssize* p_counts,
                                    bool* p_skips,
                                    struct group_infos* p_group_infos);

/*
 * Double radix ordering entry point
 *
 * Expects that `dbl_adjust()` has been called on `p_x`, which takes care
 * of `na_last` and `decreasing` and also maps `double` to `uint64_t` once
 * up front so we don't have to do it for each radix pass.
 *
 * Sorts `p_x` and `p_o` in place
 */
static
void dbl_order_radix(const r_ssize size,
                     uint64_t* p_x,
                     int* p_o,
                     uint64_t* p_x_aux,
                     int* p_o_aux,
                     uint8_t* p_bytes,
                     r_ssize* p_counts,
                     struct group_infos* p_group_infos) {
  bool p_skips[DBL_MAX_RADIX_PASS];

  uint8_t pass = dbl_compute_skips(p_x, size, p_skips);

  // Skipped all passes - Happens when `x` is 1 value repeated
  if (pass == DBL_MAX_RADIX_PASS) {
    groups_size_maybe_push(size, p_group_infos);
    return;
  }

  dbl_order_radix_recurse(
    size,
    pass,
    p_x,
    p_o,
    p_x_aux,
    p_o_aux,
    p_bytes,
    p_counts,
    p_skips,
    p_group_infos
  );
}

// -----------------------------------------------------------------------------

static inline uint8_t dbl_extract_uint64_byte(uint64_t x, uint8_t shift);

/*
 * Recursive function for radix ordering. Orders the current byte, then iterates
 * over the sub groups and recursively calls itself on each subgroup to order
 * the next byte.
 *
 * This needs 8 passes, unlike the 4 required by `int_order_radix()`.
 */
static
void dbl_order_radix_recurse(const r_ssize size,
                             const uint8_t pass,
                             uint64_t* p_x,
                             int* p_o,
                             uint64_t* p_x_aux,
                             int* p_o_aux,
                             uint8_t* p_bytes,
                             r_ssize* p_counts,
                             bool* p_skips,
                             struct group_infos* p_group_infos) {
  // Exit as fast as possible if we are below the insertion order boundary
  if (size <= ORDER_INSERTION_BOUNDARY) {
    dbl_order_insertion(size, p_x, p_o, p_group_infos);
    return;
  }

  // Skip passes where our up front check told us that all bytes were the same
  uint8_t next_pass = pass + 1;
  r_ssize* p_counts_next_pass = p_counts + UINT8_MAX_SIZE;

  while (next_pass < DBL_MAX_RADIX_PASS && p_skips[next_pass]) {
    ++next_pass;
    p_counts_next_pass += UINT8_MAX_SIZE;
  }

  const uint8_t radix = PASS_TO_RADIX(pass, DBL_MAX_RADIX_PASS);
  const uint8_t shift = radix * 8;

  uint8_t byte = 0;

  // Histogram
  for (r_ssize i = 0; i < size; ++i) {
    const uint64_t x_elt = p_x[i];

    byte = dbl_extract_uint64_byte(x_elt, shift);

    p_bytes[i] = byte;
    ++p_counts[byte];
  }

  // Fast check to see if all bytes were the same.
  // If so, skip this `pass` since we learned nothing.
  // No need to accumulate counts and iterate over chunks,
  // we know all others are zero.
  if (p_counts[byte] == size) {
    // Reset count for other group chunks
    p_counts[byte] = 0;

    if (next_pass == DBL_MAX_RADIX_PASS) {
      // If we are already at the last pass, we are done
      groups_size_maybe_push(size, p_group_infos);
    } else {
      // Otherwise, recurse on next byte using the same `size` since
      // the group size hasn't changed
      dbl_order_radix_recurse(
        size,
        next_pass,
        p_x,
        p_o,
        p_x_aux,
        p_o_aux,
        p_bytes,
        p_counts_next_pass,
        p_skips,
        p_group_infos
      );
    }

    return;
  }

  r_ssize cumulative = 0;

  // Accumulate counts, skip zeros
  for (uint16_t i = 0; i < UINT8_MAX_SIZE; ++i) {
    r_ssize count = p_counts[i];

    if (count == 0) {
      continue;
    }

    // Replace with `cumulative` first, then bump `cumulative`.
    // `p_counts` now represents starting locations for each radix group.
    p_counts[i] = cumulative;
    cumulative += count;
  }

  // Place into auxiliary arrays in the correct order, then copy back over
  for (r_ssize i = 0; i < size; ++i) {
    const uint8_t byte = p_bytes[i];
    const r_ssize loc = p_counts[byte]++;
    p_o_aux[loc] = p_o[i];
    p_x_aux[loc] = p_x[i];
  }

  // Copy back over
  memcpy(p_o, p_o_aux, size * sizeof(*p_o_aux));
  memcpy(p_x, p_x_aux, size * sizeof(*p_x_aux));

  r_ssize last_cumulative_count = 0;

  // Recurse on subgroups as required
  for (uint16_t i = 0; last_cumulative_count < size && i < UINT8_MAX_SIZE; ++i) {
    const r_ssize cumulative_count = p_counts[i];

    if (!cumulative_count) {
      continue;
    }

    p_counts[i] = 0;

    // Diff the accumulated counts to get the radix group size
    const r_ssize group_size = cumulative_count - last_cumulative_count;
    last_cumulative_count = cumulative_count;

    if (group_size == 1) {
      groups_size_maybe_push(1, p_group_infos);
      ++p_x;
      ++p_o;
      continue;
    }

    // Can get here in the case of ties, like c(1, 1), which have a
    // `group_size` of 2 in the last radix, but there is nothing left to
    // compare so we are done.
    if (next_pass == DBL_MAX_RADIX_PASS) {
      groups_size_maybe_push(group_size, p_group_infos);
      p_x += group_size;
      p_o += group_size;
      continue;
    }

    // Order next byte of this subgroup
    dbl_order_radix_recurse(
      group_size,
      next_pass,
      p_x,
      p_o,
      p_x_aux,
      p_o_aux,
      p_bytes,
      p_counts_next_pass,
      p_skips,
      p_group_infos
    );

    p_x += group_size;
    p_o += group_size;
  }
}

// -----------------------------------------------------------------------------

/*
 * Detect completely skippable bytes
 *
 * There are 8 passes over a double, 1 for each byte. Often times for the entire
 * `x` vector a few of those passes are useless because all of the bytes are
 * the same. This does an up front computation in 1 pass over the data to
 * determine which bytes are completely skippable.
 *
 * It is worth noting that just because byte 0 wasn't skippable doesn't mean
 * that byte 1 isn't. With the way that doubles are mapped to uint64_t, it
 * is often the case that, for small doubles, bytes 0-2 aren't skippable but
 * the rest of them are (for example, this happens with doubles in the range
 * of 1:128). This provides a nice performance increase there.
 */
static
uint8_t dbl_compute_skips(const uint64_t* p_x, r_ssize size, bool* p_skips) {
  uint8_t radix_start = PASS_TO_RADIX(0, DBL_MAX_RADIX_PASS);
  uint8_t shift_start = radix_start * 8;

  for (uint8_t i = 0; i < DBL_MAX_RADIX_PASS; ++i) {
    p_skips[i] = true;
  }

  uint8_t p_bytes[DBL_MAX_RADIX_PASS];
  const uint64_t elt0 = p_x[0];

  // Get bytes of first element in MSD->LSD order.
  // Placed in `p_bytes` in a way that aligns with the `pass` variable
  for (uint8_t pass = 0, shift = shift_start;
       pass < DBL_MAX_RADIX_PASS;
       ++pass, shift += SHIFT_ADJUSTMENT) {
    p_bytes[pass] = dbl_extract_uint64_byte(elt0, shift);
  }

  // Check to see which passes are skippable
  for (r_ssize i = 1; i < size; ++i) {
    uint8_t n_skips = DBL_MAX_RADIX_PASS;
    const uint64_t elt = p_x[i];

    for (uint8_t pass = 0, shift = shift_start;
         pass < DBL_MAX_RADIX_PASS;
         ++pass, shift += SHIFT_ADJUSTMENT) {
      bool skip = p_skips[pass];

      if (skip) {
        p_skips[pass] = (p_bytes[pass] == dbl_extract_uint64_byte(elt, shift));
      } else {
        --n_skips;
      }
    }

    // No passes are skippable
    if (n_skips == 0) {
      break;
    }
  }

  uint8_t pass = 0;

  // Shift forward to the first pass with varying bytes
  while (pass < DBL_MAX_RADIX_PASS && p_skips[pass]) {
    ++pass;
  }

  return pass;
}

// -----------------------------------------------------------------------------

// Bytes will be extracted 8 bits at a time.
// This is a MSB radix sort, so they are extracted MSB->LSB.
static inline
uint8_t dbl_extract_uint64_byte(uint64_t x, uint8_t shift) {
  return (x >> shift) & UINT8_MAX;
}

// -----------------------------------------------------------------------------

/*
 * `cpl_order()` uses the fact that Rcomplex is really just a rcrd
 * type of two double vectors. It orders first on the real vector, and then on
 * the imaginary vector.
 *
 * `cpl_order_chunk()` isn't required. It would only be called from data frames
 * when there is a complex column, but in those cases we split the column
 * into two double vectors (real / imaginary) and "rerun" the column using
 * `dbl_order_chunk()`.
 */
static
void cpl_order(SEXP x,
               bool decreasing,
               bool na_last,
               bool nan_distinct,
               r_ssize size,
               struct order* p_order,
               struct lazy_raw* p_lazy_x_chunk,
               struct lazy_raw* p_lazy_x_aux,
               struct lazy_raw* p_lazy_o_aux,
               struct lazy_raw* p_lazy_bytes,
               struct lazy_raw* p_lazy_counts,
               struct group_infos* p_group_infos) {
  // We treat complex as a two column data frame, so we have to use group
  // information for at least the first column.
  // - If a complex atomic vector is used, `ignore_groups` will be true unless
  //   the user also requested group information.
  // - If the first column of a df is a complex column, `ignore_groups` will
  //   be false.
  bool reset_ignore_groups = false;
  if (p_group_infos->ignore_groups) {
    p_group_infos->ignore_groups = false;
    reset_ignore_groups = true;
  }

  const Rcomplex* p_x_cpl = COMPLEX_RO(x);

  // When a complex column is present,
  // `lazy_x_chunk` and `lazy_x_aux` are created to have the
  // size of a double vector.
  double* p_x_chunk_dbl = (double*) init_lazy_raw(p_lazy_x_chunk);

  // Handle the real portion first
  for (r_ssize i = 0; i < size; ++i) {
    p_x_chunk_dbl[i] = p_x_cpl[i].r;
  }

  /*
   * Call double ordering algorithm on real section.
   *
   * In this case, both `p_x_chunk_dbl` and `p_lazy_x_chunk` are passed through,
   * but we set `copy = false` which tells `dbl_order_impl()` not to copy
   * the input (`p_x_chunk_dbl`) over to the chunk vector of (`p_lazy_x_chunk`).
   * It has already been done when we extracted the real section.
   */
  dbl_order_impl(
    p_x_chunk_dbl,
    decreasing,
    na_last,
    nan_distinct,
    size,
    false,
    p_order,
    p_lazy_x_chunk,
    p_lazy_x_aux,
    p_lazy_o_aux,
    p_lazy_bytes,
    p_lazy_counts,
    p_group_infos
  );

  // Ordering will now be initialized
  int* p_o = p_order->p_data;

  // Reset `ignore_groups` for the second pass if we don't need to track groups.
  // This happens if an atomic complex vector is passed in and the user
  // hasn't requested group information.
  if (reset_ignore_groups) {
    p_group_infos->ignore_groups = true;
  }

  // Get the number of group chunks from the first pass
  struct group_info* p_group_info_pre = groups_current(p_group_infos);
  r_ssize n_groups = p_group_info_pre->n_groups;

  // If there were no ties, we are completely done
  if (n_groups == size) {
    return;
  }

  // Swap to other group info to prepare for the imaginary section
  groups_swap(p_group_infos);

  // Fill with the imaginary portion.
  // Uses updated ordering to place it in sequential order.
  for (r_ssize i = 0; i < size; ++i) {
    const int loc = p_o[i] - 1;
    p_x_chunk_dbl[i] = p_x_cpl[loc].i;
  }

  // Iterate over the group chunks from the first pass
  for (r_ssize group = 0; group < n_groups; ++group) {
    r_ssize group_size = p_group_info_pre->p_data[group];

    // Fast handling of simplest case
    if (group_size == 1) {
      ++p_x_chunk_dbl;
      ++p_o;
      groups_size_maybe_push(1, p_group_infos);
      continue;
    }

    dbl_order_chunk_impl(
      decreasing,
      na_last,
      nan_distinct,
      group_size,
      p_x_chunk_dbl,
      p_o,
      p_lazy_x_aux,
      p_lazy_o_aux,
      p_lazy_bytes,
      p_lazy_counts,
      p_group_infos
    );

    p_x_chunk_dbl += group_size;
    p_o += group_size;
  }
}

// -----------------------------------------------------------------------------

static void chr_mark_sorted_uniques(const SEXP* p_x,
                                    r_ssize size,
                                    struct lazy_raw* p_lazy_x_aux,
                                    struct lazy_raw* p_lazy_bytes,
                                    struct truelength_info* p_truelength_info);

static inline void chr_extract_ordering(const SEXP* p_x, r_ssize size, int* p_x_aux);

static void chr_order_radix(const r_ssize size,
                            const R_len_t max_size,
                            SEXP* p_x,
                            SEXP* p_x_aux,
                            int* p_sizes,
                            int* p_sizes_aux,
                            uint8_t* p_bytes);

/*
 * These are the main entry points for character ordering.
 *
 * `chr_order_chunk()` assumes `p_x` is modifiable by reference. It also
 * assumes that `chr_mark_sorted_uniques()` has already been called. For data
 * frame columns where `chr_order_chunk()` is called on each group chunk,
 * `chr_mark_sorted_uniques()` is only called once on the entire column.
 *
 * `chr_order()` assumes `x` is user input which cannot be modified.
 * It copies `x` into another SEXP that can be modified directly.
 *
 * `chr_order_chunk()` essentially calls `int_order_chunk()`, however we can't
 * call it directly because we don't have access to all the required arguments.
 * Specifically we reuse `p_x` here as the auxiliary data structure for integer
 * ordering, but to call `int_order_chunk()` we would need the lazy wrapper
 * for it.
 *
 * Because these functions modify TRUELENGTHs, we have to reset them on the
 * way out. `chr_order()` does it directly, but `chr_order_chunk()` relies
 * on `df_order()` to do it after the entire column is processed. It is
 * important to not error inside these functions because the TRUELENGTHs won't
 * be reset if we do.
 */
static
void chr_order_chunk(bool decreasing,
                     bool na_last,
                     r_ssize size,
                     int* p_o,
                     struct lazy_raw* p_lazy_x_chunk,
                     struct lazy_raw* p_lazy_x_aux,
                     struct lazy_raw* p_lazy_o_aux,
                     struct lazy_raw* p_lazy_bytes,
                     struct lazy_raw* p_lazy_counts,
                     struct group_infos* p_group_infos) {
  void* p_x_chunk = p_lazy_x_chunk->p_data;

  const enum vctrs_sortedness sortedness = chr_sortedness(
    p_x_chunk,
    size,
    decreasing,
    na_last,
    p_group_infos
  );

  if (sortedness != VCTRS_SORTEDNESS_unsorted) {
    ord_resolve_sortedness_chunk(sortedness, size, p_o);
    return;
  }

  void* p_x_aux = init_lazy_raw(p_lazy_x_aux);

  // Move integer ordering into `p_x_aux`.
  // `p_x_aux` is allocated as the larger of `int` and `SEXP*`.
  chr_extract_ordering(p_x_chunk, size, p_x_aux);

  /*
   * Call integer ordering algorithm on TRUELENGTHs. Reuse the chunk memory of
   * `p_lazy_x_chunk` that held the current `SEXP*` chunk as the new auxiliary
   * memory since those are no longer needed. It is allocated as the
   * larger of `int` and `SEXP*` so it is large enough.
   */
  int_order_chunk_impl(
    decreasing,
    na_last,
    size,
    p_x_aux,
    p_o,
    p_lazy_x_chunk,
    p_lazy_o_aux,
    p_lazy_bytes,
    p_lazy_counts,
    p_group_infos
  );
}


struct chr_order_info {
  SEXP x;
  bool decreasing;
  bool na_last;
  r_ssize size;
  struct order* p_order;
  struct lazy_raw* p_lazy_x_chunk;
  struct lazy_raw* p_lazy_x_aux;
  struct lazy_raw* p_lazy_o_aux;
  struct lazy_raw* p_lazy_bytes;
  struct lazy_raw* p_lazy_counts;
  struct group_infos* p_group_infos;
  struct truelength_info* p_truelength_info;
};

struct chr_order_cleanup_info {
  struct truelength_info* p_truelength_info;
};

static SEXP chr_order_exec(void* p_data);
static void chr_order_cleanup(void* p_data);

/*
 * `chr_order()` directly modifies the `TRUELENGTH()` values of the CHARSXPs
 * in `x`. These must be reset after the call with `truelength_reset()`. To
 * ensure that this function is called (even on a longjump),
 * `R_ExecWithCleanup()` is used.
 */
static
void chr_order(SEXP x,
               bool decreasing,
               bool na_last,
               r_ssize size,
               struct order* p_order,
               struct lazy_raw* p_lazy_x_chunk,
               struct lazy_raw* p_lazy_x_aux,
               struct lazy_raw* p_lazy_o_aux,
               struct lazy_raw* p_lazy_bytes,
               struct lazy_raw* p_lazy_counts,
               struct group_infos* p_group_infos,
               struct truelength_info* p_truelength_info) {
  struct chr_order_info info = {
    .x = x,
    .decreasing = decreasing,
    .na_last = na_last,
    .size = size,
    .p_order = p_order,
    .p_lazy_x_chunk = p_lazy_x_chunk,
    .p_lazy_x_aux = p_lazy_x_aux,
    .p_lazy_o_aux = p_lazy_o_aux,
    .p_lazy_bytes = p_lazy_bytes,
    .p_lazy_counts = p_lazy_counts,
    .p_group_infos = p_group_infos,
    .p_truelength_info = p_truelength_info
  };

  struct chr_order_cleanup_info cleanup_info = {
    .p_truelength_info = p_truelength_info
  };

  R_ExecWithCleanup(
    chr_order_exec,
    &info,
    chr_order_cleanup,
    &cleanup_info
  );
}

static void chr_order_internal(SEXP x,
                               bool decreasing,
                               bool na_last,
                               r_ssize size,
                               struct order* p_order,
                               struct lazy_raw* p_lazy_x_chunk,
                               struct lazy_raw* p_lazy_x_aux,
                               struct lazy_raw* p_lazy_o_aux,
                               struct lazy_raw* p_lazy_bytes,
                               struct lazy_raw* p_lazy_counts,
                               struct group_infos* p_group_infos,
                               struct truelength_info* p_truelength_info);

static
SEXP chr_order_exec(void* p_data) {
  struct chr_order_info* p_info = (struct chr_order_info*) p_data;

  chr_order_internal(
    p_info->x,
    p_info->decreasing,
    p_info->na_last,
    p_info->size,
    p_info->p_order,
    p_info->p_lazy_x_chunk,
    p_info->p_lazy_x_aux,
    p_info->p_lazy_o_aux,
    p_info->p_lazy_bytes,
    p_info->p_lazy_counts,
    p_info->p_group_infos,
    p_info->p_truelength_info
  );

  return R_NilValue;
}

static
void chr_order_cleanup(void* p_data) {
  struct chr_order_cleanup_info* p_info = (struct chr_order_cleanup_info*) p_data;
  truelength_reset(p_info->p_truelength_info);
}

static
void chr_order_internal(SEXP x,
                        bool decreasing,
                        bool na_last,
                        r_ssize size,
                        struct order* p_order,
                        struct lazy_raw* p_lazy_x_chunk,
                        struct lazy_raw* p_lazy_x_aux,
                        struct lazy_raw* p_lazy_o_aux,
                        struct lazy_raw* p_lazy_bytes,
                        struct lazy_raw* p_lazy_counts,
                        struct group_infos* p_group_infos,
                        struct truelength_info* p_truelength_info) {
  const SEXP* p_x = STRING_PTR_RO(x);

  const enum vctrs_sortedness sortedness = chr_sortedness(
    p_x,
    size,
    decreasing,
    na_last,
    p_group_infos
  );

  // Handle sorted cases and set ordering to initialized
  if (sortedness != VCTRS_SORTEDNESS_unsorted) {
    int* p_o = p_order->p_data;
    ord_resolve_sortedness(sortedness, size, p_o);
    p_order->initialized = true;
    return;
  }

  // Sort unique strings and mark their truelengths with ordering.
  // Use `p_lazy_x_chunk` as auxiliary memory for `chr_order_radix()` so we
  // hopefully don't have to also allocate `p_lazy_x_aux`.
  chr_mark_sorted_uniques(
    p_x,
    size,
    p_lazy_x_chunk,
    p_lazy_bytes,
    p_truelength_info
  );

  void* p_x_chunk = init_lazy_raw(p_lazy_x_chunk);

  // Move integer ordering into `p_x_chunk`.
  // `p_x_chunk` is allocated as the larger of `int` and `SEXP*`.
  chr_extract_ordering(p_x, size, p_x_chunk);

  /*
   * Call integer ordering algorithm on TRUELENGTHs.
   *
   * In this case, both `p_x_chunk` and `p_lazy_x_chunk` are passed through,
   * but we set `copy = false` which tells `int_order_impl()` not to copy
   * the input (`p_x_chunk`) over to the chunk vector of (`p_lazy_x_chunk`).
   * It has already been done when we extracted the truelength ordering.
   */
  int_order_impl(
    p_x_chunk,
    decreasing,
    na_last,
    size,
    false,
    p_order,
    p_lazy_x_chunk,
    p_lazy_x_aux,
    p_lazy_o_aux,
    p_lazy_bytes,
    p_lazy_counts,
    p_group_infos
  );
}

// -----------------------------------------------------------------------------

/*
 * Pull ordering off of marked `p_x` and place it into `p_x_aux` working memory.
 * We mark the CHARSXP TRUELENGTHs with negative ordering to be different from
 * what R might use, so that gets reversed here to get the true ordering back.
 */
static inline
void chr_extract_ordering(const SEXP* p_x, r_ssize size, int* p_x_aux) {
  for (r_ssize i = 0; i < size; ++i) {
    SEXP elt = p_x[i];

    if (elt == NA_STRING) {
      p_x_aux[i] = NA_INTEGER;
      continue;
    }

    // Negative to flip where we set the order using a negative value.
    // Cast to `int` because `TRUELENGTH()` returns a `r_ssize`.
    p_x_aux[i] = (int) -TRUELENGTH(elt);
  }
}

// -----------------------------------------------------------------------------

static void chr_mark_uniques(const SEXP* p_x,
                             r_ssize size,
                             struct truelength_info* p_truelength_info);

/*
 * `chr_mark_sorted_uniques()` runs through the strings in `p_x` and places the
 * unique strings in `p_truelength_info->p_uniques`. It marks the unique ones
 * with a negative TRUELENGTH as it goes. Since identical strings share the
 * same CHARSXP, this marks all strings in the vector at once.
 *
 * After detecting all unique strings, it sorts them in place with
 * `chr_order_radix()`.
 *
 * Finally, it loops over the now sorted unique strings and marks them with
 * their ordering (as a negative value). This allows `chr_order_chunk()` to loop
 * through `p_x` and just pluck off the TRUELENGTH value, which will be an
 * integer proxy for the value's ordering.
 *
 * `truelength_save()` also saves the unique strings and their original
 * TRUELENGTH values so they can be reset after each column with
 * `truelength_reset()`.
 */
static
void chr_mark_sorted_uniques(const SEXP* p_x,
                             r_ssize size,
                             struct lazy_raw* p_lazy_x_aux,
                             struct lazy_raw* p_lazy_bytes,
                             struct truelength_info* p_truelength_info) {
  chr_mark_uniques(p_x, size, p_truelength_info);

  r_ssize n_uniques = p_truelength_info->n_uniques_used;

  SEXP* p_x_aux = (SEXP*) init_lazy_raw(p_lazy_x_aux);

  uint8_t* p_bytes = (uint8_t*) init_lazy_raw(p_lazy_bytes);

  // Sorts uniques in ascending order using `p_x_aux` for working memory.
  // Assumes no `NA`!
  chr_order_radix(
    n_uniques,
    p_truelength_info->max_string_size,
    p_truelength_info->p_uniques,
    p_x_aux,
    p_truelength_info->p_sizes,
    p_truelength_info->p_sizes_aux,
    p_bytes
  );

  // Mark unique sorted strings with their order.
  // Use a negative value to differentiate with R.
  for (r_ssize i = 0; i < n_uniques; ++i) {
    SEXP elt = p_truelength_info->p_uniques[i];
    SET_TRUELENGTH(elt, -i - 1);
  }
}

static
void chr_mark_uniques(const SEXP* p_x,
                      r_ssize size,
                      struct truelength_info* p_truelength_info) {
  for (r_ssize i = 0; i < size; ++i) {
    SEXP elt = p_x[i];

    // `NA_STRING` is replaced by `NA_INTEGER` for use in integer ordering
    if (elt == NA_STRING) {
      continue;
    }

    r_ssize truelength = TRUELENGTH(elt);

    if (truelength < 0) {
      // We have already seen and saved this string
      continue;
    }

    if (truelength > 0) {
      // Retain R's usage of TRUELENGTH. Normally defaults to 0, so if the value
      // is positive, it means R is using it. Should be extremely rare.
      truelength_save_string(elt, truelength, p_truelength_info);
    }

    // CHARSXP string lengths are never "long"
    int elt_size = (int) r_length(elt);

    // Track max string size to know how deep to recurse
    if (p_truelength_info->max_string_size < elt_size) {
      p_truelength_info->max_string_size = elt_size;
    }

    // Save this unique value and its size so we can order uniques
    truelength_save_unique(elt, p_truelength_info);
    truelength_save_size(elt_size, p_truelength_info);

    // Mark as negative to note that we have seen this string.
    // R uses positive or zero truelengths.
    SET_TRUELENGTH(elt, -1);
  }
}

// -----------------------------------------------------------------------------

static bool chr_str_ge(SEXP x, SEXP y, int x_size, const R_len_t pass);

/*
 * Insertion order for character vectors. This occurs in the radix ordering
 * once we drop below a certain chunk size.
 *
 * One optimization done here is to take advantage of the `pass` info, which
 * will indicate that all characters before this pass are identical already
 * and don't need to be checked by `strcmp()`.
 */
static
void chr_order_insertion(const r_ssize size,
                         const R_len_t pass,
                         SEXP* p_x,
                         int* p_sizes) {
  // Don't think this can occur, but safer this way
  if (size == 0) {
    return;
  }

  for (r_ssize i = 1; i < size; ++i) {
    const SEXP x_elt = p_x[i];
    const int x_size = p_sizes[i];

    r_ssize j = i - 1;

    while (j >= 0) {
      const SEXP x_cmp_elt = p_x[j];

      if (chr_str_ge(x_elt, x_cmp_elt, x_size, pass)) {
        break;
      }

      int x_cmp_size = p_sizes[j];

      // Swap
      p_x[j + 1] = x_cmp_elt;
      p_sizes[j + 1] = x_cmp_size;

      // Next
      --j;
    }

    // Place original elements in new location
    // closer to start of the vector
    p_x[j + 1] = x_elt;
    p_sizes[j + 1] = x_size;
  }
}

// -----------------------------------------------------------------------------

static void chr_order_radix_recurse(const r_ssize size,
                                    const R_len_t pass,
                                    const R_len_t max_size,
                                    SEXP* p_x,
                                    SEXP* p_x_aux,
                                    int* p_sizes,
                                    int* p_sizes_aux,
                                    uint8_t* p_bytes);

/*
 * Entry point for radix ordering of characters.
 *
 * This is different from with integers / doubles because:
 * - `p_x` will contain only unique strings
 * - `p_x` will not contain any `NA` strings
 * - We just need to sort `p_x` in place, no need to track group information,
 *   which is instead done by `int_order_chunk()` later
 * - The number of passes is variable here, because strings have a variable
 *   length.
 * - We also track the character sizes because repeated `r_length()` calls
 *   can get expensive over just indexing into the array.
 */
static
void chr_order_radix(const r_ssize size,
                     const R_len_t max_size,
                     SEXP* p_x,
                     SEXP* p_x_aux,
                     int* p_sizes,
                     int* p_sizes_aux,
                     uint8_t* p_bytes) {
  R_len_t pass = 0;

  chr_order_radix_recurse(
    size,
    pass,
    max_size,
    p_x,
    p_x_aux,
    p_sizes,
    p_sizes_aux,
    p_bytes
  );
}

// -----------------------------------------------------------------------------

/*
 * Recursive function for ordering the `p_x` unique strings
 *
 * For ASCII strings, 1 character aligns with 1 byte, so we can order them
 * 1 character at a time from left to right (MSB to LSB).
 *
 * For UTF-8 strings, the implementation of UTF-8 is done so that UTF-8
 * characters are made up of between 1-4 bytes. Luckily, treating them as
 * a sequence of single bytes like we do for ASCII orders identically to
 * treating them as their full 1-4 byte sequence.
 *
 * Because these are variable length, some strings are shorter than others.
 * Shorter strings should order lower than longer strings if they are otherwise
 * equivalent, so we reserve the 0-th bucket of `p_counts` for counting
 * implicit empty strings. Normally this would be an issue because this is
 * the bucket for ASCII value 0, but this is the null value, which is not
 * allowed in R strings!
 *
 * Additionally, we don't have to worry about having an `NA` bucket because
 * there will be no missing values in the unique set.
 */
static
void chr_order_radix_recurse(const r_ssize size,
                             const R_len_t pass,
                             const R_len_t max_size,
                             SEXP* p_x,
                             SEXP* p_x_aux,
                             int* p_sizes,
                             int* p_sizes_aux,
                             uint8_t* p_bytes) {
  // Exit as fast as possible if we are below the insertion order boundary
  if (size <= ORDER_INSERTION_BOUNDARY) {
    chr_order_insertion(size, pass, p_x, p_sizes);
    return;
  }

  // We don't carry along `p_counts` from an up front allocation since
  // the strings have variable length
  r_ssize p_counts[UINT8_MAX_SIZE] = { 0 };

  const int next_pass = pass + 1;

  // NA values won't be in `p_x` so we can reserve the 0th bucket for ""
  const uint8_t missing_bucket = 0;
  uint8_t byte = 0;

  // Histogram
  for (r_ssize i = 0; i < size; ++i) {
    const R_len_t x_elt_size = p_sizes[i];

    // Check if there are characters left in the string and extract the next
    // one if so, otherwise assume implicit "".
    if (pass < x_elt_size) {
      const SEXP x_elt = p_x[i];
      const char* c_x_elt = CHAR(x_elt);
      byte = (uint8_t) c_x_elt[pass];
    } else {
      byte = missing_bucket;
    }

    p_bytes[i] = byte;
    ++p_counts[byte];
  }

  // Fast check to see if all bytes were the same.
  // If so, skip this `pass` since we learned nothing.
  // No need to accumulate counts and iterate over chunks,
  // we know all others are zero.
  if (p_counts[byte] == size) {
    // Reset count for other group chunks
    p_counts[byte] = 0;

    if (next_pass != max_size) {
      // If we are not at the last pass, recurse on next byte using
      // the same `size` since the group size hasn't changed
      chr_order_radix_recurse(
        size,
        next_pass,
        max_size,
        p_x,
        p_x_aux,
        p_sizes,
        p_sizes_aux,
        p_bytes
      );
    }

    return;
  }

  r_ssize cumulative = 0;

  // Accumulate counts, skip zeros
  for (uint16_t i = 0; i < UINT8_MAX_SIZE; ++i) {
    r_ssize count = p_counts[i];

    if (count == 0) {
      continue;
    }

    // Insert current cumulative value, then increment
    p_counts[i] = cumulative;
    cumulative += count;
  }

  // Place into auxiliary arrays in the correct order, then copy back over
  for (r_ssize i = 0; i < size; ++i) {
    const uint8_t byte = p_bytes[i];
    const r_ssize loc = p_counts[byte]++;
    p_x_aux[loc] = p_x[i];
    p_sizes_aux[loc] = p_sizes[i];
  }

  // Copy back over
  memcpy(p_x, p_x_aux, size * sizeof(*p_x_aux));
  memcpy(p_sizes, p_sizes_aux, size * sizeof(*p_sizes_aux));

  r_ssize last_cumulative_count = 0;

  // Recurse on subgroups as required
  for (uint16_t i = 0; last_cumulative_count < size && i < UINT8_MAX_SIZE; ++i) {
    const r_ssize cumulative_count = p_counts[i];

    if (!cumulative_count) {
      continue;
    }

    // Diff the accumulated counts to get the radix group size
    const r_ssize group_size = cumulative_count - last_cumulative_count;
    last_cumulative_count = cumulative_count;

    if (group_size == 1) {
      ++p_x;
      ++p_sizes;
      continue;
    }

    if (next_pass == max_size) {
      p_x += group_size;
      p_sizes += group_size;
      continue;
    }

    // Order next byte of this subgroup
    chr_order_radix_recurse(
      group_size,
      next_pass,
      max_size,
      p_x,
      p_x_aux,
      p_sizes,
      p_sizes_aux,
      p_bytes
    );

    p_x += group_size;
    p_sizes += group_size;
  }
}

// -----------------------------------------------------------------------------

/*
 * Check if `x` is greater than `y` lexicographically in a C-locale.
 *
 * - `x` and `y` are guaranteed to be different and not `NA`, so we don't gain
 *   anything from pointer comparisons.
 *
 * - This is called from `chr_order_insertion()` from inside the radix ordering,
 *   so we can use information about the current `pass` to only compare
 *   characters that are actually different.
 */
static
bool chr_str_ge(SEXP x, SEXP y, int x_size, const R_len_t pass) {
  // Pure insertion sort - we know nothing yet
  if (pass == 0) {
    const char* c_x = CHAR(x);
    const char* c_y = CHAR(y);

    int cmp = strcmp(c_x, c_y);
    return cmp >= 0;
  }

  // Otherwise we know they are equal up to the position before `pass`, but
  // it might have been equality with implicit "" so we need to check the
  // length of one of them
  const int last_pass = pass - 1;

  // We are comparing length with C 0-based indexing so we have to do +1.
  if (x_size < last_pass + 1) {
    return true;
  }

  const char* c_x = CHAR(x);
  const char* c_y = CHAR(y);

  // Now start the comparison at `last_pass`, which we know exists
  c_x = c_x + last_pass;
  c_y = c_y + last_pass;

  int cmp = strcmp(c_x, c_y);
  return cmp >= 0;
}

// -----------------------------------------------------------------------------

static void chr_appearance_counting(const SEXP* p_x,
                                    r_ssize size,
                                    bool initialized,
                                    int* p_o,
                                    int* p_o_aux,
                                    struct group_infos* p_group_infos,
                                    struct truelength_info* p_truelength_info);

static
void chr_appearance_chunk(bool decreasing,
                          bool na_last,
                          r_ssize size,
                          int* p_o,
                          struct lazy_raw* p_lazy_x_chunk,
                          struct lazy_raw* p_lazy_x_aux,
                          struct lazy_raw* p_lazy_o_aux,
                          struct lazy_raw* p_lazy_bytes,
                          struct lazy_raw* p_lazy_counts,
                          struct group_infos* p_group_infos,
                          struct truelength_info* p_truelength_info) {
  const SEXP* p_x_chunk = (const SEXP*) p_lazy_x_chunk->p_data;

  const bool initialized = true;
  int* p_o_aux = (int*) init_lazy_raw(p_lazy_o_aux);

  chr_appearance_counting(
    p_x_chunk,
    size,
    initialized,
    p_o,
    p_o_aux,
    p_group_infos,
    p_truelength_info
  );
}


struct chr_appearance_info {
  SEXP x;
  bool decreasing;
  bool na_last;
  r_ssize size;
  struct order* p_order;
  struct lazy_raw* p_lazy_x_chunk;
  struct lazy_raw* p_lazy_x_aux;
  struct lazy_raw* p_lazy_o_aux;
  struct lazy_raw* p_lazy_bytes;
  struct lazy_raw* p_lazy_counts;
  struct group_infos* p_group_infos;
  struct truelength_info* p_truelength_info;
};

struct chr_appearance_cleanup_info {
  struct truelength_info* p_truelength_info;
};

static SEXP chr_appearance_exec(void* p_data);
static void chr_appearance_cleanup(void* p_data);

/*
 * `chr_appearance()` directly modifies the `TRUELENGTH()` values of the
 * CHARSXPs in `x`. These must be reset after the call with
 * `truelength_reset()`. In practice, `chr_appearance_counting()` will call
 * `truelength_reset()` for us, however, to ensure that this function is called
 * (even on a longjump), `R_ExecWithCleanup()` is used.
 */
static
void chr_appearance(SEXP x,
                    bool decreasing,
                    bool na_last,
                    r_ssize size,
                    struct order* p_order,
                    struct lazy_raw* p_lazy_x_chunk,
                    struct lazy_raw* p_lazy_x_aux,
                    struct lazy_raw* p_lazy_o_aux,
                    struct lazy_raw* p_lazy_bytes,
                    struct lazy_raw* p_lazy_counts,
                    struct group_infos* p_group_infos,
                    struct truelength_info* p_truelength_info) {
  struct chr_appearance_info info = {
   .x = x,
   .decreasing = decreasing,
   .na_last = na_last,
   .size = size,
   .p_order = p_order,
   .p_lazy_x_chunk = p_lazy_x_chunk,
   .p_lazy_x_aux = p_lazy_x_aux,
   .p_lazy_o_aux = p_lazy_o_aux,
   .p_lazy_bytes = p_lazy_bytes,
   .p_lazy_counts = p_lazy_counts,
   .p_group_infos = p_group_infos,
   .p_truelength_info = p_truelength_info
  };

  struct chr_appearance_cleanup_info cleanup_info = {
    .p_truelength_info = p_truelength_info
  };

  R_ExecWithCleanup(
    chr_appearance_exec,
    &info,
    chr_appearance_cleanup,
    &cleanup_info
  );
}

static void chr_appearance_internal(SEXP x,
                                    bool decreasing,
                                    bool na_last,
                                    r_ssize size,
                                    struct order* p_order,
                                    struct lazy_raw* p_lazy_x_chunk,
                                    struct lazy_raw* p_lazy_x_aux,
                                    struct lazy_raw* p_lazy_o_aux,
                                    struct lazy_raw* p_lazy_bytes,
                                    struct lazy_raw* p_lazy_counts,
                                    struct group_infos* p_group_infos,
                                    struct truelength_info* p_truelength_info);

static
SEXP chr_appearance_exec(void* p_data) {
  struct chr_appearance_info* p_info = (struct chr_appearance_info*) p_data;

  chr_appearance_internal(
    p_info->x,
    p_info->decreasing,
    p_info->na_last,
    p_info->size,
    p_info->p_order,
    p_info->p_lazy_x_chunk,
    p_info->p_lazy_x_aux,
    p_info->p_lazy_o_aux,
    p_info->p_lazy_bytes,
    p_info->p_lazy_counts,
    p_info->p_group_infos,
    p_info->p_truelength_info
  );

  return R_NilValue;
}

static
void chr_appearance_cleanup(void* p_data) {
  struct chr_appearance_cleanup_info* p_info = (struct chr_appearance_cleanup_info*) p_data;
  truelength_reset(p_info->p_truelength_info);
}

static
void chr_appearance_internal(SEXP x,
                             bool decreasing,
                             bool na_last,
                             r_ssize size,
                             struct order* p_order,
                             struct lazy_raw* p_lazy_x_chunk,
                             struct lazy_raw* p_lazy_x_aux,
                             struct lazy_raw* p_lazy_o_aux,
                             struct lazy_raw* p_lazy_bytes,
                             struct lazy_raw* p_lazy_counts,
                             struct group_infos* p_group_infos,
                             struct truelength_info* p_truelength_info) {
  const SEXP* p_x = STRING_PTR_RO(x);

  const bool initialized = false;
  int* p_o = p_order->p_data;
  int* p_o_aux = NULL;

  chr_appearance_counting(
    p_x,
    size,
    initialized,
    p_o,
    p_o_aux,
    p_group_infos,
    p_truelength_info
  );
}


/*
 * `chr_appearance_counting()` groups elements of `p_x` in order
 * of appearance using an algorithm that is extremely similar to the
 * counting sort used in `int_order_counting()`.
 *
 * For algorithms that end up calling `vec_order()` twice to compute order by
 * appearance, the first ordering can use appearance order for character
 * vectors, which is faster than lexicographical order, while still actually
 * sorting other atomic types. The final result after the second ordering will
 * still end up being in appearance order.
 */
static
void chr_appearance_counting(const SEXP* p_x,
                             r_ssize size,
                             bool initialized,
                             int* p_o,
                             int* p_o_aux,
                             struct group_infos* p_group_infos,
                             struct truelength_info* p_truelength_info) {
  for (r_ssize i = 0; i < size; ++i) {
    SEXP elt = p_x[i];
    r_ssize truelength = TRUELENGTH(elt);

    // We have already seen and saved this string, so "increment" its counter
    if (truelength < 0) {
      SET_TRUELENGTH(elt, truelength - 1);
      continue;
    }

    if (truelength > 0) {
      // Retain R's usage of TRUELENGTH. Normally defaults to 0, so if the value
      // is positive, it means R is using it. Should be extremely rare.
      truelength_save_string(elt, truelength, p_truelength_info);
    }

    // Save the unique string for appearance ordering below
    truelength_save_unique(elt, p_truelength_info);

    // Mark as negative to note that we have seen this string.
    // R uses positive or zero truelengths.
    SET_TRUELENGTH(elt, -1);
  }

  r_ssize cumulative = 0;

  SEXP* p_uniques = p_truelength_info->p_uniques;
  r_ssize n_uniques = p_truelength_info->n_uniques_used;

  for (r_ssize i = 0; i < n_uniques; ++i) {
    SEXP elt = p_uniques[i];
    r_ssize group_size = -TRUELENGTH(elt);

    // Push group sizes accumulated in order of appearance
    groups_size_maybe_push(group_size, p_group_infos);

    // Set cumulative value (i.e. group start location), then increment
    SET_TRUELENGTH(elt, cumulative);
    cumulative += group_size;
  }

  // If order is not initialized, we are on the first column / atomic vector
  // and can place the order directly into the result. Much faster than
  // initializing, placing in `p_o_aux`, and copying back over.
  if (initialized) {
    for (r_ssize i = 0; i < size; ++i) {
      const SEXP elt = p_x[i];
      const r_ssize loc = TRUELENGTH(elt);
      SET_TRUELENGTH(elt, loc + 1);
      p_o_aux[loc] = p_o[i];
    }

    memcpy(p_o, p_o_aux, size * sizeof(*p_o_aux));
  } else {
    for (r_ssize i = 0; i < size; ++i) {
      const SEXP elt = p_x[i];
      const r_ssize loc = TRUELENGTH(elt);
      SET_TRUELENGTH(elt, loc + 1);
      p_o[loc] = i + 1;
    }
  }

  // Reset truelengths for next chunk/column
  truelength_reset(p_truelength_info);
}

// -----------------------------------------------------------------------------

struct df_order_info {
  SEXP x;
  SEXP decreasing;
  SEXP na_last;
  bool nan_distinct;
  bool chr_ordered;
  r_ssize size;
  struct order* p_order;
  struct lazy_raw* p_lazy_x_chunk;
  struct lazy_raw* p_lazy_x_aux;
  struct lazy_raw* p_lazy_o_aux;
  struct lazy_raw* p_lazy_bytes;
  struct lazy_raw* p_lazy_counts;
  struct group_infos* p_group_infos;
  struct truelength_info* p_truelength_info;
};

struct df_order_cleanup_info {
  struct truelength_info* p_truelength_info;
};

static SEXP df_order_exec(void* p_data);
static void df_order_cleanup(void* p_data);

/*
 * `df_order()` is the main user of `p_group_infos`. It uses the grouping
 * of the current column to break up the next column into sub groups. That
 * process is continued until either all columns have been processed or we
 * can tell all of the values apart.
 *
 * Internally `df_order()` may call `chr_order_chunk()` to order character
 * columns. The TRUELENGTHs of the column are marked with
 * `chr_mark_sorted_uniques()`, and generally they are reset after each
 * column is processed by using `truelength_reset()`. However, if a longjump
 * occurs after the column is marked but before it is reset, then the
 * truelengths won't be reset. This might happen if an allocation fails, or
 * if an error is thrown. To carefully handle this case,
 * `R_ExecWithCleanup()` is used to ensure that `truelength_reset()` is
 * always called. When there aren't any character columns or if there are
 * character columns and the truelengths were reset normally, this does
 * nothing.
 */
static
void df_order(SEXP x,
              SEXP decreasing,
              SEXP na_last,
              bool nan_distinct,
              bool chr_ordered,
              r_ssize size,
              struct order* p_order,
              struct lazy_raw* p_lazy_x_chunk,
              struct lazy_raw* p_lazy_x_aux,
              struct lazy_raw* p_lazy_o_aux,
              struct lazy_raw* p_lazy_bytes,
              struct lazy_raw* p_lazy_counts,
              struct group_infos* p_group_infos,
              struct truelength_info* p_truelength_info) {
  struct df_order_info info = {
    .x = x,
    .decreasing = decreasing,
    .na_last = na_last,
    .nan_distinct = nan_distinct,
    .chr_ordered = chr_ordered,
    .size = size,
    .p_order = p_order,
    .p_lazy_x_chunk = p_lazy_x_chunk,
    .p_lazy_x_aux = p_lazy_x_aux,
    .p_lazy_o_aux = p_lazy_o_aux,
    .p_lazy_bytes = p_lazy_bytes,
    .p_lazy_counts = p_lazy_counts,
    .p_group_infos = p_group_infos,
    .p_truelength_info = p_truelength_info
  };

  struct df_order_cleanup_info cleanup_info = {
    .p_truelength_info = p_truelength_info
  };

  R_ExecWithCleanup(
    df_order_exec,
    &info,
    df_order_cleanup,
    &cleanup_info
  );
}

static void df_order_internal(SEXP x,
                              SEXP decreasing,
                              SEXP na_last,
                              bool nan_distinct,
                              bool chr_ordered,
                              r_ssize size,
                              struct order* p_order,
                              struct lazy_raw* p_lazy_x_chunk,
                              struct lazy_raw* p_lazy_x_aux,
                              struct lazy_raw* p_lazy_o_aux,
                              struct lazy_raw* p_lazy_bytes,
                              struct lazy_raw* p_lazy_counts,
                              struct group_infos* p_group_infos,
                              struct truelength_info* p_truelength_info);

static
SEXP df_order_exec(void* p_data) {
  struct df_order_info* p_info = (struct df_order_info*) p_data;

  df_order_internal(
    p_info->x,
    p_info->decreasing,
    p_info->na_last,
    p_info->nan_distinct,
    p_info->chr_ordered,
    p_info->size,
    p_info->p_order,
    p_info->p_lazy_x_chunk,
    p_info->p_lazy_x_aux,
    p_info->p_lazy_o_aux,
    p_info->p_lazy_bytes,
    p_info->p_lazy_counts,
    p_info->p_group_infos,
    p_info->p_truelength_info
  );

  return R_NilValue;
}

static
void df_order_cleanup(void* p_data) {
  struct df_order_cleanup_info* p_info = (struct df_order_cleanup_info*) p_data;
  truelength_reset(p_info->p_truelength_info);
}


static void vec_order_chunk_switch(bool decreasing,
                                   bool na_last,
                                   bool nan_distinct,
                                   bool chr_ordered,
                                   r_ssize size,
                                   const enum vctrs_type type,
                                   int* p_o,
                                   struct lazy_raw* p_lazy_x_chunk,
                                   struct lazy_raw* p_lazy_x_aux,
                                   struct lazy_raw* p_lazy_o_aux,
                                   struct lazy_raw* p_lazy_bytes,
                                   struct lazy_raw* p_lazy_counts,
                                   struct group_infos* p_group_infos,
                                   struct truelength_info* p_truelength_info);


#define DF_ORDER_EXTRACT_CHUNK(CONST_DEREF, CTYPE) do {          \
  const CTYPE* p_col = CONST_DEREF(col);                         \
  CTYPE* p_x_chunk_col = (CTYPE*) p_x_chunk;                     \
                                                                 \
  /* Extract the next group chunk and place in */                \
  /* sequential order for cache friendliness */                  \
  for (r_ssize j = 0; j < group_size; ++j) {                     \
    const int loc = p_o_col[j] - 1;                              \
    p_x_chunk_col[j] = p_col[loc];                               \
  }                                                              \
} while (0)

#define DF_ORDER_EXTRACT_CHUNK_CPL() do {                      \
  const Rcomplex* p_col = COMPLEX_RO(col);                     \
  double* p_x_chunk_col = (double*) p_x_chunk;                 \
                                                               \
  if (rerun_complex) {                                         \
    /* First pass - real */                                    \
    for (r_ssize j = 0; j < group_size; ++j) {                 \
      const int loc = p_o_col[j] - 1;                          \
      p_x_chunk_col[j] = p_col[loc].r;                         \
    }                                                          \
                                                               \
    /* Decrement `i` to rerun column */                        \
    --i;                                                       \
  } else {                                                     \
    /* Second pass - imaginary */                              \
    for (r_ssize j = 0; j < group_size; ++j) {                 \
      const int loc = p_o_col[j] - 1;                          \
      p_x_chunk_col[j] = p_col[loc].i;                         \
    }                                                          \
  }                                                            \
} while (0)

static
void df_order_internal(SEXP x,
                       SEXP decreasing,
                       SEXP na_last,
                       bool nan_distinct,
                       bool chr_ordered,
                       r_ssize size,
                       struct order* p_order,
                       struct lazy_raw* p_lazy_x_chunk,
                       struct lazy_raw* p_lazy_x_aux,
                       struct lazy_raw* p_lazy_o_aux,
                       struct lazy_raw* p_lazy_bytes,
                       struct lazy_raw* p_lazy_counts,
                       struct group_infos* p_group_infos,
                       struct truelength_info* p_truelength_info) {
  r_ssize n_cols = r_length(x);

  bool recycle_decreasing;
  r_ssize n_decreasing = r_length(decreasing);
  int* p_decreasing = LOGICAL(decreasing);

  if (n_decreasing == 1) {
    recycle_decreasing = true;
  } else if (n_decreasing == n_cols) {
    recycle_decreasing = false;
  } else {
    Rf_errorcall(
      R_NilValue,
      "Internal error: `vec_order_expand_args()` should expand "
      "`decreasing` to have length 1 or length equal "
      "to the number of columns of `x` after calling `vec_proxy_order()`."
    );
  }

  bool recycle_na_last;
  r_ssize n_na_last = r_length(na_last);
  int* p_na_last = LOGICAL(na_last);

  if (n_na_last == 1) {
    recycle_na_last = true;
  } else if (n_na_last == n_cols) {
    recycle_na_last = false;
  } else {
    Rf_errorcall(
      R_NilValue,
      "Internal error: `vec_order_expand_args()` should expand "
      "`na_last` to have length 1 or length equal "
      "to the number of columns of `x` after calling `vec_proxy_order()`."
    );
  }

  // Special case no columns
  if (n_cols == 0) {
    init_order(p_order);
    return;
  }

  SEXP col = VECTOR_ELT(x, 0);
  bool col_decreasing = p_decreasing[0];
  bool col_na_last = p_na_last[0];
  enum vctrs_type type = vec_proxy_typeof(col);

  // Apply on one column to fill `p_group_infos`.
  // First column is immutable and we must copy into `x_chunk`.
  vec_order_base_switch(
    col,
    col_decreasing,
    col_na_last,
    nan_distinct,
    chr_ordered,
    size,
    type,
    p_order,
    p_lazy_x_chunk,
    p_lazy_x_aux,
    p_lazy_o_aux,
    p_lazy_bytes,
    p_lazy_counts,
    p_group_infos,
    p_truelength_info
  );

  // For complex, we have to rerun the column a second time on the
  // imaginary part. This is done by decrementing `i` after processing
  // the real part so the column is rerun.
  bool rerun_complex = false;

  // Iterate over remaining columns by group chunk
  for (r_ssize i = 1; i < n_cols; ++i) {
    // Get the number of group chunks from previous column group info
    struct group_info* p_group_info_pre = groups_current(p_group_infos);
    r_ssize n_groups = p_group_info_pre->n_groups;

    // If there were no ties, we are completely done
    if (n_groups == size) {
      break;
    }

    if (!recycle_decreasing) {
      col_decreasing = p_decreasing[i];
    }
    if (!recycle_na_last) {
      col_na_last = p_na_last[i];
    }

    // Reset pointer between columns since we increment it as
    // we iterate through the groups, but need it to start from the beginning
    // on the next column. `p_o` is initialized now that we have already
    // processed at least one column.
    int* p_o_col = p_order->p_data;

    col = VECTOR_ELT(x, i);
    type = vec_proxy_typeof(col);

    // If we are on the rerun pass, flip this back off so the
    // imaginary part is extracted below.
    if (type == vctrs_type_complex) {
      rerun_complex = rerun_complex ? false : true;
    }

    // Pre-sort unique characters once for the whole column.
    // Don't sort uniques if computing appearance ordering.
    if (chr_ordered && type == vctrs_type_character) {
      const SEXP* p_col = STRING_PTR_RO(col);

      chr_mark_sorted_uniques(
        p_col,
        size,
        p_lazy_x_aux,
        p_lazy_bytes,
        p_truelength_info
      );
    }

    // Turn off group tracking if:
    // - We are on the last column
    // - The user didn't request group information
    // - That column isn't the first pass of a complex column
    if (i == n_cols - 1 && !p_group_infos->force_groups && !rerun_complex) {
      p_group_infos->ignore_groups = true;
    }

    // Swap to other group info to prepare for this column
    groups_swap(p_group_infos);

    // Ensure `x_chunk` is initialized to hold chunks
    void* p_x_chunk = init_lazy_raw(p_lazy_x_chunk);

    // Iterate over this column's group chunks
    for (r_ssize group = 0; group < n_groups; ++group) {
      r_ssize group_size = p_group_info_pre->p_data[group];

      // Fast handling of simplest case
      if (group_size == 1) {
        ++p_o_col;
        groups_size_maybe_push(1, p_group_infos);
        continue;
      }

      // Extract current chunk and place into `x_chunk` in sequential order
      switch (type) {
      case vctrs_type_integer: DF_ORDER_EXTRACT_CHUNK(INTEGER_RO, int); break;
      case vctrs_type_logical: DF_ORDER_EXTRACT_CHUNK(LOGICAL_RO, int); break;
      case vctrs_type_double: DF_ORDER_EXTRACT_CHUNK(REAL_RO, double); break;
      case vctrs_type_character: DF_ORDER_EXTRACT_CHUNK(STRING_PTR_RO, SEXP); break;
      case vctrs_type_complex: DF_ORDER_EXTRACT_CHUNK_CPL(); break;
      default: Rf_errorcall(R_NilValue, "Unknown data frame column type in `vec_order()`.");
      }

      vec_order_chunk_switch(
        col_decreasing,
        col_na_last,
        nan_distinct,
        chr_ordered,
        group_size,
        type,
        p_o_col,
        p_lazy_x_chunk,
        p_lazy_x_aux,
        p_lazy_o_aux,
        p_lazy_bytes,
        p_lazy_counts,
        p_group_infos,
        p_truelength_info
      );

      p_o_col += group_size;
    }

    // Reset TRUELENGTHs between columns if ordering character vectors.
    // When ordering by appearance, `chr_appearance_counting()` resets the
    // TRUELENGTHs between chunks.
    if (chr_ordered && type == vctrs_type_character) {
      truelength_reset(p_truelength_info);
    }
  }
}

#undef DF_ORDER_EXTRACT_CHUNK
#undef DF_ORDER_EXTRACT_CHUNK_CPL

// -----------------------------------------------------------------------------

/*
 * Switch function specifically for column chunks generated when
 * processing a data frame
 */
static
void vec_order_chunk_switch(bool decreasing,
                            bool na_last,
                            bool nan_distinct,
                            bool chr_ordered,
                            r_ssize size,
                            const enum vctrs_type type,
                            int* p_o,
                            struct lazy_raw* p_lazy_x_chunk,
                            struct lazy_raw* p_lazy_x_aux,
                            struct lazy_raw* p_lazy_o_aux,
                            struct lazy_raw* p_lazy_bytes,
                            struct lazy_raw* p_lazy_counts,
                            struct group_infos* p_group_infos,
                            struct truelength_info* p_truelength_info) {
  switch (type) {
  case vctrs_type_integer: {
    int_order_chunk(
      decreasing,
      na_last,
      size,
      p_o,
      p_lazy_x_chunk,
      p_lazy_x_aux,
      p_lazy_o_aux,
      p_lazy_bytes,
      p_lazy_counts,
      p_group_infos
    );

    break;
  }
  case vctrs_type_logical: {
    lgl_order_chunk(
      decreasing,
      na_last,
      size,
      p_o,
      p_lazy_x_chunk,
      p_lazy_x_aux,
      p_lazy_o_aux,
      p_lazy_bytes,
      p_lazy_counts,
      p_group_infos
    );

    break;
  }
  case vctrs_type_double: {
    dbl_order_chunk(
      decreasing,
      na_last,
      nan_distinct,
      size,
      p_o,
      p_lazy_x_chunk,
      p_lazy_x_aux,
      p_lazy_o_aux,
      p_lazy_bytes,
      p_lazy_counts,
      p_group_infos
    );

    break;
  }
  case vctrs_type_complex: {
    // Complex types are run in two passes, once over real then over imaginary
    dbl_order_chunk(
      decreasing,
      na_last,
      nan_distinct,
      size,
      p_o,
      p_lazy_x_chunk,
      p_lazy_x_aux,
      p_lazy_o_aux,
      p_lazy_bytes,
      p_lazy_counts,
      p_group_infos
    );

    break;
  }
  case vctrs_type_character: {
    if (chr_ordered) {
      chr_order_chunk(
        decreasing,
        na_last,
        size,
        p_o,
        p_lazy_x_chunk,
        p_lazy_x_aux,
        p_lazy_o_aux,
        p_lazy_bytes,
        p_lazy_counts,
        p_group_infos
      );
    } else {
      chr_appearance_chunk(
        decreasing,
        na_last,
        size,
        p_o,
        p_lazy_x_chunk,
        p_lazy_x_aux,
        p_lazy_o_aux,
        p_lazy_bytes,
        p_lazy_counts,
        p_group_infos,
        p_truelength_info
      );
    }

    break;
  }
  case vctrs_type_dataframe: {
    Rf_errorcall(R_NilValue, "Internal error: df-cols should have already been flattened.");
    break;
  }
  default: {
    Rf_errorcall(R_NilValue, "This type is not supported by `vec_order()`");
  }
  }
}

// -----------------------------------------------------------------------------

static inline size_t df_compute_n_bytes_lazy_raw(SEXP x);

/*
 * Compute the minimum size required for `lazy_x_aux` and `lazy_x_chunk`.
 *
 * For complex, we split the vector into two double vectors. We only need to
 * allocate 1 double vector though, and it will be reused for both the real
 * and imaginary parts.
 */
static inline
size_t vec_compute_n_bytes_lazy_raw(SEXP x, const enum vctrs_type type) {
  switch (type) {
  case vctrs_type_integer:
  case vctrs_type_logical:
    return sizeof(int);
  case vctrs_type_double:
    return sizeof(double);
  case vctrs_type_complex:
    // Complex types will be split into two double vectors
    return sizeof(double);
  case vctrs_type_character:
    // Auxiliary data will store SEXP and ints, so return the larger
    return sizeof(SEXP) > sizeof(int) ? sizeof(SEXP) : sizeof(int);
  case vctrs_type_dataframe:
    return df_compute_n_bytes_lazy_raw(x);
  default:
    Rf_errorcall(R_NilValue, "This type is not supported by `vec_order()`.");
  }
}

// `x` should be a flattened df with no df-cols
static inline
size_t df_compute_n_bytes_lazy_raw(SEXP x) {
  r_ssize n_cols = r_length(x);

  size_t multiplier = 0;

  for (r_ssize i = 0; i < n_cols; ++i) {
    SEXP col = VECTOR_ELT(x, i);
    const enum vctrs_type type = vec_proxy_typeof(col);

    size_t col_multiplier = vec_compute_n_bytes_lazy_raw(col, type);

    if (col_multiplier > multiplier) {
      multiplier = col_multiplier;
    }
  }

  return multiplier;
}

// -----------------------------------------------------------------------------

static size_t df_compute_n_bytes_lazy_counts(SEXP x);

/*
 * Compute the minimum size required for `p_counts`
 *
 * - For integer, we use 4 passes.
 * - For double, we use 8 passes.
 * - Logical uses integer radix sorting.
 * - Character uses integer radix sorting.
 * - Complex uses double radix sorting.
 */
static inline
size_t vec_compute_n_bytes_lazy_counts(SEXP x, const enum vctrs_type type) {
  switch (type) {
  case vctrs_type_integer:
  case vctrs_type_logical:
  case vctrs_type_character:
    return INT_MAX_RADIX_PASS;
  case vctrs_type_double:
  case vctrs_type_complex:
    return DBL_MAX_RADIX_PASS;
  case vctrs_type_dataframe:
    return df_compute_n_bytes_lazy_counts(x);
  default:
    Rf_errorcall(R_NilValue, "This type is not supported by `vec_order()`.");
  }
}

// `x` should be a flattened df with no df-cols
static
size_t df_compute_n_bytes_lazy_counts(SEXP x) {
  r_ssize n_cols = r_length(x);

  size_t multiplier = 0;

  for (r_ssize i = 0; i < n_cols; ++i) {
    SEXP col = VECTOR_ELT(x, i);
    const enum vctrs_type type = vec_proxy_typeof(col);

    size_t col_multiplier = vec_compute_n_bytes_lazy_counts(col, type);

    if (col_multiplier > multiplier) {
      multiplier = col_multiplier;
    }
  }

  return multiplier;
}

// -----------------------------------------------------------------------------

static SEXP df_expand_args(SEXP x, SEXP args);

/*
 * `vec_order_expand_args()` checks the type and length of `decreasing` and
 * `na_largest` and possibly expands them.
 *
 * `x` is expected to be the original input, before `vec_proxy_order()` is
 * called on it.
 *
 * If `x` is not a data frame, `decreasing` and `na_largest` must be boolean
 * values. If `x` is something like a rcrd type with a multi-column data frame
 * proxy, then restricting to a boolean argument is correct, and works because
 * the single value will be recycled across the columns.
 *
 * If `x` is a data frame, and `decreasing` or `na_largest` is size 1, we return
 * it untouched and it will be recycled correctly.
 *
 * If `x` is a data frame and the size of the arg matches the number of
 * columns of `x`, we have to be careful to "expand" the arg to match
 * the number of columns of `x` that will exist after `vec_proxy_order()`
 * is called. It flattens df-cols which might either already exist in `x`,
 * or may arise from rcrd columns that have data frame proxies. The majority
 * of the code here is for tracking this expansion.
 */
static
SEXP vec_order_expand_args(SEXP x, SEXP decreasing, SEXP na_largest) {
  SEXP args = PROTECT(r_new_list(2));
  SET_VECTOR_ELT(args, 0, decreasing);
  SET_VECTOR_ELT(args, 1, na_largest);

  // Don't check length here. These might be vectorized if `x` is a data frame.
  if (TYPEOF(decreasing) != LGLSXP) {
    Rf_errorcall(R_NilValue, "Internal error: `decreasing` must be logical");
  }
  if (lgl_any_na(decreasing)) {
    Rf_errorcall(R_NilValue, "Internal error: `decreasing` can't contain missing values.");
  }

  if (TYPEOF(na_largest) != LGLSXP) {
    Rf_errorcall(R_NilValue, "Internal error: `na_largest` must be logical");
  }
  if (lgl_any_na(na_largest)) {
    Rf_errorcall(R_NilValue, "Internal error: `na_largest` can't contain missing values.");
  }

  if (is_data_frame(x)) {
    args = df_expand_args(x, args);
    UNPROTECT(1);
    return args;
  }

  if (r_length(decreasing) != 1) {
    Rf_errorcall(R_NilValue, "`direction` must be a single value when `x` is not a data frame.");
  }

  if (r_length(na_largest) != 1) {
    Rf_errorcall(R_NilValue, "`na_value` must be a single value when `x` is not a data frame.");
  }

  UNPROTECT(1);
  return args;
}

static SEXP expand_arg(SEXP arg, const int* p_expansions, r_ssize arg_size, r_ssize size);
static int vec_decreasing_expansion(SEXP x);

static
SEXP df_expand_args(SEXP x, SEXP args) {
  SEXP decreasing = VECTOR_ELT(args, 0);
  SEXP na_largest = VECTOR_ELT(args, 1);

  r_ssize n_decreasing = r_length(decreasing);
  r_ssize n_na_largest = r_length(na_largest);
  r_ssize n_cols = r_length(x);

  // They will be recycled correctly even if columns get flattened
  if (n_decreasing == 1 && n_na_largest == 1) {
    return args;
  }

  // Must start out with the same length as the number of columns
  if (n_decreasing != 1 && n_decreasing != n_cols) {
    Rf_errorcall(
      R_NilValue,
      "`direction` should have length 1 or length equal to the number of "
      "columns of `x` when `x` is a data frame."
    );
  }

  if (n_na_largest != 1 && n_na_largest != n_cols) {
    Rf_errorcall(
      R_NilValue,
      "`na_value` should have length 1 or length equal to the number of "
      "columns of `x` when `x` is a data frame."
    );
  }

  SEXP expansions = PROTECT(Rf_allocVector(INTSXP, n_cols));
  int* p_expansions = INTEGER(expansions);

  int size = 0;
  bool needs_expansion = false;

  // Compute expansion factor
  for (r_ssize i = 0; i < n_cols; ++i) {
    SEXP col = VECTOR_ELT(x, i);
    int expansion = vec_decreasing_expansion(col);

    if (expansion != 1) {
      needs_expansion = true;
    }

    p_expansions[i] = expansion;
    size += expansion;
  }

  if (!needs_expansion) {
    UNPROTECT(1);
    return args;
  }

  decreasing = expand_arg(decreasing, p_expansions, n_decreasing, size);
  SET_VECTOR_ELT(args, 0, decreasing);

  na_largest = expand_arg(na_largest, p_expansions, n_na_largest, size);
  SET_VECTOR_ELT(args, 1, na_largest);

  UNPROTECT(1);
  return args;
}

static
SEXP expand_arg(SEXP arg, const int* p_expansions, r_ssize n_arg, r_ssize size) {
  if (n_arg == 1) {
    return arg;
  }

  SEXP out = PROTECT(Rf_allocVector(LGLSXP, size));
  int* p_out = LOGICAL(out);

  int* p_arg = LOGICAL(arg);

  int k = 0;

  // Fill `out` with repeated `arg` values to match expanded size
  for (r_ssize i = 0; i < n_arg; ++i) {
    int col_arg = p_arg[i];
    int expansion = p_expansions[i];

    for (r_ssize j = 0; j < expansion; ++j) {
      p_out[k] = col_arg;
      ++k;
    }
  }

  UNPROTECT(1);
  return out;
}


static int df_decreasing_expansion(SEXP x);

static
int vec_decreasing_expansion(SEXP x) {
  // Bare columns
  if (!OBJECT(x)) {
    return 1;
  }

  // Compute number of cols in df-cols,
  // and do proxy-compare on the cols as needed
  if (is_data_frame(x)) {
    return df_decreasing_expansion(x);
  }

  int expansion;

  // Otherwise we have an S3 column that could have a data frame
  // ordering proxy containing multiple columns, so we need to check for that
  SEXP proxy = PROTECT(vec_proxy_order(x));

  // If the `proxy` is a data frame, the expansion factor is the
  // number of columns. Otherwise it is 1.
  if (is_data_frame(proxy)) {
    expansion = Rf_length(proxy);
  } else {
    expansion = 1;
  }

  UNPROTECT(1);
  return expansion;
}

// 0-col df-cols get dropped from the comparison proxy, so returning `0` here
// when a df-col has no columns should be correct
static
int df_decreasing_expansion(SEXP x) {
  r_ssize n_cols = r_length(x);

  int out = 0;

  // Accumulate the expansion factors of the cols of the df-col
  for (r_ssize i = 0; i < n_cols; ++i) {
    SEXP col = VECTOR_ELT(x, i);
    out += vec_decreasing_expansion(col);
  }

  return out;
}

// -----------------------------------------------------------------------------

/*
 * `na_value` -> `na_largest` is parsed as:
 * largest -> TRUE
 * smallest -> FALSE
 * `na_largest` maps directly to `na_last` unless we are in decreasing order,
 * in which case `na_last = !na_largest`.
 */
static
SEXP vec_order_compute_na_last(SEXP na_largest, SEXP decreasing) {
  const r_ssize size = r_length(na_largest);
  if (size != r_length(decreasing)) {
    r_stop_internal(
      "vec_order_compute_na_last",
      "`na_largest` and `decreasing` should already match in size."
    );
  }

  SEXP na_last = PROTECT(r_new_logical(size));
  int* p_na_last = LOGICAL(na_last);

  const int* p_na_largest = LOGICAL_RO(na_largest);
  const int* p_decreasing = LOGICAL_RO(decreasing);

  for (r_ssize i = 0; i < size; ++i) {
    p_na_last[i] = p_decreasing[i] ? !p_na_largest[i] : p_na_largest[i];
  }

  UNPROTECT(1);
  return na_last;
}

// -----------------------------------------------------------------------------

static int parse_na_value_one(SEXP x);

static
SEXP parse_na_value(SEXP na_value) {
  // Don't care about length here, checked later
  if (TYPEOF(na_value) != STRSXP) {
    Rf_errorcall(R_NilValue, "`na_value` must be a character vector.");
  }

  R_len_t size = Rf_length(na_value);
  const SEXP* p_na_value = STRING_PTR_RO(na_value);

  SEXP na_largest = PROTECT(Rf_allocVector(LGLSXP, size));
  int* p_na_largest = LOGICAL(na_largest);

  for (R_len_t i = 0; i < size; ++i) {
    p_na_largest[i] = parse_na_value_one(p_na_value[i]);
  }

  UNPROTECT(1);
  return na_largest;
}

static
int parse_na_value_one(SEXP x) {
  if (x == NA_STRING) {
    Rf_errorcall(R_NilValue, "`na_value` can't be missing.");
  }

  const char* c_x = CHAR(x);

  if (!strcmp(c_x, "largest")) return 1;
  if (!strcmp(c_x, "smallest")) return 0;

  Rf_errorcall(
    R_NilValue,
    "`na_value` must contain only \"largest\" or \"smallest\"."
  );
}

static int parse_direction_one(SEXP x);

static
SEXP parse_direction(SEXP direction) {
  // Don't care about length here, checked later
  if (TYPEOF(direction) != STRSXP) {
    Rf_errorcall(R_NilValue, "`direction` must be a character vector.");
  }

  R_len_t size = Rf_length(direction);
  const SEXP* p_direction = STRING_PTR_RO(direction);

  SEXP decreasing = PROTECT(Rf_allocVector(LGLSXP, size));
  int* p_decreasing = LOGICAL(decreasing);

  for (R_len_t i = 0; i < size; ++i) {
    p_decreasing[i] = parse_direction_one(p_direction[i]);
  }

  UNPROTECT(1);
  return decreasing;
}

static
int parse_direction_one(SEXP x) {
  if (x == NA_STRING) {
    Rf_errorcall(R_NilValue, "`direction` can't be missing.");
  }

  const char* c_x = CHAR(x);

  if (!strcmp(c_x, "asc")) return 0;
  if (!strcmp(c_x, "desc")) return 1;

  Rf_errorcall(
    R_NilValue,
    "`direction` must contain only \"asc\" or \"desc\"."
  );
}

static inline
bool parse_nan_distinct(SEXP nan_distinct) {
  if (TYPEOF(nan_distinct) != LGLSXP) {
    Rf_errorcall(R_NilValue, "`nan_distinct` must be a logical vector.");
  }
  if (Rf_length(nan_distinct) != 1) {
    Rf_errorcall(R_NilValue, "`nan_distinct` must be length 1.");
  }

  int c_nan_distinct = LOGICAL_RO(nan_distinct)[0];

  if (c_nan_distinct == NA_LOGICAL) {
    Rf_errorcall(R_NilValue, "`nan_distinct` can't be missing.");
  }

  return (bool) c_nan_distinct;
}
