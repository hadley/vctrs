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

#ifndef VCTRS_ORDER_H
#define VCTRS_ORDER_H

#include "vctrs.h"

// -----------------------------------------------------------------------------

SEXP vec_order_info(SEXP x,
                    SEXP direction,
                    SEXP na_value,
                    bool nan_distinct,
                    SEXP chr_transform,
                    bool chr_ordered);

// -----------------------------------------------------------------------------

/*
 * `order` is an integer vector intended to hold the ordering vector
 * in `vec_order()`. It is allocated eagerly, but the initialization of its
 * values is done lazily. Typically, it is initialized to a 1-based sequential
 * ordering which is rearranged by the internal algorithm. However, for the
 * counting order, the initialization is not required for the first integer
 * column, which can result in a nice performance improvement.
 */
struct order {
  SEXP self;
  SEXP data;
  int* p_data;
  r_ssize size;
  bool initialized;
};

#define PROTECT_ORDER(p_order, p_n) do { \
  PROTECT((p_order)->self);              \
  PROTECT((p_order)->data);              \
  *(p_n) += 2;                           \
} while (0)

static inline
struct order* new_order(r_ssize size) {
  SEXP self = PROTECT(r_new_raw(sizeof(struct order)));
  struct order* p_order = (struct order*) RAW(self);

  SEXP data = PROTECT(Rf_allocVector(INTSXP, size));
  int* p_data = INTEGER(data);

  p_order->self = self;
  p_order->data = data;
  p_order->p_data = p_data;
  p_order->size = size;
  p_order->initialized = false;

  UNPROTECT(2);
  return p_order;
}

static inline
int* init_order(struct order* p_order) {
  if (p_order->initialized) {
    return p_order->p_data;
  }

  // Initialize `x` with sequential 1-based ordering
  for (r_ssize i = 0; i < p_order->size; ++i) {
    p_order->p_data[i] = i + 1;
  }

  p_order->initialized = true;

  return p_order->p_data;
}

// -----------------------------------------------------------------------------
#endif
