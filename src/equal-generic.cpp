#include "cpp-rlang.h"
#include "cpp-utils.h"


template <enum r_type R_TYPE,
          typename C_TYPE = DEFAULT_C_TYPE(R_TYPE)>
struct equal_scalar_na_equal {
  int operator()(const C_TYPE x, const C_TYPE y) {
    return x == y;
  }
};

template <enum r_type R_TYPE,
          typename C_TYPE = DEFAULT_C_TYPE(R_TYPE)>
struct equal_scalar_na_propagate {
  const C_TYPE na = na_value<R_TYPE>();

  int operator()(const C_TYPE x, const C_TYPE y) {

    if (x == na || y == na) {
      return NA_LOGICAL;
    } else {
      return x == y;
    }
  }
};

template <enum r_type R_TYPE,
          typename C_TYPE = DEFAULT_C_TYPE(R_TYPE),
          typename EQ>
static
void equal_fill_na_loop(const void* p_x_payload,
                        const void* p_y_payload,
                        EQ eq,
                        r_ssize size,
                        int* p_out) {
  const C_TYPE* p_x = (const C_TYPE*) p_x_payload;
  const C_TYPE* p_y = (const C_TYPE*) p_y_payload;

  for (r_ssize i = 0; i < size; ++i) {
    p_out[i] = eq(p_x[i], p_y[i]);
  }
}

template <enum r_type R_TYPE>
static
void equal_fill_na_equal(const void* p_x_payload,
                         const void* p_y_payload,
                         r_ssize size,
                         int* p_out) {
  auto eq = equal_scalar_na_equal<R_TYPE>();
  equal_fill_na_loop<R_TYPE>(p_x_payload, p_y_payload, eq, size, p_out);
}

template <enum r_type R_TYPE>
static
void equal_fill_na_propagate(const void* p_x_payload,
                             const void* p_y_payload,
                             r_ssize size,
                             int* p_out) {
  auto eq = equal_scalar_na_propagate<R_TYPE>();
  equal_fill_na_loop<R_TYPE>(p_x_payload, p_y_payload, eq, size, p_out);
}

static
void equal_fill_na_equal(enum r_type type,
                         const void* p_x,
                         const void* p_y,
                         r_ssize size,
                         int* p_out) {
  switch (type) {
  case r_type_logical: equal_fill_na_equal<r_type_logical>(p_x, p_y, size, p_out); return;
  case r_type_integer: equal_fill_na_equal<r_type_integer>(p_x, p_y, size, p_out); return;
  default: throw unimplemented_type_exception("equal_fill_na_equal", type);
  }
}

static
void equal_fill_na_propagate(enum r_type type,
                             const void* p_x,
                             const void* p_y,
                             r_ssize size,
                             int* p_out) {
  switch (type) {
  case r_type_logical: equal_fill_na_propagate<r_type_logical>(p_x, p_y, size, p_out); return;
  case r_type_integer: equal_fill_na_propagate<r_type_integer>(p_x, p_y, size, p_out); return;
  default: throw unimplemented_type_exception("equal_fill_na_propagate", type);
  }
}


extern "C" {

void equal_fill(enum r_type type,
                const void* p_x,
                const void* p_y,
                r_ssize size,
                bool na_equal,
                int* p_out) {
  try {
    if (na_equal) {
      equal_fill_na_equal(type, p_x, p_y, size, p_out);
    } else {
      equal_fill_na_propagate(type, p_x, p_y, size, p_out);
    }
  } RETHROW("equal_fill")
}

} // extern "C"
