#ifndef VCTRS_RANK_H
#define VCTRS_RANK_H

#include <rlang.h>
#include "vctrs.h"

enum ties {
  TIES_min,
  TIES_max,
  TIES_sequential,
  TIES_dense
};

r_obj* vec_rank(r_obj* x,
                enum ties ties_type,
                bool na_propagate,
                r_obj* direction,
                r_obj* na_value,
                bool nan_distinct,
                r_obj* chr_transform);

#endif
