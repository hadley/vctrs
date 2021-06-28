#ifndef VCTRS_TYPE_COMPLEX_H
#define VCTRS_TYPE_COMPLEX_H

#include <rlang.h>
#include "vctrs.h"


/*
 * Normalises a complex value so that if one side is missing, both are. This
 * ensures that all missing complex values are grouped together, no matter
 * what type of missingness it is. NA and NaN can still be separated by
 * `nan_distinct`, resulting in 4 different combinations of missingness. These
 * 4 groups of missingness will still all be grouped together, either before
 * or after any non-missing values have appeared.
 * See issue #1403 for more information.
 */
static inline
r_complex_t cpl_normalise_missing(r_complex_t x) {
  const double na = r_globals.na_dbl;
  const double nan = R_NaN;

  const enum vctrs_dbl_class r_type = dbl_classify(x.r);
  const enum vctrs_dbl_class i_type = dbl_classify(x.i);

  switch (r_type) {
  case vctrs_dbl_number:
    switch (i_type) {
    case vctrs_dbl_number: return x;
    case vctrs_dbl_missing: return (r_complex_t) {na, na};
    case vctrs_dbl_nan: return (r_complex_t) {nan, nan};
    }
  case vctrs_dbl_missing:
    switch (i_type) {
    case vctrs_dbl_number: return (r_complex_t) {na, na};
    case vctrs_dbl_missing: return x;
    case vctrs_dbl_nan: return x;
    }
  case vctrs_dbl_nan:
    switch (i_type) {
    case vctrs_dbl_number: return (r_complex_t) {nan, nan};
    case vctrs_dbl_missing: return x;
    case vctrs_dbl_nan: return x;
    }
  }

  never_reached("cpl_normalise_missing");
}


#endif
