#include "vctrs.h"
#include "cast.h"
#include "utils.h"

// [[ include("vctrs.h") ]]
SEXP tib_ptype2(SEXP x, SEXP y, struct vctrs_arg* x_arg, struct vctrs_arg* y_arg) {
  SEXP out = PROTECT(df_ptype2(x, y, x_arg, y_arg));

  Rf_setAttrib(out, R_ClassSymbol, classes_tibble);

  UNPROTECT(1);
  return out;
}

// [[ register() ]]
SEXP vctrs_tib_ptype2(SEXP x, SEXP y, SEXP x_arg_, SEXP y_arg_) {
  struct vctrs_arg x_arg = vec_as_arg(x_arg_);
  struct vctrs_arg y_arg = vec_as_arg(y_arg_);
  return tib_ptype2(x, y, &x_arg, &y_arg);
}

// [[ include("vctrs.h") ]]
SEXP tib_cast(SEXP x, SEXP y, struct vctrs_arg* x_arg, struct vctrs_arg* y_arg) {
  SEXP out = PROTECT(df_cast(x, y, x_arg, y_arg));

  Rf_setAttrib(out, R_ClassSymbol, classes_tibble);

  UNPROTECT(1);
  return out;
}

// [[ register() ]]
SEXP vctrs_tib_cast(SEXP x, SEXP y, SEXP x_arg_, SEXP y_arg_) {
  struct vctrs_arg x_arg = vec_as_arg(x_arg_);
  struct vctrs_arg y_arg = vec_as_arg(y_arg_);
  return tib_cast(x, y, &x_arg, &y_arg);
}
