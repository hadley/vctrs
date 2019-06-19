#include "vctrs.h"
#include "utils.h"

R_len_t rcrd_size(SEXP x);

// [[ register(); include("vctrs.h") ]]
SEXP vctrs_vec_dim(SEXP x) {
  SEXP dim = PROTECT(Rf_getAttrib(x, R_DimSymbol));

  if (dim == R_NilValue) {
    dim = r_int(Rf_length(x));
  }

  UNPROTECT(1);
  return dim;
}

// [[ include("vctrs.h") ]]
R_len_t vec_dim_n(SEXP x) {
  return Rf_length(vctrs_vec_dim(x));
}

// [[ register() ]]
SEXP vctrs_dim_n(SEXP x) {
  return r_int(vec_dim_n(x));
}

// [[ include("vctrs.h") ]]
R_len_t vec_size(SEXP x) {
  int nprot = 0;

  struct vctrs_proxy_info info = PROTECT_PROXY_INFO(vec_proxy_info(x), &nprot);
  SEXP data = info.proxy;

  R_len_t size;
  switch (info.type) {
  case vctrs_type_null:
    size = 0;
    break;
  case vctrs_type_logical:
  case vctrs_type_integer:
  case vctrs_type_double:
  case vctrs_type_complex:
  case vctrs_type_character:
  case vctrs_type_raw:
  case vctrs_type_list: {
    SEXP dims = Rf_getAttrib(data, R_DimSymbol);
    if (dims == R_NilValue || Rf_length(dims) == 0) {
      size = Rf_length(data);
      break;
    }

    if (TYPEOF(dims) != INTSXP) {
      Rf_errorcall(R_NilValue, "Corrupt vector: dims is not integer vector");
    }

    size = INTEGER(dims)[0];
    break;
  }

  case vctrs_type_dataframe:
    size = df_size(data);
    break;

  default: {
    struct vctrs_arg arg = new_wrapper_arg(NULL, "x");
    stop_scalar_type(x, &arg);
  }}

  UNPROTECT(nprot);
  return size;
}
// [[ register() ]]
SEXP vctrs_size(SEXP x) {
  return Rf_ScalarInteger(vec_size(x));
}

R_len_t df_rownames_size(SEXP x) {
  for (SEXP attr = ATTRIB(x); attr != R_NilValue; attr = CDR(attr)) {
    if (TAG(attr) != R_RowNamesSymbol) {
      continue;
    }

    SEXP rn = CAR(attr);
    R_len_t n = Rf_length(rn);

    switch(TYPEOF(rn)) {
    case INTSXP:
      if (is_compact_rownames(rn)) {
        return compact_rownames_length(rn);
      } else {
        return n;
      }
    case STRSXP:
      return n;
    default:
      Rf_errorcall(R_NilValue, "Corrupt data frame: row.names are invalid type");
    }
  }

  return -1;
}

// For performance, avoid Rf_getAttrib() because it automatically transforms
// the rownames into an integer vector
R_len_t df_size(SEXP x) {
  R_len_t n = df_rownames_size(x);

  if (n < 0) {
    Rf_errorcall(R_NilValue, "Corrupt data frame: row.names are missing");
  }

  return n;
}
// Supports bare lists as well
R_len_t df_raw_size(SEXP x) {
  R_len_t n = df_rownames_size(x);
  if (n >= 0) {
    return n;
  }

  if (Rf_length(x) >= 1) {
    return vec_size(VECTOR_ELT(x, 0));
  } else {
    return 0;
  }
}

// [[ register() ]]
SEXP vctrs_df_size(SEXP x) {
  return r_int(df_raw_size(x));
}


R_len_t rcrd_size(SEXP x) {
  int n = Rf_length(x);
  if (n == 0) {
    return 0;
  } else {
    return Rf_length(VECTOR_ELT(x, 0));
  }
}

bool has_dim(SEXP x) {
  return ATTRIB(x) != R_NilValue && Rf_getAttrib(x, R_DimSymbol) != R_NilValue;
}

// [[ include("vctrs.h") ]]
SEXP vec_recycle(SEXP x, R_len_t size) {
  if (x == R_NilValue) {
    return R_NilValue;
  }

  R_len_t n_x = vec_size(x);

  if (n_x == size) {
    return x;
  }

  if (size == 0L) {
    return vec_slice(x, R_NilValue);
  }

  if (n_x == 1L) {
    // FIXME: Replace with ALTREP repetition
    SEXP i = PROTECT(Rf_allocVector(INTSXP, size));
    r_int_fill(i, 1, size);
    SEXP out = vec_slice(x, i);

    UNPROTECT(1);
    return out;
  }

  Rf_errorcall(R_NilValue, "Incompatible lengths: %d, %d", n_x, size);
}

// [[ register() ]]
SEXP vctrs_recycle(SEXP x, SEXP size_obj) {
  if (x == R_NilValue || size_obj == R_NilValue) {
    return R_NilValue;
  }

  size_obj = PROTECT(vec_cast(size_obj, vctrs_shared_empty_int, args_empty, args_empty));
  R_len_t size = r_int_get(size_obj, 0);
  UNPROTECT(1);

  return vec_recycle(x, size);
}


// [[ include("utils.h") ]]
R_len_t size_validate(SEXP size, const char* arg) {
  size = vec_cast(size, vctrs_shared_empty_int, args_empty, args_empty);

  if (Rf_length(size) != 1) {
    Rf_errorcall(R_NilValue, "`%s` must be a single integer.", arg);
  }

  return r_int_get(size, 0);
}
