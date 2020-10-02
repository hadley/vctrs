#ifndef CPP_RLANG_H
#define CPP_RLANG_H


#define R_NO_REMAP
#include <Rinternals.h>

#include "rlang-types.h"


// Prevent some unsafe usages
#define SEXP sexp_unsafe_in_this_file
#define SEXPREC sexp_unsafe_in_this_file
#define Rf_allocVector alloc_unsafe_in_this_file


namespace rlang {

/* Only supports types that allow memory access to the underlying C array */
template <enum r_type>
struct traits;

template <>
struct traits<r_type_logical> {
  using array_type = int;
};

template <>
struct traits<r_type_integer> {
  using array_type = int;
};

template <>
struct traits<r_type_double> {
  using array_type = double;
};

template <>
struct traits<r_type_complex> {
  using array_type = r_complex_t;
};

template <>
struct traits<r_type_raw> {
  using array_type = unsigned char*;
};

} // namespace rlang


#define C_TYPE(R_TYPE) rlang::traits<R_TYPE>::array_type
#define DEFAULT_C_TYPE(R_TYPE) typename C_TYPE(R_TYPE)


template <enum r_type R_TYPE>
typename C_TYPE(R_TYPE) na_value();

template <> int na_value<r_type_logical>() { return NA_LOGICAL; }
template <> int na_value<r_type_integer>() { return NA_INTEGER; }


#endif
