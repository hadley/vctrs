#ifndef ALTREP_H
#define ALTREP_H

#include "Rversion.h"

#if (R_VERSION < R_Version(3, 5, 0))
#define HAS_ALTREP false
#define HAS_ALTREP_3_6 false
#elif (R_VERSION < R_Version(3, 6, 0))
#define HAS_ALTREP true
#define HAS_ALTREP_3_6 false
#else
#define HAS_ALTREP true
#define HAS_ALTREP_3_6 true
#endif

#if !HAS_ALTREP

# define ALTREP(x) false
# define ALTVEC_EXTRACT_SUBSET_PROXY(x, indx, call) NULL

#else

#include "R_ext/Altrep.h"

#define ALTREP_METHODS                                 \
  R_altrep_UnserializeEX_method_t UnserializeEX;       \
  R_altrep_Unserialize_method_t Unserialize;           \
  R_altrep_Serialized_state_method_t Serialized_state; \
  R_altrep_DuplicateEX_method_t DuplicateEX;           \
  R_altrep_Duplicate_method_t Duplicate;               \
  R_altrep_Coerce_method_t Coerce;                     \
  R_altrep_Inspect_method_t Inspect;                   \
  R_altrep_Length_method_t Length

#define ALTVEC_METHODS                               \
  ALTREP_METHODS;                                    \
  R_altvec_Dataptr_method_t Dataptr;                 \
  R_altvec_Dataptr_or_null_method_t Dataptr_or_null; \
  R_altvec_Extract_subset_method_t Extract_subset

typedef struct { ALTREP_METHODS; } altrep_methods_t;
typedef struct { ALTVEC_METHODS; } altvec_methods_t;

#define CLASS_METHODS_TABLE(type_class) STDVEC_DATAPTR(type_class)

#define GENERIC_METHODS_TABLE(x, type_class) \
  ((type_class##_methods_t *) CLASS_METHODS_TABLE(ALTREP_CLASS(x)))


#define ALTREP_METHODS_TABLE(x) GENERIC_METHODS_TABLE(x, altrep)
#define ALTVEC_METHODS_TABLE(x) GENERIC_METHODS_TABLE(x, altvec)

#define DISPATCH_TARGET_HELPER(x, ...) x
#define DISPATCH_TARGET(...) DISPATCH_TARGET_HELPER(__VA_ARGS__, dummy)

#define DO_DISPATCH(type, fun, ...)					\
  type##_METHODS_TABLE(DISPATCH_TARGET(__VA_ARGS__))->fun(__VA_ARGS__)

#define ALTREP_DISPATCH(fun, ...) DO_DISPATCH(ALTREP, fun, __VA_ARGS__)
#define ALTVEC_DISPATCH(fun, ...) DO_DISPATCH(ALTVEC, fun, __VA_ARGS__)

static inline SEXP ALTVEC_EXTRACT_SUBSET_PROXY(SEXP x, SEXP indx, SEXP call) {
  return ALTVEC_DISPATCH(Extract_subset, x, indx, call);
}

#endif

#endif
