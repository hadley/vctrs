#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <stdbool.h>
#include <R_ext/Rdynload.h>
#include "altrep-rle.h"
#include "vctrs.h"

// Compile with `-fvisibility=hidden -DHAVE_VISIBILITY_ATTRIBUTE` if you link to this library
#include <R_ext/Visibility.h>
#define export attribute_visible extern


extern SEXP vctrs_list_get(SEXP, SEXP);
extern SEXP vctrs_list_set(SEXP, SEXP, SEXP);
extern SEXP vctrs_field_get(SEXP, SEXP);
extern SEXP vctrs_field_set(SEXP, SEXP, SEXP);
extern SEXP vctrs_fields(SEXP);
extern SEXP vctrs_n_fields(SEXP);
extern SEXP vctrs_hash(SEXP);
extern SEXP vctrs_hash_object(SEXP);
extern SEXP vctrs_equal_object(SEXP, SEXP);
extern SEXP vctrs_in(SEXP, SEXP, SEXP);
extern SEXP vctrs_duplicated(SEXP);
extern SEXP vctrs_unique_loc(SEXP);
extern SEXP vctrs_count(SEXP);
extern SEXP vctrs_id(SEXP);
extern SEXP vctrs_n_distinct(SEXP);
extern SEXP vec_split(SEXP, SEXP);
extern SEXP vctrs_group_id(SEXP);
extern SEXP vctrs_group_rle(SEXP);
extern SEXP vec_group_loc(SEXP);
extern SEXP vctrs_equal(SEXP, SEXP, SEXP);
extern SEXP vctrs_equal_na(SEXP);
extern SEXP vctrs_compare(SEXP, SEXP, SEXP);
extern SEXP vctrs_match(SEXP, SEXP, SEXP);
extern SEXP vctrs_duplicated_any(SEXP);
extern SEXP vctrs_size(SEXP);
extern SEXP vec_dim(SEXP);
extern SEXP vctrs_dim_n(SEXP);
extern SEXP vctrs_is_unspecified(SEXP);
extern SEXP vctrs_typeof(SEXP, SEXP);
extern SEXP vctrs_is_vector(SEXP);
extern SEXP vctrs_type2(SEXP, SEXP, SEXP, SEXP);
extern SEXP vctrs_typeof2(SEXP, SEXP);
extern SEXP vctrs_typeof2_s3(SEXP, SEXP);
extern SEXP vctrs_cast(SEXP, SEXP, SEXP, SEXP);
extern SEXP vctrs_as_location(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP vctrs_slice(SEXP, SEXP);
extern SEXP vctrs_init(SEXP, SEXP);
extern SEXP vctrs_chop(SEXP, SEXP);
extern SEXP vctrs_unchop(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP vctrs_chop_seq(SEXP, SEXP, SEXP, SEXP);
extern SEXP vec_slice_seq(SEXP, SEXP, SEXP, SEXP);
extern SEXP vec_slice_rep(SEXP, SEXP, SEXP);
extern SEXP vec_restore(SEXP, SEXP, SEXP);
extern SEXP vec_restore_default(SEXP, SEXP);
extern SEXP vec_proxy(SEXP);
extern SEXP vec_proxy_equal(SEXP);
extern SEXP vctrs_unspecified(SEXP);
extern SEXP vec_type(SEXP);
extern SEXP vec_ptype_finalise(SEXP);
extern SEXP vctrs_minimal_names(SEXP);
extern SEXP vctrs_unique_names(SEXP, SEXP);
extern SEXP vctrs_as_minimal_names(SEXP);
extern SEXP vec_names(SEXP);
extern SEXP vctrs_is_unique_names(SEXP);
extern SEXP vctrs_as_unique_names(SEXP, SEXP);
extern SEXP vec_set_names(SEXP, SEXP);
extern SEXP vctrs_df_as_dataframe(SEXP, SEXP, SEXP, SEXP);
extern SEXP vctrs_type2_df_df(SEXP, SEXP, SEXP, SEXP);
extern SEXP vctrs_type_info(SEXP);
extern SEXP vctrs_proxy_info(SEXP);
extern SEXP vctrs_class_type(SEXP);
extern SEXP vec_bare_df_restore(SEXP, SEXP, SEXP);
extern SEXP vctrs_recycle(SEXP, SEXP, SEXP);
extern SEXP vctrs_coercible_cast(SEXP, SEXP, SEXP, SEXP);
extern SEXP vec_assign(SEXP, SEXP, SEXP);
extern SEXP vctrs_set_attributes(SEXP, SEXP);
extern SEXP vctrs_as_df_row(SEXP, SEXP);
extern SEXP vctrs_outer_names(SEXP, SEXP, SEXP);
extern SEXP vctrs_df_size(SEXP);
extern SEXP vctrs_as_df_col(SEXP, SEXP);
extern SEXP vctrs_apply_name_spec(SEXP, SEXP, SEXP, SEXP);
extern SEXP vctrs_unset_s4(SEXP, SEXP);
extern SEXP vctrs_proxy_recursive(SEXP, SEXP);
extern SEXP vctrs_maybe_translate_encoding(SEXP);
extern SEXP vctrs_maybe_translate_encoding2(SEXP, SEXP);
extern SEXP vctrs_validate_name_repair_arg(SEXP);
extern SEXP vctrs_validate_minimal_names(SEXP, SEXP);
extern SEXP vctrs_as_names(SEXP, SEXP, SEXP);
extern SEXP vctrs_is_partial(SEXP);
extern SEXP vctrs_is_list(SEXP);
extern SEXP vctrs_try_catch_callback(SEXP, SEXP);
extern SEXP vctrs_is_coercible(SEXP, SEXP, SEXP, SEXP);
extern SEXP vctrs_as_subscript(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP vctrs_as_subscript_result(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP vctrs_df_flat_width(SEXP);
extern SEXP df_flatten(SEXP);
extern SEXP vctrs_equal_scalar(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP vctrs_linked_version();
extern SEXP vctrs_new_vctrs_compact_rep_lgl(SEXP, SEXP);
extern SEXP vctrs_new_vctrs_compact_rep_int(SEXP, SEXP);
extern SEXP vctrs_new_vctrs_compact_rep_dbl(SEXP, SEXP);
extern SEXP vctrs_new_vctrs_compact_rep_chr(SEXP, SEXP);
extern SEXP vctrs_is_vctrs_compact_rep(SEXP);
extern SEXP vctrs_is_vctrs_compact_rep_compact(SEXP);



// Maturing
// In the public header
extern bool vec_is_vector(SEXP);
extern R_len_t short_vec_size(SEXP);
extern SEXP short_vec_recycle(SEXP, R_len_t);

// Experimental
// Exported but not available in the public header
extern SEXP exp_vec_cast(SEXP, SEXP);
extern SEXP exp_vec_restore(SEXP, SEXP);
extern SEXP exp_vec_proxy(SEXP);
extern SEXP exp_vec_chop(SEXP, SEXP);
extern SEXP exp_vec_proxy_assign(SEXP, SEXP, SEXP);
extern SEXP exp_vec_slice_impl(SEXP, SEXP);
extern SEXP exp_vec_names(SEXP);
extern SEXP exp_vec_set_names(SEXP, SEXP);
extern SEXP exp_short_vec_init(SEXP, R_len_t);
extern SEXP exp_short_compact_seq(R_len_t, R_len_t, bool);
extern SEXP exp_short_init_compact_seq(int*, R_len_t, R_len_t, bool);

// Defined below
SEXP vctrs_init_library(SEXP);

// Defined in altrep-rle.h
extern SEXP altrep_rle_Make(SEXP);
void vctrs_init_altrep_rle(DllInfo* dll);

extern void vctrs_init_vctrs_compact_rep(DllInfo* dll);

static const R_CallMethodDef CallEntries[] = {
  {"vctrs_list_get",                            (DL_FUNC) &vctrs_list_get, 2},
  {"vctrs_list_set",                            (DL_FUNC) &vctrs_list_set, 3},
  {"vctrs_field_get",                           (DL_FUNC) &vctrs_field_get, 2},
  {"vctrs_field_set",                           (DL_FUNC) &vctrs_field_set, 3},
  {"vctrs_fields",                              (DL_FUNC) &vctrs_fields, 1},
  {"vctrs_n_fields",                            (DL_FUNC) &vctrs_n_fields, 1},
  {"vctrs_hash",                                (DL_FUNC) &vctrs_hash, 1},
  {"vctrs_hash_object",                         (DL_FUNC) &vctrs_hash_object, 1},
  {"vctrs_equal_object",                        (DL_FUNC) &vctrs_equal_object, 2},
  {"vctrs_in",                                  (DL_FUNC) &vctrs_in, 3},
  {"vctrs_unique_loc",                          (DL_FUNC) &vctrs_unique_loc, 1},
  {"vctrs_duplicated",                          (DL_FUNC) &vctrs_duplicated, 1},
  {"vctrs_duplicated_any",                      (DL_FUNC) &vctrs_duplicated_any, 1},
  {"vctrs_count",                               (DL_FUNC) &vctrs_count, 1},
  {"vctrs_id",                                  (DL_FUNC) &vctrs_id, 1},
  {"vctrs_n_distinct",                          (DL_FUNC) &vctrs_n_distinct, 1},
  {"vctrs_split",                               (DL_FUNC) &vec_split, 2},
  {"vctrs_group_id",                            (DL_FUNC) &vctrs_group_id, 1},
  {"vctrs_group_rle",                           (DL_FUNC) &vctrs_group_rle, 1},
  {"vctrs_group_loc",                           (DL_FUNC) &vec_group_loc, 1},
  {"vctrs_size",                                (DL_FUNC) &vctrs_size, 1},
  {"vctrs_dim",                                 (DL_FUNC) &vec_dim, 1},
  {"vctrs_dim_n",                               (DL_FUNC) &vctrs_dim_n, 1},
  {"vctrs_is_unspecified",                      (DL_FUNC) &vctrs_is_unspecified, 1},
  {"vctrs_equal",                               (DL_FUNC) &vctrs_equal, 3},
  {"vctrs_equal_na",                            (DL_FUNC) &vctrs_equal_na, 1},
  {"vctrs_compare",                             (DL_FUNC) &vctrs_compare, 3},
  {"vctrs_match",                               (DL_FUNC) &vctrs_match, 3},
  {"vctrs_typeof",                              (DL_FUNC) &vctrs_typeof, 2},
  {"vctrs_init_library",                        (DL_FUNC) &vctrs_init_library, 1},
  {"vctrs_is_vector",                           (DL_FUNC) &vctrs_is_vector, 1},
  {"vctrs_type2",                               (DL_FUNC) &vctrs_type2, 4},
  {"vctrs_typeof2",                             (DL_FUNC) &vctrs_typeof2, 2},
  {"vctrs_typeof2_s3",                          (DL_FUNC) &vctrs_typeof2_s3, 2},
  {"vctrs_cast",                                (DL_FUNC) &vctrs_cast, 4},
  {"vctrs_as_location",                         (DL_FUNC) &vctrs_as_location, 8},
  {"vctrs_slice",                               (DL_FUNC) &vctrs_slice, 2},
  {"vctrs_init",                                (DL_FUNC) &vctrs_init, 2},
  {"vctrs_chop",                                (DL_FUNC) &vctrs_chop, 2},
  {"vctrs_unchop",                              (DL_FUNC) &vctrs_unchop, 5},
  {"vctrs_chop_seq",                            (DL_FUNC) &vctrs_chop_seq, 4},
  {"vctrs_slice_seq",                           (DL_FUNC) &vec_slice_seq, 4},
  {"vctrs_slice_rep",                           (DL_FUNC) &vec_slice_rep, 3},
  {"vctrs_restore",                             (DL_FUNC) &vec_restore, 3},
  {"vctrs_restore_default",                     (DL_FUNC) &vec_restore_default, 2},
  {"vctrs_proxy",                               (DL_FUNC) &vec_proxy, 1},
  {"vctrs_proxy_equal",                         (DL_FUNC) &vec_proxy_equal, 1},
  {"vctrs_unspecified",                         (DL_FUNC) &vctrs_unspecified, 1},
  {"vctrs_type",                                (DL_FUNC) &vec_type, 1},
  {"vctrs_ptype_finalise",                      (DL_FUNC) &vec_ptype_finalise, 1},
  {"vctrs_minimal_names",                       (DL_FUNC) &vctrs_minimal_names, 1},
  {"vctrs_unique_names",                        (DL_FUNC) &vctrs_unique_names, 2},
  {"vctrs_as_minimal_names",                    (DL_FUNC) &vctrs_as_minimal_names, 1},
  {"vctrs_names",                               (DL_FUNC) &vec_names, 1},
  {"vctrs_is_unique_names",                     (DL_FUNC) &vctrs_is_unique_names, 1},
  {"vctrs_as_unique_names",                     (DL_FUNC) &vctrs_as_unique_names, 2},
  {"vctrs_set_names",                           (DL_FUNC) &vec_set_names, 2},
  {"vctrs_df_as_dataframe",                     (DL_FUNC) &vctrs_df_as_dataframe, 4},
  {"vctrs_type2_df_df",                         (DL_FUNC) &vctrs_type2_df_df, 4},
  {"vctrs_type_info",                           (DL_FUNC) &vctrs_type_info, 1},
  {"vctrs_proxy_info",                          (DL_FUNC) &vctrs_proxy_info, 1},
  {"vctrs_class_type",                          (DL_FUNC) &vctrs_class_type, 1},
  {"vctrs_bare_df_restore",                     (DL_FUNC) &vec_bare_df_restore, 3},
  {"vctrs_recycle",                             (DL_FUNC) &vctrs_recycle, 3},
  {"vctrs_coercible_cast",                      (DL_FUNC) &vctrs_coercible_cast, 4},
  {"vctrs_assign",                              (DL_FUNC) &vec_assign, 3},
  {"vctrs_set_attributes",                      (DL_FUNC) &vctrs_set_attributes, 2},
  {"vctrs_as_df_row",                           (DL_FUNC) &vctrs_as_df_row, 2},
  {"vctrs_outer_names",                         (DL_FUNC) &vctrs_outer_names, 3},
  {"vctrs_df_size",                             (DL_FUNC) &vctrs_df_size, 1},
  {"vctrs_as_df_col",                           (DL_FUNC) &vctrs_as_df_col, 2},
  {"vctrs_apply_name_spec",                     (DL_FUNC) &vctrs_apply_name_spec, 4},
  {"vctrs_unset_s4",                            (DL_FUNC) &vctrs_unset_s4, 1},
  {"vctrs_proxy_recursive",                     (DL_FUNC) &vctrs_proxy_recursive, 2},
  {"vctrs_maybe_translate_encoding",            (DL_FUNC) &vctrs_maybe_translate_encoding, 1},
  {"vctrs_maybe_translate_encoding2",           (DL_FUNC) &vctrs_maybe_translate_encoding2, 2},
  {"vctrs_rle",                                 (DL_FUNC) &altrep_rle_Make, 1},
  {"vctrs_validate_name_repair_arg",            (DL_FUNC) &vctrs_validate_name_repair_arg, 1},
  {"vctrs_validate_minimal_names",              (DL_FUNC) &vctrs_validate_minimal_names, 2},
  {"vctrs_as_names",                            (DL_FUNC) &vctrs_as_names, 3},
  {"vctrs_is_partial",                          (DL_FUNC) &vctrs_is_partial, 1},
  {"vctrs_is_list",                             (DL_FUNC) &vctrs_is_list, 1},
  {"vctrs_try_catch_callback",                  (DL_FUNC) &vctrs_try_catch_callback, 2},
  {"vctrs_is_coercible",                        (DL_FUNC) &vctrs_is_coercible, 4},
  {"vctrs_as_subscript",                        (DL_FUNC) &vctrs_as_subscript, 5},
  {"vctrs_as_subscript_result",                 (DL_FUNC) &vctrs_as_subscript_result, 5},
  {"vctrs_df_flat_width",                       (DL_FUNC) &vctrs_df_flat_width, 1},
  {"vctrs_df_flatten",                          (DL_FUNC) &df_flatten, 1},
  {"vctrs_equal_scalar",                        (DL_FUNC) &vctrs_equal_scalar, 5},
  {"vctrs_linked_version",                      (DL_FUNC) &vctrs_linked_version, 0},
  {"vctrs_new_vctrs_compact_rep_lgl",           (DL_FUNC) &vctrs_new_vctrs_compact_rep_lgl, 2},
  {"vctrs_new_vctrs_compact_rep_int",           (DL_FUNC) &vctrs_new_vctrs_compact_rep_int, 2},
  {"vctrs_new_vctrs_compact_rep_dbl",           (DL_FUNC) &vctrs_new_vctrs_compact_rep_dbl, 2},
  {"vctrs_new_vctrs_compact_rep_chr",           (DL_FUNC) &vctrs_new_vctrs_compact_rep_chr, 2},
  {"vctrs_is_vctrs_compact_rep",                (DL_FUNC) &vctrs_is_vctrs_compact_rep, 1},
  {"vctrs_is_vctrs_compact_rep_compact",        (DL_FUNC) &vctrs_is_vctrs_compact_rep_compact, 1},
  {NULL, NULL, 0}
};

extern SEXP vctrs_type_common(SEXP, SEXP, SEXP, SEXP);
extern SEXP vctrs_size_common(SEXP, SEXP, SEXP, SEXP);
extern SEXP vctrs_recycle_common(SEXP, SEXP, SEXP, SEXP);
extern SEXP vctrs_cast_common(SEXP, SEXP, SEXP, SEXP);
extern SEXP vctrs_rbind(SEXP, SEXP, SEXP, SEXP);
extern SEXP vctrs_cbind(SEXP, SEXP, SEXP, SEXP);
extern SEXP vctrs_c(SEXP, SEXP, SEXP, SEXP);
extern SEXP vctrs_new_data_frame(SEXP);

static const R_ExternalMethodDef ExtEntries[] = {
  {"vctrs_type_common",                (DL_FUNC) &vctrs_type_common, 1},
  {"vctrs_size_common",                (DL_FUNC) &vctrs_size_common, 2},
  {"vctrs_recycle_common",             (DL_FUNC) &vctrs_recycle_common, 1},
  {"vctrs_cast_common",                (DL_FUNC) &vctrs_cast_common, 1},
  {"vctrs_rbind",                      (DL_FUNC) &vctrs_rbind, 3},
  {"vctrs_cbind",                      (DL_FUNC) &vctrs_cbind, 3},
  {"vctrs_c",                          (DL_FUNC) &vctrs_c, 3},
  {"vctrs_new_data_frame",             (DL_FUNC) &vctrs_new_data_frame, -1},
  {NULL, NULL, 0}
};

export void R_init_vctrs(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, ExtEntries);
    R_useDynamicSymbols(dll, FALSE);

    // Maturing
    // In the public header
    R_RegisterCCallable("vctrs", "vec_is_vector",      (DL_FUNC) &vec_is_vector);
    R_RegisterCCallable("vctrs", "short_vec_size",     (DL_FUNC) &short_vec_size);
    R_RegisterCCallable("vctrs", "short_vec_recycle",  (DL_FUNC) &short_vec_recycle);

    // Experimental
    // Exported but not available in the public header
    R_RegisterCCallable("vctrs", "exp_vec_cast",                (DL_FUNC) &exp_vec_cast);
    R_RegisterCCallable("vctrs", "exp_vec_restore",             (DL_FUNC) &exp_vec_restore);
    R_RegisterCCallable("vctrs", "exp_vec_proxy",               (DL_FUNC) &exp_vec_proxy);
    R_RegisterCCallable("vctrs", "exp_vec_chop",                (DL_FUNC) &exp_vec_chop);
    R_RegisterCCallable("vctrs", "exp_vec_proxy_assign",        (DL_FUNC) &exp_vec_proxy_assign);
    R_RegisterCCallable("vctrs", "exp_vec_slice_impl",          (DL_FUNC) &exp_vec_slice_impl);
    R_RegisterCCallable("vctrs", "exp_vec_names",               (DL_FUNC) &exp_vec_names);
    R_RegisterCCallable("vctrs", "exp_vec_set_names",           (DL_FUNC) &exp_vec_set_names);
    R_RegisterCCallable("vctrs", "exp_short_vec_init",          (DL_FUNC) &exp_short_vec_init);
    R_RegisterCCallable("vctrs", "exp_short_compact_seq",       (DL_FUNC) &exp_short_compact_seq);
    R_RegisterCCallable("vctrs", "exp_short_init_compact_seq",  (DL_FUNC) &exp_short_init_compact_seq);

    // Altrep classes
    vctrs_init_altrep_rle(dll);
    vctrs_init_vctrs_compact_rep(dll);
}


void vctrs_init_cast(SEXP ns);
void vctrs_init_data(SEXP ns);
void vctrs_init_dictionary(SEXP ns);
void vctrs_init_names(SEXP ns);
void vctrs_init_proxy_restore(SEXP ns);
void vctrs_init_slice(SEXP ns);
void vctrs_init_slice_assign(SEXP ns);
void vctrs_init_subscript(SEXP ns);
void vctrs_init_subscript_loc(SEXP ns);
void vctrs_init_ptype2_dispatch(SEXP ns);
void vctrs_init_type(SEXP ns);
void vctrs_init_type_info(SEXP ns);
void vctrs_init_unspecified(SEXP ns);
void vctrs_init_utils(SEXP ns);

SEXP vctrs_init_library(SEXP ns) {
  vctrs_init_cast(ns);
  vctrs_init_data(ns);
  vctrs_init_dictionary(ns);
  vctrs_init_names(ns);
  vctrs_init_proxy_restore(ns);
  vctrs_init_slice(ns);
  vctrs_init_slice_assign(ns);
  vctrs_init_subscript(ns);
  vctrs_init_subscript_loc(ns);
  vctrs_init_ptype2_dispatch(ns);
  vctrs_init_type(ns);
  vctrs_init_type_info(ns);
  vctrs_init_unspecified(ns);
  vctrs_init_utils(ns);
  return R_NilValue;
}
