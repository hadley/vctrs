#include <rlang.h>
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
extern SEXP vctrs_match(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP vctrs_in(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP vctrs_duplicated_any(SEXP);
extern SEXP vctrs_size(SEXP);
extern SEXP vctrs_list_sizes(SEXP);
extern SEXP vctrs_dim(SEXP);
extern SEXP vctrs_dim_n(SEXP);
extern SEXP vctrs_is_unspecified(SEXP);
extern SEXP vctrs_typeof(SEXP, SEXP);
extern SEXP vctrs_is_vector(SEXP);
extern SEXP vctrs_ptype2(SEXP, SEXP, SEXP, SEXP);
extern SEXP vctrs_typeof2(SEXP, SEXP);
extern SEXP vctrs_typeof2_s3(SEXP, SEXP);
extern SEXP vctrs_cast(SEXP, SEXP, SEXP, SEXP);
extern SEXP vctrs_as_location(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP vec_slice(SEXP, SEXP);
extern SEXP vctrs_init(SEXP, SEXP);
extern SEXP vctrs_chop(SEXP, SEXP);
extern SEXP vctrs_unchop(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP vctrs_chop_seq(SEXP, SEXP, SEXP, SEXP);
extern SEXP vec_slice_seq(SEXP, SEXP, SEXP, SEXP);
extern SEXP vec_slice_rep(SEXP, SEXP, SEXP);
extern SEXP vctrs_restore(SEXP, SEXP, SEXP);
extern SEXP vctrs_restore_default(SEXP, SEXP);
extern SEXP vec_proxy(SEXP);
extern SEXP vec_proxy_equal(SEXP);
extern SEXP vec_proxy_compare(SEXP);
extern SEXP vec_proxy_order(SEXP);
extern SEXP vec_proxy_complete(SEXP);
extern SEXP vctrs_df_proxy(SEXP, SEXP);
extern SEXP vctrs_unspecified(SEXP);
extern SEXP vctrs_ptype(SEXP, SEXP);
extern SEXP vec_ptype_finalise(SEXP);
extern SEXP vctrs_minimal_names(SEXP);
extern SEXP vctrs_unique_names(SEXP, SEXP);
extern SEXP vctrs_as_minimal_names(SEXP);
extern SEXP vec_names(SEXP);
extern SEXP vctrs_is_unique_names(SEXP);
extern SEXP vctrs_as_unique_names(SEXP, SEXP);
extern SEXP vec_set_names(SEXP, SEXP);
extern SEXP vctrs_df_cast_opts(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP vctrs_df_ptype2_opts(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP vctrs_type_info(SEXP);
extern SEXP vctrs_proxy_info(SEXP);
extern SEXP vctrs_class_type(SEXP);
extern SEXP vctrs_bare_df_restore(SEXP, SEXP, SEXP);
extern SEXP vctrs_recycle(SEXP, SEXP, SEXP);
extern SEXP vctrs_assign(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP vctrs_assign_seq(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP vctrs_set_attributes(SEXP, SEXP);
extern SEXP vctrs_as_df_row(SEXP, SEXP);
extern SEXP vctrs_outer_names(SEXP, SEXP, SEXP);
extern SEXP vctrs_df_size(SEXP);
extern SEXP vctrs_as_df_col(SEXP, SEXP);
extern SEXP vctrs_apply_name_spec(SEXP, SEXP, SEXP, SEXP);
extern SEXP vctrs_unset_s4(SEXP);
extern SEXP vctrs_validate_name_repair_arg(SEXP);
extern SEXP vctrs_validate_minimal_names(SEXP, SEXP);
extern SEXP vctrs_as_names(SEXP, SEXP, SEXP, SEXP);
extern SEXP vctrs_is_partial(SEXP);
extern SEXP vctrs_is_list(SEXP);
extern SEXP vctrs_try_catch_callback(SEXP, SEXP);
extern SEXP vctrs_is_coercible(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP vctrs_as_subscript(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP vctrs_as_subscript_result(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP vctrs_df_flatten_info(SEXP);
extern SEXP df_flatten(SEXP);
extern SEXP vctrs_linked_version();
extern SEXP vctrs_tib_ptype2(SEXP x, SEXP y, SEXP x_arg_, SEXP y_arg_);
extern SEXP vctrs_tib_cast(SEXP x, SEXP y, SEXP x_arg_, SEXP y_arg_);
extern SEXP vctrs_assign_params(SEXP, SEXP, SEXP, SEXP);
extern SEXP vctrs_has_dim(SEXP);
extern SEXP vctrs_rep(SEXP, SEXP);
extern SEXP vctrs_rep_each(SEXP, SEXP);
extern SEXP vctrs_maybe_shared_col(SEXP, SEXP);
extern SEXP vctrs_new_df_unshared_col();
extern SEXP vctrs_shaped_ptype(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP vctrs_shape2(SEXP, SEXP, SEXP, SEXP);
extern SEXP vctrs_new_date(SEXP);
extern SEXP vctrs_date_validate(SEXP);
extern SEXP vctrs_new_datetime(SEXP, SEXP);
extern SEXP vctrs_datetime_validate(SEXP);
extern SEXP vctrs_ptype2_opts(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP vctrs_s3_find_method(SEXP, SEXP, SEXP);
extern SEXP vctrs_implements_ptype2(SEXP);
extern SEXP vctrs_ptype2_dispatch_native(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP vctrs_cast_dispatch_native(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP vctrs_fast_c(SEXP, SEXP);
extern SEXP vctrs_data_frame(SEXP, SEXP, SEXP);
extern SEXP vctrs_df_list(SEXP, SEXP, SEXP);
extern SEXP vctrs_identify_runs(SEXP);
extern SEXP vctrs_locate_runs(SEXP, SEXP);
extern SEXP vctrs_detect_runs(SEXP, SEXP);
extern SEXP vctrs_slice_complete(SEXP);
extern SEXP vctrs_locate_complete(SEXP);
extern SEXP vctrs_detect_complete(SEXP);
extern SEXP vctrs_normalize_encoding(SEXP);
extern SEXP vctrs_order(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP vctrs_order_locs(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP vctrs_order_info(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP vctrs_unrep(SEXP);
extern SEXP vctrs_fill_missing(SEXP, SEXP, SEXP);
extern SEXP vctrs_chr_paste_prefix(SEXP, SEXP, SEXP);
extern r_obj* vctrs_rank(r_obj*, r_obj*, r_obj*, r_obj*, r_obj*, r_obj*, r_obj*);
extern r_obj* vctrs_unique_loc2(r_obj* x);
extern r_obj* vctrs_unique2(r_obj* x);
extern r_obj* vctrs_unique_count2(r_obj* x);


// Maturing
// In the public header
extern bool vec_is_vector(SEXP);
extern R_len_t short_vec_size(SEXP);
extern SEXP short_vec_recycle(SEXP, R_len_t);

// Experimental
// Exported but not available in the public header
extern SEXP exp_vec_cast(SEXP, SEXP);
extern SEXP exp_vec_chop(SEXP, SEXP);
extern SEXP exp_vec_slice_impl(SEXP, SEXP);
extern SEXP exp_vec_names(SEXP);
extern SEXP exp_vec_set_names(SEXP, SEXP);
extern SEXP exp_short_compact_seq(R_len_t, R_len_t, bool);
extern SEXP exp_short_init_compact_seq(int*, R_len_t, R_len_t, bool);

// Defined below
SEXP vctrs_init_library(SEXP);

// Defined in altrep-rle.h
extern SEXP altrep_rle_Make(SEXP);
void vctrs_init_altrep_rle(DllInfo* dll);

static const R_CallMethodDef CallEntries[] = {
  {"vctrs_list_get",                   (DL_FUNC) &vctrs_list_get, 2},
  {"vctrs_list_set",                   (DL_FUNC) &vctrs_list_set, 3},
  {"vctrs_field_get",                  (DL_FUNC) &vctrs_field_get, 2},
  {"vctrs_field_set",                  (DL_FUNC) &vctrs_field_set, 3},
  {"vctrs_fields",                     (DL_FUNC) &vctrs_fields, 1},
  {"vctrs_n_fields",                   (DL_FUNC) &vctrs_n_fields, 1},
  {"vctrs_hash",                       (DL_FUNC) &vctrs_hash, 1},
  {"vctrs_hash_object",                (DL_FUNC) &vctrs_hash_object, 1},
  {"vctrs_equal_object",               (DL_FUNC) &vctrs_equal_object, 2},
  {"vctrs_unique_loc",                 (DL_FUNC) &vctrs_unique_loc, 1},
  {"vctrs_duplicated",                 (DL_FUNC) &vctrs_duplicated, 1},
  {"vctrs_duplicated_any",             (DL_FUNC) &vctrs_duplicated_any, 1},
  {"vctrs_count",                      (DL_FUNC) &vctrs_count, 1},
  {"vctrs_id",                         (DL_FUNC) &vctrs_id, 1},
  {"vctrs_n_distinct",                 (DL_FUNC) &vctrs_n_distinct, 1},
  {"vctrs_split",                      (DL_FUNC) &vec_split, 2},
  {"vctrs_group_id",                   (DL_FUNC) &vctrs_group_id, 1},
  {"vctrs_group_rle",                  (DL_FUNC) &vctrs_group_rle, 1},
  {"vctrs_group_loc",                  (DL_FUNC) &vec_group_loc, 1},
  {"vctrs_size",                       (DL_FUNC) &vctrs_size, 1},
  {"vctrs_list_sizes",                 (DL_FUNC) &vctrs_list_sizes, 1},
  {"vctrs_dim",                        (DL_FUNC) &vctrs_dim, 1},
  {"vctrs_dim_n",                      (DL_FUNC) &vctrs_dim_n, 1},
  {"vctrs_is_unspecified",             (DL_FUNC) &vctrs_is_unspecified, 1},
  {"vctrs_equal",                      (DL_FUNC) &vctrs_equal, 3},
  {"vctrs_equal_na",                   (DL_FUNC) &vctrs_equal_na, 1},
  {"vctrs_compare",                    (DL_FUNC) &vctrs_compare, 3},
  {"vctrs_match",                      (DL_FUNC) &vctrs_match, 5},
  {"vctrs_in",                         (DL_FUNC) &vctrs_in, 5},
  {"vctrs_typeof",                     (DL_FUNC) &vctrs_typeof, 2},
  {"vctrs_init_library",               (DL_FUNC) &vctrs_init_library, 1},
  {"vctrs_is_vector",                  (DL_FUNC) &vctrs_is_vector, 1},
  {"vctrs_ptype2",                     (DL_FUNC) &vctrs_ptype2, 4},
  {"vctrs_typeof2",                    (DL_FUNC) &vctrs_typeof2, 2},
  {"vctrs_typeof2_s3",                 (DL_FUNC) &vctrs_typeof2_s3, 2},
  {"vctrs_cast",                       (DL_FUNC) &vctrs_cast, 4},
  {"vctrs_as_location",                (DL_FUNC) &vctrs_as_location, 8},
  {"vctrs_slice",                      (DL_FUNC) &vec_slice, 2},
  {"vctrs_init",                       (DL_FUNC) &vctrs_init, 2},
  {"vctrs_chop",                       (DL_FUNC) &vctrs_chop, 2},
  {"vctrs_unchop",                     (DL_FUNC) &vctrs_unchop, 5},
  {"vctrs_chop_seq",                   (DL_FUNC) &vctrs_chop_seq, 4},
  {"vctrs_slice_seq",                  (DL_FUNC) &vec_slice_seq, 4},
  {"vctrs_slice_rep",                  (DL_FUNC) &vec_slice_rep, 3},
  {"vctrs_restore",                    (DL_FUNC) &vctrs_restore, 3},
  {"vctrs_restore_default",            (DL_FUNC) &vctrs_restore_default, 2},
  {"vctrs_proxy",                      (DL_FUNC) &vec_proxy, 1},
  {"vctrs_proxy_equal",                (DL_FUNC) &vec_proxy_equal, 1},
  {"vctrs_proxy_compare",              (DL_FUNC) &vec_proxy_compare, 1},
  {"vctrs_proxy_order",                (DL_FUNC) &vec_proxy_order, 1},
  {"vctrs_proxy_complete",             (DL_FUNC) &vec_proxy_complete, 1},
  {"vctrs_df_proxy",                   (DL_FUNC) &vctrs_df_proxy, 2},
  {"vctrs_unspecified",                (DL_FUNC) &vctrs_unspecified, 1},
  {"vctrs_ptype",                      (DL_FUNC) &vctrs_ptype, 2},
  {"vctrs_ptype_finalise",             (DL_FUNC) &vec_ptype_finalise, 1},
  {"vctrs_minimal_names",              (DL_FUNC) &vctrs_minimal_names, 1},
  {"vctrs_unique_names",               (DL_FUNC) &vctrs_unique_names, 2},
  {"vctrs_as_minimal_names",           (DL_FUNC) &vctrs_as_minimal_names, 1},
  {"vctrs_names",                      (DL_FUNC) &vec_names, 1},
  {"vctrs_is_unique_names",            (DL_FUNC) &vctrs_is_unique_names, 1},
  {"vctrs_as_unique_names",            (DL_FUNC) &vctrs_as_unique_names, 2},
  {"vctrs_set_names",                  (DL_FUNC) &vec_set_names, 2},
  {"vctrs_df_cast_opts",               (DL_FUNC) &vctrs_df_cast_opts, 5},
  {"vctrs_df_ptype2_opts",             (DL_FUNC) &vctrs_df_ptype2_opts, 5},
  {"vctrs_type_info",                  (DL_FUNC) &vctrs_type_info, 1},
  {"vctrs_proxy_info",                 (DL_FUNC) &vctrs_proxy_info, 1},
  {"vctrs_class_type",                 (DL_FUNC) &vctrs_class_type, 1},
  {"vctrs_bare_df_restore",            (DL_FUNC) &vctrs_bare_df_restore, 3},
  {"vctrs_recycle",                    (DL_FUNC) &vctrs_recycle, 3},
  {"vctrs_assign",                     (DL_FUNC) &vctrs_assign, 5},
  {"vctrs_assign_seq",                 (DL_FUNC) &vctrs_assign_seq, 5},
  {"vctrs_set_attributes",             (DL_FUNC) &vctrs_set_attributes, 2},
  {"vctrs_as_df_row",                  (DL_FUNC) &vctrs_as_df_row, 2},
  {"vctrs_outer_names",                (DL_FUNC) &vctrs_outer_names, 3},
  {"vctrs_df_size",                    (DL_FUNC) &vctrs_df_size, 1},
  {"vctrs_as_df_col",                  (DL_FUNC) &vctrs_as_df_col, 2},
  {"vctrs_apply_name_spec",            (DL_FUNC) &vctrs_apply_name_spec, 4},
  {"vctrs_unset_s4",                   (DL_FUNC) &vctrs_unset_s4, 1},
  {"vctrs_altrep_rle_Make",            (DL_FUNC) &altrep_rle_Make, 1},
  {"vctrs_validate_name_repair_arg",   (DL_FUNC) &vctrs_validate_name_repair_arg, 1},
  {"vctrs_validate_minimal_names",     (DL_FUNC) &vctrs_validate_minimal_names, 2},
  {"vctrs_as_names",                   (DL_FUNC) &vctrs_as_names, 4},
  {"vctrs_is_partial",                 (DL_FUNC) &vctrs_is_partial, 1},
  {"vctrs_is_list",                    (DL_FUNC) &vctrs_is_list, 1},
  {"vctrs_try_catch_callback",         (DL_FUNC) &vctrs_try_catch_callback, 2},
  {"vctrs_is_coercible",               (DL_FUNC) &vctrs_is_coercible, 5},
  {"vctrs_as_subscript",               (DL_FUNC) &vctrs_as_subscript, 5},
  {"vctrs_as_subscript_result",        (DL_FUNC) &vctrs_as_subscript_result, 5},
  {"vctrs_df_flatten_info",            (DL_FUNC) &vctrs_df_flatten_info, 1},
  {"vctrs_df_flatten",                 (DL_FUNC) &df_flatten, 1},
  {"vctrs_linked_version",             (DL_FUNC) &vctrs_linked_version, 0},
  {"vctrs_tib_ptype2",                 (DL_FUNC) &vctrs_tib_ptype2, 4},
  {"vctrs_tib_cast",                   (DL_FUNC) &vctrs_tib_cast, 4},
  {"vctrs_assign_params",              (DL_FUNC) &vctrs_assign_params, 4},
  {"vctrs_has_dim",                    (DL_FUNC) &vctrs_has_dim, 1},
  {"vctrs_rep",                        (DL_FUNC) &vctrs_rep, 2},
  {"vctrs_rep_each",                   (DL_FUNC) &vctrs_rep_each, 2},
  {"vctrs_maybe_shared_col",           (DL_FUNC) &vctrs_maybe_shared_col, 2},
  {"vctrs_new_df_unshared_col",        (DL_FUNC) &vctrs_new_df_unshared_col, 0},
  {"vctrs_shaped_ptype",               (DL_FUNC) &vctrs_shaped_ptype, 5},
  {"vctrs_shape2",                     (DL_FUNC) &vctrs_shape2, 4},
  {"vctrs_new_date",                   (DL_FUNC) &vctrs_new_date, 1},
  {"vctrs_date_validate",              (DL_FUNC) &vctrs_date_validate, 1},
  {"vctrs_new_datetime",               (DL_FUNC) &vctrs_new_datetime, 2},
  {"vctrs_datetime_validate",          (DL_FUNC) &vctrs_datetime_validate, 1},
  {"vctrs_ptype2_opts",                (DL_FUNC) &vctrs_ptype2_opts, 5},
  {"vctrs_s3_find_method",             (DL_FUNC) &vctrs_s3_find_method, 3},
  {"vctrs_implements_ptype2",          (DL_FUNC) &vctrs_implements_ptype2, 1},
  {"vctrs_ptype2_dispatch_native",     (DL_FUNC) &vctrs_ptype2_dispatch_native, 5},
  {"vctrs_cast_dispatch_native",       (DL_FUNC) &vctrs_cast_dispatch_native, 5},
  {"vctrs_fast_c",                     (DL_FUNC) &vctrs_fast_c, 2},
  {"vctrs_data_frame",                 (DL_FUNC) &vctrs_data_frame, 3},
  {"vctrs_df_list",                    (DL_FUNC) &vctrs_df_list, 3},
  {"vctrs_identify_runs",              (DL_FUNC) &vctrs_identify_runs, 1},
  {"vctrs_locate_runs",                (DL_FUNC) &vctrs_locate_runs, 2},
  {"vctrs_detect_runs",                (DL_FUNC) &vctrs_detect_runs, 2},
  {"vctrs_slice_complete",             (DL_FUNC) &vctrs_slice_complete, 1},
  {"vctrs_locate_complete",            (DL_FUNC) &vctrs_locate_complete, 1},
  {"vctrs_detect_complete",            (DL_FUNC) &vctrs_detect_complete, 1},
  {"vctrs_normalize_encoding",         (DL_FUNC) &vctrs_normalize_encoding, 1},
  {"vctrs_order",                      (DL_FUNC) &vctrs_order, 5},
  {"vctrs_order_locs",                 (DL_FUNC) &vctrs_order_locs, 5},
  {"vctrs_order_info",                 (DL_FUNC) &vctrs_order_info, 6},
  {"vctrs_unrep",                      (DL_FUNC) &vctrs_unrep, 1},
  {"vctrs_fill_missing",               (DL_FUNC) &vctrs_fill_missing, 3},
  {"vctrs_chr_paste_prefix",           (DL_FUNC) &vctrs_chr_paste_prefix, 3},
  {"vctrs_rank",                       (DL_FUNC) &vctrs_rank, 7},
  {"vctrs_unique_loc2",                (DL_FUNC) &vctrs_unique_loc2, 1},
  {"vctrs_unique2",                    (DL_FUNC) &vctrs_unique2, 1},
  {"vctrs_unique_count2",              (DL_FUNC) &vctrs_unique_count2, 1},
  {NULL, NULL, 0}
};

extern SEXP vctrs_type_common(SEXP, SEXP, SEXP, SEXP);
extern SEXP vctrs_ptype_common_opts(SEXP, SEXP, SEXP, SEXP);
extern SEXP vctrs_size_common(SEXP, SEXP, SEXP, SEXP);
extern SEXP vctrs_recycle_common(SEXP, SEXP, SEXP, SEXP);
extern SEXP vctrs_cast_common(SEXP, SEXP, SEXP, SEXP);
extern SEXP vctrs_cast_common_opts(SEXP, SEXP, SEXP, SEXP);
extern SEXP vctrs_rbind(SEXP, SEXP, SEXP, SEXP);
extern SEXP vctrs_cbind(SEXP, SEXP, SEXP, SEXP);
extern SEXP vctrs_c(SEXP, SEXP, SEXP, SEXP);
extern SEXP vctrs_new_data_frame(SEXP);

static const R_ExternalMethodDef ExtEntries[] = {
  {"vctrs_type_common",                (DL_FUNC) &vctrs_type_common, 1},
  {"vctrs_ptype_common_opts",          (DL_FUNC) &vctrs_ptype_common_opts, 2},
  {"vctrs_size_common",                (DL_FUNC) &vctrs_size_common, 2},
  {"vctrs_recycle_common",             (DL_FUNC) &vctrs_recycle_common, 1},
  {"vctrs_cast_common",                (DL_FUNC) &vctrs_cast_common, 1},
  {"vctrs_cast_common_opts",           (DL_FUNC) &vctrs_cast_common_opts, 2},
  {"vctrs_rbind",                      (DL_FUNC) &vctrs_rbind, 4},
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
    R_RegisterCCallable("vctrs", "exp_vec_chop",                (DL_FUNC) &exp_vec_chop);
    R_RegisterCCallable("vctrs", "exp_vec_slice_impl",          (DL_FUNC) &exp_vec_slice_impl);
    R_RegisterCCallable("vctrs", "exp_vec_names",               (DL_FUNC) &exp_vec_names);
    R_RegisterCCallable("vctrs", "exp_vec_set_names",           (DL_FUNC) &exp_vec_set_names);
    R_RegisterCCallable("vctrs", "exp_short_compact_seq",       (DL_FUNC) &exp_short_compact_seq);
    R_RegisterCCallable("vctrs", "exp_short_init_compact_seq",  (DL_FUNC) &exp_short_init_compact_seq);

    // Altrep classes
    vctrs_init_altrep_rle(dll);
}


void vctrs_init_bind(SEXP ns);
void vctrs_init_cast(SEXP ns);
void vctrs_init_data(SEXP ns);
void vctrs_init_dictionary(SEXP ns);
void vctrs_init_names(SEXP ns);
void vctrs_init_proxy_restore(SEXP ns);
void vctrs_init_slice(SEXP ns);
void vctrs_init_slice_assign(SEXP ns);
void vctrs_init_subscript(SEXP ns);
void vctrs_init_subscript_loc(SEXP ns);
void vctrs_init_ptype2(SEXP ns);
void vctrs_init_ptype2_dispatch(SEXP ns);
void vctrs_init_rep(SEXP ns);
void vctrs_init_type(SEXP ns);
void vctrs_init_type_data_frame(SEXP ns);
void vctrs_init_type_date_time(SEXP ns);
void vctrs_init_type_info(SEXP ns);
void vctrs_init_unspecified(SEXP ns);
void vctrs_init_utils(SEXP ns);

SEXP vctrs_init_library(SEXP ns) {
  r_init_library(ns);

  vctrs_init_bind(ns);
  vctrs_init_cast(ns);
  vctrs_init_data(ns);
  vctrs_init_dictionary(ns);
  vctrs_init_names(ns);
  vctrs_init_proxy_restore(ns);
  vctrs_init_slice(ns);
  vctrs_init_slice_assign(ns);
  vctrs_init_subscript(ns);
  vctrs_init_subscript_loc(ns);
  vctrs_init_ptype2(ns);
  vctrs_init_ptype2_dispatch(ns);
  vctrs_init_rep(ns);
  vctrs_init_type(ns);
  vctrs_init_type_data_frame(ns);
  vctrs_init_type_date_time(ns);
  vctrs_init_type_info(ns);
  vctrs_init_unspecified(ns);
  vctrs_init_utils(ns);
  return R_NilValue;
}
