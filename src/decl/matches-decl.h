static
r_obj* vec_matches(r_obj* needles,
                   r_obj* haystack,
                   r_obj* condition,
                   r_obj* filter,
                   enum vctrs_missing_needle missing,
                   const struct vctrs_no_match* no_match,
                   const struct vctrs_no_match* remaining,
                   enum vctrs_multiple multiple,
                   enum vctrs_check_duplicates check_duplicates,
                   bool nan_distinct,
                   r_obj* chr_transform,
                   struct vctrs_arg* needles_arg,
                   struct vctrs_arg* haystack_arg);

static
r_obj* df_matches(r_obj* needles,
                  r_obj* haystack,
                  r_obj* needles_missings,
                  r_obj* haystack_missings,
                  r_ssize size_needles,
                  r_ssize size_haystack,
                  enum vctrs_missing_needle missing,
                  bool missing_propagate,
                  const struct vctrs_no_match* no_match,
                  const struct vctrs_no_match* remaining,
                  enum vctrs_multiple multiple,
                  enum vctrs_check_duplicates check_duplicates,
                  bool any_filters,
                  const enum vctrs_filter* v_filters,
                  const enum vctrs_ops* v_ops,
                  struct vctrs_arg* needles_arg,
                  struct vctrs_arg* haystack_arg);

static
void df_matches_recurse(r_ssize col,
                        r_ssize loc_lower_o_needles,
                        r_ssize loc_upper_o_needles,
                        r_ssize loc_lower_o_haystack,
                        r_ssize loc_upper_o_haystack,
                        const struct poly_df_data* p_needles,
                        const struct poly_df_data* p_haystack,
                        const struct poly_df_data* p_needles_missings,
                        const struct poly_df_data* p_haystack_missings,
                        const int* v_o_needles,
                        const int* v_o_haystack,
                        bool missing_propagate,
                        enum vctrs_multiple multiple,
                        bool any_filters,
                        const enum vctrs_filter* v_filters,
                        const enum vctrs_ops* v_ops,
                        struct r_dyn_array* p_locs_start_o_haystack,
                        struct r_dyn_array* p_match_sizes,
                        struct r_dyn_array* p_locs_needles,
                        int* v_locs_filter_match_haystack,
                        r_ssize* p_n_extra);

static
void df_matches_with_nested_groups(r_ssize size_haystack,
                                   int n_nested_groups,
                                   const int* v_nested_groups,
                                   r_ssize col,
                                   r_ssize loc_lower_o_needles,
                                   r_ssize loc_upper_o_needles,
                                   const struct poly_df_data* p_needles,
                                   const struct poly_df_data* p_haystack,
                                   const struct poly_df_data* p_needles_missings,
                                   const struct poly_df_data* p_haystack_missings,
                                   const int* v_o_needles,
                                   const int* v_o_haystack,
                                   bool missing_propagate,
                                   enum vctrs_multiple multiple,
                                   bool any_filters,
                                   const enum vctrs_filter* v_filters,
                                   const enum vctrs_ops* v_ops,
                                   struct r_dyn_array* p_locs_start_o_haystack,
                                   struct r_dyn_array* p_match_sizes,
                                   struct r_dyn_array* p_locs_needles,
                                   int* v_locs_filter_match_haystack,
                                   r_ssize* p_n_extra);

static inline
r_ssize int_locate_upper_missing(const int* v_haystack_missings,
                                 const int* v_o_haystack,
                                 r_ssize loc_lower_o_haystack,
                                 r_ssize loc_upper_o_haystack);
static inline
r_ssize int_lower_duplicate(int needle,
                            const int* v_haystack,
                            const int* v_o_haystack,
                            r_ssize loc_lower_o_haystack,
                            r_ssize loc_upper_o_haystack);
static inline
r_ssize int_upper_duplicate(int needle,
                            const int* v_haystack,
                            const int* v_o_haystack,
                            r_ssize loc_lower_o_haystack,
                            r_ssize loc_upper_o_haystack);

static
r_obj* df_joint_ranks(r_obj* x,
                      r_obj* y,
                      r_ssize x_size,
                      r_ssize y_size,
                      r_ssize n_cols,
                      r_obj* ptype,
                      bool na_propagate,
                      bool nan_distinct,
                      r_obj* chr_transform);

static
r_obj* df_joint_xtfrm_by_col(r_obj* x,
                             r_obj* y,
                             r_ssize x_size,
                             r_ssize y_size,
                             r_ssize n_cols,
                             r_obj* ptype,
                             bool nan_distinct,
                             r_obj* chr_transform);

static
r_obj* df_missings_by_col(r_obj* x, r_ssize x_size, r_ssize n_cols);

static inline
void parse_condition(r_obj* condition, enum vctrs_ops* v_ops, r_ssize n_cols);

static inline
struct vctrs_no_match parse_no_match(r_obj* no_match, const char* arg);

static inline
enum vctrs_missing_needle parse_missing(r_obj* missing);

static inline
enum vctrs_multiple parse_multiple(r_obj* multiple);

static inline
enum vctrs_check_duplicates parse_check_duplicates(r_obj* check_duplicates);

static inline
void check_for_duplicates(r_obj* info, struct vctrs_arg* arg, bool needles);

static inline
void parse_filter(r_obj* filter,
                  r_ssize n_cols,
                  enum vctrs_filter* v_filters,
                  bool* p_any_filters);

static
r_obj* expand_match_on_nothing(r_ssize size_needles,
                               r_ssize size_haystack,
                               enum vctrs_multiple multiple,
                               const struct vctrs_no_match* no_match,
                               const struct vctrs_no_match* remaining,
                               struct vctrs_arg* needles_arg,
                               struct vctrs_arg* haystack_arg);

static
r_obj* expand_compact_indices(const int* v_o_haystack,
                              struct r_dyn_array* p_locs_start_o_haystack,
                              struct r_dyn_array* p_match_sizes,
                              struct r_dyn_array* p_locs_needles,
                              bool skip_match_sizes,
                              bool skip_locs_needles,
                              enum vctrs_missing_needle missing,
                              bool missing_propagate,
                              const struct vctrs_no_match* no_match,
                              const struct vctrs_no_match* remaining,
                              enum vctrs_multiple multiple,
                              r_ssize size_needles,
                              r_ssize size_haystack,
                              bool any_directional,
                              bool has_locs_filter_match_haystack,
                              const enum vctrs_filter* v_filters,
                              const int* v_locs_filter_match_haystack,
                              const struct poly_df_data* p_haystack,
                              struct vctrs_arg* needles_arg,
                              struct vctrs_arg* haystack_arg);

static
r_obj* compute_nested_containment_info(r_obj* haystack,
                                       enum vctrs_multiple multiple,
                                       enum vctrs_check_duplicates check_duplicates,
                                       const enum vctrs_ops* v_ops,
                                       struct vctrs_arg* haystack_arg);

static
r_obj* nested_containment_order(r_obj* x,
                                r_obj* order,
                                r_obj* group_sizes,
                                r_obj* outer_run_sizes,
                                enum vctrs_multiple multiple);

static inline
int p_df_nested_containment_compare_ge_na_equal(const void* x,
                                                r_ssize i,
                                                const void* y,
                                                r_ssize j);

static inline
int p_matches_df_compare_na_equal(const void* x,
                                  r_ssize i,
                                  const void* y,
                                  r_ssize j,
                                  const enum vctrs_filter* v_filters);

static inline
bool p_matches_df_equal_na_equal(const void* x,
                                 r_ssize i,
                                 const void* y,
                                 r_ssize j,
                                 const enum vctrs_filter* v_filters);

static inline
r_ssize midpoint(r_ssize lhs, r_ssize rhs);

static inline
void stop_matches_overflow(double size);

static inline
void stop_matches_nothing(r_ssize i,
                          struct vctrs_arg* needles_arg,
                          struct vctrs_arg* haystack_arg);

static inline
void stop_matches_remaining(r_ssize i,
                            struct vctrs_arg* needles_arg,
                            struct vctrs_arg* haystack_arg);

static inline
void stop_matches_missing(r_ssize i, struct vctrs_arg* needles_arg);

static inline
void stop_matches_duplicates(r_ssize i,
                             struct vctrs_arg* arg,
                             bool needles);

static inline
void stop_matches_multiple(r_ssize i,
                           struct vctrs_arg* needles_arg,
                           struct vctrs_arg* haystack_arg);

static inline
void warn_matches_multiple(r_ssize i,
                           struct vctrs_arg* needles_arg,
                           struct vctrs_arg* haystack_arg);
