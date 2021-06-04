static
r_obj* vec_matches(r_obj* needles,
                   r_obj* haystack,
                   r_obj* condition,
                   bool na_equal,
                   const struct vctrs_no_match* no_match,
                   enum vctrs_multiple multiple,
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
                  bool na_equal,
                  const struct vctrs_no_match* no_match,
                  enum vctrs_multiple multiple,
                  enum vctrs_ops* v_ops);

static
void df_matches_recurse(r_ssize col,
                        r_ssize lower_o_needles,
                        r_ssize upper_o_needles,
                        r_ssize lower_o_haystack,
                        r_ssize upper_o_haystack,
                        const struct poly_df_data* p_needles,
                        const struct poly_df_data* p_haystack,
                        const struct poly_df_data* p_needles_missings,
                        const struct poly_df_data* p_haystack_missings,
                        const int* v_o_needles,
                        const int* v_o_haystack,
                        bool na_equal,
                        enum vctrs_multiple multiple,
                        enum vctrs_ops* v_ops,
                        struct r_dyn_array* p_o_haystack_starts,
                        struct r_dyn_array* p_match_sizes,
                        struct r_dyn_array* p_needles_locs,
                        r_ssize* p_n_extra,
                        bool* p_any_multiple);

static
void df_matches_with_nested_groups(r_ssize size_haystack,
                                   int n_nested_groups,
                                   const int* v_nested_groups,
                                   r_ssize col,
                                   r_ssize lower_o_needles,
                                   r_ssize upper_o_needles,
                                   const struct poly_df_data* p_needles,
                                   const struct poly_df_data* p_haystack,
                                   const struct poly_df_data* p_needles_missings,
                                   const struct poly_df_data* p_haystack_missings,
                                   const int* v_o_needles,
                                   const int* v_o_haystack,
                                   bool na_equal,
                                   enum vctrs_multiple multiple,
                                   enum vctrs_ops* v_ops,
                                   struct r_dyn_array* p_o_haystack_starts,
                                   struct r_dyn_array* p_match_sizes,
                                   struct r_dyn_array* p_needles_locs,
                                   r_ssize* p_n_extra,
                                   bool* p_any_multiple);

static inline
r_ssize int_locate_upper_missing(const int* v_haystack_missings,
                                 const int* v_o_haystack,
                                 r_ssize lower_o_haystack,
                                 r_ssize upper_o_haystack);
static inline
r_ssize int_lower_duplicate(int needle,
                            const int* v_haystack,
                            const int* v_o_haystack,
                            r_ssize lower_o_haystack,
                            r_ssize upper_o_haystack);
static inline
r_ssize int_upper_duplicate(int needle,
                            const int* v_haystack,
                            const int* v_o_haystack,
                            r_ssize lower_o_haystack,
                            r_ssize upper_o_haystack);

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
r_obj* df_missings_by_col(r_obj* x, r_ssize x_size, r_ssize n_cols);

static inline
void parse_condition(r_obj* condition, enum vctrs_ops* v_ops, r_ssize n_cols);

static inline
struct vctrs_no_match parse_no_match(r_obj* no_match);

static inline
enum vctrs_multiple parse_multiple(r_obj* multiple);

static
r_obj* expand_compact_indices(const int* v_o_haystack,
                              struct r_dyn_array* p_o_haystack_starts,
                              struct r_dyn_array* p_match_sizes,
                              struct r_dyn_array* p_needles_locs,
                              const struct vctrs_no_match* no_match,
                              bool any_multiple);

static
r_obj* compute_nested_containment_info(r_obj* haystack, const enum vctrs_ops* v_ops);

static
r_obj* nested_containment_order(r_obj* x,
                                r_obj* order,
                                r_obj* group_sizes,
                                r_obj* outer_run_sizes);

static inline
int p_df_nested_containment_compare_ge_na_equal(const void* x,
                                                r_ssize i,
                                                const void* y,
                                                r_ssize j);

static inline
r_ssize midpoint(r_ssize lhs, r_ssize rhs);
