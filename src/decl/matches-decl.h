static
r_obj* vec_matches(r_obj* needles,
                   r_obj* haystack,
                   r_obj* condition,
                   bool na_equal,
                   int no_match,
                   enum vctrs_multiple multiple,
                   struct vctrs_arg* needles_arg,
                   struct vctrs_arg* haystack_arg);

static
r_obj* df_matches(r_obj* needles,
                  r_obj* haystack,
                  r_ssize size_needles,
                  r_ssize size_haystack,
                  bool na_equal,
                  int no_match,
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
r_ssize int_locate_upper_missing(int const* v_haystack,
                                 const int* v_o_haystack,
                                 r_ssize lower_o_haystack,
                                 r_ssize upper_o_haystack);
static inline
r_ssize dbl_locate_upper_missing(double const* v_haystack,
                                 const int* v_o_haystack,
                                 r_ssize lower_o_haystack,
                                 r_ssize upper_o_haystack);

static inline
r_ssize int_lower_duplicate(int needle,
                            int const* v_haystack,
                            const int* v_o_haystack,
                            r_ssize lower_o_haystack,
                            r_ssize upper_o_haystack);
static inline
r_ssize dbl_lower_duplicate(double needle,
                            double const* v_haystack,
                            const int* v_o_haystack,
                            r_ssize lower_o_haystack,
                            r_ssize upper_o_haystack);

static inline
r_ssize int_upper_duplicate(int needle,
                            int const* v_haystack,
                            const int* v_o_haystack,
                            r_ssize lower_o_haystack,
                            r_ssize upper_o_haystack);
static inline
r_ssize dbl_upper_duplicate(double needle,
                            double const* v_haystack,
                            const int* v_o_haystack,
                            r_ssize lower_o_haystack,
                            r_ssize upper_o_haystack);

static inline
void parse_condition(r_obj* condition, r_ssize size, enum vctrs_ops* v_ops);

static inline
enum vctrs_multiple parse_multiple(r_obj* multiple);

static
r_obj* expand_compact_indices(const int* v_o_haystack,
                              struct r_dyn_array* p_o_haystack_starts,
                              struct r_dyn_array* p_match_sizes,
                              struct r_dyn_array* p_needles_locs,
                              int no_match,
                              bool any_multiple);

static
r_obj* compute_nested_containment_info(r_obj* haystack, const enum vctrs_ops* v_ops);

static
r_obj* nested_containment_order(r_obj* proxy,
                                r_obj* order,
                                r_obj* group_sizes,
                                r_obj* outer_run_sizes);

static inline
int p_df_nested_containment_compare_ge_na_equal(const void* x,
                                                r_ssize i,
                                                const void* y,
                                                r_ssize j);
