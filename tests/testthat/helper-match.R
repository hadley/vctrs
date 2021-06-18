compute_nested_containment_info <- function(x, condition, multiple, check_duplicates) {
  .Call(vctrs_test_compute_nested_containment_info, x, condition, multiple, check_duplicates)
}
