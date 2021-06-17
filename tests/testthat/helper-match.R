compute_nested_containment_info <- function(x, condition, multiple, unique) {
  .Call(vctrs_test_compute_nested_containment_info, x, condition, multiple, unique)
}
