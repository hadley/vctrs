
expect_error_free <- function(object, class = NULL) {
  expect_error({{ object }}, regexp = NA, class = class)
}
