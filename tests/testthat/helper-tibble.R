
expect_grouped <- function(x, groups, names = base::names(mtcars)) {
  expect_is(x, "grouped_df")
  expect_identical(dplyr::group_vars(x), groups)
  expect_named(x, names)
}
expect_dynamically_grouped <- function(x, groups, names = base::names(mtcars)) {
  expect_grouped(x, groups, names = names)
  expect_true(dplyr::group_by_drop_default(x))
}
expect_statically_grouped <- function(x, groups, names = base::names(mtcars)) {
  expect_grouped(x, groups, names = names)
  expect_false(dplyr::group_by_drop_default(x))
}
