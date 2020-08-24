
test_that("list_flatten() expects lists", {
  expect_error(list_flatten(""), "must be a list")
  expect_error(list_flatten(mtcars), "must be a list")
})

test_that("list_flatten() is generic over lists", {
  local_count_list_methods()

  x <- new_count_list(list(1, 2, 3))
  expect_identical(list_flatten(x), x)

  x <- new_count_list(list(1, list(2), new_count_list(list(3))))
  out <- list_flatten(x)
  exp <- new_count_list(list(1, 2, 3))
  expect_identical(out, exp)
})

test_that("list_flatten() flattens inner lists", {
  expect_identical(
    list_flatten(list(1L, list(2:3))),
    list(1L, 2:3)
  )
})

test_that("list_flatten() doesn't flatten data frames", {
  x <- list(1L, data.frame(x = 2:3))
  expect_identical(list_flatten(x), x)
})

test_that("list_flatten() creates output of type `ptype`", {
  local_count_list_methods()
  expect_identical(
    list_flatten(list(1, list(2)), ptype = new_count_list()),
    new_count_list(list(1, 2))
  )
})

test_that("list_flatten() creates atomic vectors if requested", {
  expect_identical(
    list_flatten(list(1, list(2)), ptype = int()),
    1:2
  )

  x <- list(x = 1, y = list(foo = 2))
  expect_identical(
    list_flatten(x, ptype = int(), name_spec = "{outer}_{inner}"),
    c(x = 1L, y_foo = 2L)
  )

  expect_error(
    list_flatten(list(1, list(list(2))), ptype = int()),
    class = "vctrs_error_incompatible_type"
  )
})

test_that("list_flatten() preserves `NULL` elements for list ptypes", {
  # They are compacted in `purrr::flatten()`. This doesn't seem right
  # because `NULL` represents the missing element of lists and is
  # semantically different from an empty list. From this viewpoint,
  # compacting `NULL` is like compacting `NA` values in atomic
  # vectors.
  expect_identical(list_flatten(list(NULL,  NULL)), list(NULL, NULL))
  expect_identical(list_flatten(list(list(), NULL, list())), list(NULL))
})

test_that("list_flatten() compacts `NULL` elements for atomic ptypes", {
  expect_identical(list_flatten(list(NA, NULL, 1:3), ptype = int()), c(NA, 1:3))
})
