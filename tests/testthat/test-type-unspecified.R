context("test-type-unspecified")

test_that("unknown type is idempotent", {
  types <- list(
    unspecified(),
    logical(),
    integer(),
    double(),
    character(),
    list(),
    new_list_of(ptype = integer()),
    new_factor(),
    new_ordered(),
    new_date(),
    new_datetime(),
    new_duration()
  )

  lhs <- map(types, vec_ptype2, x = unspecified())
  expect_equal(types, lhs)

  rhs <- map(types, vec_ptype2, y = unspecified())
  expect_equal(types, rhs)
})

test_that("subsetting works", {
  expect_identical(unspecified(4)[2:3], unspecified(2))
})

test_that("subsetting works", {
  expect_identical(unspecified(4)[2:3], unspecified(2))
})

test_that("has useful print method", {
  expect_known_output(unspecified(), print = TRUE, file = test_path("test-type-unspecified.txt"))
})

test_that("can finalise data frame containing unspecified columns", {
  df <- data.frame(y = NA, x = c(1, 2, NA))

  ptype <- vec_ptype(df)
  expect_identical(ptype$y, unspecified())

  finalised <- vec_ptype_finalise(ptype)
  expect_identical(finalised$y, lgl())

  common <- vec_ptype_common(df, df)
  expect_identical(common$y, lgl())
})

test_that("can cast to common type data frame containing unspecified columns", {
  df <- data.frame(y = NA, x = c(1, 2, NA))
  expect_identical(vec_cast_common(df, df), list(df, df))
})

test_that("unspecified vectors are always unspecified (#222)", {
  expect_true(is_unspecified(unspecified()))
  expect_true(is_unspecified(unspecified(1)))
})

test_that("S3 vectors and shaped vectors are never unspecified", {
  expect_false(is_unspecified(foobar(NA)))
  expect_false(is_unspecified(foobar(lgl(NA, NA))))
  expect_false(is_unspecified(matrix(NA, 2)))
})

test_that("can finalise lengthy unspecified vectors", {
  expect_identical(vec_ptype_finalise(unspecified(3)), rep(NA, 3))
  expect_identical(ununspecify(unspecified(3)), rep(NA, 3))
})

test_that("unspecified() validates input", {
  expect_identical(unspecified(1), unspecified(1L))
  expect_error(unspecified(1:3), "must be a single number")
})

test_that("tibble::type_sum() knows about unspecified", {
  expect_identical(tibble::type_sum(unspecified(3)), "???")
})

test_that("casting type to unspecified forwards to the logical method", {
  expect_identical(vec_cast(1L, unspecified()), TRUE)
  expect_error(vec_cast(data.frame(), unspecified()), class = "vctrs_error_incompatible_cast")
})
