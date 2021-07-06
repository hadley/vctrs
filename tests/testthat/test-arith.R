
test_that("logical/integer/numeric works", {
  expect_equal(vec_arith("+", TRUE, TRUE), 2L)
  expect_equal(vec_arith("+", 1L, 1L), 2L)
  expect_equal(vec_arith("+", 1, 1), 2)

  expect_equal(vec_arith("+", TRUE, 1L), 2L)
  expect_equal(vec_arith("+", 1L, TRUE), 2L)

  expect_equal(vec_arith("+", TRUE, 1), 2)
  expect_equal(vec_arith("+", 1, TRUE), 2L)

  expect_equal(vec_arith("+", 1L, 1), 2)
  expect_equal(vec_arith("+", 1, 1L), 2L)
})

test_that("default is error", {
  f <- new_vctr(1:10, class = "foo")

  expect_error(vec_arith("+", f, 1), class = "vctrs_error_incompatible_op")

  expect_error(vec_arith("+", TRUE, f), class = "vctrs_error_incompatible_op")
  expect_error(vec_arith("+", 1L, f), class = "vctrs_error_incompatible_op")
  expect_error(vec_arith("+", 1, f), class = "vctrs_error_incompatible_op")
})
