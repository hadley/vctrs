context("test-type-coerce")

test_that("base coercions are symmetric and unchanging", {
  types <- list(
    logical(),
    integer(),
    double(),
    character(),
    raw(),
    list()
  )
  mat <- maxtype_mat(types)

  expect_true(isSymmetric(mat))
  expect_known_output(mat, test_path("test-type2.txt"), print = TRUE)
})

test_that("new classes are uncoercible by default", {
  x <- structure(1:10, class = "vctrs_nonexistant")
  expect_error(vec_ptype2(1, x), class = "vctrs_error_incompatible_type")
  expect_error(vec_ptype2(x, 1), class = "vctrs_error_incompatible_type")
})

test_that("vec_typeof2() returns common type", {
  nms <- names(base_empty_types)

  for (i in seq_along(base_empty_types)) {
    this <- nms[[i]]

    for (j in seq_along(base_empty_types)) {
      that <- nms[[j]]

      if (i <= j) {
        exp <- paste0("vctrs_type2_", this, "_", that)
      } else {
        exp <- paste0("vctrs_type2_", that, "_", this)
      }
      out <- vec_typeof2(base_empty_types[[this]], base_empty_types[[that]])

      expect_identical(out, exp)
    }
  }
})

test_that("vec_ptype2() dispatches when inputs have shape", {
  expect_identical(dim(vec_ptype2(int(), matrix(nrow = 3, ncol = 4))), c(0L, 4L))
  expect_identical(dim(vec_ptype2(matrix("", nrow = 3), c("", "", ""))), c(0L, 1L))
})

test_that("vec_ptype2() requires vectors", {
  expect_error(vec_ptype2(NULL, quote(name)), class = "vctrs_error_scalar_type")
  expect_error(vec_ptype2(NA, quote(name)), class = "vctrs_error_scalar_type")
  expect_error(vec_ptype2(list(), quote(name)), class = "vctrs_error_scalar_type")
  expect_error(vec_ptype2(quote(name), NULL), class = "vctrs_error_scalar_type")
  expect_error(vec_ptype2(quote(name), NA), class = "vctrs_error_scalar_type")
  expect_error(vec_ptype2(quote(name), list()), class = "vctrs_error_scalar_type")
  expect_error(vec_ptype2(quote(name), quote(name)), class = "vctrs_error_scalar_type")
})

test_that("vec_ptype2() forwards argument tag", {
  expect_error(vec_ptype2(quote(name), list(), x_arg = "foo"), "`foo`", class = "vctrs_error_scalar_type")
  expect_error(vec_ptype2(list(), quote(name), y_arg = "foo"), "`foo`", class = "vctrs_error_scalar_type")
})

test_that("stop_incompatible_type() checks for scalars", {
  expect_error(stop_incompatible_type(NA, foobar()), class = "vctrs_error_scalar_type")
  expect_error(vec_ptype_common(NA, foobar()), class = "vctrs_error_scalar_type")
  expect_error(vec_ptype_common(foobar(), list()), class = "vctrs_error_scalar_type")
})

test_that("vec_ptype2() methods forward args to stop_incompatible_type()", {
  expect_args(new_hidden(), NA, x_arg = "foo", y_arg = "bar")
  expect_args(lgl(), new_hidden(), x_arg = "foo", y_arg = "bar")
  expect_args(int(), new_hidden(), x_arg = "foo", y_arg = "bar")
  expect_args(dbl(), new_hidden(), x_arg = "foo", y_arg = "bar")
  expect_args(chr(), new_hidden(), x_arg = "foo", y_arg = "bar")
  expect_args(list(), new_hidden(), x_arg = "foo", y_arg = "bar")
  expect_args(new_rcrd(list(x = NA)), new_hidden(), x_arg = "foo", y_arg = "bar")
  expect_args(data.frame(), new_hidden(), x_arg = "foo", y_arg = "bar")
  expect_args(Sys.Date(), new_hidden(), x_arg = "foo", y_arg = "bar")
  expect_args(as.difftime(1, units = "hours"), new_hidden(), x_arg = "foo", y_arg = "bar")
  expect_args(factor(), new_hidden(), x_arg = "foo", y_arg = "bar")
  expect_args(ordered(""), new_hidden(), x_arg = "foo", y_arg = "bar")
  expect_args(ordered(""), factor(), x_arg = "foo", y_arg = "bar")
  expect_args(bit64::as.integer64(1), new_hidden(), x_arg = "foo", y_arg = "bar")
})

test_that("vec_ptype2() data frame methods builds argument tags", {
  expect_known_output(file = test_path("test-type2-error-messages.txt"), {
    cat_line("Bare objects:")
    try2(vec_ptype2("foo", 10))

    cat_line("Nested dataframes:")
    df1 <- tibble(x = tibble(y = tibble(z = 1)))
    df2 <- tibble(x = tibble(y = tibble(z = "a")))
    try2(vec_ptype2(df1, df2))
  })
})

test_that("stop_incompatible_type() can be called without argument tags", {
  expect_error(stop_incompatible_type(1, 2, x_arg = "", y_arg = ""), "<double> and <double>", class = "vctrs_error_incompatible_type")
})

test_that("vec_ptype2() returns empty prototype when other input is NULL", {
  expect_identical(vec_ptype2(1:5, NULL), int())
  expect_identical(vec_ptype2(NULL, 1:5), int())
})

test_that("Subclasses of data.frame dispatch to `vec_ptype2()` methods", {
  local_methods(
    vec_ptype2.quuxframe = function(x, y, ...) UseMethod("vec_ptype2.quuxframe"),
    vec_ptype2.quuxframe.data.frame = function(x, y, ...) "dispatched!",
    vec_ptype2.data.frame.quuxframe = function(x, y, ...) "dispatched!"
  )

  quux <- structure(data.frame(), class = c("quuxframe", "data.frame"))

  expect_identical(vec_ptype2(quux, mtcars), "dispatched!")
  expect_identical(vec_ptype2(mtcars, quux), "dispatched!")

  quux <- structure(data.frame(), class = c("quuxframe", "tbl_df", "data.frame"))

  expect_identical(vec_ptype2(quux, mtcars), "dispatched!")
  expect_identical(vec_ptype2(mtcars, quux), "dispatched!")
})

test_that("Subclasses of `tbl_df` have `tbl_df` common type (#481)", {
  quux <- tibble()
  quux <- structure(quux, class = c("quux", class(quux)))
  expect_identical(vec_ptype2(quux, tibble()), tibble())
  expect_identical(vec_ptype2(tibble(), quux), tibble())
})

test_that("Column name encodings are handled correctly in the common type (#553)", {
  encs <- encodings()

  data <- list(chr())

  df_utf8 <- tibble::as_tibble(set_names(data, encs$utf8))
  df_unknown <- tibble::as_tibble(set_names(data, encs$unknown))

  expect_identical(vec_ptype2(df_utf8, df_unknown), df_utf8)
})

test_that("vec_is_subtype() determines subtyping relationship", {
  expect_true(vec_is_subtype(lgl(), int()))
  expect_false(vec_is_subtype(int(), lgl()))

  expect_false(vec_is_subtype(lgl(), chr()))
  expect_false(vec_is_subtype(chr(), lgl()))

  local_methods(
    vec_ptype2.vctrs_foobar = function(x, y, ...) UseMethod("vec_ptype2.vctrs_foobar", y),
    vec_ptype2.vctrs_foobar.logical = function(x, y, ...) logical(),
    vec_ptype2.logical.vctrs_foobar = function(x, y, ...) logical()
  )
  expect_true(vec_is_subtype(foobar(TRUE), lgl()))
  expect_false(vec_is_subtype(lgl(), foobar(TRUE)))
})

test_that("vec_ptype2() slices inputs before OO dispatch", {
  lhs <- NULL
  rhs <- NULL
  local_methods(
    vec_ptype2.vctrs_foobar = function(x, y, ...) { lhs <<- x; rhs <<- y }
  )
  vec_ptype2(foobar(1:3), foobar(TRUE))

  expect_identical(vec_size(lhs), 0L)
  expect_identical(vec_size(rhs), 0L)
})
