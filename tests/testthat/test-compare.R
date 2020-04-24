context("test-compare")

test_that("inputs must be vectors", {
  expect_error(vec_compare(NULL, 1), class = "vctrs_error_scalar_type")
  expect_error(vec_compare(1, NULL), class = "vctrs_error_scalar_type")
})

test_that("matches R ordering", {
  expect_same <- function(x, y) {
    expect_equal(vec_compare(!!x, !!y), cmp(!!x, !!y))
  }

  expect_same(c(NA, FALSE, TRUE), FALSE)
  expect_same(c(NA, -100L, 0L, 100L), 0L)
  expect_same(c(NA, -Inf, -100, 100, Inf), 0L)
  expect_same(c(NA, NaN, 0), NA)
  expect_same(c(NA, "a", "b", "c"), "b")
  expect_same(as.raw(2:5), as.raw(4))
})

test_that("NAs equal when requested", {
  expect_value <- function(x, y, val, .ptype = NULL) {
    expect_equal(vec_compare(!!x, !!y, .ptype = .ptype, na_equal = TRUE), !!val)
  }

  expect_value(NA, NA, 0L)
  expect_value(NA, FALSE, -1L)
  expect_value(FALSE, NA, 1L)

  expect_value(NA_integer_, NA_integer_, 0L)
  expect_value(NA_integer_, 0L, -1L)
  expect_value(0L, NA_integer_, 1L)

  expect_value(NA_character_, NA_character_, 0L)
  expect_value(NA_character_, "", -1L)
  expect_value("", NA_character_, 1L)

  expect_value(0, NA_real_, 1L)
  expect_value(0, NaN, 1L)
  expect_value(0, 0, 0L)
  expect_value(NA_real_, NA_real_, 0L)
  expect_value(NA_real_, NaN, 1L)
  expect_value(NA_real_, 0, -1L)
  expect_value(NaN, NA_real_, -1L)
  expect_value(NaN, NaN, 0L)
  expect_value(NaN, 0, -1L)
})

test_that("data frames are compared column by column", {
  df1 <- data.frame(x = c(1, 1, 1), y = c(-1, 0, 1))

  expect_equal(vec_compare(df1, df1[2, ]), c(-1, 0, 1))
  expect_equal(vec_compare(df1[1], df1[2, 1, drop = FALSE]), c(0, 0, 0))
  expect_equal(vec_compare(df1[2], df1[2, 2, drop = FALSE]), c(-1, 0, 1))
  expect_equal(vec_compare(df1[2:1], df1[2, 2:1]), c(-1, 0, 1))
})

test_that("can compare data frames with various types of columns", {
  x1 <- data_frame(x = 1, y = 2)
  y1 <- data_frame(x = 2, y = 1)

  x2 <- data_frame(x = "a")
  y2 <- data_frame(x = "b")

  x3 <- data_frame(x = FALSE)
  y3 <- data_frame(x = TRUE)

  x4 <- data_frame(x = 1L)
  y4 <- data_frame(x = 2L)

  expect_equal(vec_compare(x1, y1), -1)
  expect_equal(vec_compare(x2, y2), -1)
  expect_equal(vec_compare(x3, y3), -1)
  expect_equal(vec_compare(x4, y4), -1)
})

test_that("can compare data frames with data frame columns", {
  df1 <- data_frame(x = data_frame(a = 1))
  df2 <- data_frame(x = data_frame(a = 2))

  expect_equal(vec_compare(df1, df1), 0)
  expect_equal(vec_compare(df1, df2), -1)
})

test_that("can compare data frames with list columns because of `vec_proxy_compare(relax = TRUE)`", {
  # lists are replaced with `vec_seq_along()`,
  # so `list(a = 1)` and `list(a = 0)` look equivalent
  df1 <- data_frame(x = list(a = 1), y = 2)
  df2 <- data_frame(x = list(a = 0), y = 3)

  expect_equal(vec_compare(df1, df2), -1)
})

test_that("C code doesn't crash with bad inputs", {
  df <- data.frame(x = c(1, 1, 1), y = c(-1, 0, 1))

  expect_error(.Call(vctrs_compare, df, df[1], TRUE), "not comparable")

  # Names are not checked, as `vec_cast_common()` should take care of the type.
  # So if `vec_cast_common()` is not called, or is improperly specified, then
  # this could result in false equality.
  expect_equal(.Call(vctrs_compare, df, setNames(df, c("x", "z")), TRUE), c(0, 0, 0))

  df1 <- new_data_frame(list(x = 1:3, y = c(1, 1, 1)))
  df2 <- new_data_frame(list(y = 1:2, x = 1:2))
  expect_error(.Call(vctrs_compare, df1, df2, TRUE), "must have the same types and lengths")
})

test_that("xtfrm.vctrs_vctr works for variety of base classes", {
  df <- data.frame(x = c(NA, 1, 1), y = c(1, 2, 1))
  expect_equal(xtfrm.vctrs_vctr(df), c(3, 2, 1))

  x <- c(2, 3, 1)
  expect_equal(xtfrm.vctrs_vctr(x), x)
  expect_equal(xtfrm.vctrs_vctr(letters[x]), x)
})

test_that("vec_proxy_compare() refuses to deal with lists", {
  expect_error(vec_proxy_compare(list()), class = "vctrs_error_unsupported")
})

test_that("vec_compare() calls vec_proxy_compare()", {
  local_methods(
    vec_proxy_compare.vctrs_foobar = function(x) rev(x),
    vec_ptype2.integer.vctrs_foobar = function(...) foobar(int()),
    vec_ptype2.vctrs_foobar = function(...) foobar(int()),
    vec_cast.vctrs_foobar = function(...) NULL,
    vec_cast.vctrs_foobar.integer = function(x, ...) x,
  )
  expect_identical(vec_compare(1:3, 1:3), int(0, 0, 0))
  expect_identical(vec_compare(1:3, foobar(1:3)), int(-1, 0, 1))
})

test_that("vec_proxy_compare() preserves data frames and vectors", {
  df <- data_frame(x = 1:2, y = c("a", "b"))
  expect_identical(vec_proxy_compare(df), df)

  x <- c(NA, "a", "b", "c")
  expect_identical(vec_proxy_compare(x), x)
})

test_that("vec_proxy_compare() handles data frame with a POSIXlt column", {
  df <- data.frame(times = 1:5, x = 1:5)
  df$times <- as.POSIXlt(seq.Date(as.Date("2019-12-30"), as.Date("2020-01-03"), by = "day"))

  df2 <- df
  df2$times <- vec_proxy_compare(df$times)

  expect_identical(
    vec_proxy_compare(df),
    vec_proxy_compare(df2)
  )
})

test_that("vec_proxy_compare.POSIXlt() correctly orders (#720)", {
  dates <- as.POSIXlt(seq.Date(as.Date("2019-12-30"), as.Date("2020-01-03"), by = "day"))
  expect_equal(vec_order(dates), 1:5)
})

test_that("vec_proxy_compare.POSIXlt() correctly orders around DST", {
  # 1am in EDT
  x <- as.POSIXlt("2020-11-01 01:00:00", tz = "America/New_York")

  # "falls back" to 1am again, but in EST
  y <- as.POSIXlt(x + 3600)

  expect_equal(vec_order(c(y, x)), c(2, 1))
})

test_that("error is thrown with data frames with 0 columns", {
  x <- new_data_frame(.size = 1L)
  expect_error(vec_compare(x, x), "data frame with zero columns")
})

test_that("error is thrown when comparing lists", {
  expect_error(vec_compare(list(), list()), class = "vctrs_error_unsupported")
  expect_error(.Call(vctrs_compare, list(), list(), FALSE), "Can't compare lists")
})

test_that("error is thrown when comparing scalars", {
  x <- new_sclr(x = 1)
  expect_error(vec_compare(x, x), class = "vctrs_error_scalar_type")
  expect_error(.Call(vctrs_compare, x, x, FALSE), class = "vctrs_error_scalar_type")
})

test_that("`na_equal` is validated", {
  expect_error(vec_compare(1, 1, na_equal = 1), class = "vctrs_error_assert_ptype")
  expect_error(vec_compare(1, 1, na_equal = c(TRUE, FALSE)), class = "vctrs_error_assert_size")
})

test_that("can compare equal strings with different encodings", {
  for (x_encoding in encodings()) {
    for (y_encoding in encodings()) {
      expect_equal(vec_compare(x_encoding, y_encoding), 0L)
    }
  }
})

test_that("can compare non-equal strings with different encodings", {
  x <- "x"
  y <- encodings()$latin1

  expect_equal(vec_compare(x, y), -1L)
})

test_that("equality can always be determined when strings have identical encodings", {
  encs <- encodings(bytes = TRUE)

  for (enc in encs) {
    expect_equal(vec_compare(enc, enc), 0L)
  }
})

test_that("equality is known to fail when comparing bytes to other encodings", {
  error <- "translating strings with \"bytes\" encoding"

  for (enc in encodings()) {
    expect_error(vec_compare(encoding_bytes(), enc), error)
    expect_error(vec_compare(enc, encoding_bytes()), error)
  }
})

test_that("can compare unspecified", {
  expect_equal(vec_compare(NA, NA), NA_integer_)
  expect_equal(vec_compare(NA, NA, na_equal = TRUE), 0)
  expect_equal(vec_compare(c(NA, NA), unspecified(2)), c(NA_integer_, NA_integer_))
})

test_that("can't supply NA as `na_equal`", {
  expect_error(vec_compare(NA, NA, na_equal = NA), "single `TRUE` or `FALSE`")
})

# order/sort --------------------------------------------------------------

test_that("can request NAs sorted first", {
  expect_equal(vec_order(c(1, NA), "asc", "large"), 1:2)
  expect_equal(vec_order(c(1, NA), "desc", "large"), 2:1)

  expect_equal(vec_order(c(1, NA), "asc", "small"), 2:1)
  expect_equal(vec_order(c(1, NA), "desc", "small"), 1:2)
})

test_that("can sort data frames", {
  df <- data.frame(x = c(1, 2, 1), y = c(1, 2, 2))

  out1 <- vec_sort(df)
  expect_equal(out1, data.frame(x = c(1, 1, 2), y = c(1, 2, 2)))

  out2 <- vec_sort(df, "desc")
  expect_equal(out2, data.frame(x = c(2, 1, 1), y = c(2, 2, 1)))
})

test_that("can sort empty data frames (#356)", {
  df1 <- data.frame()
  expect_equal(vec_sort(df1), df1)

  df2 <- data.frame(x = numeric(), y = integer())
  expect_equal(vec_sort(df2), df2)
})

test_that("can order tibbles that contain non-comparable objects", {
  expect_equal(vec_order(data_frame(list(10, 2, 1))), 1:3)
})

test_that("can order matrices and arrays (#306)", {
  x <- matrix(c(1, 1, 1, 1, 2, 1), ncol = 2)
  expect_identical(vec_order(x), c(1L, 3L, 2L))

  x <- array(1:8, c(2, 2, 2))
  x[2] <- 1
  x[3] <- 5
  expect_identical(vec_order(x), 2:1)
})

test_that("can order empty data frames (#356)", {
  df1 <- data.frame()
  expect_equal(vec_order(df1), integer())

  df2 <- data.frame(x = numeric(), y = integer())
  expect_equal(vec_order(df2), integer())
})

test_that("can order data frames with data frame columns (#527)", {
  expect_equal(
    vec_order(iris),
    vec_order(data_frame(iris = iris))
  )
})

test_that("can order data frames (and subclasses) with matrix columns", {
  df <- new_data_frame(.size = 2L)

  df$x <- new_data_frame(list(y = matrix(1:2, 2)))
  expect_identical(vec_order(df), 1:2)

  df$x <- tibble::tibble(y = matrix(1:2, 2))
  expect_identical(vec_order(df), 1:2)
})
