context("test-equal")


# vectorised --------------------------------------------------------------

test_that("throws error for unsuported type", {
  expect_error(.Call(vctrs_equal, expression(x), expression(x), TRUE), class = "vctrs_error_scalar_type")
})

test_that("C wrapper throws error if length or type doesn't match", {
  expect_error(.Call(vctrs_equal, 1:2, 1L, TRUE), "same types and lengths")
  expect_error(.Call(vctrs_equal, 1, 1L, TRUE), "same types and lengths")
})

test_that("correct behaviour for basic vectors", {
  expect_equal(vec_equal(c(TRUE, FALSE), TRUE), c(TRUE, FALSE))
  expect_equal(vec_equal(c(1L, 2L), 1L), c(TRUE, FALSE))
  expect_equal(vec_equal(c(1, 2), 1), c(TRUE, FALSE))
  expect_equal(vec_equal(c("1", "2"), "1"), c(TRUE, FALSE))
  expect_equal(vec_equal(as.raw(1:2), as.raw(1L)), c(TRUE, FALSE))
  expect_equal(vec_equal(list(1:3, 1:2), list(1:3)), c(TRUE, FALSE))
  expect_equal(vec_equal(list(1:3, 1.5), list(1:3)), c(TRUE, FALSE))
  expect_equal(vec_equal(list(as.raw(1:3), as.raw(1.5)), list(as.raw(1:3))), c(TRUE, FALSE))
  expect_equal(vec_equal(list(1+1i, 1+0i), list(1+1i)), c(TRUE, FALSE))
  expect_equal(vec_equal(c(1, 2) + 1i, 1+1i), c(TRUE, FALSE))
})

test_that("NAs are equal", {
  expect_true(vec_equal(NA, NA, na_equal = TRUE))
  expect_true(vec_equal(NA_integer_, NA_integer_, na_equal = TRUE))
  expect_true(vec_equal(NA_real_, NA_real_, na_equal = TRUE))
  expect_true(vec_equal(NA_character_, NA_character_, na_equal = TRUE))
  expect_true(vec_equal(list(NULL), list(NULL), na_equal = TRUE))
})

test_that("double special values", {
  expect_equal(vec_equal(c(NaN, NA), NaN, na_equal = TRUE), c(TRUE, FALSE))
  expect_equal(vec_equal(c(NA, NaN), NA, na_equal = TRUE), c(TRUE, FALSE))
  expect_true(vec_equal(Inf, Inf))
  expect_true(vec_equal(-Inf, -Inf))
})

test_that("`list(NULL)` is considered a missing value (#653)", {
  expect_equal(vec_equal(list(NULL), list(NULL)), NA)
  expect_equal(vec_equal(list(NULL), list(1)), NA)
})

test_that("can compare data frames", {
  df <- data.frame(x = 1:2, y = letters[2:1], stringsAsFactors = FALSE)
  expect_equal(vec_equal(df, df[1, ]), c(TRUE, FALSE))
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

  x5 <- data_frame(x = as.raw(0))
  y5 <- data_frame(x = as.raw(1))

  x6 <- data_frame(x = 1+0i)
  y6 <- data_frame(x = 1+1i)

  expect_false(vec_equal(x1, y1))
  expect_false(vec_equal(x2, y2))
  expect_false(vec_equal(x3, y3))
  expect_false(vec_equal(x4, y4))
  expect_false(vec_equal(x5, y5))
  expect_false(vec_equal(x6, y6))
})

test_that("can compare data frames with data frame columns", {
  df1 <- data_frame(x = data_frame(a = 1))
  df2 <- data_frame(x = data_frame(a = 2))

  expect_true(vec_equal(df1, df1))
  expect_false(vec_equal(df1, df2))
})

test_that("can compare data frames with list columns", {
  df1 <- data_frame(x = list(a = 1, b = 2), y = c(1, 1))
  df2 <- data_frame(x = list(a = 0, b = 2), y = c(1, 1))

  expect_equal(vec_equal(df1, df2), c(FALSE, TRUE))
})

test_that("data frames must have same size and columns", {
  expect_error(.Call(vctrs_equal,
    data.frame(x = 1),
    data.frame(x = 1, y = 2),
    TRUE
  ),
    "must have same types and lengths"
  )

  expect_error(.Call(vctrs_equal,
    data.frame(x = 1, y = 2, z = 2),
    data.frame(x = 1, y = 2),
    TRUE
    ),
    "must have the same number of columns"
  )

  # Names are not checked, as `vec_cast_common()` should take care of the type.
  # So if `vec_cast_common()` is not called, or is improperly specified, then
  # this could result in false equality.
  expect_true(.Call(vctrs_equal,
    data.frame(x = 1),
    data.frame(y = 1),
    TRUE
  ))

  expect_error(.Call(vctrs_equal,
    data.frame(x = 1:2, y = 3:4),
    data.frame(x = 1, y = 2),
    TRUE
    ),
    "must have same types and lengths"
  )

  expect_false(.Call(vctrs_equal,
    data.frame(x = 1),
    data.frame(x = 2),
    TRUE
  ))

  expect_false(.Call(vctrs_equal,
    list(data.frame(x = 1)),
    list(10),
    TRUE
  ))

})

test_that("can compare data frames with 0 columns", {
  x <- new_data_frame(.size = 1L)
  expect_true(vec_equal(x, x))
})

test_that("can compare lists of scalars (#643)", {
  lst <- list(new_sclr(x = 1))
  expect_true(vec_equal(lst, lst))

  # NA does not propagate
  lst <- list(new_sclr(y = NA))
  expect_true(vec_equal(lst, lst))

  df <- data.frame(x = c(1, 4, 3), y = c(2, 8, 9))
  model <- lm(y ~ x, df)
  lst <- list(model)
  expect_true(vec_equal(lst, lst))
})

test_that("can determine equality of strings with different encodings (#553)", {
  for (x_encoding in encodings()) {
    for (y_encoding in encodings()) {
      expect_equal(vec_equal(x_encoding, y_encoding), TRUE)
      expect_equal(vec_equal(x_encoding, y_encoding), x_encoding == y_encoding)
    }
  }
})

test_that("equality can be determined when strings have identical encodings", {
  encs <- encodings(bytes = TRUE)

  for (enc in encs) {
    expect_true(vec_equal(enc, enc))
    expect_equal(vec_equal(enc, enc), enc == enc)
  }
})

test_that("equality is known to fail when comparing bytes to other encodings", {
  error <- "translating strings with \"bytes\" encoding"

  for (enc in encodings()) {
    expect_error(vec_equal(encoding_bytes(), enc), error)
    expect_error(vec_equal(enc, encoding_bytes()), error)
  }
})

test_that("`na_equal` is validated", {
  expect_error(vec_equal(1, 1, na_equal = 1), class = "vctrs_error_assert_ptype")
  expect_error(vec_equal(1, 1, na_equal = c(TRUE, FALSE)), class = "vctrs_error_assert_size")
})

test_that("can compare lists of expressions", {
  x <- list(expression(x), expression(y))
  y <- list(expression(x))

  expect_equal(vec_equal(x, y), c(TRUE, FALSE))
})

test_that("equal_scalar() compares", {
  expect_equal_implemented <- function(x, na) {
    expect_false(test_equal_scalar(x, 0, x, 1))
    expect_true(test_equal_scalar(x, 1, x, 1))

    if (!is_null(na)) {
      expect_false(test_equal_scalar(na, 0, x, 1, na_equal = TRUE))
      expect_true(is.na(test_equal_scalar(na, 0, x, 1, na_equal = FALSE)))
    }
  }

  expect_equal_implemented(c(FALSE, TRUE), NA)
  expect_equal_implemented(c(0L, 1L), na_int)
  expect_equal_implemented(c(0, 1), na_dbl)
  expect_equal_implemented(c(0i, 1i), na_cpl)
  expect_equal_implemented(c("foo", "bar"), na_chr)
  expect_equal_implemented(as.raw(c(0, 1)), NULL)
  expect_equal_implemented(list(0, 1), NULL)
  expect_equal_implemented(mtcars, vec_init(mtcars))

  expect_error(test_equal_scalar(expression(0), 0, expression(1), 1), "Unsupported")
})


# object ------------------------------------------------------------------

test_that("can compare NULL",{
  expect_true(obj_equal(NULL, NULL))
})

test_that("can compare objects with reference semantics", {
  expect_true(obj_equal(globalenv(), globalenv()))
  expect_false(obj_equal(globalenv(), environment()))

  expect_true(obj_equal(quote(x), quote(x)))
  expect_false(obj_equal(quote(x), quote(y)))
})

test_that("can compare pairlists", {
  expect_true(obj_equal(quote(x + y), quote(x + y)))
  expect_true(obj_equal(pairlist(x = 1, y = 2), pairlist(x = 1, y = 2)))
})

test_that("can compare functions", {
  f1 <- function(x, y) x + y
  f2 <- function(x, y) x + y
  expect_false(obj_equal(f2, f1))

  attr(f1, "srcref") <- NULL
  attr(f2, "srcref") <- NULL
  expect_true(obj_equal(f2, f1))

  f3 <- f1
  formals(f3) <- alist(x = 1)
  expect_false(obj_equal(f3, f1))

  f4 <- f1
  body(f4) <- quote(x)
  expect_false(obj_equal(f4, f2))
})

test_that("not equal if different types or lengths", {
  expect_false(obj_equal(1, 2))
  expect_false(obj_equal(1:2, 1))
})

test_that("not equal if attributes not equal", {
  x1 <- structure(1:10, x = 1, y = 2)
  x2 <- structure(1:10, x = 1, y = 3)
  expect_false(obj_equal(x1, x2))
})

test_that("can compare expressions", {
  expect_true(obj_equal(expression(x), expression(x)))
  expect_false(obj_equal(expression(x), expression(y)))
})

# na ----------------------------------------------------------------------

test_that("can detect different types of NA", {
  expect_true(vec_equal_na(NA))
  expect_true(vec_equal_na(NA_integer_))
  expect_true(vec_equal_na(NA_real_))
  expect_true(vec_equal_na(NA_complex_))
  expect_true(vec_equal_na(complex(real = NA, imaginary = 1)))
  expect_true(vec_equal_na(NaN))
  expect_true(vec_equal_na(NA_character_))
  expect_true(vec_equal_na(list(NULL)))
})

test_that("can detect different types of NA in data frames", {
  # using multiple columns to prevent proxy unwrapping
  expect_true(vec_equal_na(data.frame(x = NA, y = NA)))
  expect_true(vec_equal_na(data.frame(x = NA_integer_, y = NA_integer_)))
  expect_true(vec_equal_na(data.frame(x = NA_real_, y = NaN)))
  expect_true(vec_equal_na(data.frame(x = NA_complex_, y = NA_complex_)))
  expect_true(vec_equal_na(data.frame(x = complex(real = NA, imaginary = 1), y = complex(real = 1, imaginary = NA))))
  expect_true(vec_equal_na(data.frame(x = NA_character_, y = NA_character_)))
  expect_true(vec_equal_na(new_data_frame(list(x = list(NULL), y = list(NULL)))))
})

test_that("raw vectors can never be NA", {
  expect_false(vec_equal_na(raw(1)))
  expect_false(vec_equal_na(data.frame(x = raw(1), y = raw(1))))
})

test_that("vectorised over rows of a data frame", {
  df <- data.frame(x = c(1, 1, NA, NA), y = c(1, NA, 1, NA))
  expect_equal(vec_equal_na(df), c(FALSE, FALSE, FALSE, TRUE))
})

test_that("works recursively with data frame columns", {
  df <- data.frame(x = c(1, 1, NA, NA))
  df$df <- data.frame(y = c(NA, 1, 1, NA), z = c(1, NA, 1, NA))
  expect_equal(vec_equal_na(df), c(FALSE, FALSE, FALSE, TRUE))
})

test_that("NA propagate symmetrically (#204)", {
  exp <- c(NA, NA)

  expect_identical(vec_equal(c(TRUE, FALSE), NA), exp)
  expect_identical(vec_equal(1:2, NA), exp)
  expect_identical(vec_equal(c(1, 2), NA), exp)
  expect_identical(vec_equal(letters[1:2], NA), exp)

  expect_identical(vec_equal(NA, c(TRUE, FALSE)), exp)
  expect_identical(vec_equal(NA, 1:2), exp)
  expect_identical(vec_equal(NA, c(1, 2)), exp)
  expect_identical(vec_equal(NA, letters[1:2]), exp)
})

test_that("NA propagate from data frames columns", {
  x <- data.frame(x = 1:3)
  y <- data.frame(x = c(1L, NA, 2L))

  expect_identical(vec_equal(x, y), c(TRUE, NA, FALSE))
  expect_identical(vec_equal(y, x), c(TRUE, NA, FALSE))

  expect_identical(vec_equal(x, y, na_equal = TRUE), c(TRUE, FALSE, FALSE))
  expect_identical(vec_equal(y, x, na_equal = TRUE), c(TRUE, FALSE, FALSE))

  x <- data.frame(x = 1:3, y = 1:3)
  y <- data.frame(x = c(1L, NA, 2L), y = c(NA, 2L, 3L))

  expect_identical(vec_equal(x, y), c(NA, NA, FALSE))
  expect_identical(vec_equal(y, x), c(NA, NA, FALSE))

  expect_identical(vec_equal(x, y, na_equal = TRUE), c(FALSE, FALSE, FALSE))
  expect_identical(vec_equal(y, x, na_equal = TRUE), c(FALSE, FALSE, FALSE))
})

test_that("NA do not propagate from list components (#662)", {
  expect_true(obj_equal(NA, NA))
  expect_true(vec_equal(list(NA), list(NA)))
})

test_that("NA do not propagate from names when comparing objects", {
  x <- set_names(1:3, c("a", "b", NA))
  y <- set_names(1:3, c("a", NA, NA))

  expect_true(obj_equal(x, x))
  expect_false(obj_equal(x, y))

  expect_equal(vec_equal(list(x, x, y), list(x, y, y)), c(TRUE, FALSE, TRUE))
})

test_that("NA do not propagate from attributes", {
  x <- structure(1:3, foo = NA)
  y <- structure(1:3, foo = "")
  expect_true(obj_equal(x, x))
  expect_false(obj_equal(x, y))
})

test_that("NA do not propagate from function bodies or formals", {
  fn <- other <- function() NA
  body(other) <- TRUE

  expect_true(vec_equal(list(fn), list(fn)))
  expect_false(vec_equal(list(fn), list(other)))
  expect_true(obj_equal(fn, fn))
  expect_false(obj_equal(fn, other))

  fn <- other <- function(x = NA) NULL
  formals(other) <- list(x = NULL)

  expect_true(vec_equal(list(fn), list(fn)))
  expect_false(vec_equal(list(fn), list(other)))
})

test_that("can check equality of unspecified objects", {
  expect_equal(vec_equal(NA, NA), NA)
  expect_true(vec_equal(NA, NA, na_equal = TRUE))

  expect_equal(vec_equal(unspecified(1), unspecified(1)), NA)
  expect_true(vec_equal(unspecified(1), unspecified(1), na_equal = TRUE))

  expect_equal(vec_equal(NA, unspecified(1)), NA)
  expect_true(vec_equal(NA, unspecified(1), na_equal = TRUE))
})

test_that("can't supply NA as `na_equal`", {
  expect_error(vec_equal(NA, NA, na_equal = NA), "single `TRUE` or `FALSE`")
})


# proxy -------------------------------------------------------------------

test_that("vec_equal() takes vec_proxy() by default", {
  local_env_proxy()
  x <- new_proxy(1:3)
  y <- new_proxy(3:1)
  expect_identical(vec_equal(x, y), lgl(FALSE, TRUE, FALSE))
})

test_that("vec_equal() takes vec_proxy_equal() if implemented", {
  local_comparable_tuple()

  x <- tuple(1:3, 1:3)
  y <- tuple(1:3, 4:6)

  expect_identical(x == y, rep(TRUE, 3))
  expect_identical(vec_equal(x, y), rep(TRUE, 3))

  # Recursive case
  foo <- data_frame(x = x)
  bar <- data_frame(x = y)
  expect_identical(vec_equal(foo, bar), rep(TRUE, 3))
})
