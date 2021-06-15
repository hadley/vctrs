# ------------------------------------------------------------------------------
# vec_matches() - integers

test_that("can match in increasing order", {
  x <- vec_matches(1:2, 1:3)
  expect_identical(x$needles, 1:2)
  expect_identical(x$haystack, 1:2)
})

# ------------------------------------------------------------------------------
# vec_matches() - doubles

test_that("can match doubles", {
  x <- vec_matches(c(1, 2, 5), c(2, 2, 3, 1))

  expect_identical(x$needles, c(1L, 2L, 2L, 3L))
  expect_identical(x$haystack, c(4L, 1L, 2L, NA))
})

test_that("can match Inf and -Inf with all conditions", {
  x <- c(Inf, -Inf)
  y <- c(-Inf, 0, Inf)

  res <- vec_matches(x, y, condition = "==")
  expect_identical(res$needles, c(1L, 2L))
  expect_identical(res$haystack, c(3L, 1L))

  res <- vec_matches(x, y, condition = "<")
  expect_identical(res$needles, c(1L, 2L, 2L))
  expect_identical(res$haystack, c(NA, 2L, 3L))

  res <- vec_matches(x, y, condition = "<=")
  expect_identical(res$needles, c(1L, 2L, 2L, 2L))
  expect_identical(res$haystack, c(3L, 1L, 2L, 3L))

  res <- vec_matches(x, y, condition = ">")
  expect_identical(res$needles, c(1L, 1L, 2L))
  expect_identical(res$haystack, c(1L, 2L, NA))

  res <- vec_matches(x, y, condition = ">=")
  expect_identical(res$needles, c(1L, 1L, 1L, 2L))
  expect_identical(res$haystack, c(1L, 2L, 3L, 1L))
})

test_that("NA and NaN are the same by default", {
  res <- vec_matches(c(NaN, NA, NaN), c(NA, NaN, NA), condition = "==")
  expect_identical(res$needles, rep(c(1L, 2L, 3L), each = 3))
  expect_identical(res$haystack, rep(c(1L, 2L, 3L), times = 3))

  res <- vec_matches(c(NaN, NA, NaN), c(NA, NaN, NA), condition = "==", multiple = "first")
  expect_identical(res$needles, c(1L, 2L, 3L))
  expect_identical(res$haystack, c(1L, 1L, 1L))
})

test_that("NA and NaN are distinct if requested", {
  res <- vec_matches(c(NaN, NA, NaN), c(NA, NaN, NA), condition = "==", nan_distinct = TRUE)
  expect_identical(res$needles, c(1L, 2L, 2L, 3L))
  expect_identical(res$haystack, c(2L, 1L, 3L, 2L))

  res <- vec_matches(c(NaN, NA, NaN), c(NA, NaN, NA), condition = "==", multiple = "first", nan_distinct = TRUE)
  expect_identical(res$needles, c(1L, 2L, 3L))
  expect_identical(res$haystack, c(2L, 1L, 2L))
})

test_that("NA and NaN match each other in non-equi conditions by default", {
  res <- vec_matches(c(NaN, NA, NaN, 1), c(NA, NaN, NA), condition = ">=", nan_distinct = FALSE)
  expect_identical(res$needles, c(rep(c(1L, 2L, 3L), each = 3), 4L))
  expect_identical(res$haystack, c(rep(c(1L, 2L, 3L), times = 3), NA))

  res <- vec_matches(c(NaN, NA, NaN, 1), c(NA, NaN, NA), condition = "<=", nan_distinct = FALSE)
  expect_identical(res$needles, c(rep(c(1L, 2L, 3L), each = 3), 4L))
  expect_identical(res$haystack, c(rep(c(1L, 2L, 3L), times = 3), NA))
})

test_that("NA and NaN never match each other in non-equi conditions if treated as distinct", {
  res <- vec_matches(c(NaN, NA, NaN), c(NA, NaN, NA), condition = ">=", nan_distinct = TRUE)
  expect_identical(res$needles, c(1L, 2L, 2L, 3L))
  expect_identical(res$haystack, c(2L, 1L, 3L, 2L))

  res <- vec_matches(c(NaN, NA, NaN), c(NA, NaN, NA), condition = "<=", nan_distinct = TRUE)
  expect_identical(res$needles, c(1L, 2L, 2L, 3L))
  expect_identical(res$haystack, c(2L, 1L, 3L, 2L))
})

test_that("NA and NaN both propagate with `missing = 'propagate'` no matter the value of `nan_distinct`", {
  res <- vec_matches(c(NA, NaN), c(NA, NaN), missing = "propagate", nan_distinct = FALSE)
  expect_identical(res$needles, 1:2)
  expect_identical(res$haystack, c(NA_integer_, NA_integer_))

  res <- vec_matches(c(NA, NaN), c(NA, NaN), missing = "propagate", nan_distinct = TRUE)
  expect_identical(res$needles, 1:2)
  expect_identical(res$haystack, c(NA_integer_, NA_integer_))
})

# ------------------------------------------------------------------------------
# vec_matches() - complex

test_that("complex can be matched", {
  x <- complex(real = 1, imaginary = c(1, 2))
  y <- complex(real = 1, imaginary = c(1, 1, 3))

  res <- vec_matches(x, y)
  expect_identical(res$needles, c(1L, 1L, 2L))
  expect_identical(res$haystack, c(1L, 2L, NA))
})

test_that("complex order lexicographically", {
  x <- complex(real = 1, imaginary = c(1, 2, 5))
  y <- complex(real = 1, imaginary = c(1, 4, 3))

  res <- vec_matches(x, y, condition = "<")
  expect_identical(res$needles, c(1L, 1L, 2L, 2L, 3L))
  expect_identical(res$haystack, c(2L, 3L, 2L, 3L, NA))
})

test_that("complex missing values match correctly", {
  x <- complex(real = c(NA, NA, NaN, NaN), imaginary = c(NA, NaN, NA, NaN))
  y <- complex(real = c(NA, NA, NaN, NaN), imaginary = c(NA, NaN, NA, NaN))

  # Missings can match, and all missing values should be treated equally
  res <- vec_matches(x, y, condition = "==", missing = "match", nan_distinct = FALSE)
  expect_identical(res$needles, rep(1:4, each = 4))
  expect_identical(res$haystack, rep(1:4, times = 4))

  # Missings can match, but all combinations are different
  res <- vec_matches(x, y, condition = "==", missing = "match", nan_distinct = TRUE)
  expect_identical(res$needles, 1:4)
  expect_identical(res$haystack, 1:4)

  # Missings propagate
  res <- vec_matches(x, y, condition = "==", missing = "propagate")
  expect_identical(res$needles, 1:4)
  expect_identical(res$haystack, rep(NA_integer_, 4))

  # Propagated missings are never considered no-matches
  expect_identical(
    vec_matches(x, y, condition = "==", missing = "propagate", no_match = "error"),
    vec_matches(x, y, condition = "==", missing = "propagate")
  )
})

# ------------------------------------------------------------------------------
# vec_matches() - characters

test_that("character ordering is done in the C locale", {
  x <- c("a", "A")
  y <- c("a", "A", "b", "B")

  # a < b, but a > A and a > B
  res <- vec_matches(x, y, condition = "<")
  expect_identical(res$needles, c(1L, 2L, 2L, 2L))
  expect_identical(res$haystack, c(3L, 1L, 3L, 4L))
})

test_that("`chr_transform` can affect the matching process", {
  x <- c("a", "A")
  y <- c("a", "A")

  res <- vec_matches(x, y, condition = "==")
  expect_identical(res$needles, 1:2)
  expect_identical(res$haystack, 1:2)

  res <- vec_matches(x, y, condition = "==", chr_transform = tolower)
  expect_identical(res$needles, c(1L, 1L, 2L, 2L))
  expect_identical(res$haystack, c(1L, 2L, 1L, 2L))
})

# ------------------------------------------------------------------------------
# vec_matches() - lists

test_that("lists can be matched", {
  x <- list(1, 2, 1, NULL)
  y <- list(1, 1, 3, NULL)

  res <- vec_matches(x, y)

  expect_identical(res$needles, c(1L, 1L, 2L, 3L, 3L, 4L))
  expect_identical(res$haystack, c(1L, 2L, NA, 1L, 2L, 4L))
})

test_that("list missingness propagates", {
  res <- vec_matches(list(NULL), list(NULL), missing = "propagate")

  expect_identical(res$needles, 1L)
  expect_identical(res$haystack, NA_integer_)
})

test_that("list ordering is by first appearance in `needles` (so non-equi joins don't make much sense)", {
  x <- list(3, 2, 1, NULL)
  y <- list(1, 3, 1, 3)

  res <- vec_matches(x, y, condition = ">")

  # x[1] appears first, so it isn't greater than anything
  # x[2] is greater than x[1] (when x[1] is in y)
  # and so on...
  # NULL still doesn't match anything
  expect_identical(res$needles, c(1L, 2L, 2L, 3L, 3L, 4L))
  expect_identical(res$haystack, c(NA, 2L, 4L, 2L, 4L, NA))
})

# ------------------------------------------------------------------------------
# vec_matches() - data frame

test_that("can match with 1 column data frames", {
  df1 <- data_frame(x = c(1L, 3L, 1L, 3L))
  df2 <- data_frame(x = c(1L, 3L, 1L))

  expect_identical(
    vec_matches(df1, df2),
    vec_matches(df1$x, df2$x)
  )
})

test_that("can match with >1 column data frames", {
  df1 <- data_frame(x = c(1L, 3L, 1L, 3L), y = c(1L, 4L, 1L, 2L))
  df2 <- data_frame(x = c(1L, 3L, 1L), y = c(1L, 2L, 1L))

  res <- vec_matches(df1, df2, condition = c("==", "=="))

  expect_identical(res$needles, c(1L, 1L, 2L, 3L, 3L, 4L))
  expect_identical(res$haystack, c(1L, 3L, NA, 1L, 3L, 2L))
})

test_that("ensure that matching works if outer runs are present (i.e. `==` comes before non-equi condition)", {
  df1 <- data_frame(x = c(1, 2, 1, 1), y = c(2, 2, 3, 2))
  df2 <- data_frame(x = c(1, 1), y = c(2, 3))

  res <- vec_matches(df1, df2, condition = c("==", "<="))

  expect_identical(res$needles, c(1L, 1L, 2L, 3L, 4L, 4L))
  expect_identical(res$haystack, c(1L, 2L, NA, 2L, 1L, 2L))

  df1$z <- c(1L, 2L, 1L, 3L)
  df2$z <- c(5L, 2L)

  res <- vec_matches(df1, df2, condition = c("==", "==", "<"))

  expect_identical(res$needles, 1:4)
  expect_identical(res$haystack, c(1L, NA, 2L, 1L))
})

test_that("df-cols propagate an NA if any columns are incomplete", {
  df <- data_frame(x = 1, y = data_frame(x = c(1, 1, NA), y = c(1, NA, 2)))

  res <- vec_matches(df, df, missing = "match")
  expect_identical(res$needles, 1:3)
  expect_identical(res$haystack, 1:3)

  # 2nd and 3rd rows aren't fully complete, so their missing values propagate
  res <- vec_matches(df, df, missing = "propagate")
  expect_identical(res$needles, 1:3)
  expect_identical(res$haystack, c(1L, NA, NA))

  res <- vec_matches(df, df, missing = "drop")
  expect_identical(res$needles, 1L)
  expect_identical(res$haystack, 1L)
})

# ------------------------------------------------------------------------------
# vec_matches() - rcrd

test_that("rcrd types can be matched", {
  x <- new_rcrd(list(x = c(1L, 3L), y = c(1L, 4L)))
  y <- new_rcrd(list(x = c(1L, 2L), y = c(1L, 5L)))

  res <- vec_matches(x, y, condition = "<=")
  expect_identical(res$needles, c(1L, 1L, 2L))
  expect_identical(res$haystack, c(1L, 2L, NA))

  res <- vec_matches(x, y, condition = ">")
  expect_identical(res$needles, c(1L, 2L, 2L))
  expect_identical(res$haystack, c(NA, 1L, 2L))
})

test_that("rcrd type missingness is propagated correctly", {
  x <- new_rcrd(list(x = c(1L, NA), y = c(NA_integer_, NA_integer_)))
  y <- new_rcrd(list(x = c(1L, 2L, NA), y = c(NA, 5L, NA)))

  res <- vec_matches(x, y, condition = "==", missing = "match")
  expect_identical(res$needles, c(1L, 2L))
  expect_identical(res$haystack, c(1L, 3L))

  # Only the observation where all fields are NA is considered incomplete
  res <- vec_matches(x, y, condition = "==", missing = "propagate")
  expect_identical(res$needles, c(1L, 2L))
  expect_identical(res$haystack, c(1L, NA))
})

# ------------------------------------------------------------------------------
# vec_matches() - missing values

test_that("integer missing values can match with equality condition", {
  res <- vec_matches(NA_integer_, c(1L, NA, 2L, NA), condition = "==")

  expect_identical(res$needles, c(1L, 1L))
  expect_identical(res$haystack, c(2L, 4L))
})

test_that("integer missing values can match with <= and >= condition, but don't match any other value", {
  res <- vec_matches(NA_integer_, c(1L, NA, 2L, NA), condition = "<=")

  expect_identical(res$needles, c(1L, 1L))
  expect_identical(res$haystack, c(2L, 4L))

  res <- vec_matches(NA_integer_, c(1L, NA, 2L, NA), condition = ">=")

  expect_identical(res$needles, c(1L, 1L))
  expect_identical(res$haystack, c(2L, 4L))
})

test_that("integer missing values don't match anything with < and > conditions", {
  res <- vec_matches(NA_integer_, c(1L, NA, 2L, NA), condition = "<")

  expect_identical(res$needles, 1L)
  expect_identical(res$haystack, NA_integer_)

  res <- vec_matches(NA_integer_, c(1L, NA, 2L, NA), condition = ">")

  expect_identical(res$needles, 1L)
  expect_identical(res$haystack, NA_integer_)
})

test_that("missing values match within columns", {
  df1 <- data_frame(x = c(1L, 2L, 1L), y = rep(NA_integer_, 3))
  df2 <- data_frame(x = c(2L, 1L, 1L), y = c(1L, NA, NA))

  res <- vec_matches(df1, df2, condition = c("==", "=="))

  expect_identical(res$needles, c(1L, 1L, 2L, 3L, 3L))
  expect_identical(res$haystack, c(2L, 3L, NA, 2L, 3L))

  expect_identical(
    vec_matches(df1, df2, condition = c("<=", ">=")),
    vec_matches(df1, df2, condition = c("==", "=="))
  )

  res <- vec_matches(df1, df2, condition = c("<", ">"))

  expect_identical(res$needles, 1:3)
  expect_identical(res$haystack, rep(NA_integer_, 3))
})

test_that("integer needles can't match NAs in the haystack", {
  # At the C level, 1L > NA_integer_ (INT_MIN),
  # but we are careful to work around this
  res <- vec_matches(1L, c(1L, NA, 2L, NA), condition = ">=")

  expect_identical(res$needles, 1L)
  expect_identical(res$haystack, 1L)

  res <- vec_matches(1L, c(1L, NA, 2L, NA), condition = ">")

  expect_identical(res$needles, 1L)
  expect_identical(res$haystack, NA_integer_)
})

test_that("double needles can't match NAs or NaNs in the haystack", {
  # At the C level, our helpers assumg NA and NaN are the smallest values,
  # so we are careful to avoid including them with >= and > conditions
  res <- vec_matches(1, c(1, NA, 2, NaN), condition = ">=")

  expect_identical(res$needles, 1L)
  expect_identical(res$haystack, 1L)

  res <- vec_matches(1, c(1, NA, 2, NaN), condition = ">")

  expect_identical(res$needles, 1L)
  expect_identical(res$haystack, NA_integer_)
})

# ------------------------------------------------------------------------------
# vec_matches() - `missing`

test_that("can propagate needle NAs with `missing = 'propagate'`", {
  x <- c(1L, NA, 2L)
  y <- c(NA, 1L, 1L)

  res <- vec_matches(x, y, condition = "==", missing = "propagate")

  expect_identical(res$needles, c(1L, 1L, 2L, 3L))
  expect_identical(res$haystack, c(2L, 3L, NA, NA))

  res <- vec_matches(x, y, condition = "<=", missing = "propagate")

  expect_identical(res$needles, c(1L, 1L, 2L, 3L))
  expect_identical(res$haystack, c(2L, 3L, NA, NA))

  res <- vec_matches(x, y, condition = ">=", missing = "propagate")

  expect_identical(res$needles, c(1L, 1L, 2L, 3L, 3L))
  expect_identical(res$haystack, c(2L, 3L, NA, 2L, 3L))
})

test_that("can drop missing needle rows with `missing = 'drop'", {
  x <- c(1L, NA, 2L)
  y <- c(NA, 1L, 1L)

  res <- vec_matches(x, y, condition = "==", missing = "drop")
  expect_identical(res$needles, c(1L, 1L, 3L))
  expect_identical(res$haystack, c(2L, 3L, NA))
})

test_that("if `missing = 'propagate'`, an NA in any column results in propagation", {
  df1 <- data_frame(x = c(1L, NA, 2L, 1L, 1L), y = c(2L, 2L, NA, 1L, 1L))
  df2 <- data_frame(x = c(1L, 1L, 2L), y = c(1L, 1L, NA))

  res <- vec_matches(df1, df2, condition = c("==", "=="), missing = "propagate")

  expect_identical(res$needles, c(1L, 2L, 3L, 4L, 4L, 5L, 5L))
  expect_identical(res$haystack, c(NA, NA, NA, 1L, 2L, 1L, 2L))

  res <- vec_matches(df1, df2, condition = c(">=", ">="), missing = "propagate")

  expect_identical(res$needles, c(1L, 1L, 2L, 3L, 4L, 4L, 5L, 5L))
  expect_identical(res$haystack, c(1L, 2L, NA, NA, 1L, 2L, 1L, 2L))
})

test_that("`missing = 'propagate' / 'drop'` still propagates/drops NAs in future columns when an earlier column has no matches", {
  df1 <- data_frame(x = c(1, 1, 2, 3), y = c(1, NA, NA, 4))
  df2 <- data_frame(x = c(1, 3), y = c(1, 5))

  # The 2 in row 3 of df1 has no match, but the NA in the 2nd column still propagates
  res <- vec_matches(df1, df2, missing = "propagate", no_match = -1L)

  expect_identical(res$needles, 1:4)
  expect_identical(res$haystack, c(1L, NA, NA, -1L))

  res <- vec_matches(df1, df2, missing = "drop", no_match = -1L)

  expect_identical(res$needles, c(1L, 4L))
  expect_identical(res$haystack, c(1L, -1L))

  # The 1 in row 1 and 2 of df1 have no match, but the NA in row 2 of the 2nd column propagates
  res <- vec_matches(df1, df2, missing = "propagate", no_match = -1L, condition = ">")

  expect_identical(res$needles, 1:4)
  expect_identical(res$haystack, c(-1L, NA, NA, 1L))

  res <- vec_matches(df1, df2, missing = "drop", no_match = -1L, condition = ">")

  expect_identical(res$needles, c(1L, 4L))
  expect_identical(res$haystack, c(-1L, 1L))
})

test_that("`missing` can error informatively", {
  verify_output(test_path("error", "test-matches-missing.txt"), {
    "# default message"
    vec_matches(NA, 1, missing = "error")

    "# can control arg names"
    vec_matches(NA, 1, missing = "error", needles_arg = "foo")
  })
})

test_that("`missing` error is classed", {
  expect_error(vec_matches(NA, 1, missing = "error"), class = "vctrs_error_matches_missing")
})

test_that("`missing` is validated", {
  expect_error(vec_matches(1, 2, missing = NA), "`missing` must be a string.")
  expect_error(vec_matches(1, 2, missing = c("match", "propagate")), "`missing` must be a string.")
  expect_error(vec_matches(1, 2, missing = "x"), 'must be one of "match", "propagate", "drop", or "error"')
})

# ------------------------------------------------------------------------------
# vec_matches() - `condition`

test_that("multiple matches from a non-equi condition are returned in first appearance order", {
  res <- vec_matches(0L, c(1L, 0L, -1L, 0L), condition = "<=")

  expect_identical(res$needles, rep(1L, 3))
  expect_identical(res$haystack, c(1L, 2L, 4L))

  # Checking equi for good measure
  res <- vec_matches(0L, c(1L, 0L, -1L, 0L), condition = "==")

  expect_identical(res$needles, rep(1L, 2))
  expect_identical(res$haystack, c(2L, 4L))
})

test_that("multiple matches from a non-equi condition are returned in first appearance order when the matches are in different nested groups", {
  df <- data_frame(x = 0, y = 0)
  df2 <- data_frame(x = 2:1, y = 1:2)

  res <- vec_matches(df, df2, condition = c("<=", "<="))

  expect_identical(res$needles, c(1L, 1L))
  expect_identical(res$haystack, c(1L, 2L))
})

test_that("`condition` is validated", {
  expect_error(vec_matches(1, 2, condition = 1), "`condition` must be a character vector, or `NULL`")
  expect_error(vec_matches(1, 2, condition = "x"), 'must only contain "==", ">", ">=", "<", or "<="')
  expect_error(vec_matches(1, 2, condition = c("==", "==")), "must be length 1, or the same length as the number of columns of the input")
})

# ------------------------------------------------------------------------------
# vec_matches() - `multiple`

test_that("can get all matches", {
  x <- vec_matches(c(1L, 3L), c(1L, 3L, 1L, 3L), multiple = "all")

  expect_identical(x$needles, c(1L, 1L, 2L, 2L))
  expect_identical(x$haystack, c(1L, 3L, 2L, 4L))
})

test_that("can get first match", {
  x <- vec_matches(c(1L, 3L), c(1L, 3L, 1L, 3L), multiple = "first")

  expect_identical(x$needles, 1:2)
  expect_identical(x$haystack, 1:2)
})

test_that("can get last match", {
  x <- vec_matches(c(1L, 3L), c(1L, 3L, 1L, 3L), multiple = "last")

  expect_identical(x$needles, 1:2)
  expect_identical(x$haystack, 3:4)
})

test_that("duplicate needles match the same haystack locations", {
  x <- vec_matches(c(1L, 3L, 1L, 3L), c(1L, 3L, 1L), multiple = "all")

  expect_identical(x$needles, c(1L, 1L, 2L, 3L, 3L, 4L))
  expect_identical(x$haystack, c(1L, 3L, 2L, 1L, 3L, 2L))
})

test_that("`multiple` can error informatively", {
  verify_output(test_path("error", "test-matches-multiple.txt"), {
    "# default message"
    vec_matches(1L, c(1L, 1L), multiple = "error")

    "# can control arg names"
    vec_matches(1L, c(1L, 1L), multiple = "error", needles_arg = "foo")
    vec_matches(1L, c(1L, 1L), multiple = "error", needles_arg = "foo", haystack_arg = "bar")

    "# with `condition = NULL`"
    vec_matches(1, 1:2, multiple = "error", condition = NULL)
  })
})

test_that("`multiple` can warn informatively", {
  verify_output(test_path("error", "test-matches-multiple-warning.txt"), {
    "# default message"
    vec_matches(1L, c(1L, 1L), multiple = "warning")

    "# can control arg names"
    vec_matches(1L, c(1L, 1L), multiple = "warning", needles_arg = "foo")
    vec_matches(1L, c(1L, 1L), multiple = "warning", needles_arg = "foo", haystack_arg = "bar")

    "# with `condition = NULL`"
    vec_matches(1, 1:2, multiple = "warning", condition = NULL)
  })
})

test_that("warning falls back to 'all'", {
  expect_identical(
    expect_warning(
      vec_matches(c(1L, 3L, 1L, 3L), c(1L, 3L, 1L), multiple = "warning"),
      class = "vctrs_warning_matches_multiple"
    ),
    vec_matches(c(1L, 3L, 1L, 3L), c(1L, 3L, 1L), multiple = "all")
  )
})

test_that("errors on multiple matches that come from different nested containment groups", {
  df <- data_frame(x = 0, y = 0)
  df2 <- data_frame(x = 1:2, y = 2:1)

  expect_error(
    vec_matches(df, df2, condition = c("<=", "<="), multiple = "error"),
    "multiple matches"
  )
})

test_that("`multiple` is validated", {
  expect_error(vec_matches(1, 2, multiple = 1.5), "`multiple` must be a string")
  expect_error(vec_matches(1, 2, multiple = c("first", "last")), "`multiple` must be a string")
  expect_error(vec_matches(1, 2, multiple = "x"), '`multiple` must be one of "all", "first", "last", "warning", or "error"')
})

# ------------------------------------------------------------------------------
# vec_matches() - `no_match`

test_that("can control `no_match`", {
  x <- vec_matches(1:3, 1L)
  expect_identical(x$haystack, c(1L, NA, NA))

  x <- vec_matches(1:3, 1L, no_match = 0L)
  expect_identical(x$haystack, c(1L, 0L, 0L))
})

test_that("can drop unmatched needles", {
  x <- vec_matches(1:3, 2L, no_match = "drop")
  expect_identical(x$needles, 2L)
  expect_identical(x$haystack, 1L)
})

test_that("can drop unmatched missings when not propagating them", {
  x <- vec_matches(c(NaN, 2, NA), 2, no_match = "drop")
  expect_identical(x$needles, 2L)
  expect_identical(x$haystack, 1L)

  x <- vec_matches(c(NaN, 2, NA), NA, no_match = "drop", nan_distinct = FALSE)
  expect_identical(x$needles, c(1L, 3L))
  expect_identical(x$haystack, c(1L, 1L))

  x <- vec_matches(c(NaN, 2, NA), NA, no_match = "drop", nan_distinct = TRUE)
  expect_identical(x$needles, 3L)
  expect_identical(x$haystack, 1L)
})

test_that("can differentiate between `no_match` and propagated NAs", {
  res <- vec_matches(c(1, NA), 2, missing = "propagate", no_match = -1L)

  expect_identical(res$needles, 1:2)
  expect_identical(res$haystack, c(-1L, NA))
})

test_that("`no_match` can error informatively", {
  verify_output(test_path("error", "test-matches-nothing.txt"), {
    "# default message"
    vec_matches(1, 2, no_match = "error")

    "# can control arg names"
    vec_matches(1, 2, no_match = "error", needles_arg = "foo")
    vec_matches(1, 2, no_match = "error", needles_arg = "foo", haystack_arg = "bar")

    "# with `condition = NULL`"
    vec_matches(1, double(), no_match = "error", condition = NULL)
  })
})

test_that("`no_match = 'error'` passes propagated NAs through untouched", {
  res <- vec_matches(c(NA, NaN, NA, 1), c(NA, 1), missing = "propagate", no_match = "error")

  expect_identical(res$needles, 1:4)
  expect_identical(res$haystack, c(rep(NA, 3), 2L))
})

test_that("`no_match = 'drop'` passes propagated NAs through untouched", {
  res <- vec_matches(c(NA, NaN, NA, 1), c(NA, 1), missing = "propagate", no_match = "drop")

  expect_identical(res$needles, 1:4)
  expect_identical(res$haystack, c(rep(NA, 3), 2L))
})

test_that("`no_match` is validated", {
  expect_error(vec_matches(1, 2, no_match = 1.5), "length 1 integer, \"drop\", or \"error\"")
  expect_error(vec_matches(1, 2, no_match = c(1L, 2L)), "length 1 integer, \"drop\", or \"error\"")
})

# ------------------------------------------------------------------------------
# vec_matches() - `remaining`

test_that("`remaining` can retain `haystack` values that `needles` didn't match", {
  res <- vec_matches(1, 0:2, remaining = NA_integer_)
  expect_identical(res$needles, c(1L, NA, NA))
  expect_identical(res$haystack, c(2L, 1L, 3L))

  res <- vec_matches(1, 0:2, remaining = NA_integer_, condition = ">=")
  expect_identical(res$needles, c(1L, 1L, NA))
  expect_identical(res$haystack, c(1L, 2L, 3L))

  res <- vec_matches(1, 0:2, remaining = NA_integer_, condition = "<")
  expect_identical(res$needles, c(1L, NA, NA))
  expect_identical(res$haystack, c(3L, 1L, 2L))
})

test_that("`missing` affects `needles` but not `haystack`", {
  # Matches NA to NA, so nothing remaining
  res <- vec_matches(c(1, NA), c(NA, 1), missing = "match", remaining = NA_integer_)
  expect_identical(res$needles, c(1L, 2L))
  expect_identical(res$haystack, c(2L, 1L))

  # `needles` NA value is propagated, so `haystack` is left with a remaining value
  res <- vec_matches(c(1, NA), c(NA, 1), missing = "propagate", remaining = NA_integer_)
  expect_identical(res$needles, c(1L, 2L, NA))
  expect_identical(res$haystack, c(2L, NA, 1L))

  # `needles` NA value is dropped, so `haystack` is left with a remaining value
  res <- vec_matches(c(1, NA), c(NA, 1), missing = "drop", remaining = NA_integer_)
  expect_identical(res$needles, c(1L, NA))
  expect_identical(res$haystack, c(2L, 1L))
})

test_that("`remaining` works with `condition = NULL` and empty `needles`", {
  res <- vec_matches(integer(), 1:5, condition = NULL, remaining = NA_integer_)
  expect_identical(res$needles, rep(NA_integer_, 5))
  expect_identical(res$haystack, 1:5)
})

test_that("`remaining` can error informatively", {
  verify_output(test_path("error", "test-matches-remaining.txt"), {
    "# default message"
    vec_matches(1, 2, remaining = "error")

    "# can control arg names"
    vec_matches(1, 2, remaining = "error", needles_arg = "foo")
    vec_matches(1, 2, remaining = "error", needles_arg = "foo", haystack_arg = "bar")

    "# with `condition = NULL`"
    vec_matches(double(), c(1, 2), remaining = "error", condition = NULL)
  })
})

test_that("`remaining` is validated", {
  expect_error(vec_matches(1, 2, remaining = 1.5), "length 1 integer, \"drop\", or \"error\"")
  expect_error(vec_matches(1, 2, remaining = c(1L, 2L)), "length 1 integer, \"drop\", or \"error\"")
})

# ------------------------------------------------------------------------------
# vec_matches() - filter

test_that("simple `filter`s work", {
  needles <- c(1, 2, 4)
  haystack <- c(2, 1, 3, 0)

  res <- vec_matches(needles, haystack, condition = "<", filter = "max")
  expect_identical(res$haystack, c(3L, 3L, NA))

  res <- vec_matches(needles, haystack, condition = "<", filter = "min")
  expect_identical(res$haystack, c(1L, 3L, NA))

  res <- vec_matches(needles, haystack, condition = ">=", filter = "max")
  expect_identical(res$haystack, c(2L, 1L, 3L))

  res <- vec_matches(needles, haystack, condition = ">=", filter = "min")
  expect_identical(res$haystack, c(4L, 4L, 4L))
})

test_that("haystack duplicates are preserved", {
  needles <- c(1, 2, 4)
  haystack <- c(2, 1, 2, 3, 0, 1, 0)

  res <- vec_matches(needles, haystack, condition = ">=", filter = "max")
  expect_identical(res$needles, c(1L, 1L, 2L, 2L, 3L))
  expect_identical(res$haystack, c(2L, 6L, 1L, 3L, 4L))

  res <- vec_matches(needles, haystack, condition = ">=", filter = "min")
  expect_identical(res$needles, c(1L, 1L, 2L, 2L, 3L, 3L))
  expect_identical(res$haystack, c(5L, 7L, 5L, 7L, 5L, 7L))
})

test_that("haystack duplicates can be controlled by `multiple`", {
  needles <- c(1, 2, 4)
  haystack <- c(2, 1, 2, 3, 0, 1, 0)

  res <- vec_matches(needles, haystack, condition = ">=", filter = "max", multiple = "first")
  expect_identical(res$needles, 1:3)
  expect_identical(res$haystack, c(2L, 1L, 4L))

  res <- vec_matches(needles, haystack, condition = ">=", filter = "max", multiple = "last")
  expect_identical(res$needles, 1:3)
  expect_identical(res$haystack, c(6L, 3L, 4L))
})

test_that("`filter` works when valid matches are in different nested containment groups", {
  needles <- data_frame(x = 0L, y = 1L, z = 2L)
  haystack <- data_frame(x = c(1L, 2L, 1L, 0L), y = c(2L, 1L, 2L, 3L), z = c(3L, 3L, 2L, 2L))

  info <- compute_nested_containment_info(haystack, c("<=", "<=", "<="), "all")
  haystack_order <- info[[1]]
  nested_groups <- info[[2]]

  # Rows 1 and 2 of haystack are in different nested containment groups, but
  # both have the "max" filter value of `z=3` so both should be in the result.
  # Row 4 is in its own containment group, so it will be considered the "max"
  # of its group, but it is less than rows 1 and 2 so it will ultimately be
  # filtered out.
  expect_identical(nested_groups, c(1L, 2L, 1L, 0L))
  expect_identical(haystack_order, c(4L, 3L, 1L, 2L))

  res <- vec_matches(needles, haystack, condition = c("<=", "<=", "<="), filter = c("none", "none", "max"))
  expect_identical(res$needles, c(1L, 1L))
  expect_identical(res$haystack, c(1L, 2L))

  res <- vec_matches(needles, haystack, condition = c("<=", "<=", "<="), filter = c("none", "none", "max"), multiple = "first")
  expect_identical(res$haystack, 1L)

  res <- vec_matches(needles, haystack, condition = c("<=", "<=", "<="), filter = c("none", "none", "max"), multiple = "last")
  expect_identical(res$haystack, 2L)
})

test_that("single filter is applied to all columns", {
  needles <- data_frame(x = 5L, y = 8L, z = 4L)
  haystack <- data_frame(x = c(1L, 3L, 2L, 2L), y = c(1L, 3L, 2L, 3L), z = c(1L, 2L, 3L, 3L))

  res <- vec_matches(needles, haystack, condition = ">=", filter = "max")
  expect_identical(res$haystack, 2L)

  res <- vec_matches(needles, haystack, condition = ">=", filter = "min")
  expect_identical(res$haystack, 1L)
})

test_that("different `filter`s can be used per column", {
  needles <- data_frame(x = c(0, 2, 1, 1), y = c(2, 0, 0, 4))
  haystack <- data_frame(x = c(2, 2, 2, 1, 1), y = c(1, 1, 2, 2, 1))

  res <- vec_matches(needles, haystack, condition = c(">=", "<"), filter = c("max", "min"))
  expect_identical(res$needles, c(1L, 2L, 2L, 3L, 4L))
  expect_identical(res$haystack, c(NA, 1L, 2L, 5L, NA))
})

test_that("`filter` works with missing values", {
  needles <- c(1, NA, 4, NA)
  haystack <- c(NA, 1, NA, 1, 3)

  res <- vec_matches(needles, haystack, condition = ">=", filter = "max", missing = "match")
  expect_identical(res$needles, c(1L, 1L, 2L, 2L, 3L, 4L, 4L))
  expect_identical(res$haystack, c(2L, 4L, 1L, 3L, 5L, 1L, 3L))

  res <- vec_matches(needles, haystack, condition = ">=", filter = "max", missing = "match", multiple = "first")
  expect_identical(res$needles, 1:4)
  expect_identical(res$haystack, c(2L, 1L, 5L, 1L))

  res <- vec_matches(needles, haystack, condition = ">=", filter = "max", missing = "propagate")
  expect_identical(res$needles, c(1L, 1L, 2L, 3L, 4L))
  expect_identical(res$haystack, c(2L, 4L, NA, 5L, NA))
})

test_that("`filter` works with mixed NA and NaN", {
  needles <- c(1, NA, 4, NaN)
  haystack <- c(NA, 1, NaN, 1, 3)

  res <- vec_matches(needles, haystack, condition = ">=", filter = "min", missing = "match", nan_distinct = FALSE)
  expect_identical(res$needles, c(1L, 1L, 2L, 2L, 3L, 3L, 4L, 4L))
  expect_identical(res$haystack, c(2L, 4L, 1L, 3L, 2L, 4L, 1L, 3L))

  res <- vec_matches(needles, haystack, condition = ">=", filter = "min", missing = "match", nan_distinct = TRUE)
  expect_identical(res$needles, c(1L, 1L, 2L, 3L, 3L, 4L))
  expect_identical(res$haystack, c(2L, 4L, 1L, 2L, 4L, 3L))
})

test_that("`filter` is validated", {
  expect_error(vec_matches(1, 2, filter = 1.5), "character vector")
  expect_error(vec_matches(1, 2, filter = "x"), 'one of "none", "min", or "max"')
  expect_error(vec_matches(1, 2, filter = c("min", "max")), "length 1, or the same length as")
})

# ------------------------------------------------------------------------------
# vec_matches() - edge cases

test_that("zero row `needles` results in zero row data frame output", {
  res <- vec_matches(integer(), 1:3)

  expect_identical(res$needles, integer())
  expect_identical(res$haystack, integer())

  res <- vec_matches(integer(), 1:3, condition = "<")

  expect_identical(res$needles, integer())
  expect_identical(res$haystack, integer())
})

test_that("zero row `haystack` results in no-matches for all needles", {
  res <- vec_matches(1:3, integer())

  expect_identical(res$needles, 1:3)
  expect_identical(res$haystack, rep(NA_integer_, 3))

  res <- vec_matches(1:3, integer(), condition = "<")

  expect_identical(res$needles, 1:3)
  expect_identical(res$haystack, rep(NA_integer_, 3))
})

test_that("zero row `haystack` still allows needle NA propagation", {
  res <- vec_matches(c(1, NA), integer(), missing = "propagate", no_match = 0L)

  expect_identical(res$needles, 1:2)
  expect_identical(res$haystack, c(0L, NA))

  res <- vec_matches(c(1, NA), integer(), missing = "propagate", no_match = 0L, condition = "<")

  expect_identical(res$needles, 1:2)
  expect_identical(res$haystack, c(0L, NA))
})

test_that("`condition = NULL` is correct in all possible cases", {
  matches <- function(needles, haystack, multiple, no_match = NA_integer_) {
    vec_matches(needles, haystack, condition = NULL, multiple = multiple, no_match = no_match)
  }
  exp <- function(needles, haystack) {
    data_frame(needles = as.integer(needles), haystack = as.integer(haystack))
  }

  zero <- data_frame(.size = 0L)
  one <- data_frame(.size = 1L)
  two <- data_frame(.size = 2L)

  multiples <- c("all", "warning", "error", "first", "last")

  for (multiple in multiples) {
    # `zero` haystack
    expect_identical(matches(zero, zero, multiple = multiple), exp(integer(), integer()))
    expect_identical(matches(one, zero, multiple = multiple), exp(1, NA))
    expect_identical(matches(two, zero, multiple = multiple), exp(1:2, c(NA, NA)))
  }

  for (multiple in multiples) {
    # `one` haystack
    expect_identical(matches(zero, one, multiple = multiple), exp(integer(), integer()))
    expect_identical(matches(one, one, multiple = multiple), exp(1, 1))
    expect_identical(matches(two, one, multiple = multiple), exp(1:2, c(1, 1)))
  }

  # `two` haystack
  expect_identical(matches(zero, two, multiple = "all"), exp(integer(), integer()))
  expect_identical(matches(one, two, multiple = "all"), exp(c(1, 1), c(1, 2)))
  expect_identical(matches(two, two, multiple = "all"), exp(c(1, 1, 2, 2), c(1, 2, 1, 2)))

  expect_identical(matches(zero, two, multiple = "warning"), exp(integer(), integer()))
  expect_identical(expect_warning(matches(one,  two, multiple = "warning")), exp(c(1, 1), c(1, 2)))
  expect_identical(expect_warning(matches(two,  two, multiple = "warning")), exp(c(1, 1, 2, 2), c(1, 2, 1, 2)))

  expect_identical(matches(zero, two, multiple = "error"), exp(integer(), integer()))
  expect_error(matches(one, two, multiple = "error"), "multiple matches")
  expect_error(matches(two, two, multiple = "error"), "multiple matches")

  expect_identical(matches(zero, two, multiple = "first"), exp(integer(), integer()))
  expect_identical(matches(one, two, multiple = "first"), exp(1, 1))
  expect_identical(matches(two, two, multiple = "first"), exp(1:2, c(1, 1)))

  expect_identical(matches(zero, two, multiple = "last"), exp(integer(), integer()))
  expect_identical(matches(one, two, multiple = "last"), exp(1, 2))
  expect_identical(matches(two, two, multiple = "last"), exp(1:2, c(2, 2)))

  for (multiple in multiples) {
    # `zero` haystack with `no_match = "error"`
    expect_identical(matches(zero, zero, multiple = multiple, no_match = "error"), exp(integer(), integer()))
    expect_error(matches(one, zero, multiple = multiple, no_match = "error"), "must have a match")
    expect_error(matches(two, zero, multiple = multiple, no_match = "error"), "must have a match")
  }
})

test_that("zero column data frames are not allowed if `condition != NULL`", {
  expect_error(
    vec_matches(data_frame(.size = 2L), data_frame(.size = 2L)),
    "at least 1 column"
  )
})

test_that("zero column input still checks `condition` correctness", {
  x <- data_frame(.size = 2)
  y <- data_frame(.size = 3)

  expect_error(
    vec_matches(x, y, condition = c("==", "<=")),
    "length 1, or the same length as the number of columns"
  )
})

test_that("`multiple = 'first'/'last'` returns the first/last by appearance", {
  x <- c(1, 2, 3)
  y <- c(2, 1, 0)

  res <- vec_matches(x, y, condition = ">=", multiple = "first")
  expect_identical(res$haystack, c(2L, 1L, 1L))

  res <- vec_matches(x, y, condition = ">=", multiple = "last")
  expect_identical(res$haystack, c(3L, 3L, 3L))
})

test_that("NA adjustment of `>` and `>=` conditions is protected from empty haystack", {
  res <- vec_matches(1L, integer(), condition = ">")
  expect_identical(res$needles, 1L)
  expect_identical(res$haystack, NA_integer_)
})

test_that("`condition = NULL` works with `no_match = 'drop'`", {
  # All needles are unmatched with an empty haystack
  res <- vec_matches(1:2, integer(), condition = NULL, no_match = "drop")
  expect_identical(res$needles, integer())
  expect_identical(res$haystack, integer())
})

test_that("potential overflow on large output size is caught informatively", {
  verify_output(test_path("error", "test-matches-overflow-output.txt"), {
    "# catches potential overflow"
    vec_matches(1:1e7, 1:1e7, condition = ">=")

    "# catches overflow with `condition = NULL`"
    vec_matches(1:1e7, 1:1e7, condition = NULL)
  })
})

# ------------------------------------------------------------------------------
# vec_matches() - nested containment

test_that("`multiple = 'first' doesn't require nested groups if completely ordered", {
  # Single nested containment group.
  # It is already completely ordered (in ascending order by value and row number).
  df <- data_frame(x = c(1L, 1L, 2L, 3L))
  res <- compute_nested_containment_info(df, ">=", "first")
  nested_groups <- res[[2]]
  expect_identical(nested_groups, integer())
})

test_that("`multiple = 'first'` requires increasing row order, and looks at group starts", {
  # 2 different nested groups:
  # - We are doing multiple = "first", so we are looking at group starts
  # - Look at the 1 at location 2, it becomes the first group along with its
  #   group partner at location 4.
  # - Look at the 2 at location 3, it is greater than the 1 at location 1 in
  #   both size and row number. It joins that group.
  # - Look at the 3 at location 1, it is greater than the 1 at location 1,
  #   but is smaller in row number. It becomes a new group with its partner at
  #   location 5.
  df <- data_frame(x = c(3L, 1L, 2L, 1L, 3L))
  res <- compute_nested_containment_info(df, ">=", "first")
  nested_groups <- res[[2]]
  expect_identical(nested_groups, c(1L, 0L, 0L, 0L, 1L))
})

test_that("`multiple = 'last'` requires increasing row order, and looks at group ends", {
  # 2 different nested groups:
  # - We are doing multiple = "last", so we are looking at group ends
  # - Look at the 1 at location 4, it becomes the first group along with its
  #   group partner at location 2.
  # - Look at the 2 at location 3, it is greater than the 1 at location 4,
  #   but is smaller in row number. It becomes a new group.
  # - Look at the 3 at location 5, it is greater than the 1 at location 4 in
  #   both size and row number. It joins that group.
  df <- data_frame(x = c(3L, 1L, 2L, 1L, 3L))
  res <- compute_nested_containment_info(df, ">=", "last")
  nested_groups <- res[[2]]
  expect_identical(nested_groups, c(0L, 0L, 1L, 0L, 0L))
})

