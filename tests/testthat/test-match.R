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

test_that("NA and NaN both propagate with `na_equal = FALSE` no matter the value of `nan_distinct`", {
  res <- vec_matches(c(NA, NaN), c(NA, NaN), na_equal = FALSE, nan_distinct = FALSE)
  expect_identical(res$needles, 1:2)
  expect_identical(res$haystack, c(NA_integer_, NA_integer_))

  res <- vec_matches(c(NA, NaN), c(NA, NaN), na_equal = FALSE, nan_distinct = TRUE)
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
  res <- vec_matches(x, y, condition = "==", na_equal = TRUE, nan_distinct = FALSE)
  expect_identical(res$needles, rep(1:4, each = 4))
  expect_identical(res$haystack, rep(1:4, times = 4))

  # Missings can match, but all combinations are different
  res <- vec_matches(x, y, condition = "==", na_equal = TRUE, nan_distinct = TRUE)
  expect_identical(res$needles, 1:4)
  expect_identical(res$haystack, 1:4)

  # Missings propagate
  res <- vec_matches(x, y, condition = "==", na_equal = FALSE)
  expect_identical(res$needles, 1:4)
  expect_identical(res$haystack, rep(NA_integer_, 4))

  # Propagated missings are never considered no-matches
  expect_identical(
    vec_matches(x, y, condition = "==", na_equal = FALSE, no_match = "error"),
    vec_matches(x, y, condition = "==", na_equal = FALSE)
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
  res <- vec_matches(list(NULL), list(NULL), na_equal = FALSE)

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

test_that("df-cols must be entirely missing to propagate missingness", {
  df1 <- data_frame(x = 1, y = data_frame(x = NA, y = c(NA, 2)))
  df2 <- data_frame(x = 1, y = data_frame(x = NA, y = c(NA, 2)))

  res <- vec_matches(df1, df2, na_equal = TRUE)
  expect_identical(res$needles, 1:2)
  expect_identical(res$haystack, 1:2)

  # 2nd row isn't completely missing, so it still matches
  res <- vec_matches(df1, df2, na_equal = FALSE)
  expect_identical(res$needles, 1:2)
  expect_identical(res$haystack, c(NA, 2L))
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

test_that("rcrd type missingness can be propagated", {
  x <- new_rcrd(list(x = c(1L, NA), y = c(NA_integer_, NA_integer_)))
  y <- new_rcrd(list(x = c(1L, 2L, NA), y = c(NA, 5L, NA)))

  res <- vec_matches(x, y, condition = "==", na_equal = TRUE)
  expect_identical(res$needles, c(1L, 2L))
  expect_identical(res$haystack, c(1L, 3L))

  res <- vec_matches(x, y, condition = "==", na_equal = FALSE)
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
# vec_matches() - `na_equal`

test_that("can propagate needle NAs with `na_equal = FALSE`", {
  x <- c(1L, NA, 2L)
  y <- c(NA, 1L, 1L)

  res <- vec_matches(x, y, condition = "==", na_equal = FALSE)

  expect_identical(res$needles, c(1L, 1L, 2L, 3L))
  expect_identical(res$haystack, c(2L, 3L, NA, NA))

  res <- vec_matches(x, y, condition = "<=", na_equal = FALSE)

  expect_identical(res$needles, c(1L, 1L, 2L, 3L))
  expect_identical(res$haystack, c(2L, 3L, NA, NA))

  res <- vec_matches(x, y, condition = ">=", na_equal = FALSE)

  expect_identical(res$needles, c(1L, 1L, 2L, 3L, 3L))
  expect_identical(res$haystack, c(2L, 3L, NA, 2L, 3L))
})

test_that("if `na_equal = FALSE`, an NA in any column results in propagation", {
  df1 <- data_frame(x = c(1L, NA, 2L, 1L, 1L), y = c(2L, 2L, NA, 1L, 1L))
  df2 <- data_frame(x = c(1L, 1L, 2L), y = c(1L, 1L, NA))

  res <- vec_matches(df1, df2, condition = c("==", "=="), na_equal = FALSE)

  expect_identical(res$needles, c(1L, 2L, 3L, 4L, 4L, 5L, 5L))
  expect_identical(res$haystack, c(NA, NA, NA, 1L, 2L, 1L, 2L))

  res <- vec_matches(df1, df2, condition = c(">=", ">="), na_equal = FALSE)

  expect_identical(res$needles, c(1L, 1L, 2L, 3L, 4L, 4L, 5L, 5L))
  expect_identical(res$haystack, c(1L, 2L, NA, NA, 1L, 2L, 1L, 2L))
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

test_that("can error on `multiple` matches", {
  expect_error(
    vec_matches(c(1L, 3L, 1L, 3L), c(1L, 3L, 1L), multiple = "error"),
    "multiple matches"
  )
})

test_that("can warn on `multiple` matches (with fallback to all)", {
  expect_identical(
    expect_warning(
      vec_matches(c(1L, 3L, 1L, 3L), c(1L, 3L, 1L), multiple = "warning"),
      "multiple matches"
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

# ------------------------------------------------------------------------------
# vec_matches() - `no_match`

test_that("can control `no_match`", {
  x <- vec_matches(1:3, 1L)
  expect_identical(x$haystack, c(1L, NA, NA))

  x <- vec_matches(1:3, 1L, no_match = 0L)
  expect_identical(x$haystack, c(1L, 0L, 0L))
})

test_that("can differentiate between `no_match` and propagated NAs", {
  res <- vec_matches(c(1, NA), 2, na_equal = FALSE, no_match = -1L)

  expect_identical(res$needles, 1:2)
  expect_identical(res$haystack, c(-1L, NA))
})

test_that("`no_match` can error", {
  expect_error(
    vec_matches(1, 2, no_match = "error"),
    "no matches"
  )
})

test_that("`no_match = 'error'` passes propagated NAs through untouched", {
  res <- vec_matches(c(NA, NaN, NA, 1), c(NA, 1), na_equal = FALSE, no_match = "error")

  expect_identical(res$needles, 1:4)
  expect_identical(res$haystack, c(rep(NA, 3), 2L))
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
  res <- vec_matches(c(1, NA), integer(), na_equal = FALSE, no_match = 0L)

  expect_identical(res$needles, 1:2)
  expect_identical(res$haystack, c(0L, NA))

  res <- vec_matches(c(1, NA), integer(), na_equal = FALSE, no_match = 0L, condition = "<")

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
    expect_error(matches(one, zero, multiple = multiple, no_match = "error"), "no matches")
    expect_error(matches(two, zero, multiple = multiple, no_match = "error"), "no matches")
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
