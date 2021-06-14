# ------------------------------------------------------------------------------
# vec_order(<integer>) - insertion

test_that("can order size zero input", {
  expect_identical(vec_order(integer()), integer())
})

test_that("can order integers", {
  x <- c(2L, 3L, 1L, 5L)
  expect_identical(vec_order(x), order(x))
})

test_that("can order sorted vector", {
  x <- 1:5
  expect_identical(vec_order(x), order(x))
})

test_that("orders correctly around the UINT8_MAX boundary", {
  x <- 251:255
  expect_identical(vec_order(x), order(x))
})

test_that("ordering on ties is done stably", {
  x <- c(1L, 3L, 1L, 3L)
  expect_identical(vec_order(x)[1:2], c(1L, 3L))
  expect_identical(vec_order(x)[3:4], c(2L, 4L))
})

test_that("`NA` order defaults to last", {
  x <- c(1L, NA_integer_, 3L)
  expect_identical(vec_order(x), c(1L, 3L, 2L))
})

test_that("integer, small: `NA` order can be first", {
  x <- c(1L, NA_integer_, 3L)
  expect_identical(vec_order(x, na_value = "smallest"), c(2L, 1L, 3L))
})

test_that("double: `direction` can be set to `desc`", {
  x <- c(1, 5, 3)
  expect_identical(vec_order(x, direction = "desc"), c(2L, 3L, 1L))
  x <- c(1L, .Machine$integer.max, 3L)
  expect_identical(vec_order(x, direction = "desc"), c(2L, 3L, 1L))
})

test_that("all combinations of `direction` and `na_value` work", {
  x <- c(3L, NA_integer_, 1L, 2L)

  expect_identical(
    x[vec_order(x, na_value = "largest", direction = "asc")],
    x[order(x, na.last = TRUE, decreasing = FALSE)]
  )
  expect_identical(
    x[vec_order(x, na_value = "smallest", direction = "asc")],
    x[order(x, na.last = FALSE, decreasing = FALSE)]
  )
  expect_identical(
    x[vec_order(x, na_value = "largest", direction = "desc")],
    x[order(x, na.last = FALSE, decreasing = TRUE)]
  )
  expect_identical(
    x[vec_order(x, na_value = "smallest", direction = "desc")],
    x[order(x, na.last = TRUE, decreasing = TRUE)]
  )
})

test_that("all `NA` values works", {
  x <- c(NA_integer_, NA_integer_)
  expect_identical(vec_order(x), order(x))
})

test_that("can order when in expected order", {
  x <- c(1L, 1L, 2L, NA, NA)
  expect_identical(vec_order(x, direction = "asc", na_value = "largest"), 1:5)

  x <- c(NA, NA, 3L, 3L, 2L)
  expect_identical(vec_order(x, direction = "desc", na_value = "largest"), 1:5)

  x <- c(NA, NA, 1L, 1L, 2L)
  expect_identical(vec_order(x, direction = "asc", na_value = "smallest"), 1:5)

  x <- c(3L, 3L, 2L, NA, NA)
  expect_identical(vec_order(x, direction = "desc", na_value = "smallest"), 1:5)
})

test_that("can order when in strictly opposite of expected order (no ties)", {
  x <- c(NA, 2L, 1L)
  expect_identical(vec_order(x, direction = "asc", na_value = "largest"), 3:1)

  x <- c(1L, 2L, NA)
  expect_identical(vec_order(x, direction = "desc", na_value = "largest"), 3:1)

  x <- c(2L, 1L, NA)
  expect_identical(vec_order(x, direction = "asc", na_value = "smallest"), 3:1)

  x <- c(NA, 1L, 2L)
  expect_identical(vec_order(x, direction = "desc", na_value = "smallest"), 3:1)
})

# ------------------------------------------------------------------------------
# vec_order(<integer>) - counting

# To trigger counting ordering, get above the insertion order boundary and then
# have a range less than the counting order range boundary.

test_that("can order integers with counting order", {
  x <- (ORDER_INSERTION_BOUNDARY + 1L):1L
  expect_identical(vec_order(x), order(x))
})

test_that("can order sorted vector", {
  x <- 1:(ORDER_INSERTION_BOUNDARY + 1L)
  expect_identical(vec_order(x), order(x))
})

test_that("ordering on ties is done stably", {
  x <- c(1:ORDER_INSERTION_BOUNDARY, 1L)
  expect_identical(vec_order(x)[1:2], c(1L, ORDER_INSERTION_BOUNDARY + 1L))
})

test_that("all combinations of `direction` and `na_value` work", {
  x <- c(3L, NA_integer_, 1L, 2L, 1:ORDER_INSERTION_BOUNDARY)

  expect_identical(
    x[vec_order(x, na_value = "largest", direction = "asc")],
    x[order(x, na.last = TRUE, decreasing = FALSE)]
  )
  expect_identical(
    x[vec_order(x, na_value = "smallest", direction = "asc")],
    x[order(x, na.last = FALSE, decreasing = FALSE)]
  )
  expect_identical(
    x[vec_order(x, na_value = "largest", direction = "desc")],
    x[order(x, na.last = FALSE, decreasing = TRUE)]
  )
  expect_identical(
    x[vec_order(x, na_value = "smallest", direction = "desc")],
    x[order(x, na.last = TRUE, decreasing = TRUE)]
  )
})

# ------------------------------------------------------------------------------
# vec_order(<integer>) - radix

# To trigger radix ordering, get above the insertion order boundary and then
# have a range greater than the counting order range boundary.

test_that("can order integers with radix order", {
  x <- c(INT_ORDER_COUNTING_RANGE_BOUNDARY + 1L, 1:ORDER_INSERTION_BOUNDARY)
  expect_identical(vec_order(x), order(x))
})

test_that("can order sorted vector", {
  x <- c(1:ORDER_INSERTION_BOUNDARY, INT_ORDER_COUNTING_RANGE_BOUNDARY + 1L)
  expect_identical(vec_order(x), order(x))
})

test_that("ordering on ties is done stably", {
  x <- c(1:ORDER_INSERTION_BOUNDARY, 1L, INT_ORDER_COUNTING_RANGE_BOUNDARY + 1L)
  expect_identical(vec_order(x)[1:2], c(1L, ORDER_INSERTION_BOUNDARY + 1L))
})

test_that("all combinations of `direction` and `na_value` work", {
  x <- c(3L, NA_integer_, 1L, 2L, 1:ORDER_INSERTION_BOUNDARY, INT_ORDER_COUNTING_RANGE_BOUNDARY + 1L)

  expect_identical(
    x[vec_order(x, na_value = "largest", direction = "asc")],
    x[order(x, na.last = TRUE, decreasing = FALSE)]
  )
  expect_identical(
    x[vec_order(x, na_value = "smallest", direction = "asc")],
    x[order(x, na.last = FALSE, decreasing = FALSE)]
  )
  expect_identical(
    x[vec_order(x, na_value = "largest", direction = "desc")],
    x[order(x, na.last = FALSE, decreasing = TRUE)]
  )
  expect_identical(
    x[vec_order(x, na_value = "smallest", direction = "desc")],
    x[order(x, na.last = TRUE, decreasing = TRUE)]
  )
})

test_that("can order all 1 value", {
  x <- rep(1L, ORDER_INSERTION_BOUNDARY + 1L)
  expect_identical(vec_order(x), base_order(x))
  expect_identical(vec_order(x, direction = "desc"), base_order(x, decreasing = TRUE))
})

test_that("all `NA` values works - ensures that we can compute the 'range' of all NAs", {
  x <- rep(NA_integer_, ORDER_INSERTION_BOUNDARY + 1L)
  expect_identical(vec_order(x), base_order(x))
  expect_identical(vec_order(x, direction = "desc"), base_order(x, decreasing = TRUE))
})

test_that("can order with many NAs first", {
  x <- c(rep(NA_integer_, ORDER_INSERTION_BOUNDARY + 1L), 2L)
  expect_identical(vec_order(x), base_order(x))
  expect_identical(vec_order(x, na_value = "smallest"), base_order(x, na.last = FALSE))
})

# ------------------------------------------------------------------------------
# vec_order(<logical>)

# Really this just goes through the integer infrastructure. Just checking that
# it is working.

test_that("can order size zero input", {
  expect_identical(vec_order(logical()), integer())
})

test_that("can order logicals", {
  x <- c(FALSE, TRUE, FALSE)
  expect_identical(vec_order(x), order(x))
})

test_that("all combinations of `direction` and `na_value` work", {
  x <- c(TRUE, NA, FALSE)

  expect_identical(
    x[vec_order(x, na_value = "largest", direction = "asc")],
    x[order(x, na.last = TRUE, decreasing = FALSE)]
  )
  expect_identical(
    x[vec_order(x, na_value = "smallest", direction = "asc")],
    x[order(x, na.last = FALSE, decreasing = FALSE)]
  )
  expect_identical(
    x[vec_order(x, na_value = "largest", direction = "desc")],
    x[order(x, na.last = FALSE, decreasing = TRUE)]
  )
  expect_identical(
    x[vec_order(x, na_value = "smallest", direction = "desc")],
    x[order(x, na.last = TRUE, decreasing = TRUE)]
  )
})

test_that("all `NA` values works", {
  x <- c(NA, NA)
  expect_identical(vec_order(x), order(x))
})

# ------------------------------------------------------------------------------
# vec_order(<double>) - insertion

test_that("can order size zero input", {
  expect_identical(vec_order(double()), integer())
})

test_that("can order doubles", {
  x <- c(2, 3, 1, 5)
  expect_identical(vec_order(x), order(x))
})

test_that("can order sorted vector", {
  x <- 1:5 + 0
  expect_identical(vec_order(x), order(x))
})

test_that("ordering on ties is done stably", {
  x <- c(1, 3, 1, 3)
  expect_identical(vec_order(x)[1:2], c(1L, 3L))
  expect_identical(vec_order(x)[3:4], c(2L, 4L))
})

test_that("`NA` order defaults to last", {
  x <- c(1, NA_real_, 3)
  expect_identical(vec_order(x), c(1L, 3L, 2L))
})

test_that("double: `NA` order can be first", {
  x <- c(1, NA_real_, 3)
  expect_identical(vec_order(x, na_value = "smallest"), c(2L, 1L, 3L))
})

test_that("all combinations of `direction` and `na_value` work", {
  x <- c(3, NA_real_, 1, 2)

  expect_identical(
    x[vec_order(x, na_value = "largest", direction = "asc")],
    x[order(x, na.last = TRUE, decreasing = FALSE)]
  )
  expect_identical(
    x[vec_order(x, na_value = "smallest", direction = "asc")],
    x[order(x, na.last = FALSE, decreasing = FALSE)]
  )
  expect_identical(
    x[vec_order(x, na_value = "largest", direction = "desc")],
    x[order(x, na.last = FALSE, decreasing = TRUE)]
  )
  expect_identical(
    x[vec_order(x, na_value = "smallest", direction = "desc")],
    x[order(x, na.last = TRUE, decreasing = TRUE)]
  )
})

test_that("all `NA` values works", {
  x <- c(NA_real_, NA_real_)
  expect_identical(vec_order(x), order(x))
})

test_that("NA_real_ and NaN look identical for ordering", {
  x <- c(NA_real_, NaN)
  expect_identical(vec_order(x, na_value = "largest"), c(1L, 2L))
  expect_identical(vec_order(x, na_value = "smallest"), c(1L, 2L))
})

test_that("double: -Inf / Inf order correctly", {
  x <- c(0, -Inf, Inf)
  expect_identical(vec_order(x, direction = "asc"), c(2L, 1L, 3L))
  expect_identical(vec_order(x, direction = "desc"), c(3L, 1L, 2L))
})

test_that("double: -0 and 0 order identically / stably", {
  x <- c(0, -0)
  expect_identical(vec_order(x, direction = "desc"), c(1L, 2L))
  expect_identical(vec_order(x, direction = "asc"), c(1L, 2L))
})

test_that("can order when in expected order", {
  x <- c(1, 1, 2, NA, NaN)
  expect_identical(vec_order(x, direction = "asc", na_value = "largest"), 1:5)

  x <- c(NA, NaN, 3, 3, 2)
  expect_identical(vec_order(x, direction = "desc", na_value = "largest"), 1:5)

  x <- c(NA, NaN, 1, 1, 2)
  expect_identical(vec_order(x, direction = "asc", na_value = "smallest"), 1:5)

  x <- c(3, 3, 2, NA, NaN)
  expect_identical(vec_order(x, direction = "desc", na_value = "smallest"), 1:5)
})

test_that("can order when in expected order - using distinct NaN values", {
  x <- c(1, 1, 2, NaN, NA)
  expect_identical(vec_order(x, direction = "asc", na_value = "largest", nan_distinct = TRUE), 1:5)

  x <- c(NA, NaN, 3, 3, 2)
  expect_identical(vec_order(x, direction = "desc", na_value = "largest", nan_distinct = TRUE), 1:5)

  x <- c(NA, NaN, 1, 1, 2)
  expect_identical(vec_order(x, direction = "asc", na_value = "smallest", nan_distinct = TRUE), 1:5)

  x <- c(3, 3, 2, NaN, NA)
  expect_identical(vec_order(x, direction = "desc", na_value = "smallest", nan_distinct = TRUE), 1:5)
})

test_that("can order when in strictly opposite of expected order (no ties)", {
  x <- c(NA, 2, 1)
  expect_identical(vec_order(x, direction = "asc", na_value = "largest"), 3:1)

  x <- c(1, 2, NA)
  expect_identical(vec_order(x, direction = "desc", na_value = "largest"), 3:1)

  x <- c(2, 1, NA)
  expect_identical(vec_order(x, direction = "asc", na_value = "smallest"), 3:1)

  x <- c(NA, 1, 2)
  expect_identical(vec_order(x, direction = "desc", na_value = "smallest"), 3:1)
})

test_that("can order when in strictly opposite of expected order (no ties) - using distinct NaN values", {
  x <- c(NA, NaN, 2, 1)
  expect_identical(vec_order(x, direction = "asc", na_value = "largest", nan_distinct = TRUE), 4:1)

  x <- c(1, 2, NaN, NA)
  expect_identical(vec_order(x, direction = "desc", na_value = "largest", nan_distinct = TRUE), 4:1)

  x <- c(2, 1, NaN, NA)
  expect_identical(vec_order(x, direction = "asc", na_value = "smallest", nan_distinct = TRUE), 4:1)

  x <- c(NA, NaN, 1, 2)
  expect_identical(vec_order(x, direction = "desc", na_value = "smallest", nan_distinct = TRUE), 4:1)
})

test_that("NaN is always placed next to numbers when treated as distinct", {
  x <- c(1, 2, NA, NaN)

  expect_identical(vec_order(x, direction = "asc", na_value = "largest", nan_distinct = TRUE), c(1L, 2L, 4L, 3L))
  expect_identical(vec_order(x, direction = "asc", na_value = "smallest", nan_distinct = TRUE), c(3L, 4L, 1L, 2L))
  expect_identical(vec_order(x, direction = "desc", na_value = "largest", nan_distinct = TRUE), c(3L, 4L, 2L, 1L))
  expect_identical(vec_order(x, direction = "desc", na_value = "smallest", nan_distinct = TRUE), c(2L, 1L, 4L, 3L))
})

# ------------------------------------------------------------------------------
# vec_order(<double>) - radix

# To trigger radix ordering, get above the insertion order boundary. There is
# no intermediate counting sort for doubles.

test_that("can order doubles with radix order", {
  x <- (ORDER_INSERTION_BOUNDARY + 1L):1L + 0
  expect_identical(vec_order(x), order(x))
})

test_that("can order sorted vector", {
  x <- 1:(ORDER_INSERTION_BOUNDARY + 1L) + 0
  expect_identical(vec_order(x), order(x))
})

test_that("ordering on ties is done stably", {
  x <- c(1:ORDER_INSERTION_BOUNDARY, 1L) + 0
  expect_identical(vec_order(x)[1:2], c(1L, ORDER_INSERTION_BOUNDARY + 1L))
})

test_that("all combinations of `direction` and `na_value` work", {
  x <- c(3, NA_real_, 1, 2, 1:ORDER_INSERTION_BOUNDARY)

  expect_identical(
    x[vec_order(x, na_value = "largest", direction = "asc")],
    x[order(x, na.last = TRUE, decreasing = FALSE)]
  )
  expect_identical(
    x[vec_order(x, na_value = "smallest", direction = "asc")],
    x[order(x, na.last = FALSE, decreasing = FALSE)]
  )
  expect_identical(
    x[vec_order(x, na_value = "largest", direction = "desc")],
    x[order(x, na.last = FALSE, decreasing = TRUE)]
  )
  expect_identical(
    x[vec_order(x, na_value = "smallest", direction = "desc")],
    x[order(x, na.last = TRUE, decreasing = TRUE)]
  )
})

test_that("all `NA` values works", {
  x <- rep(NA_real_, ORDER_INSERTION_BOUNDARY + 1L)
  expect_identical(vec_order(x), order(x))
})

test_that("NA_real_ and NaN generally look identical for ordering", {
  x <- rep(c(NA_real_, NaN), ORDER_INSERTION_BOUNDARY + 1L)
  expect_identical(vec_order(x, na_value = "largest"), seq_along(x))
  expect_identical(vec_order(x, na_value = "smallest"), seq_along(x))
})

test_that("NA_real_ and NaN can be considered distinct with `nan_distinct`", {
  x <- rep(c(NA_real_, NaN), ORDER_INSERTION_BOUNDARY + 1L)

  loc_nan <- seq(2L, length(x), by = 2L)
  loc_na <- seq(1L, length(x), by = 2L)

  expect_identical(vec_order(x, na_value = "largest", nan_distinct = TRUE), c(loc_nan, loc_na))
  expect_identical(vec_order(x, na_value = "smallest", nan_distinct = TRUE), c(loc_na, loc_nan))
})

test_that("-Inf / Inf order correctly", {
  x <- c(rep(0, ORDER_INSERTION_BOUNDARY), -Inf, Inf)
  expect_identical(vec_order(x, direction = "asc"), order(x, decreasing = FALSE))
  expect_identical(vec_order(x, direction = "desc"), order(x, decreasing = TRUE))
})

test_that("double, large: -0 and 0 order identically / stably", {
  x <- c(rep(0, ORDER_INSERTION_BOUNDARY), -0)
  expect_identical(vec_order(x, direction = "desc"), order(x, decreasing = TRUE))
  expect_identical(vec_order(x, direction = "asc"), order(x, decreasing = FALSE))
})

# ------------------------------------------------------------------------------
# vec_order(<complex>)

test_that("can order size zero input", {
  expect_identical(vec_order(complex()), integer())
})

test_that("can order complex", {
  x <- complex(real = c(3, 1, 2))
  expect_identical(vec_order(x), c(2L, 3L, 1L))
})

test_that("ordering on ties is done stably", {
  x <- complex(real = c(1, 3, 1, 3))
  expect_identical(vec_order(x)[1:2], c(1L, 3L))
  expect_identical(vec_order(x)[3:4], c(2L, 4L))
})

test_that("imaginary section is used to break ties", {
  x <- complex(
    real = c(1L, 2L, 1L),
    imaginary = c(3L, 2L, 1L)
  )
  expect_identical(vec_order(x), c(3L, 1L, 2L))
})

test_that("can be used in a data frame", {
  x <- c(1L, 1L, 1L, 2L, 1L)

  y <- complex(
    real = c(1L, 2L, 1L, 3L, 1L),
    imaginary = c(3L, 2L, 1L, 4L, 1L)
  )

  z <- c(1, 2, 5, 4, 3)

  # as second column
  df1 <- data.frame(x = x, y = y)

  # as first column
  df2 <- data.frame(y = y, x = x)

  # as second column with a third after it to break ties
  df3 <- data.frame(x = x, y = y, z = z)

  # Base R can't do radix sorting with complex
  expect_identical(vec_order(df1), c(3L, 5L, 1L, 2L, 4L))
  expect_identical(vec_order(df2), c(3L, 5L, 1L, 2L, 4L))
  expect_identical(vec_order(df3), c(5L, 3L, 1L, 2L, 4L))
})

test_that("all combinations of `direction` and `na_value` work", {
  x <- complex(real = c(3, NA, 1.5, 2, NA), imaginary = c(1, 1, 1, 1, 2))

  expect_identical(
    x[vec_order(x, na_value = "largest", direction = "asc")],
    x[order(x, na.last = TRUE, decreasing = FALSE)]
  )
  expect_identical(
    x[vec_order(x, na_value = "smallest", direction = "asc")],
    x[order(x, na.last = FALSE, decreasing = FALSE)]
  )

  # Base R is actually wrong here! It doesn't consider the imaginary part
  # when ordering in decreasing order with `NA` real.
  expect_identical(
    x[vec_order(x, na_value = "largest", direction = "desc")],
    #x[order(x, na.last = FALSE, decreasing = TRUE)]
    x[c(5L, 2L, 1L, 4L, 3L)]
  )
  expect_identical(
    x[vec_order(x, na_value = "smallest", direction = "desc")],
    #x[order(x, na.last = FALSE, decreasing = TRUE)]
    x[c(1L, 4L, 3L, 5L, 2L)]
  )
})

test_that("can treat NaN and NA_real_ as distinct values", {
  x <- complex(real = c(NA, NA, NaN, NaN), imaginary = c(NA, NaN, NaN, NA))

  expect_identical(vec_order(x, direction = "asc", na_value = "largest", nan_distinct = TRUE), c(3L, 4L, 2L, 1L))
  expect_identical(vec_order(x, direction = "asc", na_value = "smallest", nan_distinct = TRUE), c(1L, 2L, 4L, 3L))
  expect_identical(vec_order(x, direction = "desc", na_value = "largest", nan_distinct = TRUE), c(1L, 2L, 4L, 3L))
  expect_identical(vec_order(x, direction = "desc", na_value = "smallest", nan_distinct = TRUE), c(3L, 4L, 2L, 1L))
})

# ------------------------------------------------------------------------------
# vec_order(<character>) - insertion

test_that("can order size zero input", {
  expect_identical(vec_order(character()), integer())
})

test_that("can order characters", {
  x <- c("xy", "x", "a", "bc")
  expect_identical(vec_order(x), order(x))
})

test_that("can order sorted vector", {
  x <- c("a", "b", "c")
  expect_identical(vec_order(x), order(x))
})

test_that("ordering on ties is done stably", {
  x <- c("ab", "ba", "ab", "ba")
  expect_identical(vec_order(x)[1:2], c(1L, 3L))
  expect_identical(vec_order(x)[3:4], c(2L, 4L))
})

test_that("`NA` order defaults to last", {
  x <- c("x", NA_character_, "y")
  expect_identical(vec_order(x), c(1L, 3L, 2L))
})

test_that("character, small: `NA` order can be first", {
  x <- c("x", NA_character_, "y")
  expect_identical(vec_order(x, na_value = "smallest"), c(2L, 1L, 3L))
})

test_that("character, small: `direction` can be set to `desc`", {
  x <- c("x", "abcde", "yz")
  expect_identical(vec_order(x, direction = "desc"), c(3L, 1L, 2L))
})

test_that("all combinations of `direction` and `na_value` work", {
  x <- c("aaa", NA_character_, "a", "aa")

  expect_identical(
    x[vec_order(x, na_value = "largest", direction = "asc")],
    x[order(x, na.last = TRUE, decreasing = FALSE)]
  )
  expect_identical(
    x[vec_order(x, na_value = "smallest", direction = "asc")],
    x[order(x, na.last = FALSE, decreasing = FALSE)]
  )
  expect_identical(
    x[vec_order(x, na_value = "largest", direction = "desc")],
    x[order(x, na.last = FALSE, decreasing = TRUE)]
  )
  expect_identical(
    x[vec_order(x, na_value = "smallest", direction = "desc")],
    x[order(x, na.last = TRUE, decreasing = TRUE)]
  )
})

test_that("all `NA` values works", {
  x <- c(NA_character_, NA_character_)
  expect_identical(vec_order(x), order(x))
})

test_that("can order empty string vs ASCII value 1 'Start of Header'", {
  x <- c("\001", "")
  expect_identical(vec_order(x), c(2L, 1L))
})

test_that("can be used in a data frame", {
  x <- c(1L, 4L, 1L, 3L, 1L)

  y <- c("zy", "zz", "abcd", "gfa", "zy")

  z <- c(1, 2, 5, 4, 3)

  # as second column
  df1 <- data.frame(x = x, y = y)

  # as first column
  df2 <- data.frame(y = y, x = x)

  # as second column with a third after it to break ties
  df3 <- data.frame(x = x, y = y, z = z)

  expect_identical(vec_order(df1), base_order(df1))
  expect_identical(vec_order(df2), base_order(df2))
  expect_identical(vec_order(df3), base_order(df3))
})

test_that("can have multiple character columns in a data frame", {
  df <- data.frame(
    x = c("def", "aba", "aba", "aba", "q"),
    y = c("zy", "zz", "zz", "gfa", "zy"),
    z = c("foo", "qux", "bar", "baz", "boo")
  )

  expect_identical(vec_order(df), base_order(df))
})

test_that("can order with varying encodings by converting to UTF-8", {
  encs <- encodings()
  x <- c(encs$utf8, encs$unknown, encs$latin1, "AC")

  expect_identical(vec_order(x), c(4L, 1L, 2L, 3L))
  expect_identical(vec_order(x, direction = "desc"), c(1L, 2L, 3L, 4L))
})

test_that("can order when in expected order", {
  x <- c("a", "a", "b", NA, NA)
  expect_identical(vec_order(x, direction = "asc", na_value = "largest"), 1:5)

  x <- c(NA, NA, "c", "c", "b")
  expect_identical(vec_order(x, direction = "desc", na_value = "largest"), 1:5)

  x <- c(NA, NA, "a", "a", "b")
  expect_identical(vec_order(x, direction = "asc", na_value = "smallest"), 1:5)

  x <- c("c", "c", "b", NA, NA)
  expect_identical(vec_order(x, direction = "desc", na_value = "smallest"), 1:5)
})

test_that("can order when in strictly opposite of expected order (no ties)", {
  x <- c(NA, "b", "a")
  expect_identical(vec_order(x, direction = "asc", na_value = "largest"), 3:1)

  x <- c("a", "b", NA)
  expect_identical(vec_order(x, direction = "desc", na_value = "largest"), 3:1)

  x <- c("b", "a", NA)
  expect_identical(vec_order(x, direction = "asc", na_value = "smallest"), 3:1)

  x <- c(NA, "a", "b")
  expect_identical(vec_order(x, direction = "desc", na_value = "smallest"), 3:1)
})

# ------------------------------------------------------------------------------
# vec_order(<character>) - radix

# Have to get the number of unique strings above the ORDER_INSERTION_BOUNDARY
# to trigger radix ordering.

test_that("can order character vectors", {
  x <- paste0("x", seq(1L, ORDER_INSERTION_BOUNDARY + 1L))
  expect_identical(vec_order(x), base_order(x))
})

test_that("ordering on ties is done stably", {
  x <- c(paste0("x", seq(1L, ORDER_INSERTION_BOUNDARY + 1L)), "x1")
  expect_identical(vec_order(x)[1:2], c(1L, length(x)))
})

test_that("`NA` order defaults to last", {
  x <- paste0("x", seq(1L, ORDER_INSERTION_BOUNDARY + 1L))
  x <- c(x, NA_character_, "y")
  expect_identical(vec_order(x)[length(x)], length(x) - 1L)
})

test_that("character, large: `NA` order can be first", {
  x <- paste0("x", seq(1L, ORDER_INSERTION_BOUNDARY + 1L))
  x <- c(x, NA_character_, "y")
  expect_identical(vec_order(x, na_value = "smallest")[[1L]], length(x) - 1L)
})

test_that("character, large: `direction` can be set to `desc`", {
  x <- paste0("x", seq(1L, ORDER_INSERTION_BOUNDARY + 1L))
  expect_identical(vec_order(x, direction = "desc"), base_order(x, decreasing = TRUE))
})

test_that("all combinations of `direction` and `na_value` work", {
  x <- paste0("x", seq(1L, ORDER_INSERTION_BOUNDARY + 1L))
  x <- c(x, NA_character_, "x", "aa", "x1")

  expect_identical(
    x[vec_order(x, na_value = "largest", direction = "asc")],
    x[base_order(x, na.last = TRUE, decreasing = FALSE)]
  )
  expect_identical(
    x[vec_order(x, na_value = "smallest", direction = "asc")],
    x[base_order(x, na.last = FALSE, decreasing = FALSE)]
  )
  expect_identical(
    x[vec_order(x, na_value = "largest", direction = "desc")],
    x[base_order(x, na.last = FALSE, decreasing = TRUE)]
  )
  expect_identical(
    x[vec_order(x, na_value = "smallest", direction = "desc")],
    x[base_order(x, na.last = TRUE, decreasing = TRUE)]
  )
})

# ------------------------------------------------------------------------------
# vec_order(<list>)

test_that("list elements are ordered by first appearance", {
  expect_identical(vec_order(list(1:2, "a", 1:2)), c(1L, 3L, 2L))
})

# ------------------------------------------------------------------------------
# vec_order(<data.frame>) - insertion

test_that("data frame with no columns and no rows returns integer()", {
  x <- data.frame()
  expect_identical(vec_order(x), integer())
})

test_that("data frame with no columns and some rows returns sequential rows", {
  x <- new_data_frame(n = 5L)
  expect_identical(vec_order(x), 1:5)
})

test_that("can order with multiple pre-sorted keys", {
  df <- data.frame(x = 1:2, y = 3:4)
  expect_identical(vec_order(df), 1:2)
})

test_that("first column has ordering presedence", {
  df <- data.frame(x = c(3L, 2L, 1L), y = c(1L, 2L, 3L))
  expect_identical(vec_order(df), 3:1)
})

test_that("secondary columns break ties - integer", {
  df <- data.frame(
    x = c(1L, 2L, 1L),
    y = c(3L, 2L, 1L)
  )
  expect_identical(vec_order(df), c(3L, 1L, 2L))
})

test_that("secondary columns break ties - double", {
  df <- data.frame(
    x = c(1, 2, 1),
    y = c(3, 2, 1)
  )
  expect_identical(vec_order(df), c(3L, 1L, 2L))
})

test_that("secondary columns break ties - logical", {
  df <- data.frame(
    x = c(FALSE, TRUE, FALSE),
    y = c(TRUE, TRUE, FALSE)
  )
  expect_identical(vec_order(df), c(3L, 1L, 2L))
})

test_that("orders correctly when first column is already ordered but second isn't", {
  df <- data.frame(
    x = c(1L, 1L, 2L, 2L),
    y = c(3L, 2L, 4L, 1L)
  )
  expect_identical(vec_order(df), c(2L, 1L, 4L, 3L))
})

test_that("orders correctly when first column is already ordered but second isn't - character", {
  df <- data.frame(
    x = c("a", "a", "b", "b"),
    y = c("c", "b", "d", "a")
  )

  expect_identical(vec_order(df), c(2L, 1L, 4L, 3L))
})

test_that("`direction` is recycled", {
  df <- data.frame(
    x = c(1L, 1L, 2L, 2L),
    y = c(3L, 2L, 4L, 1L)
  )
  expect_identical(vec_order(df, direction = "desc"), c(3L, 4L, 1L, 2L))
})

test_that("`na_value` is recycled", {
  df <- data.frame(
    x = c(1L, 1L, 2L, 2L, NA),
    y = c(3L, 2L, 4L, 1L, NA)
  )
  expect_identical(vec_order(df, na_value = "smallest"), c(5L, 2L, 1L, 4L, 3L))
})

test_that("`direction` can be a vector", {
  df <- data.frame(
    x = c(1L, 1L, 2L, 2L, NA, 1L),
    y = c(3L, 2L, 4L, 1L, 3L, NA)
  )
  expect_identical(vec_order(df, direction = c("desc", "asc")), c(5L, 4L, 3L, 2L, 1L, 6L))
})

test_that("`na_value` can be a vector", {
  df <- data.frame(
    x = c(1L, 1L, 2L, 2L, NA, NA),
    y = c(3L, 2L, 4L, 1L, NA, 2)
  )
  expect_identical(vec_order(df, na_value = c("smallest", "largest")), c(6L, 5L, 2L, 1L, 4L, 3L))
})

test_that("`na_value` and `direction` can both be vectors", {
  df <- data.frame(
    x = c(1L, 1L, 2L, 2L, NA, NA),
    y = c(3L, 2L, 4L, 1L, NA, 2)
  )

  expect_identical(
    vec_order(df, direction = c("desc", "asc"), na_value = c("smallest", "largest")),
    c(4:1, 6:5)
  )
})

# ------------------------------------------------------------------------------
# vec_order(<data.frame>) - counting

test_that("can order 2+ integer column chunks with counting sort", {
  half <- floor(ORDER_INSERTION_BOUNDARY / 2) + 1L
  quarter_low <- floor(half / 2)
  quarter_high <- ceiling(half / 2)

  df <- data.frame(
    x = 1L,
    y = c(rep(2L, quarter_low), rep(1L, quarter_high), rep(3L, half))
  )

  expect_identical(vec_order(df), base_order(df))
})

# ------------------------------------------------------------------------------
# vec_order(<data.frame>) - radix

test_that("can order 2+ integer column chunks with radix sort", {
  half <- floor(ORDER_INSERTION_BOUNDARY / 2) + 1L
  quarter_low <- floor(half / 2)
  quarter_high <- ceiling(half / 2)

  df <- data.frame(
    x = 1L,
    y = c(rep(2L, quarter_low), rep(1L, quarter_high), rep(3L, half), INT_ORDER_COUNTING_RANGE_BOUNDARY + 1L)
  )

  expect_identical(vec_order(df), base_order(df))
})

test_that("can order 2+ double column chunks with radix sort", {
  half <- floor(ORDER_INSERTION_BOUNDARY / 2) + 1L
  quarter_low <- floor(half / 2)
  quarter_high <- ceiling(half / 2)

  df <- data.frame(
    x = 1,
    y = c(rep(2, quarter_low), rep(1, quarter_high), rep(3, half), INT_ORDER_COUNTING_RANGE_BOUNDARY + 1)
  )

  expect_identical(vec_order(df), base_order(df))
})

# ------------------------------------------------------------------------------
# vec_order() - chr_transform

test_that("`chr_transform` transforms string input", {
  x <- c("b", "a", "A")
  expect_identical(vec_order(x, chr_transform = tolower), c(2L, 3L, 1L))
  expect_identical(vec_order(x, chr_transform = ~tolower(.x)), c(2L, 3L, 1L))
})

test_that("`chr_transform` works with data frame columns and is applied to all string columns", {
  df <- data_frame(x = c(1, 1, 1), y = c("B", "a", "a"), z = c("a", "D", "c"))
  expect_identical(vec_order(df, chr_transform = tolower), c(3L, 2L, 1L))
})

test_that("`chr_transform` is validated", {
  expect_error(vec_order("x", chr_transform = 1), "Can't convert `chr_transform` to a function")
  expect_error(vec_order("x", chr_transform = ~c("y", "z")), "1, not 2")
  expect_error(vec_order("x", chr_transform = ~1), "character vector")
  expect_error(vec_order("x", chr_transform = function() {"y"}))
})

test_that("`chr_transform` can return bytes-encoded strings (like `stringi::stri_sort_key()`)", {
  x <- c("A", "a", "b", "B")

  # Mimic stringi::stri_sort_key(x, locale = "en")
  sort_key <- function(x) {
    # dput(lapply(stringi::stri_sort_key(x, locale = "en"), charToRaw))
    out <- list(
      as.raw(c(0x2a, 0x01, 0x05, 0x01, 0xdc)),
      as.raw(c(0x2a, 0x01, 0x05, 0x01, 0x05)),
      as.raw(c(0x2c, 0x01, 0x05, 0x01, 0x05)),
      as.raw(c(0x2c, 0x01, 0x05, 0x01, 0xdc))
    )

    out <- vapply(out, FUN.VALUE = character(1), function(x) {
      # Uses native encoding
      x <- rawToChar(x)
      Encoding(x) <- "bytes"
      x
    })

    out
  }

  expect_identical(
    vec_order(x, chr_transform = sort_key),
    c(2L, 1L, 3L, 4L)
  )
})

# ------------------------------------------------------------------------------
# vec_order() - error checking

test_that("`na_value` is checked", {
  expect_error(vec_order(1L, na_value = "x"), "\"largest\" or \"smallest\"")
  expect_error(vec_order(1L, na_value = c(TRUE, TRUE)), "must be a character vector")
  expect_error(vec_order(1L, na_value = NA_character_), "can't be missing")
})

test_that("`direction` is checked", {
  expect_error(vec_order(1L, direction = "x"), "must contain only")
  expect_error(vec_order(1L, direction = c("asc", "asc")), "single value")
  expect_error(vec_order(1L, direction = NA_character_), "can't be missing")
  expect_error(vec_order(data.frame(x = 1), direction = c("asc", "asc")), "length 1 or")
})

test_that("`x` is checked", {
  expect_error(vec_order(foobar()), class = "vctrs_error_scalar_type")
})

# ------------------------------------------------------------------------------
# vec_order() - groups

test_that("groups can be reallocated if we exceed the max group data size", {
  set.seed(123)

  # The first column has all unique groups so 1 more than the default group
  # data size is needed and will be reallocated on the fly
  df <- data.frame(
    x = sample(GROUP_DATA_SIZE_DEFAULT + 1L, replace = TRUE),
    y = sample(GROUP_DATA_SIZE_DEFAULT + 1L, replace = TRUE),
    z = sample(GROUP_DATA_SIZE_DEFAULT + 1L, replace = TRUE)
  )

  expect_identical(vec_order(df), base_order(df))
})

# ------------------------------------------------------------------------------
# vec_order() - comparison proxy

test_that("ordering works with rcrd types", {
  x <- tuple(c(1, 2, 1), c(3, 2, 1))
  expect_identical(vec_order(x), c(3L, 1L, 2L))
})

test_that("data frame comparison proxies don't allow vector `direction` or `na_value`", {
  x <- tuple(c(1, 2, 1), c(3, 2, 1))
  expect_error(vec_order(x, direction = c("desc", "asc")), "single value")
  expect_error(vec_order(x, na_value = c("largest", "smallest")), "single value")
})

test_that("ordering works with df-cols", {
  df_col <- new_data_frame(list(y = c(2, 1, 2), z = c(3, 3, 3)))
  df <- new_data_frame(list(x = c(1, 1, 1), y = df_col))

  expect_identical(vec_order(df), c(2L, 1L, 3L))

  # Can only supply a max of 2 `direction` or `na_value` values which get internally
  # expanded to 3 to match the flattened df proxy
  expect_identical(vec_order(df, direction = c("asc", "desc")), c(1L, 3L, 2L))

  expect_error(vec_order(df, direction = c("desc", "desc", "asc")), "or length equal to")
})

test_that("ordering works with df-cols with 0 cols", {
  df_col <- new_data_frame(list(), n = 3L)
  df <- new_data_frame(list(x = c(1, 3, 1), y = df_col, z = c(2, 1, 1)))

  expect_identical(vec_order(df), c(3L, 1L, 2L))

  # Can supply 3 `direction` values even though the 0-col df-col gets dropped
  expect_identical(vec_order(df, direction = c("asc", "desc", "desc")), c(1L, 3L, 2L))

  expect_error(vec_order(df, direction = c("desc", "asc")), "or length equal to")
})

test_that("ordering works with rcrd cols", {
  y <- tuple(c(1, 2, 1), c(3, 2, 1))
  df <- new_data_frame(list(z = c(1, 1, 1), y = y))

  expect_identical(vec_order(df), c(3L, 1L, 2L))

  # Can only supply a max of 2 `direction` values which get internally
  # expanded to 3 to match the flattened df proxy
  expect_identical(vec_order(df, direction = c("asc", "desc")), c(2L, 1L, 3L))

  expect_error(vec_order(df, direction = c("desc", "desc", "asc")), "or length equal to")
})

# ------------------------------------------------------------------------------
# `vec_order()` - Pre-existing tests

test_that("can request NAs sorted first", {
  expect_equal(vec_order(c(1, NA), direction = "asc", na_value = "largest"), 1:2)
  expect_equal(vec_order(c(1, NA), direction = "desc", na_value = "largest"), 2:1)

  expect_equal(vec_order(c(1, NA), direction = "asc", na_value = "smallest"), 2:1)
  expect_equal(vec_order(c(1, NA), direction = "desc", na_value = "smallest"), 1:2)
})

test_that("can sort data frames", {
  df <- data.frame(x = c(1, 2, 1), y = c(1, 2, 2))

  out1 <- vec_sort(df)
  expect_equal(out1, data.frame(x = c(1, 1, 2), y = c(1, 2, 2)))

  out2 <- vec_sort(df, direction = "desc")
  expect_equal(out2, data.frame(x = c(2, 1, 1), y = c(2, 2, 1)))
})

test_that("can sort empty data frames (#356)", {
  df1 <- data.frame()
  expect_equal(vec_sort(df1), df1)

  df2 <- data.frame(x = numeric(), y = integer())
  expect_equal(vec_sort(df2), df2)
})

test_that("can order tibbles that contain non-comparable objects", {
  expect_equal(vec_order(data_frame(x = list(10, 2, 1))), 1:3)
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
  df <- new_data_frame(n = 2L)

  df$x <- new_data_frame(list(y = matrix(1:2, 2)))
  expect_identical(vec_order(df), 1:2)

  df$x <- tibble::tibble(y = matrix(1:2, 2))
  expect_identical(vec_order(df), 1:2)
})

# ------------------------------------------------------------------------------
# vec_order_locs()

test_that("`vec_order_locs()` is working", {
  x <- c(1, 3, 1, 5, 2, 5, 1)

  expect <- new_data_frame(
    list(
      key = c(1, 2, 3, 5),
      loc = list(c(1L, 3L, 7L), 5L, 2L, c(4L, 6L))
    )
  )

  expect_identical(vec_order_locs(x), expect)
})

test_that("`chr_transform` can result in keys being seen as identical", {
  x <- c("b", "A", "a")
  y <- c("b", "a", "A")

  x_expect <- data_frame(key = c("A", "b"), loc = list(c(2L, 3L), 1L))
  y_expect <- data_frame(key = c("a", "b"), loc = list(c(2L, 3L), 1L))

  expect_identical(vec_order_locs(x, chr_transform = tolower), x_expect)
  expect_identical(vec_order_locs(y, chr_transform = tolower), y_expect)
})

# ------------------------------------------------------------------------------
# `vec_order_info(chr_ordered = FALSE)`

test_that("can order character vectors in appearance order", {
  x <- c("b", "a", "B", "B", "a")
  info <- vec_order_info(x, chr_ordered = FALSE)

  expect_identical(info[[1]], c(1L, 2L, 5L, 3L, 4L))
  expect_identical(info[[2]], c(1L, 2L, 2L))
})

test_that("using appearance order means `direction` has no effect", {
  x <- c("b", "a", "B", "B", "a")

  info1 <- vec_order_info(x, direction = "asc", chr_ordered = FALSE)
  info2 <- vec_order_info(x, direction = "desc", chr_ordered = FALSE)

  expect_identical(info1[[1]], info2[[1]])
  expect_identical(info1[[2]], info2[[2]])
})

test_that("appearance order works with NA - `na_value` has no effect", {
  x <- c(NA, "foo", NA, "bar")
  info <- vec_order_info(x, chr_ordered = FALSE)

  expect_identical(info[[1]], c(1L, 3L, 2L, 4L))
  expect_identical(info[[2]], c(2L, 1L, 1L))
})

test_that("appearance order can be mixed with regular ordering", {
  x <- c("b", "a", "B", "B", "a", "a")
  y <- c(1, 2, 3, 4, 1, 2)
  df <- data_frame(x = x, y = y)

  # `y` breaks ties
  info <- vec_order_info(df, chr_ordered = FALSE)

  expect_identical(info[[1]], c(1L, 5L, 2L, 6L, 3L, 4L))
  expect_identical(info[[2]], c(1L, 1L, 2L, 1L, 1L))
})

# ------------------------------------------------------------------------------
# vec_sort

test_that("can sort data frames", {
  df <- data.frame(x = c(1, 2, 1), y = c(1, 2, 2))

  out1 <- vec_sort(df)
  expect_equal(out1, data.frame(x = c(1, 1, 2), y = c(1, 2, 2)))

  out2 <- vec_sort(df, direction = "desc")
  expect_equal(out2, data.frame(x = c(2, 1, 1), y = c(2, 2, 1)))
})

test_that("can sort empty data frames (#356)", {
  df1 <- data.frame()
  expect_equal(vec_sort(df1), df1)

  df2 <- data.frame(x = numeric(), y = integer())
  expect_equal(vec_sort(df2), df2)
})

# ------------------------------------------------------------------------------
# vec_order_base

test_that("can request NAs sorted first", {
  expect_equal(vec_order_base(c(1, NA), "asc", "largest"), 1:2)
  expect_equal(vec_order_base(c(1, NA), "desc", "largest"), 2:1)

  expect_equal(vec_order_base(c(1, NA), "asc", "smallest"), 2:1)
  expect_equal(vec_order_base(c(1, NA), "desc", "smallest"), 1:2)
})

test_that("can order complex vectors", {
  x <- complex(real = c(1, 2, 2, 3, 3), imaginary = c(5, 4, 3, 2, NA))

  expect_equal(vec_order_base(x, direction = "asc", na_value = "largest"), c(1, 3, 2, 4, 5))
  expect_equal(vec_order_base(x, direction = "desc", na_value = "largest"), rev(c(1, 3, 2, 4, 5)))
  expect_equal(vec_order_base(x, direction = "asc", na_value = "smallest"), c(5, 1, 3, 2, 4))
  expect_equal(vec_order_base(x, direction = "desc", na_value = "smallest"), rev(c(5, 1, 3, 2, 4)))
})

test_that("can order tibbles that contain non-comparable objects", {
  expect_equal(vec_order_base(data_frame(x = list(10, 2, 1))), 1:3)
})

test_that("can order matrices and arrays (#306)", {
  x <- matrix(c(1, 1, 1, 1, 2, 1), ncol = 2)
  expect_identical(vec_order_base(x), c(1L, 3L, 2L))

  x <- array(1:8, c(2, 2, 2))
  x[2] <- 1
  x[3] <- 5
  expect_identical(vec_order_base(x), 2:1)
})

test_that("can order empty data frames (#356)", {
  df1 <- data.frame()
  expect_equal(vec_order_base(df1), integer())

  df2 <- data.frame(x = numeric(), y = integer())
  expect_equal(vec_order_base(df2), integer())
})

test_that("can order data frames with data frame columns (#527)", {
  expect_equal(
    vec_order_base(iris),
    vec_order_base(data_frame(iris = iris))
  )
})

test_that("can order data frames (and subclasses) with matrix columns", {
  df <- new_data_frame(n = 2L)

  df$x <- new_data_frame(list(y = matrix(1:2, 2)))
  expect_identical(vec_order_base(df), 1:2)

  df$x <- tibble::tibble(y = matrix(1:2, 2))
  expect_identical(vec_order_base(df), 1:2)
})

test_that("classed proxies do not affect performance (tidyverse/dplyr#5423)", {
  skip_on_cran()
  x <- glue::glue("{1:10000}")
  expect_time_lt(vec_order_base(x), 0.2)
})

test_that("can order data frames that don't allow removing the column names (#1298)", {
  skip_if_not_installed("withr")

  local_methods(
    `names<-.vctrs_foobar` = function(x, value) {
      if (is.null(value)) {
        abort("Cannot remove names.")
      }
      NextMethod()
    }
  )

  df <- foobar(data.frame(x = 1, y = 2))

  expect_silent(expect_identical(vec_order_base(df), 1L))
})
