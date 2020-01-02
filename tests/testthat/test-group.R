context("test-group")

# group id ----------------------------------------------------------------

test_that("vec_group_id detects groups in order of appearance", {
  x <- c(2, 4, 2, 1, 4)
  expect <- structure(c(1L, 2L, 1L, 3L, 2L), n = 3L)
  expect_equal(vec_group_id(x), expect)
})

test_that("vec_group_id works for size 0 input", {
  expect <- structure(integer(), n = 0L)
  expect_equal(vec_group_id(NULL), expect)
  expect_equal(vec_group_id(numeric()), expect)
})

test_that("vec_group_id works on base S3 objects", {
  x <- factor(c("x", "y", "x"))
  expect <- structure(c(1L, 2L, 1L), n = 2L)
  expect_equal(vec_group_id(x), expect)

  x <- new_date(c(0, 1, 0))
  expect <- structure(c(1L, 2L, 1L), n = 2L)
  expect_equal(vec_group_id(x), expect)
})

test_that("vec_group_id works row wise on data frames", {
  df <- data.frame(x = c(1, 2, 1, 1), y = c(2, 3, 2, 3))
  expect <- structure(c(1L, 2L, 1L, 3L), n = 3L)
  expect_equal(vec_group_id(df), expect)
})

test_that("vec_group_id works row wise on arrays", {
  x <- array(c(1, 1, 1, 2, 4, 2), c(3, 2))
  expect <- structure(c(1L, 2L, 1L), n = 2L)
  expect_equal(vec_group_id(x), expect)
})

test_that("vec_group_id works with different encodings", {
  expect <- structure(c(1L, 1L, 1L), n = 1L)
  expect_equal(vec_group_id(encodings()), expect)
})

test_that("vec_group_id takes the equality proxy", {
  local_comparable_tuple()
  x <- tuple(c(1, 2, 1, 1), c(1, 1, 1, 2))
  # Compares on only the first field
  expect <- structure(c(1L, 2L, 1L, 1L), n = 2L)
  expect_equal(vec_group_id(x), expect)
})

test_that("vec_group_id takes the equality proxy recursively", {
  local_comparable_tuple()

  x <- tuple(c(1, 2, 1, 1), 1:4)
  df <- data_frame(x = x)

  expect <- structure(c(1L, 2L, 1L, 1L), n = 2L)

  expect_equal(vec_group_id(df), expect)
})

# group rle ---------------------------------------------------------------

test_that("vec_group_rle returns a `vctrs_group_rle` object", {
  expect_is(vec_group_rle(1), "vctrs_group_rle")
})

test_that("vec_group_rle works with size 0 input", {
  expect <- new_group_rle(integer(), integer(), 0L)
  expect_equal(vec_group_rle(integer()), expect)
  expect_equal(vec_group_rle(NULL), expect)
})

test_that("vec_group_rle detects groups in order of appearance", {
  x <- c(2, 2, 3, 1, 1)
  expect <- new_group_rle(1:3, c(2L, 1L, 2L), 3L)
  expect_equal(vec_group_rle(x), expect)
})

test_that("vec_group_rle can refer to groups it has already seen", {
  x <- c(2, 3, 2)
  expect <- new_group_rle(c(1L, 2L, 1L), rep(1L, 3), 2L)
  expect_equal(vec_group_rle(x), expect)
})

test_that("vec_group_rle works on base S3 objects", {
  expect <- new_group_rle(c(1L, 2L, 1L, 3L), c(1L, 2L, 1L, 1L), 3L)

  x <- factor(c("x", "y", "y", "x", "z"))
  expect_equal(vec_group_rle(x), expect)

  x <- new_date(c(0, 1, 1, 0, 2))
  expect_equal(vec_group_rle(x), expect)
})

test_that("vec_group_rle takes the equality proxy", {
  local_comparable_tuple()
  x <- tuple(c(1, 2, 1, 1), c(1, 1, 1, 2))
  # Compares on only the first field
  expect <- new_group_rle(c(1L, 2L, 1L), c(1L, 1L, 2L), 2L)
  expect_equal(vec_group_rle(x), expect)
})

test_that("vec_group_rle works row wise on data frames", {
  df <- data.frame(x = c(1, 1, 2, 1), y = c(2, 2, 3, 2))
  expect <- new_group_rle(c(1L, 2L, 1L), c(2L, 1L, 1L), 2L)
  expect_equal(vec_group_rle(df), expect)
})

test_that("vec_group_rle takes the equality proxy recursively", {
  local_comparable_tuple()

  x <- tuple(c(1, 2, 1, 1), 1:4)
  df <- data_frame(x = x)

  expect <- new_group_rle(c(1L, 2L, 1L), c(1L, 1L, 2L), 2L)

  expect_equal(vec_group_rle(df), expect)
})

test_that("can access fields", {
  x <- vec_group_rle(c(1, 1, 2))
  expect_equal(fields(x), c("group", "length"))
  expect_identical(field(x, "group"), c(1L, 2L))
  expect_identical(field(x, "length"), c(2L, 1L))
})

test_that("can access number of groups", {
  x <- vec_group_rle(c(1, 1, 2))
  expect_identical(attr(x, "n"), 2L)
})

test_that("print method is useful", {
  x <- new_group_rle(c(1L, 2L, 1L), c(3L, 2L, 1L), 2L)
  expect_known_output(print(x), file = test_path("test-type-group-rle.txt"))
})

# group pos --------------------------------------------------------------

test_that("can locate unique groups of an empty vector", {
  out <- vec_group_pos(integer())

  expect_s3_class(out, "data.frame")
  expect_equal(out$key, integer())
  expect_equal(out$pos, list())
})

test_that("can locate unique groups of a data frame", {
  df <- data_frame(x = c(1, 1, 1, 2, 2), y = c("a", "a", "b", "a", "b"))
  out <- vec_group_pos(df)

  expect_equal(nrow(out), 4L)
  expect_equal(out$key, vec_unique(df))
})

test_that("can locate unique groups of a data frame with a list column", {
  df <- data_frame(x = list(1:2, 1:2, "a", 5.5, "a"))
  out <- vec_group_pos(df)

  expect_equal(nrow(out), 3L)
  expect_equal(out$key, vec_unique(df))
})

test_that("`x` must be a vector", {
  expect_error(vec_group_pos(environment()), class = "vctrs_error_scalar_type")
})

test_that("`key` column retains full type information", {
  x <- factor(letters[c(1, 2, 1)], levels = letters[1:3])
  out <- vec_group_pos(x)

  expect_equal(levels(out$key), levels(x))
})

test_that("vec_group_pos takes the equality proxy", {
  local_comparable_tuple()
  x <- tuple(c(1, 2, 1), 1:3)
  expect_equal(vec_group_pos(x)$key, x[1:2])
  expect_equal(vec_group_pos(x)$pos, list(c(1L, 3L), 2L))

  x <- as.POSIXlt(new_datetime(c(1, 2, 1)))
  expect_equal(vec_group_pos(x)$key, x[1:2])
  expect_equal(vec_group_pos(x)$pos, list(c(1L, 3L), 2L))
})

test_that("vec_group_pos takes the equality proxy recursively", {
  local_comparable_tuple()

  x <- tuple(c(1, 2, 1, 1), 1:4)
  df <- data_frame(x = x)

  expect <- data_frame(key = vec_slice(df, c(1, 2)), pos = list(c(1L, 3L, 4L), 2L))

  expect_equal(vec_group_pos(df), expect)
})

test_that("vec_group_pos works with different encodings", {
  encs <- encodings()
  expect_identical(nrow(vec_group_pos(encs)), 1L)
})

test_that("vec_group_pos_as_id() expands list of indices", {
  expect_identical(
    vec_group_pos_as_id(list(int(1, 3), int(2, 5, 6), 4L)),
    int(1, 2, 1, 3, 2, 2)
  )
})

test_that("vec_group_id_as_pos() collapses to list of indices", {
  expect_identical(
    vec_group_id_as_pos(int(1, 3, 1, 2), 4),
    list(int(1, 3), int(4), int(2), int())
  )
})
