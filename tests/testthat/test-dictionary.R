
# counting ----------------------------------------------------------------

test_that("vec_count counts number observations", {
  x <- vec_count(rep(1:3, 1:3), sort = "key")
  expect_equal(x, data.frame(key = 1:3, count = 1:3))
})

test_that("vec_count works with matrices", {
  x <- matrix(c(1, 1, 1, 2, 2, 1), c(3, 2))

  out <- vec_count(x)
  exp <- data_frame(key = c(NA, NA), count = int(2L, 1L))
  exp$key <- vec_slice(x, c(1, 3))

  expect_identical(out, exp)
})

test_that("vec_count works with arrays", {
  x <- array(c(rep(1, 3), rep(2, 3)), dim = c(3, 2, 1))
  expect <- data.frame(key = NA, count = 3)
  expect$key <- vec_slice(x, 1L)
  expect_equal(vec_count(x), expect)
})

test_that("vec_count works for zero-length input", {
  x <- vec_count(integer(), sort = "none")
  expect_equal(x, data.frame(key = integer(), count = integer()))
})

test_that("vec_count works with different encodings", {
  x <- vec_count(encodings())
  expect_equal(x, new_data_frame(list(key = encodings()[1], count = 3L)))
})

test_that("vec_count recursively takes the equality proxy", {
  local_comparable_tuple()

  x <- tuple(c(1, 1, 2), 1:3)
  df <- data_frame(x = x)
  expect <- data_frame(key = vec_slice(df, c(1, 3)), count = c(2L, 1L))

  expect_equal(vec_count(df), expect)
})

# duplicates and uniques --------------------------------------------------

test_that("vec_duplicated reports on duplicates regardless of position", {
  x <- c(1, 1, 2, 3, 4, 4)
  expect_equal(vec_duplicate_detect(x), c(TRUE, TRUE, FALSE, FALSE, TRUE, TRUE))
})

test_that("vec_duplicate_any returns single TRUE/FALSE", {
  expect_false(vec_duplicate_any(c(1:10)))
  expect_true(vec_duplicate_any(c(1:10, 1)))
})

test_that("vec_duplicate_any returns FALSE for size 0 input", {
  expect_false(vec_duplicate_any(integer()))
})

test_that("vec_all_duplicate returns single TRUE/FALSE", {
  expect_true(vec_all_duplicate(c(1, 1, 2, 2)))
  expect_false(vec_all_duplicate(c(1, 1, 2, 3)))
})

test_that("vec_all_duplicate returns TRUE for size 0 input", {
  expect_true(vec_all_duplicate(integer()))
})

test_that("vec_duplicate_id gives position of first found", {
  x <- c(1, 2, 3, 1, 4)
  expect_equal(vec_duplicate_id(x), c(1, 2, 3, 1, 5))
})

test_that("vec_unique matches unique", {
  x <- sample(100, 1000, replace = TRUE)
  expect_equal(vec_unique(x), unique(x))
  expect_equal(vec_unique(c("x", "x")), "x")
})

test_that("vec_unique matches unique for matrices", {
  x <- matrix(c(1, 1, 2, 2), ncol = 2)
  expect_equal(vec_unique(x), unique(x))
})

test_that("vec_unique_count matches length + unique", {
  x <- sample(100, 1000, replace = TRUE)
  expect_equal(vec_unique_count(x), length(unique(x)))
})

test_that("also works for data frames", {
  df <- data.frame(x = 1:3, y = letters[3:1], stringsAsFactors = FALSE)
  idx <- c(1L, 1L, 1L, 2L, 2L, 3L)
  df2 <- df[idx, , drop = FALSE]
  rownames(df2) <- NULL

  expect_equal(vec_duplicate_detect(df2), vec_duplicate_detect(idx))
  expect_equal(vec_unique(df2), vec_slice(df, vec_unique(idx)))

  count <- vec_count(df2, sort = "key")
  expect_equal(count$key, df)
  expect_equal(count$count, vec_count(idx)$count)

  exp <- tibble(x = c(1, 1, 2), y = c(1, 2, 3))
  expect_identical(vec_unique(vec_slice(exp, c(1, 1, 2, 3))), exp)
})

test_that("vec_unique() handles matrices (#327)", {
  x <- matrix(c(1, 2, 3, 4), c(2, 2))
  y <- matrix(c(1, 2, 3, 5), c(2, 2))
  expect_identical(vec_unique(list(x, x)), list(x))
  expect_identical(vec_unique(list(x, y)), list(x, y))

  x <- matrix(c(1, 2, 1, 1, 2, 1), nrow = 3)
  expect_identical(vec_unique(x), vec_slice(x, 1:2))
})

test_that("vec_unique() works with 1D arrays", {
  # 1D arrays are dispatched to `as.data.frame.vector()` which
  # currently does not strip dimensions. This caused an infinite
  # recursion.
  expect_identical(vec_unique(array(1:2)), array(1:2))

  x <- new_vctr(c(1, 1, 1, 2, 1, 2), dim = c(3, 2))
  expect_identical(vec_unique(x), new_vctr(c(1, 1, 2, 1), dim = c(2, 2)))
})

test_that("unique functions take the equality proxy (#375)", {
  local_comparable_tuple()
  x <- tuple(c(1, 2, 1), 1:3)

  expect_true(vec_in(tuple(2, 100), x))
  expect_identical(vec_match(tuple(2, 100), x), 2L)
})

test_that("unique functions take the equality proxy recursively", {
  local_comparable_tuple()

  x <- tuple(c(1, 1, 2), 1:3)
  df <- data_frame(x = x)

  expect_equal(vec_unique(df), vec_slice(df, c(1, 3)))
  expect_equal(vec_unique_count(df), 2L)
  expect_equal(vec_unique_loc(df), c(1, 3))
})

test_that("duplicate functions take the equality proxy recursively", {
  local_comparable_tuple()

  x <- tuple(c(1, 1, 2), 1:3)
  df <- data_frame(x = x)

  expect_equal(vec_duplicate_any(df), TRUE)
  expect_equal(vec_all_duplicate(df), FALSE)
  expect_equal(vec_duplicate_detect(df), c(TRUE, TRUE, FALSE))
  expect_equal(vec_duplicate_id(df), c(1, 1, 3))
})

test_that("unique functions treat positive and negative 0 as equivalent (#637)", {
  expect_equal(vec_unique(c(0, -0)), 0)
  expect_equal(vec_unique_count(c(0, -0)), 1)
  expect_equal(vec_unique_loc(c(0, -0)), 1)
})

test_that("unique functions work with different encodings", {
  encs <- encodings()

  expect_equal(vec_unique(encs), encs[1])
  expect_equal(vec_unique_count(encs), 1L)
  expect_equal(vec_unique_loc(encs), 1L)
})

test_that("unique functions can handle scalar types in lists", {
  x <- list(x = a ~ b, y = a ~ b, z = a ~ c)
  expect_equal(vec_unique(x), vec_slice(x, c(1, 3)))

  x <- list(x = call("x"), y = call("y"), z = call("x"))
  expect_equal(vec_unique(x), vec_slice(x, c(1, 2)))
})

test_that("duplicate functions works with different encodings", {
  encs <- encodings()

  expect_equal(vec_duplicate_id(encs), rep(1, 3))
  expect_equal(vec_duplicate_detect(encs), rep(TRUE, 3))
  expect_equal(vec_duplicate_any(encs), TRUE)
  expect_equal(vec_all_duplicate(encs), TRUE)
})

test_that("vec_unique() returns differently encoded strings in the order they appear", {
  encs <- encodings()
  x <- c(encs$unknown, encs$utf8)
  y <- c(encs$utf8, encs$unknown)

  expect_equal_encoding(vec_unique(x), encs$unknown)
  expect_equal_encoding(vec_unique(y), encs$utf8)
})

test_that("vec_unique() works on lists containing expressions", {
  x <- list(expression(x), expression(y), expression(x))
  expect_equal(vec_unique(x), x[1:2])
})

test_that("vec_unique() works with glm objects (#643)", {
  # class(model$family$initialize) == "expression"
  model <- glm(mpg ~ wt, data = mtcars)
  expect_equal(vec_unique(list(model, model)), list(model))
})

test_that("can take the unique locations of dfs with list-cols", {
  df <- tibble(x = list(1, 2, 1, 3), y = list(1, 2, 1, 3))
  expect_identical(vec_unique_loc(df), c(1L, 2L, 4L))
})


# matching ----------------------------------------------------------------

test_that("vec_match() matches match()", {
  n <- c(1:3, NA)
  h <- c(4, 2, 1, NA)
  expect_equal(vec_match(n, h), match(n, h))

  expect_equal(vec_match(1.5, c(2, 1.5, NA)), match(1.5, c(2, 1.5, NA)))
  expect_equal(vec_match("x", "x"), match("x", "x"))
})

test_that("vec_match() and vec_in() check types", {
  verify_errors({
    df1 <- data_frame(x = data_frame(foo = 1))
    df2 <- data_frame(x = data_frame(foo = ""))
    expect_error(vec_match(df1, df2), class = "vctrs_error_incompatible_type")
    expect_error(vec_match(df1, df2, needles_arg = "n", haystack_arg = "h"), class = "vctrs_error_incompatible_type")
    expect_error(vec_in(df1, df2), class = "vctrs_error_incompatible_type")
    expect_error(vec_in(df1, df2, needles_arg = "n", haystack_arg = "h"), class = "vctrs_error_incompatible_type")
  })
})

test_that("vec_in() matches %in%", {
  n <- c(1:3, NA)
  h <- c(4, 2, 1, NA)

  expect_equal(vec_in(n, h), n %in% h)
})

test_that("can opt out of NA matching", {
  n <- c(1, NA)
  h <- c(1:3, NA)

  expect_equal(vec_in(n, h, na_equal = FALSE), c(TRUE, NA))
})

test_that("vec_match works with empty data frame", {
  out <- vec_match(
    new_data_frame(n = 3L),
    new_data_frame(n = 0L)
  )
  expect_equal(out, vec_init(integer(), 3))
})

test_that("matching functions take the equality proxy (#375)", {
  local_comparable_tuple()
  x <- tuple(c(1, 2, 1), 1:3)

  expect_identical(vec_unique_loc(x), 1:2)
  expect_identical(unique(x), tuple(c(1, 2), 1:2))

  expect_true(vec_duplicate_any(x))
  expect_identical(vec_duplicate_id(x), c(1L, 2L, 1L))
  expect_identical(vec_unique_count(x), 2L)

  expect_identical(vec_duplicate_detect(x), c(TRUE, FALSE, TRUE))
})

test_that("can take the unique loc of 1d arrays (#461)", {
  x <- array(c(1, 1, 2, 2, 3))
  y <- array(c(1, 1, 2, 2, 3), dimnames = list(NULL))
  expect_identical(vctrs::vec_unique_loc(x), int(1, 3, 5))
  expect_identical(vctrs::vec_unique_loc(y), int(1, 3, 5))

  z <- array(c(1, 1, 2, 2, 3, 4), c(3, 2))
  expect_silent(expect_identical(vctrs::vec_unique_loc(y), int(1, 3, 5)))
})

test_that("matching functions work with different encodings", {
  encs <- encodings()

  expect_equal(vec_match(encs, encs[1]), rep(1, 3))
  expect_equal(vec_in(encs, encs[1]), rep(TRUE, 3))
})

test_that("matching functions take the equality proxy recursively", {
  local_comparable_tuple()

  x <- tuple(c(1, 2), 1:2)
  df <- data_frame(x = x)

  y <- tuple(c(2, 3), c(3, 3))
  df2 <- data_frame(x = y)

  expect_equal(vec_match(df, df2), c(NA, 1))
  expect_equal(vec_in(df, df2), c(FALSE, TRUE))
})

test_that("can propagate missing values while matching", {
  exp <- c(NA, 3L, NA, 1L)
  expect_identical(vec_match(lgl(NA, TRUE, NA, FALSE), lgl(FALSE, NA, TRUE), na_equal = FALSE), exp)
  expect_identical(vec_match(int(NA, 1L, NA, 2L), int(2L, NA, 1L), na_equal = FALSE), exp)
  expect_identical(vec_match(dbl(NA, 1, NA, 2), dbl(2, NA, 1), na_equal = FALSE), exp)
  expect_identical(vec_match(cpl(NA, 1, NA, 2), cpl(2, NA, 1), na_equal = FALSE), exp)
  expect_identical(vec_match(chr(NA, "1", NA, "2"), chr("2", NA, "1"), na_equal = FALSE), exp)
  expect_identical(vec_match(list(NULL, 1, NULL, 2), list(2, NULL, 1), na_equal = FALSE), exp)

  # No missing values for raw vectors
  expect_identical(vec_match(bytes(0, 1, 0, 2), bytes(2, 0, 1), na_equal = FALSE), c(2L, 3L, 2L, 1L))
})

test_that("missing values are propagated across columns", {
  for (na_value in list(NA, na_int, na_dbl, na_cpl, na_chr, list(NULL))) {
    df <- data_frame(x = 1, y = data_frame(foo = 2, bar = na_value), z = 3)
    expect_identical(vec_match(df, df), 1L)
    expect_identical(vec_match(df, df, na_equal = FALSE), na_int)
  }
})

test_that("can't supply NA as `na_equal`", {
  expect_error(vec_match(NA, NA, na_equal = NA), "single `TRUE` or `FALSE`")
})

test_that("dictionary tools have informative errors", {
  verify_output(test_path("error", "test-dictionary.txt"), {
    "# vec_match() and vec_in() check types"
    df1 <- data_frame(x = data_frame(foo = 1))
    df2 <- data_frame(x = data_frame(foo = ""))
    vec_match(df1, df2)
    vec_match(df1, df2, needles_arg = "n", haystack_arg = "h")
    vec_in(df1, df2)
    vec_in(df1, df2, needles_arg = "n", haystack_arg = "h")
  })
})

test_that("vec_match() and vec_in() silently fall back to base data frame", {
  expect_silent(expect_identical(
    vec_match(foobar(mtcars), foobar(tibble::as_tibble(mtcars))),
    1:32
  ))
  expect_silent(expect_identical(
    vec_in(foobar(mtcars), foobar(tibble::as_tibble(mtcars))),
    rep(TRUE, 32)
  ))
})
