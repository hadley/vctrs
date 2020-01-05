test_that("vec_slice throws error with non-vector inputs", {
  expect_error(vec_slice(environment(), 1L), class = "vctrs_error_scalar_type")
})

test_that("vec_slice throws error with non-vector indexes", {
  expect_error(vec_slice(1:3, Sys.Date()), class = "vctrs_error_incompatible_cast")
  expect_error(vec_slice(1:3, matrix(TRUE, nrow = 1)), "must have one dimension")
})

test_that("can subset base vectors", {
  i <- 2:3
  expect_identical(vec_slice(lgl(1, 0, 1), i), lgl(0, 1))
  expect_identical(vec_slice(int(1, 2, 3), i), int(2, 3))
  expect_identical(vec_slice(dbl(1, 2, 3), i), dbl(2, 3))
  expect_identical(vec_slice(cpl(1, 2, 3), i), cpl(2, 3))
  expect_identical(vec_slice(chr("1", "2", "3"), i), chr("2", "3"))
  expect_identical(vec_slice(bytes(1, 2, 3), i), bytes(2, 3))
  expect_identical(vec_slice(list(1, 2, 3), i), list(2, 3))
})

test_that("can subset shaped base vectors", {
  i <- 2:3
  mat <- as.matrix
  expect_identical(vec_slice(mat(lgl(1, 0, 1)), i), mat(lgl(0, 1)))
  expect_identical(vec_slice(mat(int(1, 2, 3)), i), mat(int(2, 3)))
  expect_identical(vec_slice(mat(dbl(1, 2, 3)), i), mat(dbl(2, 3)))
  expect_identical(vec_slice(mat(cpl(1, 2, 3)), i), mat(cpl(2, 3)))
  expect_identical(vec_slice(mat(chr("1", "2", "3")), i), mat(chr("2", "3")))
  expect_identical(vec_slice(mat(bytes(1, 2, 3)), i), mat(bytes(2, 3)))
  expect_identical(vec_slice(mat(list(1, 2, 3)), i), mat(list(2, 3)))
})

test_that("can subset with missing indices", {
  for (i in list(int(2L, NA), lgl(FALSE, TRUE, NA))) {
    expect_identical(vec_slice(lgl(1, 0, 1), i), lgl(0, NA))
    expect_identical(vec_slice(int(1, 2, 3), i), int(2, NA))
    expect_identical(vec_slice(dbl(1, 2, 3), i), dbl(2, NA))
    expect_identical(vec_slice(cpl(1, 2, 3), i), cpl(2, NA))
    expect_identical(vec_slice(chr("1", "2", "3"), i), c("2", NA))
    expect_identical(vec_slice(bytes(1, 2, 3), i), bytes(2, 0))
    expect_identical(vec_slice(list(1, 2, 3), i), list(2, NULL))
  }
})

test_that("can subset with a recycled NA", {
  expect_identical(vec_slice(1:3, NA), int(NA, NA, NA))
  expect_identical(vec_slice(new_vctr(1:3), NA), new_vctr(int(NA, NA, NA)))

  rownames <- rep_len("", nrow(mtcars))
  rownames <- vec_as_names(rownames, repair = "unique")
  expect_identical(vec_slice(mtcars, NA), structure(mtcars[NA, ], row.names = rownames))
})

test_that("can subset with a recycled TRUE", {
  expect_identical(vec_slice(1:3, TRUE), 1:3)
  expect_identical(vec_slice(mtcars, TRUE), mtcars)
  expect_identical(vec_slice(new_vctr(1:3), TRUE), new_vctr(1:3))
  expect_identical(vec_as_index(TRUE, 2), 1:2)
})

test_that("can subset with a recycled FALSE", {
  expect_identical(vec_slice(1:3, FALSE), int())
  expect_identical(vec_slice(mtcars, FALSE), mtcars[NULL, ])
  expect_identical(vec_slice(new_vctr(1:3), FALSE), new_vctr(integer()))
})

test_that("can't index beyond the end of a vector", {
  expect_error(vec_slice(1:2, 3L), class = "vctrs_error_index_oob_positions")
  expect_error(vec_slice(1:2, -3L), class = "vctrs_error_index_oob_positions")
})

test_that("oob error messages are properly constructed", {
  verify_output(test_path("out", "error-index-oob.txt"), {
    vec_slice(1:2, 3L)
    vec_slice(c(bar = 1), "foo")

    "Multiple OOB indices"
    vec_slice(letters, c(100, 1000))
    vec_slice(letters, c(1, 100:103, 2, 104:110))

    vec_slice(set_names(letters), c("foo", "bar"))
    vec_slice(set_names(letters), toupper(letters))
  })
})

test_that("slicing non existing elements fails", {
  expect_error(vec_as_index("foo", 1L, "f"), class = "vctrs_error_index_oob_names")
  expect_error(vec_slice(c(f = 1), "foo"), class = "vctrs_error_index_oob_names")
})

test_that("can subset object of any dimensionality", {
  x0 <- c(1, 1)
  x1 <- ones(2)
  x2 <- ones(2, 3)
  x3 <- ones(2, 3, 4)
  x4 <- ones(2, 3, 4, 5)

  expect_equal(vec_slice(x0, 1L), 1)
  expect_identical(vec_slice(x1, 1L), ones(1))
  expect_identical(vec_slice(x2, 1L), ones(1, 3))
  expect_identical(vec_slice(x3, 1L), ones(1, 3, 4))
  expect_identical(vec_slice(x4, 1L), ones(1, 3, 4, 5))
})

test_that("can subset using logical index", {
  x0 <- c(1, 1)

  expect_identical(vec_slice(x0, TRUE), x0)
  expect_identical(vec_slice(x0, c(TRUE, FALSE)), 1)

  expect_error(
    vec_slice(x0, c(TRUE, FALSE, TRUE)),
    "has size 2 whereas the index has size 3",
    fixed = TRUE
  )

  expect_error(
    vec_slice(x0, lgl()),
    "has size 2 whereas the index has size 0",
    fixed = TRUE
  )

  expect_error(
    vec_slice(mtcars, c(TRUE, FALSE)),
    "has size 32 whereas the index has size 2"
  )
})

test_that("can subset data frame columns", {
  df <- data.frame(x = 1:2)
  df$y <- data.frame(a = 2:1)

  expect_equal(vec_slice(df, 1L)$y, vec_slice(df$y, 1L))
})

test_that("can subset empty data frames", {
  df <- new_data_frame(n = 3L)
  expect_equal(vec_size(vec_slice(df, integer())), 0)
  expect_equal(vec_size(vec_slice(df, 1L)), 1)
  expect_equal(vec_size(vec_slice(df, 1:3)), 3)

  df$df <- df
  expect_equal(vec_size(vec_slice(df, integer())), 0)
  expect_equal(vec_size(vec_slice(df, 1L)), 1)
  expect_equal(vec_size(vec_slice(df, 1:3)), 3)
})

test_that("ignores NA in logical subsetting", {
  x <- c(NA, 1, 2)
  expect_equal(vec_slice(x, x > 0), c(NA, 1, 2))
})

test_that("ignores NA in integer subsetting", {
  expect_equal(vec_slice(0:2, c(NA, 2:3)), c(NA, 1, 2))
})

test_that("can't slice with missing argument", {
  expect_error(vec_slice(1:3))
  expect_error(vec_slice(mtcars))
  expect_error(vec_slice(new_vctr(1:3)))
})

test_that("can slice with NULL argument", {
  expect_identical(vec_slice(1:3, NULL), integer())
  expect_identical(vec_slice(iris, NULL), iris[0, ])
  expect_identical(vec_slice(new_vctr(1:3), NULL), new_vctr(integer()))
})

test_that("slicing unclassed structures preserves attributes", {
  x <- structure(1:3, foo = "bar")
  expect_identical(vec_slice(x, 1L), structure(1L, foo = "bar"))
})

test_that("can slice with negative indices", {
  expect_identical(vec_slice(1:3, -c(1L, 3L)), 2L)
  expect_identical(vec_slice(mtcars, -(1:30)), vec_slice(mtcars, 31:32))

  expect_error(vec_slice(1:3, -c(1L, NA)), "mix of negative indices and missing values")
  expect_error(vec_slice(1:3, c(-1L, 1L)), "mix of negative and positive indices")
})

test_that("0 is ignored in negative indices", {
  expect_identical(vec_slice(1:3, c(-2L, 0L)), int(1L, 3L))
  expect_identical(vec_slice(1:3, c(0L, -2L)), int(1L, 3L))
})

test_that("0 is ignored in positive indices", {
  expect_identical(vec_slice(1:3, 0L), int())
  expect_identical(vec_slice(1:3, c(0L, 0L)), int())
  expect_identical(vec_slice(1:3, c(0L, 2L, 0L)), 2L)
})

test_that("can slice with double indices", {
  expect_identical(vec_slice(1:3, dbl(2, 3)), 2:3)
  err <- expect_error(vec_as_index(2^31, 3L), class = "vctrs_error_index_bad_type")
  expect_is(err$parent, "vctrs_error_cast_lossy")
})

test_that("vec_as_index() checks type", {
  expect_error(vec_as_index(quote(foo), 1L), class = "vctrs_error_index_bad_type")
  expect_error(vec_as_index("foo", "bar"), class = "vctrs_error_incompatible_type")
  expect_error(vec_as_index("foo", 1L, names = 1L), "must be a character vector")
  expect_error(vec_as_index(Sys.Date(), 3L), class = "vctrs_error_index_bad_type")
  expect_error(vec_as_index(matrix(TRUE, nrow = 1), 3L), "must have one dimension")
})

test_that("can `vec_slice()` S3 objects without dispatch infloop", {
  expect_identical(new_vctr(1:3)[1], new_vctr(1L))
  expect_identical(new_vctr(as.list(1:3))[1], new_vctr(list(1L)))
})

test_that("can `vec_slice()` records", {
  out <- vec_slice(new_rcrd(list(a = 1L, b = 2L)), rep(1, 3))
  expect_size(out, 3)

  out <- vec_init(new_rcrd(list(a = 1L, b = 2L)), 2)
  expect_size(out, 2)
})

test_that("vec_restore() is called after proxied slicing", {
  local_methods(
    vec_proxy.vctrs_foobar = identity,
    vec_restore.vctrs_foobar = function(x, to, ...) "dispatch"
  )
  expect_identical(vec_slice(foobar(1:3), 2), "dispatch")
})

test_that("vec_slice() is proxied", {
  local_proxy()
  x <- vec_slice(new_proxy(1:3), 2:3)
  expect_identical(proxy_deref(x), 2:3)
})

test_that("dimensions are preserved by vec_slice()", {
  attrib <- NULL

  local_methods(
    vec_restore.vctrs_foobar = function(x, ...) attrib <<- attributes(x)
  )

  x <- foobar(1:4)
  dim(x) <- c(2, 2)
  dimnames(x) <- list(a = c("foo", "bar"), b = c("quux", "hunoz"))

  vec_slice(x, 1)

  exp <- list(dim = 1:2, dimnames = list(a = "foo", b = c("quux", "hunoz")))
  expect_identical(attrib, exp)
})

test_that("can slice shaped objects by name", {
  x <- matrix(1:2)

  expect_error(vec_slice(x, "foo"), "unnamed")

  dimnames(x) <- list(c("foo", "bar"))

  expect_equal(vec_slice(x, "foo"), vec_slice(x, 1L))
  expect_error(vec_slice(x, "baz"), class = "vctrs_error_index_oob_names")
})

test_that("vec_slice() unclasses input before calling `vec_restore()`", {
  oo <- NULL
  local_methods(
    vec_proxy.vctrs_foobar = identity,
    vec_restore.vctrs_foobar = function(x, ...) oo <<- is.object(x)
  )

  x <- foobar(1:4)
  dim(x) <- c(2, 2)

  vec_slice(x, 1)
  expect_false(oo)
})

test_that("can call `vec_slice()` from `[` methods with shaped objects without infloop", {
  local_methods(
    `[.vctrs_foobar` = function(x, i, ...) vec_slice(x, i)
  )

  x <- foobar(1:4)
  dim(x) <- c(2, 2)

  exp <- foobar(c(1L, 3L))
  dim(exp) <- c(1, 2)
  expect_identical(x[1], exp)
})

test_that("vec_slice() falls back to `[` with S3 objects", {
  local_methods(
    `[.vctrs_foobar` = function(x, i, ...) "dispatched"
  )
  expect_identical(vec_slice(foobar(NA), 1), foobar("dispatched"))

  expect_error(vec_slice(foobar(list(NA)), 1), class = "vctrs_error_scalar_type")
  local_methods(
    vec_proxy.vctrs_foobar = identity
  )
  expect_identical(vec_slice(foobar(list(NA)), 1), foobar(list(NA)))
})

test_that("vec_slice() doesn't restore when attributes have already been restored", {
  local_methods(
    `[.vctrs_foobar` = function(x, i, ...) structure("dispatched", foo = "bar"),
    vec_restore.vctrs_foobar = function(...) stop("not called")
  )
  expect_error(vec_slice(foobar(NA), 1), NA)
})

test_that("can vec_slice() without inflooping when restore calls math generics", {
  local_methods(
    new_foobar = function(x) {
      new_vctr(as.double(x), class = "vctrs_foobar")
    },
    vec_restore.vctrs_foobar = function(x, ...) {
      abs(x)
      sum(x)
      mean(x)
      is.finite(x)
      is.infinite(x)
      is.nan(x)
      new_foobar(x)
    }
  )
  expect_identical(new_foobar(1:10)[1:2], new_foobar(1:2))
})

test_that("vec_restore() is called after slicing data frames", {
  local_methods(
    vec_restore.vctrs_tabble = function(...) "dispatched"
  )
  df <- structure(mtcars, class = c("vctrs_tabble", "data.frame"))
  expect_identical(vec_slice(df, 1), "dispatched")
})

test_that("additional subscripts are forwarded to `[`", {
  local_methods(
    `[.vctrs_foobar` = function(x, i, ...) vec_index(x, i, ...)
  )

  x <- foobar(c("foo", "bar", "quux", "hunoz"))
  dim(x) <- c(2, 2)

  exp <- foobar("quux")
  dim(exp) <- c(1, 1)

  expect_identical(x[1, 2], exp)
})

test_that("can use names to vec_slice() a named object", {
  x0 <- c(a = 1, b = 2)
  x1 <- c(a = 1, a = 2)

  expect_identical(vec_slice(x0, letters[1]), c(a = 1))
  expect_identical(vec_slice(x0, letters[2:1]), c(b = 2, a = 1))
  expect_identical(vec_slice(x1, letters[1]), c(a = 1))

  expect_error(vec_slice(x0, letters[3:1]), class = "vctrs_error_index_oob_names")
  expect_error(vec_slice(x1, letters[2]), class = "vctrs_error_index_oob_names")
})

test_that("can't use names to vec_slice() an unnamed object", {
  x0 <- 1:3

  expect_error(
    vec_slice(x0, letters[1]),
    "Can't use character to index an unnamed vector.",
    fixed = TRUE
  )
  expect_error(
    vec_slice(x0, letters[25:27]),
    "Can't use character to index an unnamed vector.",
    fixed = TRUE
  )
})

test_that("can slice with missing character indices (#244)", {
  expect_identical(vec_as_index(na_chr, 2L, c("x", "")), na_int)
  expect_identical(vec_slice(c(x = 1), na_chr), set_names(na_dbl, ""))
  expect_identical(vec_slice(c(x = "foo"), na_chr), set_names(na_chr, ""))
})

test_that("can slice with numerics (#577)", {
  expect_identical(vec_as_index(1:2, 3), 1:2)
  expect_error(vec_as_index(1:2, 3.5), class = "vctrs_error_cast_lossy")
})

test_that("missing indices don't create NA names", {
  x <- set_names(letters)
  expect_identical(vec_slice(x, na_int), set_names(na_chr, ""))
  expect_identical(vec_slice(x, int(1, NA, 3, NA)), chr(a = "a", NA, c = "c", NA))

  # Preserves existing NA names
  x <- set_names(1:2, c(NA, "foo"))
  expect_identical(vec_slice(x, 1:2), x)
})

test_that("vec_slice throws error with non-vector inputs", {
  expect_error(vec_slice(environment(), 1L), class = "vctrs_error_scalar_type")
})

test_that("vec_slice() asserts vectorness (#301)", {
  expect_error(vec_slice(NULL, 1), class = "vctrs_error_scalar_type")
})


# vec_get -----------------------------------------------------------------

test_that("vec_get throws error with non-vector inputs", {
  expect_error(vec_get(environment(), 1L), class = "vctrs_error_scalar_type")
})

test_that("cannot extract more than one element", {
  expect_error(vec_get(1, 1:2), class = "vctrs_error_position_bad_type")
})

test_that("can extract from atomic vectors", {
  i <- 2L
  expect_identical(vec_get(lgl(1, 0, 1), i), lgl(0))
  expect_identical(vec_get(int(1, 2, 3), i), int(2))
  expect_identical(vec_get(dbl(1, 2, 3), i), dbl(2))
  expect_identical(vec_get(cpl(1, 2, 3), i), cpl(2))
  expect_identical(vec_get(chr("1", "2", "3"), i), chr("2"))
  expect_identical(vec_get(bytes(1, 2, 3), i), bytes(2))
})

test_that("can extract from a list", {
  expect_identical(vec_get(list(1, 2, 3), 2L), 2)
})

test_that("can extract from shaped atomic vectors", {
  i <- 2L
  mat <- as.matrix
  expect_identical(vec_get(mat(lgl(1, 0, 1)), i), mat(lgl(0)))
  expect_identical(vec_get(mat(int(1, 2, 3)), i), mat(int(2)))
  expect_identical(vec_get(mat(dbl(1, 2, 3)), i), mat(dbl(2)))
  expect_identical(vec_get(mat(cpl(1, 2, 3)), i), mat(cpl(2)))
  expect_identical(vec_get(mat(chr("1", "2", "3")), i), mat(chr("2")))
  expect_identical(vec_get(mat(bytes(1, 2, 3)), i), mat(bytes(2)))
})

test_that("can extract from a shaped list", {
  x <- matrix(list(1, 2, 3, 4), nrow = 2)
  expect <- matrix(list(2, 4), nrow = 1)
  expect_identical(vec_get(x, 2), expect)
})

test_that("can extract object of any dimensionality", {
  x0 <- c(1, 1)
  x1 <- ones(2)
  x2 <- ones(2, 3)
  x3 <- ones(2, 3, 4)
  x4 <- ones(2, 3, 4, 5)

  expect_equal(vec_get(x0, 1L), 1)
  expect_identical(vec_get(x1, 1L), ones(1))
  expect_identical(vec_get(x2, 1L), ones(1, 3))
  expect_identical(vec_get(x3, 1L), ones(1, 3, 4))
  expect_identical(vec_get(x4, 1L), ones(1, 3, 4, 5))
})

test_that("can extract from data frames row wise", {
  df <- data.frame(x = 1:2, y = c("a", "b"))
  expect_equal(vec_get(df, 1), vec_slice(df, 1))
})

test_that("can extract from data frames with data frame columns", {
  df <- data.frame(x = 1:2)
  df$y <- data.frame(a = 2:1)
  expect_equal(vec_get(df, 1), vec_slice(df, 1))
})

test_that("names are lost from atomics", {
  x <- set_names(1:2)
  expect_equal(names(vec_get(x, 1)), NULL)
})

test_that("row names are lost from data frames", {
  df <- data.frame(x = 1:2, row.names = c("r1", "r2"))
  expect_equal(rownames(vec_get(df, 1)), "1")
  expect_equal(.row_names_info(vec_get(df, 1)), -1)
})

test_that("row names are lost from matrices / arrays", {
  x <- array(1, c(2, 2, 2), dimnames = list(c("r1", "r2")))
  expect_equal(rownames(vec_get(x, 1)), NULL)
})

test_that("non-row names are kept on matrices / arrays", {
  dim_names <- list(c("r1", "r2"), c("c1", "c2"), c("d1", "d2"))
  x <- array(1, c(2, 2, 2), dimnames = dim_names)

  expect <- dim_names
  expect[1] <- list(NULL)

  expect_equal(dimnames(vec_get(x, 1)), expect)
})

# TODO - Is this right?
test_that("dimname names are kept on the 1st dimension", {
  x <- array(1, c(2, 2), dimnames = list(kept = c("r1", "r2")))
  expect_equal(dimnames(vec_get(x, 1)), list(kept = NULL, NULL))
})

test_that("row names are lost from shaped S3 objects", {
  dim_names <- list(c("r1", "r2"), c("c1", "c2"))
  x <- structure(1:4, dim = c(2L, 2L), dimnames = dim_names, class = "vctrs_mat")

  expect_equal(dimnames(vec_get(x, 1)), list(NULL, c("c1", "c2")))
})

test_that("vec_get() falls back to `[[` with S3 objects", {
  local_methods(
    `[[.vctrs_foobar` = function(x, i, ...) "dispatched"
  )
  expect_identical(vec_get(foobar(NA), 1), foobar("dispatched"))
})

test_that("can extract from S3 lists that implement a proxy", {
  expect_error(vec_get(foobar(list(NA)), 1), class = "vctrs_error_scalar_type")

  local_methods(
    vec_proxy.vctrs_foobar = identity
  )

  expect_identical(vec_get(foobar(list(NA)), 1), NA)
})

test_that("vec_get() doesn't restore when attributes have already been restored", {
  local_methods(
    `[[.vctrs_foobar` = function(x, i, ...) structure("dispatched", foo = "bar"),
    vec_restore.vctrs_foobar = function(...) stop("not called")
  )
  expect_error(vec_get(foobar(NA), 1), NA)
})

test_that("vec_restore() is called after extracting from data frames", {
  local_methods(
    vec_restore.vctrs_tabble = function(...) "dispatched"
  )
  df <- structure(mtcars, class = c("vctrs_tabble", "data.frame"))
  expect_identical(vec_get(df, 1), "dispatched")
})

# vec_init ----------------------------------------------------------------

test_that("na of atomic vectors is as expected", {
  expect_equal(vec_init(TRUE), NA)
  expect_equal(vec_init(1L), NA_integer_)
  expect_equal(vec_init(1), NA_real_)
  expect_equal(vec_init("x"), NA_character_)
  expect_equal(vec_init(1i), NA_complex_)
})

test_that("na of factor preserves levels", {
  f1 <- factor("a", levels = c("a", "b"))
  f2 <- vec_init(f1)

  expect_equal(levels(f1), levels(f2))
})

test_that("na of POSIXct preserves tz", {
  dt1 <- as.POSIXct("2010-01-01", tz = "America/New_York")
  dt2 <- vec_init(dt1)
  expect_equal(attr(dt2, "tzone"), "America/New_York")
})

test_that("na of list is list(NULL)", {
  expect_equal(vec_init(list()), list(NULL))
})

test_that("na of array is 1d slice", {
  x1 <- array(1:12, c(2, 3, 4))
  x2 <- vec_init(x1)

  expect_equal(x2, array(NA_integer_, c(1, 3, 4)))
})

test_that("na of list-array is 1d slice", {
  x1 <- array(as.list(1:12), c(2, 3, 4))
  x2 <- vec_init(x1)

  expect_equal(x2, array(list(), c(1, 3, 4)))
})

test_that("vec_init() asserts vectorness (#301)", {
  expect_error(vec_init(NULL, 1L), class = "vctrs_error_scalar_type")
})

# vec_chop ----------------------------------------------------------------

test_that("vec_chop() throws error with non-vector inputs", {
  expect_error(vec_chop(NULL), class = "vctrs_error_scalar_type")
  expect_error(vec_chop(environment()), class = "vctrs_error_scalar_type")
})

test_that("atomics are split into a list", {
  x <- 1:5
  expect_equal(vec_chop(x), as.list(x))

  x <- letters[1:5]
  expect_equal(vec_chop(x), as.list(x))
})

test_that("atomic names are kept", {
  x <- set_names(1:5)
  result <- lapply(vec_chop(x), names)
  expect_equal(result, as.list(names(x)))
})

test_that("base R classed objects are split into a list", {
  fctr <- factor(c("a", "b"))
  expect <- lapply(vec_seq_along(fctr), vec_slice, x = fctr)
  expect_equal(vec_chop(fctr), expect)

  date <- new_date(c(0, 1))
  expect <- lapply(vec_seq_along(date), vec_slice, x = date)
  expect_equal(vec_chop(date), expect)
})

test_that("base R classed object names are kept", {
  fctr <- set_names(factor(c("a", "b")))
  result <- lapply(vec_chop(fctr), names)
  expect_equal(result, as.list(names(fctr)))
})

test_that("list elements are split", {
  x <- list(1, 2)
  result <- list(vec_slice(x, 1), vec_slice(x, 2))
  expect_equal(vec_chop(x), result)
})

test_that("data frames are split rowwise", {
  x <- data_frame(x = 1:2, y = c("a", "b"))
  result <- list(vec_slice(x, 1), vec_slice(x, 2))
  expect_equal(vec_chop(x), result)
})

test_that("data frame row names are kept", {
  x <- data_frame(x = 1:2, y = c("a", "b"))
  rownames(x) <- c("r1", "r2")
  result <- lapply(vec_chop(x), rownames)
  expect_equal(result, list("r1", "r2"))
})

test_that("matrices / arrays are split rowwise", {
  x <- array(1:12, c(2, 2, 2))
  result <- list(vec_slice(x, 1), vec_slice(x, 2))
  expect_equal(vec_chop(x), result)
})

test_that("matrix / array row names are kept", {
  x <- array(1:12, c(2, 2, 2), dimnames = list(c("r1", "r2"), c("c1", "c2")))
  result <- lapply(vec_chop(x), rownames)
  expect_equal(result, list("r1", "r2"))
})

test_that("matrices / arrays without row names have other dimension names kept", {
  x <- array(1:12, c(2, 2, 2), dimnames = list(NULL, c("c1", "c2")))
  result <- lapply(vec_chop(x), colnames)
  expect_equal(result, list(c("c1", "c2"), c("c1", "c2")))
})

test_that("vec_chop() doesn't restore when attributes have already been restored", {
  local_methods(
    `[.vctrs_foobar` = function(x, i, ...) structure("dispatched", foo = "bar"),
    vec_restore.vctrs_foobar = function(...) structure("dispatched-and-restored", foo = "bar")
  )

  result <- vec_chop(foobar(NA))[[1]]
  expect_equal(result, structure("dispatched", foo = "bar"))
})

test_that("vec_chop() restores when attributes have not been restored by `[`", {
  local_methods(
    `[.vctrs_foobar` = function(x, i, ...) "dispatched",
    vec_restore.vctrs_foobar = function(...) "dispatched-and-restored"
  )

  result <- vec_chop(foobar(NA))[[1]]
  expect_equal(result, "dispatched-and-restored")
})

test_that("vec_chop() falls back to `[` for shaped objects with no proxy", {
  x <- foobar(1)
  dim(x) <- c(1, 1)
  result <- vec_chop(x)[[1]]
  expect_equal(result, x)
})

test_that("`indices` are validated", {
  expect_error(vec_chop(1, 1), "`indices` must be a list of index values, or `NULL`")
  expect_error(vec_chop(1, list(1.5)), class = "vctrs_error_cast_lossy")
  expect_error(vec_chop(1, list(2)), class = "vctrs_error_index_oob_positions")
})

test_that("size 0 `indices` list is allowed", {
  expect_equal(vec_chop(1, list()), list())
})

test_that("individual index values of size 0 are allowed", {
  expect_equal(vec_chop(1, list(integer())), list(numeric()))

  df <- data.frame(a = 1, b = "1")
  expect_equal(vec_chop(df, list(integer())), list(vec_ptype(df)))
})

test_that("data frame row names are kept when `indices` are used", {
  x <- data_frame(x = 1:2, y = c("a", "b"))
  rownames(x) <- c("r1", "r2")
  result <- lapply(vec_chop(x, list(1, 1:2)), rownames)
  expect_equal(result, list("r1", c("r1", "r2")))
})

test_that("vec_chop(<atomic>, indices =) can be equivalent to the default", {
  x <- 1:5
  indices <- as.list(vec_seq_along(x))
  expect_equal(vec_chop(x, indices), vec_chop(x))
})

test_that("vec_chop(<data.frame>, indices =) can be equivalent to the default", {
  x <- data.frame(x = 1:5)
  indices <- as.list(vec_seq_along(x))
  expect_equal(vec_chop(x, indices), vec_chop(x))
})

test_that("vec_chop(<array>, indices =) can be equivalent to the default", {
  x <- array(1:8, c(2, 2, 2))
  indices <- as.list(vec_seq_along(x))
  expect_equal(vec_chop(x, indices), vec_chop(x))
})

test_that("`indices` can use names", {
  x <- set_names(1:3, c("a", "b", "c"))

  expect_equal(
    vec_chop(x, list(1, 2:3)),
    vec_chop(x, list("a", c("b", "c")))
  )
})

test_that("`indices` can use array row names", {
  x <- array(1:4, c(2, 2), dimnames = list(c("r1", "r2")))

  expect_equal(
    vec_chop(x, list("r1")),
    vec_chop(x, list(1))
  )
})

test_that("`indices` cannot use data frame row names", {
  df <- data.frame(x = 1, row.names = "r1")
  expect_error(vec_chop(df, list("r1")), "Can't use character")
})

test_that("fallback method with `indices` works", {
  fctr <- factor(c("a", "b"))
  indices <- list(1, c(1, 2))

  expect_equal(
    vec_chop(fctr, indices),
    map(indices, vec_slice, x = fctr)
  )
})

test_that("vec_chop() falls back to `[` for shaped objects with no proxy when indices are provided", {
  x <- foobar(1)
  dim(x) <- c(1, 1)
  result <- vec_chop(x, list(1))[[1]]
  expect_equal(result, x)
})

# vec_slice + compact_rep -------------------------------------------------

# `i` is 1-based

test_that("names are repaired correctly with compact reps and `NA_integer_`", {
  x <- list(a = 1L, b = 2L)
  expect <- set_names(list(NULL, NULL), c("", ""))

  expect_equal(vec_slice_rep(x, NA_integer_, 2L), expect)
})

# vec_slice + compact_seq -------------------------------------------------

# `start` is 0-based

test_that("can subset base vectors with compact seqs", {
  start <- 1L
  size <- 2L
  increasing <- TRUE
  expect_identical(vec_slice_seq(lgl(1, 0, 1), start, size, increasing), lgl(0, 1))
  expect_identical(vec_slice_seq(int(1, 2, 3), start, size, increasing), int(2, 3))
  expect_identical(vec_slice_seq(dbl(1, 2, 3), start, size, increasing), dbl(2, 3))
  expect_identical(vec_slice_seq(cpl(1, 2, 3), start, size, increasing), cpl(2, 3))
  expect_identical(vec_slice_seq(chr("1", "2", "3"), start, size, increasing), chr("2", "3"))
  expect_identical(vec_slice_seq(bytes(1, 2, 3), start, size, increasing), bytes(2, 3))
  expect_identical(vec_slice_seq(list(1, 2, 3), start, size, increasing), list(2, 3))
})

test_that("can subset base vectors with decreasing compact seqs", {
  start <- 2L
  size <- 2L
  increasing <- FALSE
  expect_identical(vec_slice_seq(lgl(1, 0, 1), start, size, increasing), lgl(1, 0))
  expect_identical(vec_slice_seq(int(1, 2, 3), start, size, increasing), int(3, 2))
  expect_identical(vec_slice_seq(dbl(1, 2, 3), start, size, increasing), dbl(3, 2))
  expect_identical(vec_slice_seq(cpl(1, 2, 3), start, size, increasing), cpl(3, 2))
  expect_identical(vec_slice_seq(chr("1", "2", "3"), start, size, increasing), chr("3", "2"))
  expect_identical(vec_slice_seq(bytes(1, 2, 3), start, size, increasing), bytes(3, 2))
  expect_identical(vec_slice_seq(list(1, 2, 3), start, size, increasing), list(3, 2))
})

test_that("can subset base vectors with size 0 compact seqs", {
  start <- 1L
  size <- 0L
  increasing <- TRUE
  expect_identical(vec_slice_seq(lgl(1, 0, 1), start, size, increasing), lgl())
  expect_identical(vec_slice_seq(int(1, 2, 3), start, size, increasing), int())
  expect_identical(vec_slice_seq(dbl(1, 2, 3), start, size, increasing), dbl())
  expect_identical(vec_slice_seq(cpl(1, 2, 3), start, size, increasing), cpl())
  expect_identical(vec_slice_seq(chr("1", "2", "3"), start, size, increasing), chr())
  expect_identical(vec_slice_seq(bytes(1, 2, 3), start, size, increasing), bytes())
  expect_identical(vec_slice_seq(list(1, 2, 3), start, size, increasing), list())
})

test_that("can subset shaped base vectors with compact seqs", {
  start <- 1L
  size <- 2L
  increasing <- TRUE
  mat <- as.matrix
  expect_identical(vec_slice_seq(mat(lgl(1, 0, 1)), start, size, increasing), mat(lgl(0, 1)))
  expect_identical(vec_slice_seq(mat(int(1, 2, 3)), start, size, increasing), mat(int(2, 3)))
  expect_identical(vec_slice_seq(mat(dbl(1, 2, 3)), start, size, increasing), mat(dbl(2, 3)))
  expect_identical(vec_slice_seq(mat(cpl(1, 2, 3)), start, size, increasing), mat(cpl(2, 3)))
  expect_identical(vec_slice_seq(mat(chr("1", "2", "3")), start, size, increasing), mat(chr("2", "3")))
  expect_identical(vec_slice_seq(mat(bytes(1, 2, 3)), start, size, increasing), mat(bytes(2, 3)))
  expect_identical(vec_slice_seq(mat(list(1, 2, 3)), start, size, increasing), mat(list(2, 3)))
})

test_that("can subset shaped base vectors with decreasing compact seqs", {
  start <- 2L
  size <- 2L
  increasing <- FALSE
  mat <- as.matrix
  expect_identical(vec_slice_seq(mat(lgl(1, 0, 1)), start, size, increasing), mat(lgl(1, 0)))
  expect_identical(vec_slice_seq(mat(int(1, 2, 3)), start, size, increasing), mat(int(3, 2)))
  expect_identical(vec_slice_seq(mat(dbl(1, 2, 3)), start, size, increasing), mat(dbl(3, 2)))
  expect_identical(vec_slice_seq(mat(cpl(1, 2, 3)), start, size, increasing), mat(cpl(3, 2)))
  expect_identical(vec_slice_seq(mat(chr("1", "2", "3")), start, size, increasing), mat(chr("3", "2")))
  expect_identical(vec_slice_seq(mat(bytes(1, 2, 3)), start, size, increasing), mat(bytes(3, 2)))
  expect_identical(vec_slice_seq(mat(list(1, 2, 3)), start, size, increasing), mat(list(3, 2)))
})

test_that("can subset shaped base vectors with size 0 compact seqs", {
  start <- 1L
  size <- 0L
  increasing <- TRUE
  mat <- as.matrix
  expect_identical(vec_slice_seq(mat(lgl(1, 0, 1)), start, size, increasing), mat(lgl()))
  expect_identical(vec_slice_seq(mat(int(1, 2, 3)), start, size, increasing), mat(int()))
  expect_identical(vec_slice_seq(mat(dbl(1, 2, 3)), start, size, increasing), mat(dbl()))
  expect_identical(vec_slice_seq(mat(cpl(1, 2, 3)), start, size, increasing), mat(cpl()))
  expect_identical(vec_slice_seq(mat(chr("1", "2", "3")), start, size, increasing), mat(chr()))
  expect_identical(vec_slice_seq(mat(bytes(1, 2, 3)), start, size, increasing), mat(bytes()))
  expect_identical(vec_slice_seq(mat(list(1, 2, 3)), start, size, increasing), mat(list()))
})

test_that("can subset object of any dimensionality with compact seqs", {
  x0 <- c(1, 1)
  x1 <- ones(2)
  x2 <- ones(2, 3)
  x3 <- ones(2, 3, 4)
  x4 <- ones(2, 3, 4, 5)

  expect_equal(vec_slice_seq(x0, 0L, 1L), 1)
  expect_identical(vec_slice_seq(x1, 0L, 1L), ones(1))
  expect_identical(vec_slice_seq(x2, 0L, 1L), ones(1, 3))
  expect_identical(vec_slice_seq(x3, 0L, 1L), ones(1, 3, 4))
  expect_identical(vec_slice_seq(x4, 0L, 1L), ones(1, 3, 4, 5))
})

test_that("can subset data frames with compact seqs", {
  df <- data_frame(x = 1:5, y = letters[1:5])
  expect_equal(vec_slice_seq(df, 0L, 0L), vec_slice(df, integer()))
  expect_equal(vec_slice_seq(df, 0L, 1L), vec_slice(df, 1L))
  expect_equal(vec_slice_seq(df, 0L, 3L), vec_slice(df, 1:3))
  expect_equal(vec_slice_seq(df, 2L, 3L, FALSE), vec_slice(df, 3:1))

  df$df <- df
  expect_equal(vec_slice_seq(df, 0L, 0L), vec_slice(df, integer()))
  expect_equal(vec_slice_seq(df, 0L, 1L), vec_slice(df, 1L))
  expect_equal(vec_slice_seq(df, 0L, 3L), vec_slice(df, 1:3))
  expect_equal(vec_slice_seq(df, 2L, 3L, FALSE), vec_slice(df, 3:1))
})

test_that("can subset S3 objects using the fallback method with compact seqs", {
  x <- factor(c("a", "b", "c", "d"))
  expect_equal(vec_slice_seq(x, 0L, 0L), vec_slice(x, integer()))
  expect_equal(vec_slice_seq(x, 0L, 1L), vec_slice(x, 1L))
  expect_equal(vec_slice_seq(x, 2L, 2L), vec_slice(x, 3:4))
  expect_equal(vec_slice_seq(x, 3L, 2L, FALSE), vec_slice(x, 4:3))
})

# vec_chop + compact_seq --------------------------------------------------

# `start` is 0-based

test_that("can chop base vectors with compact seqs", {
  start <- 1L
  size <- 2L
  expect_identical(vec_chop_seq(lgl(1, 0, 1), start, size), list(lgl(0, 1)))
  expect_identical(vec_chop_seq(int(1, 2, 3), start, size), list(int(2, 3)))
  expect_identical(vec_chop_seq(dbl(1, 2, 3), start, size), list(dbl(2, 3)))
  expect_identical(vec_chop_seq(cpl(1, 2, 3), start, size), list(cpl(2, 3)))
  expect_identical(vec_chop_seq(chr("1", "2", "3"), start, size), list(chr("2", "3")))
  expect_identical(vec_chop_seq(bytes(1, 2, 3), start, size), list(bytes(2, 3)))
  expect_identical(vec_chop_seq(list(1, 2, 3), start, size), list(list(2, 3)))
})

test_that("can chop with a decreasing compact seq", {
  expect_equal(vec_chop_seq(int(1, 2, 3), 1L, 2L, FALSE), list(int(2, 1)))
})

test_that("can chop with multiple compact seqs", {
  start <- c(1L, 0L)
  size <- c(1L, 3L)

  expect_equal(
    vec_chop_seq(int(1, 2, 3), start, size),
    list(int(2), int(1, 2, 3))
  )
})

test_that("can chop S3 objects using the fallback method with compact seqs", {
  x <- factor(c("a", "b", "c", "d"))
  expect_equal(vec_chop_seq(x, 0L, 0L), list(vec_slice(x, integer())))
  expect_equal(vec_chop_seq(x, 0L, 1L), list(vec_slice(x, 1L)))
  expect_equal(vec_chop_seq(x, 2L, 2L), list(vec_slice(x, 3:4)))
})

# Position / index coercion -----------------------------------------------

test_that("vec_as_position() returns a position", {
  expect_identical(vec_as_position(2, 2L), 2L)
  expect_identical(vec_as_position("foo", 2L, c("bar", "foo")), 2L)
})

test_that("vec_as_position() requires integer or character inputs", {
  expect_error(vec_as_position(TRUE, 10L), class = "vctrs_error_position_bad_type")
  expect_error(vec_as_position(mtcars, 10L), class = "vctrs_error_position_bad_type")
  expect_error(vec_as_position(env(), 10L), class = "vctrs_error_position_bad_type")
  expect_error(vec_as_position(foobar(), 10L), class = "vctrs_error_position_bad_type")
  expect_error(vec_as_position(2.5, 10L), class = "vctrs_error_position_bad_type")

  verify_output(test_path("out", "error-position-type.txt"), {
    vec_as_position(TRUE, 10L)
    vec_as_position(mtcars, 10L)
    vec_as_position(env(), 10L)
    vec_as_position(foobar(), 10L)
    vec_as_position(2.5, 3L)

    "# Custom `arg`"
    vec_as_position(foobar(), 10L, arg = "foo")
    vec_as_position(2.5, 3L, arg = "foo")
  })
})

test_that("vec_as_index() requires integer, character, or logical inputs", {
  expect_error(vec_as_index(mtcars, 10L), class = "vctrs_error_index_bad_type")
  expect_error(vec_as_index(env(), 10L), class = "vctrs_error_index_bad_type")
  expect_error(vec_as_index(foobar(), 10L), class = "vctrs_error_index_bad_type")
  expect_error(vec_as_index(2.5, 10L), class = "vctrs_error_index_bad_type")
  expect_error(vec_as_index(list(), 10L), class = "vctrs_error_index_bad_type")
  expect_error(vec_as_index(function() NULL, 10L), class = "vctrs_error_index_bad_type")

  verify_output(test_path("out", "error-index-type.txt"), {
    vec_as_index(mtcars, 10L)
    vec_as_index(env(), 10L)
    vec_as_index(foobar(), 10L)
    vec_as_index(2.5, 3L)
    vec_as_index(list(), 10L)
    vec_as_index(function() NULL, 10L)

    "# Custom `arg`"
    vec_as_index(env(), 10L, arg = "foo")
    vec_as_index(foobar(), 10L, arg = "foo")
    vec_as_index(2.5, 3L, arg = "foo")
  })
})

test_that("vec_as_position() and vec_as_index() require integer- or character-like OO inputs", {
  expect_identical(vec_as_position(factor("foo"), 2L, c("bar", "foo")), 2L)
  expect_identical(vec_as_index(factor("foo"), 2L, c("bar", "foo")), 2L)
  expect_error(vec_as_position(foobar(1L), 10L), class = "vctrs_error_position_bad_type")
  expect_error(vec_as_index(foobar(1L), 10L), class = "vctrs_error_index_bad_type")

  # Define subtype of logical and integer
  local_methods(
    vec_ptype2.vctrs_foobar = function(x, y, ...) UseMethod("vec_ptype2.vctrs_foobar", y),
    vec_ptype2.vctrs_foobar.default = function(x, y, ...) vec_default_ptype2(x, y, ...),
    vec_ptype2.vctrs_foobar.logical = function(x, y, ...) logical(),
    vec_ptype2.vctrs_foobar.integer = function(x, y, ...) integer(),
    vec_ptype2.logical.vctrs_foobar = function(x, y, ...) logical(),
    vec_ptype2.integer.vctrs_foobar = function(x, y, ...) integer(),
    vec_cast.vctrs_foobar = function(x, to, ...) UseMethod("vec_cast.vctrs_foobar"),
    vec_cast.vctrs_foobar.integer = function(x, to, ...) foobar(x),
    vec_cast.integer.vctrs_foobar = function(x, to, ...) vec_cast(unclass(x), int()),
    vec_cast.logical.vctrs_foobar = function(x, to, ...) vec_cast(unclass(x), lgl())
  )
  expect_error(vec_as_position(foobar(TRUE), 10L), class = "vctrs_error_position_bad_type")
  expect_identical(vec_as_index(foobar(TRUE), 10L), 1:10)
  expect_identical(vec_as_index(foobar(FALSE), 10L), int())
})

test_that("vec_as_position() and vec_as_index() require existing elements", {
  expect_error(vec_as_position(10L, 2L), class = "vctrs_error_index_oob_positions")
  expect_error(vec_as_position("foo", 1L, names = "bar"), class = "vctrs_error_index_oob_names")
  expect_error(vec_as_index(10L, 2L), class = "vctrs_error_index_oob_positions")
  expect_error(vec_as_index("foo", 1L, names = "bar"), class = "vctrs_error_index_oob_names")
})

test_that("vec_as_position() requires length 1 inputs", {
  expect_error(vec_as_position(1:2, 2L), class = "vctrs_error_position_bad_type")
  expect_error(vec_as_position(c("foo", "bar"), 2L, c("foo", "bar")), class = "vctrs_error_position_bad_type")

  verify_output(test_path("out", "error-position-size.txt"), {
    vec_as_position(1:2, 2L)
    vec_as_position(mtcars, 10L)

    "# Custom `arg`"
    vec_as_position(1:2, 2L, arg = "foo")
    vec_as_position(mtcars, 10L, arg = "foo")
    vec_as_position(1:2, 2L, arg = "foo")
  })
})

test_that("vec_as_position() requires positive integers", {
  expect_error(vec_as_position(0, 2L), class = "vctrs_error_position_bad_type")
  expect_error(vec_as_position(-1, 2L), class = "vctrs_error_position_bad_type")

  verify_output(test_path("out", "error-position-sign.txt"), {
    vec_as_position(0, 2L)
    vec_as_position(-1, 2L)

    "# Custom `arg`"
    vec_as_position(0, 2L, arg = "foo")
  })
})

test_that("vec_as_position() fails with NA", {
  expect_error(vec_as_position(na_int, 2L), class = "vctrs_error_position_bad_type")
  expect_error(vec_as_position(na_chr, 1L, names = "foo"), class = "vctrs_error_position_bad_type")

  verify_output(test_path("out", "error-position-na.txt"), {
    vec_as_position(na_int, 2L)
    vec_as_position(na_chr, 1L, names = "foo")

    "# Custom `arg`"
    vec_as_position(na_int, 2L)
  })
})

test_that("vec_as_position() doesn't allow lossy casts", {
  expect_error(vec_as_position(2^31, 3L), class = "vctrs_error_position_bad_type")

  # Lossy casts generate missing values, which are disallowed
  expect_error(allow_lossy_cast(vec_as_position(2^31, 3L)), class = "vctrs_error_position_bad_type")
})

test_that("all index errors inherit from `vctrs_error_index`", {
  expect_error(vec_as_index(100, 2L), class = "vctrs_error_index")
  expect_error(vec_as_index("foo", 2L, names = c("bar", "baz")), class = "vctrs_error_index")
  expect_error(vec_as_index(foobar(1L), 2L), class = "vctrs_error_index")
  expect_error(vec_as_index(1.5, 2L), class = "vctrs_error_index")
  expect_error(vec_as_position(TRUE, 2L), class = "vctrs_error_index")
  expect_error(vec_as_position(1.5, 2L), class = "vctrs_error_index")
})

test_that("all OOB errors inherit from `vctrs_error_index_oob`", {
  expect_error(vec_as_index(100, 2L), class = "vctrs_error_index_oob")
  expect_error(vec_as_index("foo", 2L, names = c("bar", "baz")), class = "vctrs_error_index_oob")
})

test_that("vec_as_index() preserves names if possible", {
  expect_identical(vec_as_index(c(a = 1L, b = 3L), 3L), c(a = 1L, b = 3L))
  expect_identical(vec_as_index(c(a = 1, b = 3), 3L), c(a = 1L, b = 3L))
  expect_identical(vec_as_index(c(a = "z", b = "y"), 26L, letters), c(a = 26L, b = 25L))

  expect_identical(vec_as_index(c(foo = TRUE, bar = FALSE, baz = TRUE), 3L), c(foo = 1L, baz = 3L))
  expect_identical(vec_as_index(c(foo = TRUE), 3L), c(foo = 1L, foo = 2L, foo = 3L))
  expect_identical(vec_as_index(c(foo = NA), 3L), c(foo = na_int, foo = na_int, foo = na_int))

  # Names of negative selections are dropped
  expect_identical(vec_as_index(c(a = -1L, b = -3L), 3L), 2L)
})

test_that("vec_as_position() optionally allows missing and negative positions", {
  expect_identical(vec_as_position(NA, 2L, allow_values = "missing"), na_int)
  expect_identical(vec_as_position(-1, 2L, allow_values = "negative"), -1L)
  expect_error(vec_as_position(-3, 2L, allow_values = "negative"), class = "vctrs_error_index_oob_positions")
  expect_error(vec_as_position(letters, 2L, allow_values = "negative"), class = "vctrs_error_position_bad_type")
  expect_error(vec_as_position(0, 2L, allow_values = "negative"), class = "vctrs_error_position_bad_type")
})

test_that("vec_as_index() optionally allows negative indices", {
  expect_identical(vec_as_index(dbl(1, -1), 2L, convert_values = NULL), int(1L, -1L))
  expect_error(vec_as_index(c(1, -10), 2L, convert_values = NULL), class = "vctrs_error_index_oob_positions")
})

test_that("vec_coerce_index() handles `allow_types`", {
  expect_identical(vec_coerce_index(NA, allow_types = c("position", "name")), na_int)
  expect_identical(vec_coerce_index(NA, allow_types = "name"), na_chr)

  verify_output(test_path("out", "test-coerce-index-allow-types.txt"), {
    vec_coerce_index(1L, allow_types = "name")
    vec_coerce_index("foo", allow_types = "position")
    vec_coerce_index(TRUE, allow_types = c("position", "name"))
    vec_coerce_index("foo", allow_types = c("indicator", "position"))
  })
})

test_that("vec_coerce_position() handles `allow_types`", {
  verify_output(test_path("out", "test-coerce-position-allow-types.txt"), {
    vec_coerce_position(1L, allow_types = "name")
    vec_coerce_position("foo", allow_types = "position")
    vec_coerce_position(TRUE, allow_types = c("position", "name"))
  })
})

test_that("vec_splice() works with Altrep classes with custom extract methods", {
  skip_if(getRversion() < "3.5")

  x <- .Call(vctrs_rle, c(foo = 10L, bar = 5L))

  idx <- c(1, 3, 15)
  expect_equal(vec_slice(x, idx), x[idx])
})
