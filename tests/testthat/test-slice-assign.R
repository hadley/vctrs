context("test-slice-assign")

test_that("slice-assign throws error with non-vector inputs", {
  x <- environment()
  expect_error(vec_slice(x, 1L) <- 1L, class = "vctrs_error_scalar_type")
})

test_that("slice-assign throws error with non-vector `value`", {
  x <- 1L
  expect_error(vec_slice(x, 1L) <- NULL, class = "vctrs_error_scalar_type")
  expect_error(vec_slice(x, 1L) <- environment(), class = "vctrs_error_scalar_type")
})

test_that("can slice-assign NULL", {
  x <- NULL
  vec_slice(x, 1L) <- 1
  expect_identical(x, NULL)
})

test_that("can slice-assign base vectors", {
  x <- rep(FALSE, 3)
  vec_slice(x, 2) <- TRUE
  expect_identical(x, lgl(FALSE, TRUE, FALSE))

  x <- rep(0L, 3)
  vec_slice(x, 2) <- 1L
  expect_identical(x, int(0L, 1L, 0L))

  x <- rep(0., 3)
  vec_slice(x, 2) <- 1
  expect_identical(x, dbl(0, 1, 0))

  x <- rep(0i, 3)
  vec_slice(x, 2) <- 1i
  expect_identical(x, cpl(0i, 1i, 0i))

  x <- rep("", 3)
  vec_slice(x, 2) <- "foo"
  expect_identical(x, chr("", "foo", ""))

  x <- as.raw(rep(0, 3))
  vec_slice(x, 2) <- as.raw(1)
  expect_identical(x, as.raw(c(0, 1, 0)))
})

test_that("can assign base vectors", {
  x <- rep(FALSE, 3)
  expect_identical(vec_assign(x, 2, TRUE), lgl(FALSE, TRUE, FALSE))
  expect_identical(x, rep(FALSE, 3))

  x <- rep(0L, 3)
  expect_identical(vec_assign(x, 2, 1L), int(0L, 1L, 0L))
  expect_identical(x, rep(0L, 3))

  x <- rep(0., 3)
  expect_identical(vec_assign(x, 2, 1), dbl(0, 1, 0))
  expect_identical(x, rep(0., 3))

  x <- rep(0i, 3)
  expect_identical(vec_assign(x, 2, 1i), cpl(0i, 1i, 0i))
  expect_identical(x, rep(0i, 3))

  x <- rep("", 3)
  expect_identical(vec_assign(x, 2, "foo"), chr("", "foo", ""))
  expect_identical(x, rep("", 3))

  x <- as.raw(rep(0, 3))
  expect_identical(vec_assign(x, 2, as.raw(1)), as.raw(c(0, 1, 0)))
  expect_identical(x, as.raw(rep(0, 3)))
})

test_that("can slice-assign lists", {
  x <- rep(list(NULL), 3)
  vec_slice(x, 2) <- list(NA)
  expect_identical(x, list(NULL, NA, NULL))
})

test_that("can assign lists", {
  x <- rep(list(NULL), 3)
  expect_identical(vec_assign(x, 2, list(NA)), list(NULL, NA, NULL))
  expect_identical(x, rep(list(NULL), 3))
})

test_that("atomics can't be assigned in lists", {
  x <- list(NULL)
  expect_error(vec_slice(x, 1) <- "foo", class = "vctrs_error_incompatible_type")
  expect_error(vec_assign(x, 1, "foo"), class = "vctrs_error_incompatible_type")

  skip("FIXME: should we always allow assignment of unspecified vectors?")
  # Should it then assign `NULL` rather than `NA`?
  expect_error(vec_slice(x, 1) <- NA, class = "vctrs_error_incompatible_type")
  expect_error(vec_assign(x, 1, NA), class = "vctrs_error_incompatible_type")
})

test_that("can assign and slice-assign data frames", {
  df <- data.frame(x = 1:2)
  df$y <- data.frame(a = 2:1)

  orig <- duplicate(df, shallow = FALSE)

  other <- data.frame(x = 3)
  other$y <- data.frame(a = 3)

  exp <- data.frame(x = int(3, 2))
  exp$y <- data.frame(a = int(3, 1))

  expect_identical(vec_assign(df, 1, other), exp)
  expect_identical(df, orig)

  vec_slice(df, 1) <- other
  expect_identical(df, exp)
})

test_that("can slice-assign arrays", {
  x <- array(c(2, 1, 2, 1), c(2, 2))
  vec_slice(x, 1L) <- 1
  expect_equal(x, array(1, c(2, 2)))

  x <- array(c(2, 1, 2, 1, 2, 1, 2, 1), c(2, 2, 2))
  vec_slice(x, 1L) <- 1
  expect_equal(x, array(1, c(2, 2, 2)))
})

test_that("can assign arrays", {
  x <- array(c(2, 1, 2, 1), c(2, 2))
  expect_identical(vec_assign(x, 1L, 1), array(1, c(2, 2)))
  expect_identical(x, array(c(2, 1, 2, 1), c(2, 2)))

  x <- array(c(2, 1, 2, 1, 2, 1, 2, 1), c(2, 2, 2))
  expect_identical(vec_assign(x, 1L, 1), array(1, c(2, 2, 2)))
  expect_identical(x, array(c(2, 1, 2, 1, 2, 1, 2, 1), c(2, 2, 2)))
})

test_that("can slice-assign using logical index", {
  x <- c(2, 1)
  vec_slice(x, TRUE) <- 3
  expect_equal(x, c(3, 3))

  vec_slice(x, c(TRUE, FALSE)) <- 4
  expect_equal(x, c(4, 3))

  expect_error(
    vec_slice(x, c(TRUE, FALSE, TRUE)) <- 5,
    "has size 2 whereas the index has size 3",
    fixed = TRUE
  )

  expect_error(
    vec_slice(mtcars, c(TRUE, FALSE)) <- mtcars[1, ],
    "has size 32 whereas the index has size 2"
  )
})

test_that("slice-assign ignores NA in logical subsetting", {
  x <- c(NA, 1, 2)
  expect_equal(`vec_slice<-`(x, x > 0, 1), c(NA, 1, 1))
  expect_equal(`vec_slice<-`(x, x > 0, c(NA, 2:1)), c(NA, 2, 1))
})

test_that("slice-assign ignores NA in integer subsetting", {
  x <- 0:2
  expect_equal(`vec_slice<-`(x, c(NA, 2:3), 1), c(0, 1, 1))
  expect_equal(`vec_slice<-`(x, c(NA, 2:3), c(NA, 2:1)), c(0, 2, 1))
})

test_that("can't modify subset with missing argument", {
  x <- 1:3
  expect_error(vec_slice(x, ) <- 2L)
})

test_that("can modify subset with recycled NA argument", {
  x <- 1:3
  vec_slice(x, NA) <- 2L
  expect_identical(x, 1:3)
})

test_that("can modify subset with recycled TRUE argument", {
  x <- 1:3
  vec_slice(x, TRUE) <- 2L
  expect_identical(x, rep(2L, 3))
})

test_that("can modify subset with recycled FALSE argument", {
  x <- 1:3
  vec_slice(x, FALSE) <- 2L
  expect_identical(x, 1:3)
})

test_that("can modify subset with NULL argument", {
  x <- 1:3
  vec_slice(x, NULL) <- 2L

  expect_identical(x, 1:3)
})

test_that("can slice-assign with missing indices", {
  x <- 1:3
  y <- 4:6
  test <- c(NA, TRUE, FALSE)
  vec_slice(x, test) <- vec_slice(y, test)
  expect_identical(x, int(1, 5, 3))
})

test_that("slice-assign checks vectorness", {
  x <- foobar(list(1))
  expect_error(vec_slice(x, 1) <- 10, class = "vctrs_error_scalar_type")
})

test_that("a coercible RHS is cast to LHS before assignment (#140)", {
  x <- 1:2
  expect_error(vec_slice(x, 1) <- "1", class = "vctrs_error_incompatible_type")

  x <- c("foo", "bar")
  expect_error(vec_slice(x, 1) <- 1, class = "vctrs_error_incompatible_type")

  x <- 1:2
  expect_error(vec_slice(x, 1) <- 3.5, class = "vctrs_error_cast_lossy")

  allow_lossy_cast(vec_slice(x, 1) <- 3.5)
  expect_identical(x, int(3, 2))

  x <- matrix(1:4, 2)
  vec_slice(x, 1) <- matrix(c(FALSE, FALSE), 1)
  expect_identical(x, matrix(int(0, 2, 0, 4), 2))
  expect_error(vec_assign(x, 1, matrix(c("", ""), 1)), class = "vctrs_error_incompatible_type")
})

test_that("slice-assign takes the proxy", {
  local_proxy()

  x <- new_proxy(1:3)
  y <- new_proxy(20:21)

  vec_slice(x, 2:3) <- y

  expect_identical(proxy_deref(x), int(1, 20, 21))
})

test_that("can use names to vec_slice<-() a named object", {
  x0 <- c(a = 1, b = 2)
  x1 <- c(a = 1, a = 2)

  vec_slice(x0, "b") <- 3
  expect_identical(x0, c(a = 1, b = 3))

  vec_slice(x1, "a") <- 3
  expect_identical(x1, c(a = 3, a = 2))
})

test_that("can use names to vec_slice<-() a named object", {
  x0 <- 1:3

  expect_error(
    vec_slice(x0, letters[1]) <- 4L,
    "Can't use character to index an unnamed vector.",
    fixed = TRUE
  )
  expect_error(
    vec_slice(x0, letters[25:27]) <- 5L,
    "Can't use character to index an unnamed vector.",
    fixed = TRUE
  )
})

test_that("slice-assign falls back to `[<-` when proxy is not implemented", {
  obj <- foobar(c("foo", "bar", "baz"))
  expect_error(vec_slice(obj, 1:2) <- NA, class = "vctrs_error_incompatible_cast")

  vec_slice(obj, 1:2) <- foobar("quux")

  vec_ptype2(foobar(""), foobar(""))
  vec_cast(foobar(""), foobar(""))
  #> Error: Can't cast <vctrs_foobar> to <vctrs_foobar>

  local_methods(
    `[<-.vctrs_foobar` = function(x, i, value) {
      x <- unclass(x)
      x[i] <- "dispatched"
      x
    },
    vec_ptype2.logical.vctrs_foobar = function(...) foobar(""),
    vec_ptype2.vctrs_foobar = function(...) foobar(""),
    vec_cast.vctrs_foobar = function(x, to, ...) x
  )

  obj <- foobar(c("foo", "bar", "baz"))
  vec_slice(obj, 1:2) <- NA
  expect_identical(obj, c("dispatched", "dispatched", "baz"))
})

test_that("slice-assign restores value before falling back to `[<-` (#443)", {
  called <- FALSE

  local_methods(
    vec_proxy.vctrs_proxy = proxy_deref,
    vec_restore.vctrs_proxy = function(x, to, ...) new_proxy(x),
    vec_ptype2.vctrs_proxy = function(...) new_proxy(NA),
    vec_cast.vctrs_foobar = function(x, ...) proxy_deref(x),
    `[<-.vctrs_foobar` = function(x, i, value) {
      called <<- TRUE
      expect_is(value, "vctrs_proxy")
    }
  )

  x <- foobar(1)
  y <- new_proxy(10)
  vec_slice(x, 1) <- y
  expect_true(called)
})

test_that("index and value are sliced before falling back", {
  # Work around a bug in base R `[<-`
  lhs <- foobar(c(NA, 1:4))
  rhs <- foobar(int(0L, 10L))
  exp <- foobar(int(10L, 1:4))
  expect_identical(vec_assign(lhs, c(NA, 1), rhs), exp)
})

test_that("can assign to data frame", {
  x <- data_frame(x = 1:3)
  y <- data_frame(x = 20)
  expect_identical(vec_assign(x, 2, y), data_frame(x = int(1, 20, 3)))
})

test_that("can slice-assign unspecified vectors with default type2 method", {
  local_rational_class()
  x <- rational(1:2, 2:3)
  x[[1]] <- NA
  expect_identical(x, rational(c(NA, 2L), c(NA, 3L)))
})
