context("test-altrep-rep")
skip_if_no_altrep()

test_that("No_NA method keeps compact vectors from being expanded", {
  test <- function(test_info) {
    for (info in test_info) {
      x <- info$x
      na <- info$na
      ctor <- info$ctor

      x <- ctor(x, 2)
      na <- ctor(na, 2)

      expect_identical(anyNA(x), FALSE)
      expect_identical(anyNA(na), TRUE)

      expect_true(vec_is_vctrs_compact_rep_compact(x))
      expect_true(vec_is_vctrs_compact_rep_compact(na))
    }
  }

  test(vctrs_compact_rep_test_info())

  skip_if_no_altrep_3_6()
  test(vctrs_compact_rep_test_info_3_6())
})

test_that("vec_ptype2 doesn't expand compact reps", {
  test <- function(test_info) {
    for (info in test_info) {
      x <- info$x
      ctor <- info$ctor

      x <- ctor(x, 1)

      vec_ptype2(x, x)

      expect_true(vec_is_vctrs_compact_rep_compact(x))
    }
  }

  test(vctrs_compact_rep_test_info())

  skip_if_no_altrep_3_6()
  test(vctrs_compact_rep_test_info_3_6())
})

test_that("vec_cast-ing to the same type doesn't expand compact reps", {
  test <- function(test_info) {
    for (info in test_info) {
      x <- info$x
      ctor <- info$ctor

      x <- ctor(x, 1)

      vec_cast(x, x)

      expect_true(vec_is_vctrs_compact_rep_compact(x))
    }
  }

  test(vctrs_compact_rep_test_info())

  skip_if_no_altrep_3_6()
  test(vctrs_compact_rep_test_info_3_6())
})

test_that("vec_recycle() generates compact rep objects", {
  test <- function(test_info) {
    for (info in test_info) {
      x <- info$x

      x <- vec_recycle(x, 2)

      expect_true(vec_is_vctrs_compact_rep_compact(x))
    }
  }

  test(vctrs_compact_rep_test_info())

  skip_if_no_altrep_3_6()
  test(vctrs_compact_rep_test_info_3_6())
})

test_that("vec_init() generates compact rep objects", {
  test <- function(test_info) {
    for (info in test_info) {
      x <- info$x

      x <- vec_init(x, 2)

      expect_true(vec_is_vctrs_compact_rep_compact(x))
    }
  }

  test(vctrs_compact_rep_test_info())

  skip_if_no_altrep_3_6()
  test(vctrs_compact_rep_test_info_3_6())
})

test_that("recycling or initializing classed objects or objects with attributes does not create compact reps", {
  test <- function(test_info) {
    for (info in test_info) {
      x <- info$x

      x_classed <- foobar(x)
      x_attrs <- structure(x, foo = "bar")

      x_classed_recycle <- vec_recycle(x_classed, 2)
      x_attrs_recycle <- vec_recycle(x_attrs, 2)

      expect_false(vec_is_vctrs_compact_rep(x_classed_recycle))
      expect_false(vec_is_vctrs_compact_rep(x_attrs_recycle))

      x_classed_init <- vec_init(x_classed, 2)
      x_attrs_init <- vec_init(x_attrs, 2)

      expect_false(vec_is_vctrs_compact_rep(x_classed_init))
      expect_false(vec_is_vctrs_compact_rep(x_attrs_init))
    }
  }

  test(vctrs_compact_rep_test_info())

  skip_if_no_altrep_3_6()
  test(vctrs_compact_rep_test_info_3_6())
})

test_that("subsetting with `[` works with integers and doubles (through Extract_Subset)", {
  test <- function(test_info) {
    for (info in test_info) {
      x <- info$x
      ctor <- info$ctor

      rep <- ctor(x, 2)

      expect_identical(rep[1L], x)
      expect_identical(rep[1:2], c(x, x))

      expect_identical(rep[1], x)
      expect_identical(rep[c(1, 2)], c(x, x))
    }
  }

  test(vctrs_compact_rep_test_info())

  skip_if_no_altrep_3_6()
  test(vctrs_compact_rep_test_info_3_6())
})

test_that("subsetting with `[` is lenient with fractional doubles", {
  test <- function(test_info) {
    for (info in test_info) {
      x <- info$x
      ctor <- info$ctor

      rep <- ctor(x, 2)

      expect_identical(rep[1.5], x)
      expect_identical(rep[c(1.9, 2.1)], c(x, x))
    }
  }

  test(vctrs_compact_rep_test_info())

  skip_if_no_altrep_3_6()
  test(vctrs_compact_rep_test_info_3_6())
})

test_that("subsetting with `[` extends OOB indices with `NA`", {
  test <- function(test_info) {
    for (info in test_info) {
      x <- info$x
      ctor <- info$ctor

      rep <- ctor(x, 2)

      expect_identical(rep[c(3, 1, 4)], c(NA, x, NA))
    }
  }

  test(vctrs_compact_rep_test_info())

  skip_if_no_altrep_3_6()
  test(vctrs_compact_rep_test_info_3_6())
})

test_that("subsetting with `[` and long vectors is allowed", {
  skip_on_32_bit()

  test <- function(test_info) {
    for (info in test_info) {
      x <- info$x
      ctor <- info$ctor

      size <- .Machine$integer.max + 1

      rep <- ctor(x, size)

      expect_identical(rep[c(size, size + 1)], c(x, NA))
    }
  }

  test(vctrs_compact_rep_test_info())

  skip_if_no_altrep_3_6()
  test(vctrs_compact_rep_test_info_3_6())
})

test_that("can serialize compact reps and retain compactness", {
  test <- function(test_info) {
    for (info in test_info) {
      x <- info$x
      ctor <- info$ctor

      rep <- ctor(x, 2L)

      serialized <- serialize(rep, NULL, version = 3)
      rep <- unserialize(serialized)

      expect_true(vec_is_vctrs_compact_rep_compact(rep))
    }
  }

  test(vctrs_compact_rep_test_info())

  skip_if_no_altrep_3_6()
  test(vctrs_compact_rep_test_info_3_6())
})

test_that("can construct size 0 compact reps", {
  test <- function(test_info) {
    for (info in test_info) {
      x <- info$x
      ctor <- info$ctor

      rep <- ctor(x, 0L)

      expect_length(rep, 0L)
      expect_identical(rep, x[0])
    }
  }

  test(vctrs_compact_rep_test_info())

  skip_if_no_altrep_3_6()
  test(vctrs_compact_rep_test_info_3_6())
})

test_that("can get the length of compact reps", {
  test <- function(test_info) {
    for (info in test_info) {
      x <- info$x
      ctor <- info$ctor

      rep <- ctor(x, 5L)

      expect_identical(length(rep), 5L)
      expect_true(vec_is_vctrs_compact_rep_compact(rep))
    }
  }

  test(vctrs_compact_rep_test_info())

  skip_if_no_altrep_3_6()
  test(vctrs_compact_rep_test_info_3_6())
})

test_that("can get the length of a long vector compact rep", {
  skip_on_32_bit()

  test <- function(test_info) {
    for (info in test_info) {
      x <- info$x
      ctor <- info$ctor

      rep <- ctor(x, .Machine$integer.max + 1)

      # Length is a double here!
      expect_identical(length(rep), .Machine$integer.max + 1)
      expect_true(vec_is_vctrs_compact_rep_compact(rep))
    }
  }

  test(vctrs_compact_rep_test_info())

  skip_if_no_altrep_3_6()
  test(vctrs_compact_rep_test_info_3_6())
})

# ------------------------------------------------------------------------------
context("test-altrep-rep-chr")

test_that("recycling named vectors generates ALTREP names which can still be repaired", {
  expect_named(vec_recycle(c(x = 1), 2), c("x", "x"))
})

# ------------------------------------------------------------------------------
context("test-altrep-rep-lgl")

test_that("`is_unspecified()` does not expand ALTREP compact rep lgls", {
  skip_if_no_altrep_3_6()

  x <- new_vctrs_compact_rep_lgl(TRUE, 2)
  y <- new_vctrs_compact_rep_lgl(NA, 2)

  expect_identical(is_unspecified(x), FALSE)
  expect_identical(is_unspecified(y), TRUE)

  expect_true(vec_is_vctrs_compact_rep_compact(x))
  expect_true(vec_is_vctrs_compact_rep_compact(y))
})

# ------------------------------------------------------------------------------
context("test-altrep-rep-dbl")

test_that("No_NA method works with NaN", {
  x <- new_vctrs_compact_rep_dbl(NaN, 2)
  expect_true(anyNA(x))
  expect_true(vec_is_vctrs_compact_rep_compact(x))
})

# ------------------------------------------------------------------------------
context("test-altrep-rep-int")
