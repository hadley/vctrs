
test_that("`numeric_version` is a vector (#723)", {
  w <- numeric_version("1.1")
  x <- numeric_version("0.1.0")
  y <- numeric_version("0.2.0")
  z <- c(x, y)

  expect_true(vec_is(x))

  expect_true(vec_equal(x, x))
  expect_false(vec_equal(x, y))
  expect_identical(vec_equal(y, z), c(FALSE, TRUE))

  expect_identical(vec_unique(z), z)
  expect_identical(vec_unique(c(y, z, x)), z[2:1])

  expect_identical(vec_compare(w, w), 0L)
  expect_identical(vec_compare(w, x), 1L)
  expect_identical(vec_compare(x, y), -1L)

  expect_identical(vec_sort(c(y, w, y, x)), c(x, y, y, w))
})

test_that("`numeric_version` falls back to base methods", {
  x <- utils::packageVersion("rlang")
  y <- utils::packageVersion("vctrs")
  z <- c(x, y)

  # `z` is a `list-of`-like type but slicing 1 element returns the
  # atomic type. To implement this in vctrs we'd need to provide a way
  # of customising the "wrapper" type for size > 1 vectors.
  expect_identical(vec_slice(z, 1:2), z)
  expect_identical(vec_slice(z, 1), x)
  expect_identical(vec_slice(z, 2), y)

  expect_identical(vec_c(x, y), z)
})

test_that("common type of data.table and data.frame is data.table", {
  # As data.table is not in Suggests, these checks are only run on the
  # devs' machines
  testthat_import_from("data.table", "data.table")

  expect_identical(
    vec_ptype2(data.table(x = TRUE), data.table(y = 2)),
    data.table(x = lgl(), y = dbl())
  )
  expect_identical(
    vec_ptype2(data.table(x = TRUE), data.frame(y = 2)),
    data.table(x = lgl(), y = dbl())
  )
  expect_identical(
    vec_ptype2(data.frame(y = 2), data.table(x = TRUE)),
    data.table(y = dbl(), x = lgl())
  )

  expect_identical(
    vec_cast(data.table(y = 2), data.table(x = TRUE, y = 1L)),
    data.table(x = NA, y = 2L)
  )
  expect_identical(
    vec_cast(data.frame(y = 2), data.table(x = TRUE, y = 1L)),
    data.table(x = NA, y = 2L)
  )
  expect_identical(
    vec_cast(data.table(y = 2), data.frame(x = TRUE, y = 1L)),
    data.frame(x = NA, y = 2L)
  )
})

test_that("data.table and tibble do not have a common type", {
  testthat_import_from("data.table", "data.table")

  expect_incompatible_df(
    vec_ptype_common(data.table(x = TRUE), tibble(y = 2)),
    tibble(x = lgl(), y = dbl())
  )
  expect_incompatible_df(
    vec_ptype_common(tibble(y = 2), data.table(x = TRUE)),
    tibble(y = dbl(), x = lgl())
  )

  expect_incompatible_df_cast(
    vec_cast(tibble(y = 2), data.table(x = TRUE, y = 1L)),
    data.frame(x = NA, y = 2L)
  )
  expect_incompatible_df_cast(
    vec_cast(data.table(y = 2), tibble(x = TRUE, y = 1L)),
    tibble(x = NA, y = 2L)
  )
})

test_that("can slice `ts` vectors", {
  x <- ts(1:3)
  expect_identical(vec_ptype(x), x[0])
  expect_identical(vec_slice(x, 2), x[2])
})

test_that("can concatenate `ts` vectors", {
  x <- ts(1:3)
  expect_identical(vec_c(x, x), c(x, x))

  df <- data_frame(x = x)
  expect_identical(vec_rbind(df, df), data_frame(x = c(x, x)))
})

test_that("`omit` class is numeric (#1160)", {
  x <- c(NA, 1:3, NA)
  omit <- attr(na.omit(x), "na.action")

  expect_identical(vec_ptype_common(omit, omit), structure(int(), class = "omit"))
  expect_identical(vec_ptype_common(1.5, omit), dbl())
  expect_identical(vec_ptype_common(omit, 1L), int())

  expect_identical(vec_cast_common(omit, omit), list(omit, omit))
  expect_identical(vec_cast_common(omit, 1L), list(unstructure(omit), 1L))
  expect_identical(vec_cast_common(1.5, omit), list(1.5, unstructure(as.double(omit))))

  expect_error(vec_cast(1L, omit), class = "vctrs_error_incompatible_type")
  expect_error(vec_cast(1.0, omit), class = "vctrs_error_incompatible_type")

  expect_identical(vec_slice(omit, 1), structure(1L, class = "omit"))
  expect_identical(vec_c(omit, omit), structure(c(1L, 5L, 1L, 5L), class = "omit"))
  expect_identical(vec_c(omit, omit, 10L), c(1L, 5L, 1L, 5L, 10L))

  expect_identical(vec_slice(x, omit), x[omit])
})

test_that("`exclude` class is numeric (#1160)", {
  x <- c(NA, 1:3, NA)
  exc <- attr(na.exclude(x), "na.action")

  expect_identical(vec_ptype_common(exc, exc), structure(int(), class = "exclude"))
  expect_identical(vec_ptype_common(1.5, exc), dbl())
  expect_identical(vec_ptype_common(exc, 1L), int())

  expect_identical(vec_cast_common(exc, exc), list(exc, exc))
  expect_identical(vec_cast_common(exc, 1L), list(unstructure(exc), 1L))
  expect_identical(vec_cast_common(1.5, exc), list(1.5, unstructure(as.double(exc))))

  expect_error(vec_cast(1L, exc), class = "vctrs_error_incompatible_type")
  expect_error(vec_cast(1.0, exc), class = "vctrs_error_incompatible_type")

  expect_identical(vec_slice(exc, 1), structure(1L, class = "exclude"))
  expect_identical(vec_c(exc, exc), structure(c(1L, 5L, 1L, 5L), class = "exclude"))
  expect_identical(vec_c(exc, exc, 10L), c(1L, 5L, 1L, 5L, 10L))

  expect_identical(vec_slice(x, exc), x[exc])
})
