context("test-type-data-frame")

# printing ----------------------------------------------------------------

test_that("data frames print nicely", {
  expect_equal(vec_ptype_abbr(mtcars), "df[,11]")

  expect_known_output(
    file = test_path("test-type-data-frame.txt"),
    {
      cat("mtcars:\n")
      vec_ptype_show(mtcars)
      cat("\n")
      cat("iris:\n")
      vec_ptype_show(iris)
    }
  )
})

test_that("embedded data frames print nicely", {
  df <- data.frame(x = 1:3)
  df$a <- data.frame(a = 1:3, b = letters[1:3])
  df$b <- list_of(1, 2, 3)
  df$c <- as_list_of(split(data.frame(x = 1:3, y = letters[1:3]), 1:3))

  expect_known_output(
    file = test_path("test-type-data-frame-embedded.txt"),
    {
      vec_ptype_show(df)
    }
  )
})

# coercing ----------------------------------------------------------------

test_that("data frame only combines with other data frames or NULL", {
  dt <- data.frame(x = 1)
  expect_equal(vec_ptype_common(dt, NULL), vec_ptype(dt))
  expect_error(vec_ptype_common(dt, 1:10), class = "vctrs_error_incompatible_type")
})

test_that("data frame takes max of individual variables", {
  dt1 <- data.frame(x = FALSE, y = 1L)
  dt2 <- data.frame(x = 1.5, y = 1.5)

  expect_equal(vec_ptype_common(dt1, dt2), vec_ptype_common(dt2))
})

test_that("data frame combines variables", {
  dt1 <- data.frame(x = 1)
  dt2 <- data.frame(y = 1)

  dt3 <- max(dt1, dt2)
  expect_equal(
    vec_ptype_common(dt1, dt2),
    vec_ptype_common(data.frame(x = double(), y = double()))
  )
})

test_that("empty data frame still has names", {
  df <- data.frame()
  out <- vec_ptype_common(df, df)

  expect_equal(names(out), character())
})

# casting -----------------------------------------------------------------

test_that("safe casts work as expected", {
  df <- data.frame(x = 1, y = 0)

  expect_equal(vec_cast(NULL, df), NULL)
  expect_equal(vec_cast(df, df), df)

  expect_equal(vec_cast(data.frame(x = TRUE, y = FALSE), df), df)
})

test_that("warn about lossy coercions", {
  df1 <- data.frame(x = 1, y = 1)
  df2 <- data.frame(x = c("a", 1), stringsAsFactors = FALSE)

  expect_lossy(vec_cast(df1, df1[1]), df1[1], x = df1, to = df1[1])
  expect_lossy(vec_cast(df2, df1), data.frame(x = dbl(NA, 1), y = dbl(NA, NA)), x = chr(), to = dbl())

  out <-
    allow_lossy_cast(
      allow_lossy_cast(
        vec_cast(df2, df1),
        chr(), dbl()
      ),
      df2, df1
    )

  expect_identical(out, data.frame(x = dbl(NA, 1), y = dbl(NA, NA)))
})

test_that("invalid cast generates error", {
  expect_error(vec_cast(1L, data.frame()), class = "vctrs_error_incompatible_cast")
})

test_that("column order matches type", {
  df1 <- data.frame(x = 1, y = "a")
  df2 <- data.frame(x = TRUE, z = 3)

  df3 <- vec_cast(df2, vec_ptype_common(df1, df2))
  expect_named(df3, c("x", "y", "z"))
})

test_that("casts preserve outer class", {
  df <- data.frame(x = 1)
  dt <- tibble::tibble(x = 1)

  expect_s3_class(vec_cast(df, dt), "tbl_df")
  expect_s3_class(vec_cast(dt, df), "data.frame")
})

test_that("restore generates correct row/col names", {
  df1 <- data.frame(x = NA, y = 1:4, z = 1:4)
  df1$x <- data.frame(a = 1:4, b = 1:4)

  df2 <- vec_restore(lapply(df1[1:3], vec_slice, 1:2), df1)

  expect_named(df2, c("x", "y", "z"))
  expect_equal(.row_names_info(df2), -2)
})

test_that("restore keeps automatic row/col names", {
  df1 <- data.frame(x = NA, y = 1:4, z = 1:4)
  df1$x <- data.frame(a = 1:4, b = 1:4)

  df2 <- vec_restore(df1, df1)

  expect_named(df2, c("x", "y", "z"))
  expect_equal(.row_names_info(df2), -4)
})

test_that("cast to empty data frame preserves number of rows", {
  out <- vec_cast(new_data_frame(n = 10L), new_data_frame())
  expect_equal(nrow(out), 10L)
})

test_that("can cast unspecified to data frame", {
  df <- data.frame(x = 1, y = 2L)
  expect_identical(vec_cast(unspecified(3), df), vec_init(df, 3))
})

test_that("can cast list of data frames to data frame", {
  df <- data.frame(x = 1, y = 2L)
  expect_equal(vec_cast(list(df, df), df), vec_slice(df, c(1, 1)))
})

test_that("can only cast list of data frames to data frame if they are all size 1", {
  df <- data.frame(x = 1:2)
  expect_error(vec_cast(list(df), df), class = "vctrs_error_cast_lossy")
})

test_that("can restore lists with empty names", {
  expect_identical(vec_restore(list(), data.frame()), data.frame())
})

test_that("can restore subclasses of data frames", {
  expect_identical(vec_restore(list(), subclass(data.frame())), subclass(data.frame()))
  local_methods(
    vec_restore.vctrs_foobar = function(x, to, ..., i) "dispatched"
  )
  expect_identical(vec_restore(list(), subclass(data.frame())), "dispatched")
})

test_that("df_as_dataframe() checks for names", {
  x <- data_frame(1)
  y <- data_frame(2)
  expect_error(vec_cast_common(x, y), "must have names")
})

test_that("can slice AsIs class", {
  df <- data.frame(x = I(1:3), y = I(list(4, 5, 6)))
  expect_identical(vec_slice(df, 2:3), unrownames(df[2:3, ]))
})

test_that("common tabular type of data frames is a data frame of the same size", {
  df1 <- data_frame(x = 1:3)
  df2 <- data_frame(y = 4:6)
  expect_identical(tbl_ptype2(df1, df2), new_data_frame(n = 3L))
  expect_identical(tbl_ptype2(df2, df1), new_data_frame(n = 3L))
})

test_that("data frames of incompatible size don't have a common tabular type", {
  df1 <- data_frame(w = 1:3)
  df2 <- data_frame(x = 1:4)
  df3 <- data_frame(y = 1)
  df4 <- data_frame(z = 4:6)
  df_ptype <- tbl_ptype(df1)

  expect_error(tbl_ptype2(df1, df2), class = "vctrs_error_incompatible_size")
  expect_error(tbl_ptype2(df2, df1), class = "vctrs_error_incompatible_size")

  expect_identical(tbl_ptype2(df1, df3), df_ptype)
  expect_identical(tbl_ptype2(df3, df1), df_ptype)

  expect_identical(tbl_ptype2(df1, df4), df_ptype)
  expect_identical(tbl_ptype2(df4, df1), df_ptype)
})

test_that("data frames with incompatible row names don't have a common tabular type", {
  df1 <- data_frame(x = 1:3)
  df2 <- data_frame(y = 4:6)
  row.names(df1) <- letters[1:3]
  row.names(df2) <- letters[4:6]

  expect_error(tbl_ptype2(df1, df2), "row names must be compatible")
  expect_error(tbl_ptype2(df2, df1), "row names must be compatible")

  row.names(df2) <- row.names(df1)
  expect_identical(tbl_ptype2(df1, df2), tbl_ptype(df1))
  expect_identical(tbl_ptype2(df2, df1), tbl_ptype(df1))

  df3 <- data_frame(z = 1L)
  row.names(df3) <- "a"
  expect_error(tbl_ptype2(df1, df3), "row names must be compatible")
  expect_error(tbl_ptype2(df3, df1), "row names must be compatible")
})

test_that("can cast to tabular type", {
  expect_identical(tbl_cast(mtcars, mtcars), mtcars)
  expect_identical(tbl_cast(mtcars[1:3], mtcars), mtcars[1:3])

  new_sub_df <- function(x) {
    structure(x, class = c("df_subclass", "data.frame"))
  }
  local_methods(
    tbl_ptype2.df_subclass = function(x, y, ...) {
      new_sub_df(tbl_ptype2.data.frame.data.frame(x, y))
    }
  )

  sub_df <- new_sub_df(mtcars)
  expect_identical(tbl_cast(sub_df, mtcars), mtcars)
  expect_error(tbl_cast(mtcars, sub_df), class = "vctrs_error_incompatible_cast")
})

test_that("can cast to data frame tabular type", {
  df1 <- data_frame(w = 1:3, x = 4:6)
  df2 <- data_frame(y = 7, z = 8)

  expect_error(tbl_cast(df1, df2), class = "vctrs_error_recycle_incompatible_size")
  expect_identical(tbl_cast(df2, df1), vec_recycle(df2, 3L))
})
