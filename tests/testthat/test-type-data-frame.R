context("test-type-data-frame")

# printing ----------------------------------------------------------------

test_that("data frames print nicely", {
  expect_equal(vec_ptype_abbr(mtcars), "df[,11]")

  verify_output(test_path("test-type-data-frame.txt"), {
    vec_ptype_show(mtcars)
    vec_ptype_show(iris)
  })
})

test_that("embedded data frames print nicely", {
  df <- data.frame(x = 1:3)
  df$a <- data.frame(a = 1:3, b = letters[1:3])
  df$b <- list_of(1, 2, 3)
  df$c <- as_list_of(split(data.frame(x = 1:3, y = letters[1:3]), 1:3))

  verify_output(test_path("test-type-data-frame-embedded.txt"), {
    vec_ptype_show(df)
  })
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

test_that("combining data frames with foreign classes uses fallback", {
  foo <- foobar(data.frame())
  df <- data.frame()

  # Same type fallback
  expect_identical(vec_ptype_common(foo, foo, foo), foo)
  expect_error(vec_ptype_common(foo, foo, df, foo), class = "vctrs_error_incompatible_type")

  expect_identical(expect_df_fallback(vec_ptype2_fallback(foo, df)), new_fallback_df(df, c("vctrs_foobar", "data.frame")))
  expect_identical(expect_df_fallback(vec_ptype2_fallback(df, foo)), new_fallback_df(df, c("data.frame", "vctrs_foobar")))
  expect_identical(expect_df_fallback(vec_ptype_common_fallback(foo, df)), df)
  expect_identical(expect_df_fallback(vec_ptype_common_fallback(df, foo)), df)

  cnds <- list()
  withCallingHandlers(
    warning = function(cnd) {
      cnds <<- append(cnds, list(cnd))
      invokeRestart("muffleWarning")
    },
    expect_identical(
      vec_ptype_common_fallback(foo, df, foo, foo),
      df
    )
  )

  # There should be only one warning even if many fallbacks
  expect_length(cnds, 1)
  expect_is(cnds[[1]], "warning")
  expect_match(cnds[[1]]$message, "falling back to <data.frame>")

  expect_identical(
    expect_df_fallback(vec_cbind(foobar(data.frame(x = 1)), data.frame(y = 2))),
    data.frame(x = 1, y = 2)
  )
  expect_identical(
    expect_df_fallback(vec_rbind(foo, data.frame(), foo)),
    df
  )

  verify_errors({
    foo <- structure(mtcars[1:3], class = c("foo", "data.frame"))
    bar <- structure(mtcars[4:6], class = c("bar", "data.frame"))
    baz <- structure(mtcars[7:9], class = c("baz", "data.frame"))
    expect_warning(vec_ptype_common_fallback(foo, bar, baz))
    expect_warning(vec_ptype_common_fallback(foo, baz, bar, baz, foo, bar))

    expect_df_fallback(invisible(vec_rbind(foo, data.frame(), foo)))

    expect_df_fallback(invisible(vec_cbind(foo, data.frame(x = 1))))
    expect_df_fallback(invisible(vec_cbind(foo, data.frame(x = 1), bar)))
  })
})


# casting -----------------------------------------------------------------

test_that("safe casts work as expected", {
  df <- data.frame(x = 1, y = 0)

  expect_equal(vec_cast(NULL, df), NULL)
  expect_equal(vec_cast(df, df), df)

  expect_equal(vec_cast(data.frame(x = TRUE, y = FALSE), df), df)
})

test_that("warn about lossy coercions", {
  df1 <- data.frame(x = factor("foo"), y = 1)
  df2 <- data.frame(x = factor("bar"))

  expect_lossy(vec_cast(df1, df1[1]), df1[1], x = df1, to = df1[1])

  expect_lossy(
    vec_cast(df1[1], df2),
    data.frame(x = factor(NA, levels = "bar")),
    x = factor("foo"),
    to = factor("bar")
  )

  out <-
    allow_lossy_cast(
      allow_lossy_cast(
        vec_cast(df1, df2),
        factor("foo"), factor("bar")
      ),
      df1, df2
    )

  expect_identical(out, data.frame(x = factor(NA, levels = "bar")))
})

test_that("invalid cast generates error", {
  expect_error(vec_cast(1L, data.frame()), class = "vctrs_error_incompatible_type")
})

test_that("column order matches type", {
  df1 <- data.frame(x = 1, y = "a")
  df2 <- data.frame(x = TRUE, z = 3)

  df3 <- vec_cast(df2, vec_ptype_common(df1, df2))
  expect_named(df3, c("x", "y", "z"))
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
  out <- vec_cast(new_data_frame(.size = 10L), new_data_frame())
  expect_equal(nrow(out), 10L)
})

test_that("can cast unspecified to data frame", {
  df <- data.frame(x = 1, y = 2L)
  expect_identical(vec_cast(unspecified(3), df), vec_init(df, 3))
})

test_that("cannot cast list to data frame", {
  df <- data.frame(x = 1, y = 2L)
  expect_error(vec_cast(list(df, df), df), class = "vctrs_error_incompatible_type")
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

test_that("df_cast() checks for names", {
  x <- data_frame(1)
  y <- data_frame(2)
  expect_error(vec_cast_common(x, y), "must have names")
})

test_that("casting to and from data frame preserves row names", {
  out <- vec_cast(mtcars, unrownames(mtcars))
  expect_identical(row.names(out), row.names(mtcars))

  out <- vec_cast(out, unrownames(mtcars))
  expect_identical(row.names(out), row.names(mtcars))
})


# new_data_frame ----------------------------------------------------------

test_that("can construct an empty data frame", {
  expect_identical(new_data_frame(), data.frame())
})

test_that("can validly set the number of rows when there are no columns", {
  expect <- structure(
    list(),
    class = "data.frame",
    row.names = .set_row_names(2L),
    names = character()
  )

  expect_identical(new_data_frame(.size = 2L), expect)
})

test_that("can add additional classes", {
  expect_s3_class(new_data_frame(.class = "foobar"), "foobar")
  expect_s3_class(new_data_frame(.class = c("foo", "bar")), c("foo", "bar"))
})

test_that("can add additional attributes", {
  expect <- data.frame()
  attr(expect, "foo") <- "bar"
  attr(expect, "a") <- "b"

  expect_identical(new_data_frame(foo = "bar", a = "b"), expect)
})

test_that("size is pulled from first column if not supplied", {
  x <- new_data_frame(list(x = 1:5, y = 1:6))
  expect_identical(.row_names_info(x, type = 1), -5L)
})

test_that("can construct a data frame without column names", {
  expect_named(new_data_frame(list(1, 2)), NULL)
})

test_that("the names on an empty data frame are an empty character vector", {
  expect_identical(names(new_data_frame()), character())
})

test_that("class attribute", {
  expect_identical(
    class(new_data_frame(list(a = 1))),
    "data.frame"
  )
  expect_identical(
    class(new_data_frame(list(a = 1), .class = "tbl_df")),
    c("tbl_df", "data.frame")
  )
  expect_identical(
    class(new_data_frame(list(a = 1), .class = c("tbl_df", "tbl", "data.frame"))),
    c("tbl_df", "tbl", "data.frame", "data.frame")
  )
  expect_identical(
    class(new_data_frame(list(a = 1), .class = "foo_frame")),
    c("foo_frame", "data.frame")
  )
})

test_that("attributes with special names are merged", {
  expect_identical(
    names(new_data_frame(list(a = 1))),
    "a"
  )

  expect_identical(
    names(new_data_frame(list(a = 1), names = "name")),
    "name"
  )

  expect_identical(
    names(new_data_frame(list(1), names = "name")),
    "name"
  )

  expect_identical(
    attr(new_data_frame(list()), "row.names"),
    integer()
  )

  expect_identical(
    .row_names_info(new_data_frame(list(), .size = 3L)),
    -3L
  )

  expect_error(new_data_frame(list(), .size = 1L, row.names = 1:3), ".")

  expect_identical(
    .row_names_info(new_data_frame(list(), .size = 3L, row.names = 1:3)),
    3L
  )

  expect_identical(
    .row_names_info(new_data_frame(list(), .size = 3L, row.names = c(NA, -3L))),
    -3L
  )

  expect_identical(
    attr(new_data_frame(list(), .size = 1L, row.names = "rowname"), "row.names"),
    "rowname"
  )
})

test_that("`.size` and `row.names` (#894)", {
  # Can omit .size if row.names attribute is given
  expect_identical(
    row.names(new_data_frame(list(), row.names = "rowname")),
    "rowname"
  )
  expect_identical(
    attr(new_data_frame(list(), row.names = 2L), "row.names"),
    2L
  )
  expect_identical(
    row.names(new_data_frame(list(), row.names = chr())),
    chr()
  )
})

test_that("`.x` must be a list", {
  expect_error(new_data_frame(1), "`.x` must be a list")
})

test_that("if supplied, `.size` must be an integer of size 1", {
  expect_error(new_data_frame(.size = c(1L, 2L)), "must be an integer of size 1")
  expect_error(new_data_frame(.size = "x"), "must be an integer of size 1")
})

test_that("`.class` must be a character vector", {
  expect_error(new_data_frame(.class = 1), "must be NULL or a character vector")
})

test_that("flat width is computed", {
  df_flat_width <- function(x) {
    .Call(vctrs_df_flat_width, x)
  }
  expect_identical(df_flat_width(mtcars), ncol(mtcars))

  df <- tibble(x = 1, y = tibble(x = 2, y = tibble(x = 3), z = 4), z = 5)
  expect_identical(df_flat_width(df), 5L)
})

test_that("can flatten data frames", {
  df_flatten <- function(x) {
    .Call(vctrs_df_flatten, x)
  }
  expect_identical(df_flatten(mtcars), as.data.frame(as.list(mtcars)))

  df <- tibble(x = 1, y = tibble(x = 2, y = tibble(x = 3), z = 4), z = 5)
  expect_identical(df_flatten(df), new_data_frame(list(x = 1, x = 2, x = 3, z = 4, z = 5)))
})

test_that("new_data_frame() zaps existing attributes", {
  struct <- structure(list(), foo = 1)
  expect_identical(
    attributes(new_data_frame(struct)),
    attributes(new_data_frame(list())),
  )
  expect_identical(
    attributes(new_data_frame(struct, bar = 2)),
    attributes(new_data_frame(list(), bar = 2)),
  )
})

test_that("data frame fallback handles column types (#999)", {
  df1 <- foobar(data.frame(x = 1))
  df2 <- foobar(data.frame(x = 1, y = 2))
  df3 <- foobar(data.frame(x = "", y = 2))

  common <- foobar(data.frame(x = dbl(), y = dbl()))
  expect_identical(vec_ptype2(df1, df2), common)
  expect_identical(vec_ptype2(df2, df1), common)

  expect_error(
    vec_ptype2(df1, df3),
    class = "vctrs_error_incompatible_type"
  )
  expect_error(
    vec_ptype2(df3, df1),
    class = "vctrs_error_incompatible_type"
  )

  expect_identical(
    vec_cast(df1, df2),
    foobar(data.frame(x = 1, y = na_dbl))
  )
  expect_error(
    vec_cast(df2, df1),
    class = "vctrs_error_cast_lossy"
  )

  expect_identical(
    vec_rbind(df1, df2),
    foobar(data.frame(x = c(1, 1), y = c(NA, 2)))
  )

  # Attributes are not restored
  df1_attrib <- foobar(df1, foo = "foo")
  df2_attrib <- foobar(df2, bar = "bar")
  exp <- data.frame(x = c(1, 1), y = c(NA, 2))
  out <- expect_df_fallback(vec_rbind(df1_attrib, df2_attrib))
  expect_identical(out, exp)

  out <- with_methods(
    `[.vctrs_foobar` = function(x, i, ...) structure(NextMethod(), dispatched = TRUE),
    vec_rbind(df1_attrib, df2_attrib)
  )
  expect_identical(out, foobar(exp, dispatched = TRUE))
})

test_that("data frame output is informative", {
  verify_output(test_path("error", "test-type-data-frame.txt"), {
    "# combining data frames with foreign classes uses fallback"
    foo <- structure(mtcars[1:3], class = c("foo", "data.frame"))
    bar <- structure(mtcars[4:6], class = c("bar", "data.frame"))
    baz <- structure(mtcars[7:9], class = c("baz", "data.frame"))
    vec_ptype_common_fallback(foo, bar, baz)
    vec_ptype_common_fallback(foo, baz, bar, baz, foo, bar)

    invisible(vec_rbind(foo, data.frame(), foo))
    invisible(vec_rbind(foo, baz, bar, baz, foo, bar))

    invisible(vec_cbind(foo, data.frame(x = 1)))
    invisible(vec_cbind(foo, data.frame(x = 1), bar))
  })
})
