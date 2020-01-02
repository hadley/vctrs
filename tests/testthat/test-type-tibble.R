context("test-type-tibble")

test_that("tibble beats data frame", {
  df <- new_data_frame()
  dt <- tibble::tibble()

  expect_s3_class(vec_ptype_common(dt, df), "tbl_df")
  expect_s3_class(vec_ptype_common(df, dt), "tbl_df")
})

test_that("can cast tibble to df and vice versa", {
  df <- new_data_frame()
  dt <- tibble::tibble()

  expect_equal(vec_cast(df, dt), dt)
  expect_equal(vec_cast(dt, df), df)
})

test_that("can't cast vector to tibble", {
  dt <- tibble::tibble()
  v <- logical()

  expect_error(vec_ptype2(v, dt), class = "vctrs_error_incompatible_type")
  expect_error(vec_ptype2(dt, v), class = "vctrs_error_incompatible_type")
  expect_error(vec_cast(v, dt), class = "vctrs_error_incompatible_cast")
})

test_that("no common type between list and tibble", {
  dt <- tibble::tibble()
  l <- list()

  expect_error(vec_ptype2(l, dt), class = "vctrs_error_incompatible_type")
  expect_error(vec_ptype2(dt, l), class = "vctrs_error_incompatible_type")
})

test_that("can cast a list of 1 or 0 row tibbles to a tibble", {
  dt1 <- tibble::tibble(x = numeric())
  dt2 <- tibble::tibble(x = 1)
  lst <- list(dt1, dt2)

  expect <- tibble::tibble(x = c(NA, 1))

  expect_equal(vec_cast(lst, dt1), expect)
})

test_that("vec_restore restores tibbles", {
  df1 <- tibble::tibble(x = 1:4)
  df2 <- vec_restore(vec_data(df1), df1)

  expect_s3_class(df2, "tbl_df")
})

test_that("tibbles have common tabular type with data frames", {
  tib1 <- tibble::as_tibble(mtcars)
  tib_ptype <- tibble::new_tibble(list(), nrow = 32L)
  expect_identical(tbl_ptype2(tib1, mtcars), tib_ptype)
  expect_identical(tbl_ptype2(mtcars, tib1), tib_ptype)

  tib2 <- tib1[1:3, ]
  expect_error(tbl_ptype2(tib1, tib2), class = "vctrs_error_incompatible_type")
  expect_error(tbl_ptype2(tib2, tib1), class = "vctrs_error_incompatible_type")
})

test_that("can cast data frame to tibble", {
  tib <- tibble::as_tibble(mtcars)
  expect_identical(tbl_cast(mtcars[3], tib), tibble::as_tibble(mtcars[3]))
  expect_error(tbl_cast(mtcars[1:3, 3], tib), class = "vctrs_error_incompatible_cast")
})


# grouped_df ---------------------------------------------------------

test_that("common type of dynamic gdf and df is dynamic gdf", {
  gdf <- dplyr::group_by(mtcars, cyl)
  expect_dynamically_grouped(vec_ptype_common(gdf, mtcars[1:3]), "cyl")
  expect_dynamically_grouped(vec_ptype_common(mtcars[1:3], gdf), "cyl")
  expect_dynamically_grouped(vec_ptype_common(gdf, mtcars["drat"]), "cyl")
})

test_that("common type of dynamic gdf and tib is dynamic gdf", {
  gdf <- dplyr::group_by(mtcars, cyl)
  tib <- dplyr::as_tibble(mtcars)
  expect_dynamically_grouped(vec_ptype_common(gdf, tib[1:3]), "cyl")
  expect_dynamically_grouped(vec_ptype_common(tib[1:3], gdf), "cyl")
  expect_dynamically_grouped(vec_ptype_common(gdf, tib["drat"]), "cyl")
})

test_that("common type of two dynamic gdfs takes union of groups", {
  gdf1 <- dplyr::group_by(mtcars, cyl)
  gdf2 <- dplyr::group_by(mtcars, vs, am)
  expect_dynamically_grouped(vec_ptype_common(gdf1, gdf1), "cyl")
  expect_dynamically_grouped(vec_ptype_common(gdf1, gdf2), c("cyl", "vs", "am"))
  expect_dynamically_grouped(vec_ptype_common(gdf2, gdf1), c("vs", "am", "cyl"))
})

test_that("common type of static gdf with df", {
  gdf <- dplyr::group_by(mtcars, cyl, am, .drop = FALSE)
  tib <- tibble::as_tibble(mtcars)

  gdata1 <- dplyr::group_data(vec_ptype_common(gdf, mtcars))
  gdata2 <- dplyr::group_data(vec_ptype_common(gdf, tib))
  gdata3 <- dplyr::group_data(vec_ptype_common(mtcars, gdf))
  gdata4 <- dplyr::group_data(vec_ptype_common(tib, gdf))

  exp <- dplyr::group_data(gdf)
  exp$.rows <- rep(list(int()), nrow(exp))

  expect_identical(gdata1, exp)
  expect_identical(gdata2, exp)
  expect_identical(gdata3, exp)
  expect_identical(gdata4, exp)
})

test_that("TODO: common type of static and dynamic gdf is still unimplemented", {
  static <- dplyr::group_by(mtcars, cyl, am, .drop = FALSE)
  dynamic <- dplyr::group_by(mtcars, am, cyl)
  expect_error(vec_ptype_common(static, dynamic), "unimplemented")
})

test_that("TODO: common type of two static gdfs is still unimplemented", {
  gdf1 <- dplyr::group_by(mtcars, cyl, am, .drop = FALSE)
  gdf2 <- dplyr::group_by(mtcars, am, cyl, .drop = FALSE)
  expect_error(vec_ptype_common(gdf1, gdf1), "unimplemented")
})

test_that("common tabular type of gdfs requires common size", {
  dyn_gdf1 <- dplyr::group_by(mtcars, cyl, am, .drop = TRUE)
  dyn_gdf2 <- dplyr::group_by(iris, Species, .drop = TRUE)
  expect_error(tbl_ptype2(dyn_gdf1, dyn_gdf2), class = "vctrs_error_incompatible_size")

  static_gdf1 <- dplyr::group_by(mtcars, cyl, am, .drop = FALSE)
  static_gdf2 <- dplyr::group_by(mtcars[1:3, ], cyl, am, .drop = FALSE)
  expect_error(tbl_ptype2(static_gdf1, static_gdf2), class = "vctrs_error_incompatible_size")
})

test_that("common tabular type of dynamic gdfs", {
  dyn_gdf1 <- dplyr::group_by(mtcars, cyl, am, .drop = TRUE)
  dyn_gdf2 <- dplyr::group_by(mtcars, vs, .drop = TRUE)
  expect_identical(
    tbl_ptype2(dyn_gdf1, dyn_gdf2),
    empty_dynamic_gdf(32L, row_names = row_names(mtcars))
  )
})

test_that("common tabular type of static gdfs", {
  static_gdf1 <- dplyr::group_by(mtcars, cyl, am, .drop = FALSE)
  static_gdf2 <- dplyr::group_by(mtcars, vs, .drop = FALSE)
  expect_error(tbl_ptype2(static_gdf1, static_gdf2), "TODO")
})

test_that("common tabular type of dynamic gdf with df and tib", {
  dyn_gdf <- dplyr::group_by(mtcars, cyl, am, .drop = TRUE)
  df <- mtcars[10]
  tib <- tibble::as_tibble(mtcars[11])

  empty_mtcars_gdf <- empty_dynamic_gdf(32L, row_names = row_names(mtcars))

  expect_identical(tbl_ptype2(df, dyn_gdf), empty_mtcars_gdf)
  expect_identical(tbl_ptype2(dyn_gdf, df), empty_mtcars_gdf)

  row.names(dyn_gdf) <- NULL

  expect_identical(tbl_ptype2(tib, dyn_gdf), empty_dynamic_gdf(32L))
  expect_identical(tbl_ptype2(dyn_gdf, tib), empty_dynamic_gdf(32L))
})

test_that("common tabular type of static gdf with df and tib", {

  static_gdf <- dplyr::group_by(mtcars, cyl, am, .drop = FALSE)
  empty_static_gdf <- dplyr::new_grouped_df(
    new_data_frame(n = 32L),
    dplyr::group_data(static_gdf)
  )
  df <- mtcars[10]
  tib <- tibble::as_tibble(mtcars[11])

  expect_identical(tbl_ptype2(df, static_gdf), empty_static_gdf)
  expect_identical(tbl_ptype2(static_gdf, df), empty_static_gdf)

  row.names(static_gdf) <- NULL

  expect_identical(tbl_ptype2(tib, static_gdf), empty_static_gdf)
  expect_identical(tbl_ptype2(static_gdf, tib), empty_static_gdf)
})

test_that("static groups are proxied and restored", {
  static <- dplyr::group_by(mtcars, cyl, am, .drop = FALSE)

  proxy <- vec_proxy(static)
  proxy <- proxy[1:3, c("cyl", "am", "disp", "vctrs::virtual_cols")]

  expect_identical(
    proxy$`vctrs::virtual_cols`,
    data_frame(`dplyr::grouped_df_groups` = c(4L, 4L, 2L))
  )

  out <- vec_restore(proxy, static)

  gdata <- dplyr::group_data(out)
  gtable <- gdata[-length(gdata)]

  expect_identical(gtable, group_table(static))
  expect_identical(
    gdata$.rows,
    vec_match_all(gtable, mtcars[1:3, c("cyl", "am")])
  )
})

test_that("handles multiple id columns in the static gdf proxy", {
  static <- dplyr::group_by(mtcars, cyl, am, .drop = FALSE)

  proxy <- vec_proxy(static)
  proxy <- proxy[, c(2, 9, 12)]
  proxy$`vctrs::virtual_cols` <- new_data_frame(rep(proxy$`vctrs::virtual_cols`, 2))

  # Allowed because they are congruent
  expect_identical(
    dplyr::group_data(vec_restore(proxy, static)),
    dplyr::group_data(static)
  )

  proxy$`vctrs::virtual_cols`[[1]][[4]] <- 100L
  expect_error(
    vec_restore(proxy, static),
    "incongruent"
  )
})

test_that("dynamic groups are recomputed after restoration", {
  gdf <- dplyr::group_by(mtcars, cyl)
  out <- vec_restore(vec_proxy(gdf), to = gdf)
  expect_identical(
    unstructure(dplyr::group_data(gdf)),
    unstructure(dplyr::group_data(out))
  )
})

test_that("drop is restored", {
  df_true <- dplyr::group_by(mtcars, cyl, .drop = TRUE)
  proxy_true <- vec_proxy(df_true)
  drop_true <- vec_restore(proxy_true, df_true)

  df_false <- dplyr::group_by(mtcars, cyl, .drop = FALSE)
  proxy_false <- vec_proxy(df_false)
  drop_false <- vec_restore(proxy_false, df_false)

  expect_true(dplyr::group_by_drop_default(drop_true))
  expect_false(dplyr::group_by_drop_default(drop_false))
})

test_that("can cast df to dynamic gdf", {
  gdf <- dplyr::group_by(mtcars, cyl)
  expect_equal(
    vec_cast(mtcars[1:3], gdf),
    dplyr::group_by(vec_cast(mtcars[1:3], mtcars), cyl)
  )
  expect_equal(
    vec_cast(dplyr::group_by(mtcars[8:10], vs, am), gdf),
    dplyr::group_by(vec_cast(mtcars[8:10], mtcars), cyl)
  )

  expect_true(dplyr::group_by_drop_default(vec_cast(mtcars, gdf)))
})

test_that("can cast df to static gdf", {
  static <- dplyr::group_by(mtcars, cyl, .drop = FALSE)
  out <- vec_cast(mtcars, static)
  expect_false(dplyr::group_by_drop_default(out))
})

test_that("can cast dynamic gdf to static gdf", {
  static <- dplyr::group_by(mtcars, cyl, .drop = FALSE)
  dynamic <- dplyr::group_by(mtcars, vs, am, .drop = TRUE)

  out <- vec_cast(dynamic, static)
  expect_false(dplyr::group_by_drop_default(out))
  expect_identical(dplyr::group_data(out), dplyr::group_data(static))
})

test_that("can cast static gdf to static gdf", {
  static_x <- dplyr::group_by(mtcars, cyl, .drop = FALSE)
  static_to <- dplyr::group_by(mtcars, vs, am, .drop = FALSE)

  out <- vec_cast(static_x, static_to)
  expect_false(dplyr::group_by_drop_default(out))
  expect_identical(dplyr::group_data(out), dplyr::group_data(static_to))
})

test_that("can cast static gdf to static gdf with different sizes", {
  static_x <- dplyr::group_by(mtcars[1:15, ], cyl, .drop = FALSE)
  static_to <- dplyr::group_by(mtcars, vs, am, .drop = FALSE)
  gdata_to <- dplyr::group_data(static_to)
  gtable_to <- gdata_to[-length(gdata_to)]

  out <- vec_cast(static_x, static_to)
  gdata_out <- dplyr::group_data(out)
  gtable_out <- gdata_out[-length(gdata_out)]

  expect_identical(gtable_out, gtable_to)
  expect_identical(
    gdata_out$.rows,
    vec_match_all(gtable_to, static_x[names(gtable_to)])
  )
})

test_that("can table-cast to static gdf", {
  static_gdf <- dplyr::group_by(mtcars, cyl, .drop = FALSE)

  expect_static_cast <- function(x) {
    gdata <- dplyr::group_data(static_gdf)
    gtable <- gdata[-length(gdata)]
    gdata$.rows <- vec_match_all(gtable, x[names(gtable)])
    expect_identical(tbl_cast(x, static_gdf), dplyr::new_grouped_df(x, gdata))
  }

  expect_static_cast(mtcars[1:3])
  expect_static_cast(tibble::as_tibble(mtcars[1:3]))
  expect_static_cast(dplyr::group_by(mtcars, vs, am, .drop = TRUE))

  expect_error(tbl_cast(mtcars[10:11], static_gdf), "externally grouped")
  expect_error(tbl_cast(tibble::as_tibble(mtcars[10:11]), static_gdf), "externally grouped")
})

test_that("can table-cast to dynamic gdf", {
  expect_dyn_cast <- function(x, to) {
    out <- tbl_cast(x, to)
    expect_is(out, "grouped_df")
    expect_true(!is_static_grouped_df(out))
    expect_identical(dplyr::group_vars(out), chr())
  }
  dyn_gdf <- dplyr::group_by(mtcars, cyl, .drop = TRUE)
  expect_dyn_cast(mtcars[1:3], dyn_gdf)
  expect_dyn_cast(mtcars[10:11], dyn_gdf)
  expect_dyn_cast(tibble::as_tibble(mtcars[10:11]), dyn_gdf)

  gdf <- dplyr::group_by(mtcars, vs, am, .drop = TRUE)
  expect_identical(tbl_cast(gdf, dyn_gdf), gdf)

  static_gdf <- dplyr::group_by(mtcars, vs, am, .drop = FALSE)
  expect_error(tbl_cast(static_gdf, dyn_gdf), "explicitly grouped")
})

test_that("can rbind grouped-dfs", {
  gdf <- dplyr::group_by(mtcars, cyl)
  exp <- dplyr::group_by(vec_rbind(mtcars, mtcars), cyl)
  exp_data <- unstructure(dplyr::group_data(exp))

  out <- vec_rbind(gdf, gdf)
  expect_grouped(out, "cyl")
  expect_identical(unstructure(dplyr::group_data(out)), exp_data)

  out <- vec_rbind(gdf, mtcars)
  expect_grouped(out, "cyl")
  expect_identical(unstructure(dplyr::group_data(out)), exp_data)

  out <- vec_rbind(mtcars, gdf)
  expect_grouped(out, "cyl")
  expect_identical(unstructure(dplyr::group_data(out)), exp_data)

  gdf2 <- dplyr::group_by(mtcars, vs, am)
  out <- vec_rbind(gdf2, mtcars, gdf)
  exp_data <- dplyr::group_by(vec_rbind(mtcars, mtcars, mtcars), cyl, vs, am)
  exp_data <- unstructure(dplyr::group_data(exp_data))
  expect_grouped(out, c("cyl", "vs", "am"))
  expect_identical(unstructure(dplyr::group_data(out)), exp_data)
})

test_that("can cbind dynamic grouped-dfs", {
  gdf <- dplyr::group_by(mtcars, cyl)
  exp <- dplyr::group_by(vec_cbind(mtcars, mtcars), cyl...2, cyl...13)
  exp_data <- unstructure(dplyr::group_data(exp))

  expect_cbinded <- function(out, groups) {
    repaired_names <- vec_as_names(rep(names(mtcars), 2), repair = "unique")
    expect_grouped(out, groups, names = repaired_names)
  }
  expect_cbinded(vec_cbind(gdf, gdf), groups = c("cyl...2", "cyl...13"))
  expect_cbinded(vec_cbind(gdf, mtcars), groups = c("cyl...2"))
  expect_cbinded(vec_cbind(mtcars, gdf), groups = c("cyl...13"))

  out <- vec_cbind(
    dplyr::group_by(mtcars[1:3], cyl),
    dplyr::group_by(mtcars, am, vs)
  )
  expect_named(out, names(vec_cbind(mtcars[1:3], mtcars)))
  expect_identical(dplyr::group_vars(out), c("cyl...2", "vs", "am"))

  gdf1 <- dplyr::group_by(mtcars[1:3], cyl)
  gdf2 <- dplyr::group_by(mtcars[8:10], vs, am)
  out <- vec_cbind(
    gdf1,
    mtcars[4:6],
    gdf2
  )
  expect_identical(dplyr::group_vars(out), c("cyl", "vs", "am"))
  expect_named(out, names(mtcars)[c(1:3, 4:6, 8:10)])
})

test_that("can cbind static gdfs", {
  df <- mtcars[1:3]
  static_gdf <- dplyr::group_by(df, cyl, .drop = FALSE)
  out <- vec_cbind(static_gdf, df)

  exp <- vec_cbind(df, df)
  exp <- dplyr::new_grouped_df(exp, dplyr::group_data(static_gdf))

  expect_identical(out, exp)
})

test_that("can concatenate grouped-dfs", {
  out <- vec_c(
    dplyr::group_by(mtcars, cyl),
    mtcars,
    dplyr::group_by(mtcars, cyl, am)
  )
  expect_is(out, "grouped_df")
  expect_identical(dplyr::group_vars(out), c("cyl", "am"))
})

test_that("can slice grouped-dfs", {
  out <- vec_slice(dplyr::group_by(mtcars, cyl), 0)
  expect_identical(dplyr::group_vars(out), "cyl")
})

test_that("grouped columns are equal to ungrouped ones", {
  gdf <- dplyr::group_by(mtcars, cyl)
  expect_identical(vec_equal(gdf, mtcars), rep(TRUE, nrow(mtcars)))
})

test_that("can restore empty dynamic gdf", {
  expect_identical(
    vec_restore(data.frame(), dplyr::group_by(mtcars, cyl)),
    empty_dynamic_gdf(0L)
  )
})

test_that("row names of dynamic gdf are preserved upon proxying and restoration", {
  df <- mtcars
  row.names(df) <- 101:132
  dyn_gdf <- dplyr::group_by(df, cyl, .drop = TRUE)
  dyn_gdf_proxy <- vec_proxy(dyn_gdf)

  out <- vec_restore(dyn_gdf_proxy, dplyr::group_by(mtcars, cyl, .drop = TRUE))
  expect_identical(row_names(out), 101:132)

  empty_df <- mtcars
  row.names(empty_df) <- 101:132
  empty_dyn_gdf <- dplyr::group_by(empty_df, cyl, .drop = TRUE)
  empty_dyn_gdf_proxy <- vec_proxy(empty_dyn_gdf)

  out <- vec_restore(empty_dyn_gdf_proxy, dplyr::group_by(mtcars, cyl, .drop = TRUE))
  expect_identical(row_names(out), 101:132)
})

test_that("row names of static gdf are preserved upon proxying and restoration", {
  df <- mtcars
  row.names(df) <- 101:132
  static_gdf <- dplyr::group_by(df, cyl, .drop = FALSE)
  static_gdf_proxy <- vec_proxy(static_gdf)

  out <- vec_restore(static_gdf_proxy, dplyr::group_by(mtcars, cyl, .drop = FALSE))
  expect_identical(out, static_gdf)
})
